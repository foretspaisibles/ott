;;;; continuation.lisp — Tests for the event dictionary and the rewriting

;;;; OTT (https://github.com/foretspaisibles/ott)
;;;; This file is part of OTT.
;;;;
;;;; Copyright © 2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:ott/testsuite)


;;;;
;;;; Event dictionary
;;;;

(define-testcase testsuite-event-dictionary ()
  (flet ((parsed-type (type)
	   (ott:event-type
	    (ott:parse-event
	     (format nil "{\"type\":\"~A\",\"id\":\"1\",\"payload\":null}" type)))))
    (assert-eq :a (parsed-type "A"))
    (assert-eq :resume (parsed-type "RESUME"))
    (assert-equal "NO-SUCH-TYPE" (parsed-type "NO-SUCH-TYPE")))
  (assert-condition (ott:dispatch (ott:make-event "NO-SUCH-TYPE" "1")) ott:unknown-event-type)
  (assert-condition (ott:dispatch (ott:make-event :unregistered "1")) ott:unknown-event-type))

(define-testcase testsuite-custom-event-handler ()
  (let ((handled nil))
    (ott:register-event-handler :testsuite-ping
				(lambda (event)
				  (push (ott:event-id event) handled)))
    (ott:with-local-queue
      (ott:send-event (ott:make-event :testsuite-ping "ID-1"))
      (assert-equal nil handled)
      (assert-equal 1 (ott:drain-local-queue))
      (assert-equal '("ID-1") handled))))

(define-testcase testsuite-data-encoding ()
  (let ((data '(:completed :items ((:sku "KB-100" :quantity 2)) :flag nil :price 12900)))
    (assert-equal data (ott::decode-data (ott::encode-data data)))))


;;;;
;;;; Rewriting
;;;;

(ott:define-business-logic testsuite-no-service-call (x)
  (* x 2))

(ott:define-business-logic testsuite-stock-difference (sku-a sku-b)
  (- (warehouse:stock-level sku-a) (warehouse:stock-level sku-b)))

(ott:define-business-logic testsuite-non-tail-if (sku)
  (let ((label (if (> (warehouse:stock-level sku) 0) :in-stock :empty)))
    (list label (warehouse:stock-level sku))))

(ott:define-business-logic testsuite-evaluation-order (sku)
  ;; The first stock level is read before the reservation, the second after.
  (list (warehouse:stock-level sku)
	(progn (warehouse:reserve-items (list (list :sku sku :quantity 1))) :reserved)
	(warehouse:stock-level sku)))

(ott:define-business-logic testsuite-case-and-cond (sku)
  (case (warehouse:stock-level sku)
    (0 :none)
    ((1 2 3) :few)
    (otherwise
     (cond ((> (warehouse:stock-level sku) 8) :plenty)
	   (t :some)))))

(define-testcase testsuite-rewriting ()
  (dolist (convention '(:monolith :mesh))
    (warehouse:reset-demo-data)
    (flet ((run (function &rest arguments)
	     (quietly (apply #'business-logic:run-logic convention function arguments))))
      (assert-equal 42 (run #'testsuite-no-service-call 21))
      (assert-equal 7 (run #'testsuite-stock-difference "MS-200" "CB-400"))
      (assert-equal '(:in-stock 5) (run #'testsuite-non-tail-if "KB-100"))
      (assert-equal '(:empty 0) (run #'testsuite-non-tail-if "MN-300"))
      (assert-equal '(5 :reserved 4) (run #'testsuite-evaluation-order "KB-100"))
      (assert-equal :none (run #'testsuite-case-and-cond "MN-300"))
      (assert-equal :few (run #'testsuite-case-and-cond "CB-400"))
      (assert-equal :some (run #'testsuite-case-and-cond "KB-100"))
      (assert-equal :plenty (run #'testsuite-case-and-cond "MS-200")))))

(define-testcase testsuite-cannot-rewrite ()
  (flet ((expand (form)
	   (macroexpand-1 form)))
    ;; A service call under a lambda cannot be rewritten.
    (assert-condition
     (expand '(ott:define-business-logic testsuite-bad (x)
	       (mapcar (lambda (sku) (warehouse:stock-level sku)) x)))
     ott:cannot-rewrite)
    ;; Variables must not be shadowed.
    (assert-condition
     (expand '(ott:define-business-logic testsuite-bad (x)
	       (let ((x (warehouse:stock-level "KB-100"))) x)))
     ott:cannot-rewrite)
    ;; Only required parameters.
    (assert-condition
     (expand '(ott:define-business-logic testsuite-bad (&optional x)
	       (warehouse:stock-level x)))
     error)
    ;; The report names the macro that introduced the shadowing variable.
    (handler-case
	(progn
	  (expand '(ott:define-business-logic testsuite-bad (x)
		    (ott:with-properties ((x :x)) (list :x "KB-100") (warehouse:stock-level x))))
	  (assert-t nil))
      (ott:cannot-rewrite (condition)
	(assert-t (and (search "in the expansion of (OTT:WITH-PROPERTIES"
			       (princ-to-string condition))
		       t))))))

(define-testcase testsuite-labels ()
  (flet ((expand (form)
	   (macroexpand-1 form)))
    ;; A local function cannot call itself, directly or not.
    (assert-condition
     (expand '(ott:define-business-logic testsuite-bad (x)
	       (labels ((again (n) (warehouse:stock-level (again n))))
		 (again x))))
     ott:cannot-rewrite)
    (assert-condition
     (expand '(ott:define-business-logic testsuite-bad (x)
	       (labels ((ping (n) (warehouse:stock-level (pong n)))
			(pong (n) (ping n)))
		 (ping x))))
     ott:cannot-rewrite)
    ;; A function can call one defined after it.
    (expand '(ott:define-business-logic testsuite-good (x)
	      (labels ((first-step (n) (second-step n))
		       (second-step (n) (warehouse:stock-level n)))
		(first-step x))))
    ;; A function can be called many times.
    (expand '(ott:define-business-logic testsuite-good (x)
	      (labels ((stock (sku) (let ((level (warehouse:stock-level sku))) level))
		       (twice (sku) (+ (stock sku) (stock sku))))
		(twice x))))))

(define-testcase testsuite-labels-evaluation ()
  (ott:define-business-logic testsuite-labels-logic (sku)
    (labels ((stock (sku) (let ((level (warehouse:stock-level sku))) level))
	     (twice (sku) (+ (stock sku) (stock sku))))
      (list (twice sku) (stock sku))))
  (dolist (convention '(:monolith :mesh))
    (business-logic:reset-demo-data)
    (assert-equal '(10 5)
		  (quietly (business-logic::run-logic convention #'testsuite-labels-logic "KB-100")))))

(define-testcase testsuite-service-call-in-macro-expansion ()
  ;; The service call only shows once the macro is expanded.
  (let ((rewritten (macroexpand-1 '(ott:define-business-logic testsuite-hidden (items)
				    (warehouse:with-items-reservation
					(reservation items :keep-if #'identity)
				      nil)))))
    (assert-t (and (search "CALL-SERVICE" (prin1-to-string rewritten)) t))))

;;;; End of file `continuation.lisp'
