;;;; retail.lisp — Tests for the retail demo

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

(defparameter *checkout-scenarios*
  '((:happy (:completed :payment-id "PAY-0001" :shipment-id "SHIP-0001") 10)
    (:out-of-stock (:rejected :out-of-stock ("MN-300")) 2)
    (:no-credit (:rejected :insufficient-credit) 6)
    (:bad-address (:cancelled :address-not-found) 12)
    (:absent-recipient (:cancelled :returned-to-sender) 14))
  "Demo orders, with the value of the checkout and the number of events it takes
on the continuation queue in the :MESH calling convention.")

(define-testcase testsuite-checkout ()
  (dolist (scenario *checkout-scenarios*)
    (destructuring-bind (kind expected-value expected-events) scenario
      (multiple-value-bind (value events)
	  (quietly (business-logic:run-checkout kind))
	(assert-equal expected-value value)
	(assert-equal 0 events))
      (multiple-value-bind (value events)
	  (quietly (business-logic:run-checkout kind :convention :mesh))
	(assert-equal expected-value value)
	(assert-equal expected-events events)))))

(define-testcase testsuite-checkout-leaves-consistent-state ()
  (dolist (convention '(:monolith :mesh))
    ;; The stock is untouched when the order is undone, and the credit refunded.
    (dolist (kind '(:out-of-stock :no-credit :bad-address :absent-recipient))
      (quietly (business-logic:run-checkout kind :convention convention))
      (assert-equal 5 (warehouse:stock-level "KB-100"))
      (assert-equal 100000 (payment:credit "alice"))
      (assert-equal 100000 (payment:credit "carol")))
    ;; A completed order takes its items from the stock and its price from the credit.
    (quietly (business-logic:run-checkout :happy :convention convention))
    (assert-equal 4 (warehouse:stock-level "KB-100"))
    (assert-equal 8 (warehouse:stock-level "MS-200"))
    (assert-equal (- 100000 12900) (payment:credit "alice"))))

(define-testcase testsuite-return ()
  (dolist (convention '(:monolith :mesh))
    (assert-equal '(:returned :receipt :quarantined :refunded 12900)
		  (quietly (business-logic:run-return :defective :convention convention)))
    (assert-equal 4 (warehouse:stock-level "KB-100"))
    (assert-equal '(:returned :receipt :restocked :refunded 11610)
		  (quietly (business-logic:run-return :changed-mind :convention convention)))
    (assert-equal 5 (warehouse:stock-level "KB-100"))
    (assert-equal (- 100000 (- 12900 11610)) (payment:credit "alice"))))

(define-testcase testsuite-micro-service-dependencies ()
  ;; Only the business logic depends on other micro-services.
  (dolist (name '("ott/warehouse" "ott/payment" "ott/shipping"))
    (assert-equal '("ott") (asdf:system-depends-on (asdf:find-system name))))
  (assert-set-equal '("ott" "ott/warehouse" "ott/payment" "ott/shipping")
		    (asdf:system-depends-on (asdf:find-system "ott/business-logic"))
		    :test #'string=))

;;;; End of file `retail.lisp'
