;;;; payment.lisp — Micro-service: payments

;;;; OTT (https://github.com/foretspaisibles/ott)
;;;; This file is part of OTT.
;;;;
;;;; Copyright © 2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

;;;; Payment owns the credit of customers.  Amounts are integers, in cents.

(defpackage #:payment
  (:use #:common-lisp)
  (:import-from #:ott #:define-service-function)
  (:export
   #:capture-amount
   #:captured-p
   #:check-credit
   #:credit
   #:refund-amount
   #:reset-demo-data
   #:with-captured-payment))

(in-package #:payment)

(defvar *credit* (make-hash-table :test 'equal)
  "The credit available to a user, by user name.")

(defvar *payments* (make-hash-table :test 'equal)
  "The captured payments, by payment identifier.  A payment is (USER AMOUNT).")

(defvar *payment-counter* 0)

(defun reset-demo-data ()
  "Give the demo users their credit and forget all payments."
  (clrhash *credit*)
  (clrhash *payments*)
  (setf *payment-counter* 0)
  (loop :for (user amount) :in '(("alice" 100000) ("bob" 5000) ("carol" 100000))
	:do (setf (gethash user *credit*) amount))
  :reset)

(reset-demo-data)

(define-service-function credit (user)
  "The credit available to USER."
  (gethash user *credit* 0))

(define-service-function check-credit (user amount)
  "True when USER has the credit to pay AMOUNT."
  (>= (gethash user *credit* 0) amount))

(define-service-function capture-amount (user amount)
  "Capture AMOUNT on the credit of USER.
The value is (:STATUS :CAPTURED :PAYMENT-ID ID), or (:STATUS :DECLINED)."
  (cond ((check-credit user amount)
	 (let ((id (format nil "PAY-~4,'0D" (incf *payment-counter*))))
	   (decf (gethash user *credit*) amount)
	   (setf (gethash id *payments*) (list user amount))
	   (list :status :captured :payment-id id)))
	(t
	 (list :status :declined))))

(defun captured-p (payment)
  "True when PAYMENT is the value of a successful CAPTURE-AMOUNT."
  (eq (getf payment :status) :captured))

(defmacro with-captured-payment (((payment payment-id) user amount &key declined keep-if) &body body)
  "Capture AMOUNT on the credit of USER and evaluate BODY, or DECLINED when
the capture fails.  PAYMENT is bound to the value of CAPTURE-AMOUNT and
PAYMENT-ID to the identifier of the payment.  The value is the one of BODY,
and the amount is refunded unless the function designated by KEEP-IF is true
of this value.  See WAREHOUSE:WITH-ITEMS-RESERVATION for the cleanup by value."
  (let ((amount-variable (gensym "AMOUNT"))
	(outcome (gensym "OUTCOME")))
    `(let* ((,amount-variable ,amount)
	    (,payment (capture-amount ,user ,amount-variable)))
       (if (captured-p ,payment)
	   (let* ((,payment-id (getf ,payment :payment-id))
		  (,outcome (progn ,@body)))
	     (unless (funcall ,keep-if ,outcome)
	       (refund-amount ,payment-id ,amount-variable))
	     ,outcome)
	   ,declined))))

(define-service-function refund-amount (payment-id amount)
  "Refund AMOUNT of the payment PAYMENT-ID, at most what it captured.
The value is (:STATUS :REFUNDED :AMOUNT AMOUNT), or (:STATUS :UNKNOWN-PAYMENT)."
  (let ((payment (gethash payment-id *payments*)))
    (cond (payment
	   (destructuring-bind (user captured) payment
	     (let ((refund (min amount captured)))
	       (incf (gethash user *credit*) refund)
	       (setf (second payment) (- captured refund))
	       (list :status :refunded :amount refund))))
	  (t
	   (list :status :unknown-payment)))))

;;;; End of file `payment.lisp'
