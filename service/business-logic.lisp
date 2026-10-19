;;;; business-logic.lisp — Micro-service: the business logic of the retail demo

;;;; OTT (https://github.com/foretspaisibles/ott)
;;;; This file is part of OTT.
;;;;
;;;; Copyright © 2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

;;;; This micro-service owns the orders and the procedures that make
;;;; warehouse, payment and shipping cooperate.  It is the only one that
;;;; depends on other micro-services.

(defpackage #:business-logic
  (:use #:common-lisp)
  (:import-from #:ott #:define-business-logic)
  (:export
   #:checkout
   #:demo-order
   #:make-order
   #:order-address
   #:order-id
   #:order-items
   #:order-price
   #:order-user
   #:process-return
   #:refund-due
   #:reset-demo-data
   #:run-checkout
   #:run-return
   #:run-logic))

(in-package #:business-logic)


;;;;
;;;; Entities
;;;;

(defun make-order (&key id user items price address)
  "An order, a plist so that it travels on the continuation queue."
  (list :id id :user user :items items :price price :address address))

(defun order-id (order) (getf order :id))
(defun order-user (order) (getf order :user))
(defun order-items (order) (getf order :items))
(defun order-price (order) (getf order :price))
(defun order-address (order) (getf order :address))

(defun demo-order (kind)
  "A demo order, KIND is :HAPPY, :OUT-OF-STOCK, :NO-CREDIT, :BAD-ADDRESS or
:ABSENT-RECIPIENT."
  (ecase kind
    (:happy
     (make-order :id "ORD-1" :user "alice" :price 12900
		 :items '((:sku "KB-100" :quantity 1) (:sku "MS-200" :quantity 2))
		 :address "1 Rue de Rivoli, Paris"))
    (:out-of-stock
     (make-order :id "ORD-2" :user "alice" :price 29900
		 :items '((:sku "MN-300" :quantity 1))
		 :address "1 Rue de Rivoli, Paris"))
    (:no-credit
     (make-order :id "ORD-3" :user "bob" :price 12900
		 :items '((:sku "KB-100" :quantity 1))
		 :address "1 Rue de Rivoli, Paris"))
    (:bad-address
     (make-order :id "ORD-4" :user "alice" :price 12900
		 :items '((:sku "KB-100" :quantity 1))
		 :address "42 Nowhere Lane"))
    (:absent-recipient
     (make-order :id "ORD-5" :user "carol" :price 12900
		 :items '((:sku "KB-100" :quantity 1))
		 :address "7 Rue Absente, Lyon"))))

(defun refund-due (order reason)
  "The amount to refund when the items of ORDER come back for REASON.
Defective items are refunded in full, a customer who changed their mind pays
a restocking fee of 10%."
  (if (eq reason :defective)
      (order-price order)
      (- (order-price order) (round (order-price order) 10))))


;;;;
;;;; Business logic
;;;;

(define-business-logic checkout (order)
  "Check out ORDER: reserve the items, take the payment and ship them.
The value is (:COMPLETED ...), (:REJECTED REASON ...) before anything was
shipped, or (:CANCELLED REASON) when shipping failed and the order was undone."
  (let ((reservation (warehouse:reserve-items (order-items order))))
    (if (not (eq (getf reservation :status) :reserved))
	(list :rejected :out-of-stock (getf reservation :missing))
	(if (not (payment:check-credit (order-user order) (order-price order)))
	    (progn
	      (warehouse:release-items (order-items order))
	      (list :rejected :insufficient-credit))
	    (let ((payment (payment:capture-amount (order-user order) (order-price order))))
	      (if (not (eq (getf payment :status) :captured))
		  (progn
		    (warehouse:release-items (order-items order))
		    (list :rejected :payment-declined))
		  (let ((shipment (shipping:dispatch-items (order-id order)
							   (order-items order)
							   (order-address order))))
		    (if (not (eq (getf shipment :status) :dispatched))
			;; The carrier cannot find the address, undo the order.
			(progn
			  (warehouse:release-items (order-items order))
			  (payment:refund-amount (getf payment :payment-id) (order-price order))
			  (list :cancelled :address-not-found))
			(case (getf (shipping:track-delivery (getf shipment :shipment-id)) :status)
			  (:delivered
			   (list :completed
				 :payment-id (getf payment :payment-id)
				 :shipment-id (getf shipment :shipment-id)))
			  (otherwise
			   ;; The package came back, undo the order.
			   (warehouse:receive-return (order-items order) :undelivered)
			   (payment:refund-amount (getf payment :payment-id) (order-price order))
			   (list :cancelled :returned-to-sender)))))))))))

(define-business-logic process-return (order payment-id shipment-id reason)
  "Process the return of a delivered ORDER, REASON is :DEFECTIVE or :CHANGED-MIND.
The value is (:RETURNED :RECEIPT STATUS :REFUNDED AMOUNT), or (:REJECTED REASON)."
  (let ((pickup (shipping:schedule-pickup shipment-id)))
    (if (not (eq (getf pickup :status) :pickup-scheduled))
	(list :rejected :unknown-shipment)
	(let* ((receipt (warehouse:receive-return (order-items order) reason))
	       (refund (payment:refund-amount payment-id (refund-due order reason))))
	  (list :returned
		:receipt (getf receipt :status)
		:refunded (getf refund :amount))))))


;;;;
;;;; Running the logic
;;;;

(defun reset-demo-data ()
  "Reset the demo data of all the micro-services."
  (warehouse:reset-demo-data)
  (payment:reset-demo-data)
  (shipping:reset-demo-data)
  :reset)

(defun run-logic (convention function &rest arguments)
  "Apply the business logic FUNCTION to ARGUMENTS under the calling CONVENTION.
Return its value, and the number of events it took on the continuation queue.
With :MESH the events travel on an in-memory queue, which this function drains."
  (let ((ott:*calling-convention* convention))
    (ecase convention
      (:monolith
       (values (apply function arguments) 0))
      (:mesh
       (ott:with-local-queue
	 (let* ((id (nth-value 1 (apply function arguments)))
		(count (ott:drain-local-queue)))
	   (values (ott:run-result id) count)))))))

(defun run-checkout (kind &key (convention :monolith))
  "Check out the demo order KIND on fresh demo data."
  (reset-demo-data)
  (run-logic convention #'checkout (demo-order kind)))

(defun run-return (reason &key (convention :monolith))
  "Check out the happy demo order, then return it for REASON."
  (reset-demo-data)
  (let ((order (demo-order :happy)))
    (destructuring-bind (status &key payment-id shipment-id)
	(run-logic convention #'checkout order)
      (assert (eq status :completed))
      (run-logic convention #'process-return order payment-id shipment-id reason))))

;;;; End of file `business-logic.lisp'
