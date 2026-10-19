;;;; shipping.lisp — Micro-service: shipping

;;;; OTT (https://github.com/foretspaisibles/ott)
;;;; This file is part of OTT.
;;;;
;;;; Copyright © 2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

;;;; Shipping owns the shipments.  The demo carrier knows some addresses, and
;;;; the recipient of some of them is never home: after three attempts the
;;;; package comes back.

(defpackage #:shipping
  (:use #:common-lisp)
  (:import-from #:ott #:define-service-function)
  (:export
   #:delivered-p
   #:dispatch-items
   #:dispatched-p
   #:reset-demo-data
   #:schedule-pickup
   #:track-delivery))

(in-package #:shipping)

(defvar *known-addresses* nil
  "The addresses the carrier can find.")

(defvar *absent-addresses* nil
  "The known addresses where nobody ever receives the package.")

(defvar *shipments* (make-hash-table :test 'equal)
  "The shipments, by identifier.  A shipment is (ORDER-ID ADDRESS).")

(defvar *shipment-counter* 0)

(defun reset-demo-data ()
  "Teach the carrier the demo addresses and forget all shipments."
  (setf *known-addresses* '("1 Rue de Rivoli, Paris" "7 Rue Absente, Lyon")
	*absent-addresses* '("7 Rue Absente, Lyon")
	*shipment-counter* 0)
  (clrhash *shipments*)
  :reset)

(reset-demo-data)

(define-service-function dispatch-items (order-id items address)
  "Hand the ITEMS of the order ORDER-ID to the carrier, for delivery at ADDRESS.
The value is (:STATUS :DISPATCHED :SHIPMENT-ID ID), or (:STATUS :ADDRESS-NOT-FOUND)."
  (declare (ignore items))
  (cond ((member address *known-addresses* :test #'string=)
	 (let ((id (format nil "SHIP-~4,'0D" (incf *shipment-counter*))))
	   (setf (gethash id *shipments*) (list order-id address))
	   (list :status :dispatched :shipment-id id)))
	(t
	 (list :status :address-not-found))))

(define-service-function track-delivery (shipment-id)
  "The outcome of the delivery of the shipment SHIPMENT-ID.
The value is (:STATUS :DELIVERED), (:STATUS :RETURNED-TO-SENDER :ATTEMPTS 3),
or (:STATUS :UNKNOWN-SHIPMENT)."
  (let ((shipment (gethash shipment-id *shipments*)))
    (cond ((null shipment)
	   (list :status :unknown-shipment))
	  ((member (second shipment) *absent-addresses* :test #'string=)
	   (list :status :returned-to-sender :attempts 3))
	  (t
	   (list :status :delivered)))))

(defun dispatched-p (shipment)
  "True when SHIPMENT is the value of a successful DISPATCH-ITEMS."
  (eq (getf shipment :status) :dispatched))

(defun delivered-p (delivery)
  "True when DELIVERY is the value of TRACK-DELIVERY for a delivered package."
  (eq (getf delivery :status) :delivered))

(define-service-function schedule-pickup (shipment-id)
  "Schedule the pickup of the package of the delivered shipment SHIPMENT-ID.
The value is (:STATUS :PICKUP-SCHEDULED :ADDRESS ADDRESS), or (:STATUS :UNKNOWN-SHIPMENT)."
  (let ((shipment (gethash shipment-id *shipments*)))
    (if shipment
	(list :status :pickup-scheduled :address (second shipment))
	(list :status :unknown-shipment))))

;;;; End of file `shipping.lisp'
