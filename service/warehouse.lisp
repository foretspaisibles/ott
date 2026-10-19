;;;; warehouse.lisp — Micro-service: the warehouse

;;;; OTT (https://github.com/foretspaisibles/ott)
;;;; This file is part of OTT.
;;;;
;;;; Copyright © 2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

;;;; The warehouse owns the stock.  Items are lists (:SKU "KB-100" :QUANTITY 2).

(defpackage #:warehouse
  (:use #:common-lisp)
  (:import-from #:ott #:define-service-function)
  (:export
   #:with-items-reservation
   #:receive-return
   #:release-items
   #:reserve-items
   #:reserved-p
   #:reset-demo-data
   #:stock-level))

(in-package #:warehouse)

(defvar *stock* (make-hash-table :test 'equal)
  "The quantity of items available for sale, by SKU.")

(defvar *quarantine* (make-hash-table :test 'equal)
  "The quantity of defective items set aside, by SKU.")

(defun reset-demo-data ()
  "Fill the warehouse with the demo data."
  (clrhash *stock*)
  (clrhash *quarantine*)
  (loop :for (sku quantity) :in '(("KB-100" 5) ("MS-200" 10) ("MN-300" 0) ("CB-400" 3))
	:do (setf (gethash sku *stock*) quantity))
  :reset)

(reset-demo-data)

(defun add-items (table items)
  (dolist (item items)
    (incf (gethash (getf item :sku) table 0) (getf item :quantity))))

(define-service-function stock-level (sku)
  "The quantity of items SKU available for sale."
  (gethash sku *stock* 0))

(define-service-function reserve-items (items)
  "Take ITEMS out of the stock, all of them or none.
The value is (:STATUS :RESERVED), or (:STATUS :OUT-OF-STOCK :MISSING SKUS)."
  (let ((missing (loop :for item :in items
		       :unless (>= (gethash (getf item :sku) *stock* 0) (getf item :quantity))
			 :collect (getf item :sku))))
    (cond (missing
	   (list :status :out-of-stock :missing missing))
	  (t
	   (dolist (item items)
	     (decf (gethash (getf item :sku) *stock*) (getf item :quantity)))
	   (list :status :reserved)))))

(define-service-function release-items (items)
  "Put the reserved ITEMS back in the stock."
  (add-items *stock* items)
  (list :status :released))

(defun reserved-p (reservation)
  "True when RESERVATION is the value of a successful RESERVE-ITEMS."
  (eq (getf reservation :status) :reserved))

(defmacro with-items-reservation ((reservation items &key unavailable keep-if) &body body)
  "Reserve ITEMS and evaluate BODY, or UNAVAILABLE when they cannot be reserved.
RESERVATION is bound to the value of RESERVE-ITEMS in both cases.  The value is
the one of BODY, and the items are released unless the function designated by
KEEP-IF is true of this value.

This is UNWIND-PROTECT for business logic, and like it, it cleans up after BODY.
The cleanup is by value, not by non-local exit: the logic is rewritten in
continuation-passing style, which supports neither UNWIND-PROTECT nor the
non-local exits that would trigger it."
  (let ((items-variable (gensym "ITEMS"))
	(outcome (gensym "OUTCOME")))
    `(let* ((,items-variable ,items)
	    (,reservation (reserve-items ,items-variable)))
       (if (reserved-p ,reservation)
	   (let ((,outcome (progn ,@body)))
	     (unless (funcall ,keep-if ,outcome)
	       (release-items ,items-variable))
	     ,outcome)
	   ,unavailable))))

(define-service-function receive-return (items reason)
  "Receive ITEMS that came back for REASON.
Defective items are set aside, others go back in the stock."
  (cond ((eq reason :defective)
	 (add-items *quarantine* items)
	 (list :status :quarantined))
	(t
	 (add-items *stock* items)
	 (list :status :restocked))))

;;;; End of file `warehouse.lisp'
