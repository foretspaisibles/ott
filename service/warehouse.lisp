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
   #:receive-return
   #:release-items
   #:reserve-items
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
