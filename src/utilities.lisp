;;;; utilities.lisp — Utilities for OTT

;;;; OTT (https://github.com/foretspaisibles/ott)
;;;; This file is part of OTT.
;;;;
;;;; Copyright © 2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:ott)

(defmacro with-properties (properties plist &body body)
  "Evaluate BODY with variables bound to the values of properties of PLIST.
Each element of PROPERTIES is either a symbol, naming a variable and the
keyword of its property, or (VARIABLE KEYWORD).  Like WITH-SLOTS for plists,
except that the variables are bound once, not aliased.

Business logic cannot use SYMBOL-MACROLET, so the variables are bound with LET."
  (let ((plist-variable (gensym "PLIST")))
    `(let* ((,plist-variable ,plist)
	    ,@(loop :for property :in properties
		    :for (variable keyword) := (if (consp property)
						   property
						   (list property (intern (symbol-name property) :keyword)))
		    :collect `(,variable (getf ,plist-variable ,keyword))))
       ,@body)))

;;;; End of file `utilities.lisp'
