;;;; utilities.lisp — Utilities for OTT tests

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

(defmacro quietly (&body body)
  "Evaluate BODY without the trace of the events on *TRACE-OUTPUT*."
  `(let ((*trace-output* (make-broadcast-stream)))
     ,@body))

;;;; End of file `utilities.lisp'
