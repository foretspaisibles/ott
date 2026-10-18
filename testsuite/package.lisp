;;;; package.lisp — Package for OTT tests

;;;; OTT (https://github.com/foretspaisibles/ott)
;;;; This file is part of OTT.
;;;;
;;;; Copyright © 2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:ott/testsuite
  (:use #:common-lisp)
  (:import-from #:org.melusina.confidence
   #:define-testcase
   #:define-assertion
   #:assert-t
   #:assert-eq
   #:assert-set-equal
   #:assert-string=
   #:assert-type))

(in-package #:ott/testsuite)

;;;; End of file `package.lisp'
