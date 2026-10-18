;;;; entrypoint.lisp — Entrypoint for OTT acceptance tests

;;;; OTT (https://github.com/foretspaisibles/ott)
;;;; This file is part of OTT.
;;;;
;;;; Copyright © 2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:ott/acceptance)

(define-testcase run-all-tests ()
  (build-ott-system-without-warnings)
  (lint-ott-system-without-warnings)
  (build-ott-documentation-without-warnings)
  (test-ott-system-succesfully)
  (review-ott-readme)
  (enforce-ott-sibling-systems)
  (enforce-ott-acceptance-run-all-tests))

;;;; End of file `entrypoint.lisp'
