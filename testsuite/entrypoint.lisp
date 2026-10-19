;;;; entrypoint.lisp — Entrypoint for OTT

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

(define-testcase run-all-tests ()
  (testsuite-event-dictionary)
  (testsuite-custom-event-handler)
  (testsuite-data-encoding)
  (testsuite-rewriting)
  (testsuite-cannot-rewrite)
  (testsuite-labels)
  (testsuite-service-call-in-macro-expansion)
  (testsuite-checkout)
  (testsuite-checkout-leaves-consistent-state)
  (testsuite-return)
  (testsuite-micro-service-dependencies))

;;;; End of file `entrypoint.lisp'
