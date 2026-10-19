;;;; package.lisp — Package for OTT

;;;; OTT (https://github.com/foretspaisibles/ott)
;;;; This file is part of OTT.
;;;;
;;;; Copyright © 2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:ott
  (:use #:common-lisp)
  (:export
   #:*aws-program*
   #:*calling-convention*
   #:*clock*
   #:*queue-name*
   #:*role-name*
   #:aws-error
   #:cannot-rewrite
   #:define-business-logic
   #:define-service-function
   #:delete-message
   #:dispatch
   #:drain-local-queue
   #:event-id
   #:event-json
   #:event-payload
   #:event-type
   #:handle-a
   #:handle-b
   #:listen-loop
   #:listen-once
   #:make-b-from
   #:make-event
   #:message-body
   #:message-id
   #:message-receipt-handle
   #:message-receive-count
   #:parse-event
   #:queue-url
   #:receive-message
   #:register-event-handler
   #:run-result
   #:send-a
   #:send-event
   #:send-message
   #:unknown-event-type
   #:with-local-queue))

(in-package #:ott)

;;;; End of file `package.lisp'
