;;;; service.lisp — Run OTT micro-services as processes

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

(defun process-id ()
  #+sbcl (sb-unix:unix-getpid)
  #-sbcl 0)

(defun service-banner (name)
  "Print the identity of the micro-service NAME on *STANDARD-OUTPUT*."
  (format t "~&;; Service:    ~A~%;; PID:        ~D~%;; Convention: ~S~
~%;; Queue:      ~A~%;; Region:     ~A~%;; Lisp:       ~A ~A~%"
	  name (process-id) *calling-convention* *queue-name* (aws-region)
	  (lisp-implementation-type) (lisp-implementation-version))
  (finish-output))

(defun preflight-check ()
  "Verify that the AWS CLI is authenticated and that the queue is reachable
under the role.  Return the URL of the queue, signal an AWS-ERROR otherwise."
  (format t "~&;; Pre-flight: AWS account ~A~%" (aws-account))
  (let ((url (queue-url)))
    (format t ";; Pre-flight: role ~A can reach ~A~%" *role-name* url)
    (finish-output)
    url))

(defun service-event-owner (event)
  "The name of the micro-service that handles EVENT, a string, or NIL when
it is the one of the business logic (continuations)."
  (let ((type (event-type event)))
    (when (keywordp type)
      (let ((colon (position #\: (symbol-name type))))
	(and colon (subseq (symbol-name type) 0 colon))))))

(defun service-owns-p (name event)
  "True when the micro-service NAME handles EVENT.  Micro-services handle the calls
to their own functions, and the business logic handles the continuations."
  (let ((owner (service-event-owner event)))
    (if owner
	(string-equal owner name)
	(and (eq (event-type event) :resume)
	     (string-equal name "business-logic")))))

(defun service-once (name)
  "Receive a message and handle it when the micro-service NAME owns it.
A message of another service is given back to the queue at once."
  (dolist (message (receive-message (queue-url) :maximum 1))
    (let ((event (parse-event (message-body message))))
      (cond ((service-owns-p name event)
	     (dispatch event)
	     (delete-message (queue-url) message))
	    (t
	     (trace-event "SKIP ~A, not for ~A" (message-id message) name)
	     (run-aws-as-role "sqs" "change-message-visibility"
			      "--queue-url" (queue-url)
			      "--receipt-handle" (message-receipt-handle message)
			      "--visibility-timeout" "0"))))))

(defun run-service (name &key (convention :mesh))
  "Run the micro-service NAME: print its identity, check that AWS is usable and
handle the events of the continuation queue until interrupted with SIGINT.
Return the exit code of the process."
  (let ((*calling-convention* convention)
	(*trace-output* *standard-output*))
    (service-banner name)
    (handler-case (preflight-check)
      (aws-error (condition)
	(format *error-output* "~&;; Pre-flight FAILED: ~A~
~%;; Authenticate the AWS CLI and check that the infrastructure exists.~%"
		condition)
	(return-from run-service 1)))
    (format t ";; ~A is processing events, press Control-C to stop.~%" name)
    (finish-output)
    (handler-case
	(loop
	  (handler-case (service-once name)
	    (error (condition)
	      (format *error-output* "~&;; ERROR ~A~%" condition)
	      (finish-output *error-output*)
	      (sleep 1))))
      (#+sbcl sb-sys:interactive-interrupt #-sbcl serious-condition ()
	(format t "~&;; ~A (PID ~D) interrupted, shutting down.~%" name (process-id))
	(finish-output)
	0))))

;;;; End of file `service.lisp'
