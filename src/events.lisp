;;;; events.lisp — Typed events and their handlers

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

(defparameter *queue-name* "ott-continuation"
  "The name of the SQS queue carrying the events.")

(defvar *queue-url* nil
  "The URL of the queue *QUEUE-NAME*, looked up on first use.")

(defun queue-url ()
  "The URL of the queue ott-continuation."
  (unless *queue-url*
    (setf *queue-url*
	  (run-aws-as-role "sqs" "get-queue-url" "--queue-name" *queue-name*
			   "--query" "QueueUrl" "--output" "text")))
  *queue-url*)

;;;;
;;;; Events
;;;;

(defvar *event-handlers* (make-hash-table :test 'eq)
  "The dictionary of event types.  It maps a type, a keyword, to the function
that handles the events of this type.")

(defun register-event-handler (type handler)
  "Handle the events of TYPE with HANDLER, a function designator."
  (setf (gethash type *event-handlers*) handler))

(define-condition unknown-event-type (error)
  ((type
    :initarg :type
    :reader unknown-event-type-type))
  (:report (lambda (condition stream)
	     (format stream "Unknown event type ~S." (unknown-event-type-type condition))))
  (:documentation "An event has a type no handler knows."))

(defstruct (event (:constructor make-event (type id &optional payload)))
  "An event: its TYPE is :A or :B, its ID is the idempotency key."
  type id payload)

(defun generate-id ()
  (format nil "~(~32,'0X~)" (random (expt 2 128) (make-random-state t))))

(defun event-json (event)
  "The JSON representation of EVENT."
  (let ((table (make-hash-table :test 'equal)))
    (setf (gethash "type" table) (string-upcase (symbol-name (event-type event)))
	  (gethash "id" table) (event-id event)
	  (gethash "payload" table) (or (event-payload event) :null))
    (with-output-to-string (stream)
      (yason:encode table stream))))

(defun parse-event (string)
  "The event denoted by the JSON STRING.
The type of an event of unknown type is the string found in STRING."
  (let* ((table (yason:parse string))
	 (type (gethash "type" table)))
    (make-event (or (loop :for known :being :the :hash-keys :of *event-handlers*
			  :when (string-equal known type)
			    :return known)
		    type)
		(gethash "id" table)
		(gethash "payload" table))))

;;;;
;;;; Handlers
;;;;

(defun make-b-from (event)
  "The B event derived from the A event EVENT, it keeps the identifier of EVENT."
  (let ((payload (make-hash-table :test 'equal)))
    (setf (gethash "from" payload) "A"
	  (gethash "payload" payload) (event-payload event))
    (make-event :b (event-id event) payload)))

(defun print-details (event)
  "Print the details of EVENT on *STANDARD-OUTPUT*."
  (format t "~&~A event ~A: ~A~%"
	  (event-type event) (event-id event)
	  (with-output-to-string (stream)
	    (yason:encode (or (event-payload event) :null) stream))))

(defun handle-a (event)
  "Handle an A event by sending the derived B event."
  (send-message (queue-url) (make-b-from event)))

(defun handle-b (event)
  "Handle a B event by printing its details."
  (print-details event))

(defun dispatch (event)
  "Dispatch EVENT on its type, signal UNKNOWN-EVENT-TYPE for an unknown type."
  (let ((handler (gethash (event-type event) *event-handlers*)))
    (unless handler
      (error 'unknown-event-type :type (event-type event)))
    (funcall handler event)))

(register-event-handler :a 'handle-a)

(register-event-handler :b 'handle-b)

(defvar *local-queue* nil
  "When non-NIL, a LOCAL-QUEUE that carries the events instead of SQS.")

(defstruct (local-queue (:constructor make-local-queue ()))
  "An in-memory continuation queue, it holds the JSON of the events."
  (messages nil))

(defmacro with-local-queue (&body body)
  "Evaluate BODY with events sent to an in-memory queue instead of SQS."
  `(let ((*local-queue* (make-local-queue)))
     ,@body))

(defun send-event (event)
  "Send EVENT to the continuation queue, SQS or the local queue when there is one.
The local queue holds JSON, so that events go through the same encoding as on SQS."
  (if *local-queue*
      (let ((json (event-json event)))
	(trace-event "SEND ~A" json)
	(setf (local-queue-messages *local-queue*)
	      (append (local-queue-messages *local-queue*) (list json))))
      (send-message (queue-url) event)))

(defun drain-local-queue (&key (limit 10000))
  "Dispatch the events of the local queue until it is empty, return their number.
Signal an error when more than LIMIT events are handled, which means events
keep begetting events."
  (let ((count 0))
    (loop :while (local-queue-messages *local-queue*)
	  :do (when (>= count limit)
		(error "The local queue still holds events after ~D of them." limit))
	      (incf count)
	      (let ((json (pop (local-queue-messages *local-queue*))))
		(trace-event "RECEIVE ~A" json)
		(dispatch (parse-event json))))
    count))

(defun send-a (&key (id (generate-id)) payload)
  "Send an A event to the continuation queue and return its identifier."
  (send-message (queue-url) (make-event :a id payload))
  id)


;;;;
;;;; Listener
;;;;

(defun listen-once (&key wait)
  "Receive a message from the continuation queue and handle it, return the number handled.
The message is deleted once handled, a handler that signals an error leaves it
on the queue, so that SQS delivers it again, and eventually to the DLQ."
  (let ((messages (receive-message (queue-url) :maximum 1 :wait wait)))
    (dolist (message messages)
      (dispatch (parse-event (message-body message)))
      (delete-message (queue-url) message))
    (length messages)))

(defun listen-loop (&key (continue-p (constantly t)) wait)
  "Handle the messages of the continuation queue while CONTINUE-P returns true.
Errors are reported on *ERROR-OUTPUT* and do not stop the listener."
  (loop :while (funcall continue-p)
	:do (handler-case (listen-once :wait wait)
	      (error (condition)
		(format *error-output* "~&;; ERROR ~A~%" condition)
		(finish-output *error-output*)
		(sleep 1)))))

;;;; End of file `events.lisp'
