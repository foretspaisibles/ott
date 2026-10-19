;;;; continuation.lisp — Calling conventions and continuation events

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


;;;;
;;;; Calling convention and runs
;;;;

(defvar *calling-convention* :monolith
  "How business logic calls the functions of micro-services.
With :MONOLITH it is a plain function call.  With :MESH the call is an event
on the continuation queue, and the rest of the logic is its continuation.")

(defvar *run-id* nil
  "The identifier of the run of business logic being executed.
It is the identifier of all the events of the run.")

(defvar *results* (make-hash-table :test 'equal)
  "The values of the completed runs, by run identifier.")

(defvar *logic-steps* (make-hash-table :test 'eq)
  "The steps of the business logic, by name.  A step is a function of a frame,
the list of the values of the variables in scope, and of a value.")

(defun complete-run (value)
  "Record VALUE as the result of the current run and return it."
  (trace-event "COMPLETE ~A ~S" *run-id* value)
  (setf (gethash *run-id* *results*) value))

(defun run-result (id)
  "The result of the run ID, and whether the run is complete."
  (gethash id *results*))

(defun start-run (thunk)
  "Call THUNK in a new run.  Return its value and the identifier of the run.
When the calling convention is :MESH the value is :SUSPENDED unless the run
completed without calling a micro-service."
  (let ((*run-id* (generate-id)))
    (values (funcall thunk) *run-id*)))

(defun resume-step (logic step frame value)
  "Resume the business logic LOGIC at STEP in the environment FRAME with VALUE."
  (funcall (aref (gethash logic *logic-steps*) step) frame value))


;;;;
;;;; Data on the queue
;;;;

(defun encode-data (object)
  "The readable representation of OBJECT, a string."
  (with-standard-io-syntax
    (prin1-to-string object)))

(defun decode-data (string)
  "The object that the readable representation STRING denotes."
  (with-standard-io-syntax
    (let ((*read-eval* nil))
      (values (read-from-string string)))))

(defun continuation-payload (logic step frame &rest properties)
  "The payload of an event that carries the continuation LOGIC, STEP, FRAME.
PROPERTIES is a plist of other entries, with values encoded by ENCODE-DATA."
  (let ((payload (make-hash-table :test 'equal)))
    (setf (gethash "logic" payload) (encode-data logic)
	  (gethash "step" payload) step
	  (gethash "frame" payload) (encode-data frame))
    (loop :for (key value) :on properties :by #'cddr
	  :do (setf (gethash key payload) (encode-data value)))
    payload))


;;;;
;;;; Micro-service functions
;;;;

(defun service-function-p (symbol)
  "True when SYMBOL names a function defined by DEFINE-SERVICE-FUNCTION."
  (and (symbolp symbol) (get symbol 'service-function)))

(defun service-event-type (name)
  "The event type of the calls to the service function NAME, a keyword."
  (intern (format nil "~A:~A" (package-name (symbol-package name)) (symbol-name name))
	  :keyword))

(defun handle-service-call (name event)
  "Apply the service function NAME to the arguments of EVENT, and send the
value to the continuation that EVENT carries."
  (let* ((payload (event-payload event))
	 (value (apply name (decode-data (gethash "arguments" payload)))))
    (send-event
     (make-event :resume (event-id event)
		 (continuation-payload (decode-data (gethash "logic" payload))
				       (gethash "step" payload)
				       (decode-data (gethash "frame" payload))
				       "value" value)))))

(defun register-service-function (name)
  "Mark the symbol NAME as a service function, and handle the events of its calls."
  (setf (get name 'service-function) t)
  (register-event-handler (service-event-type name)
			  (lambda (event) (handle-service-call name event)))
  name)

(defmacro define-service-function (name lambda-list &body body)
  "Define the function NAME, as DEFUN does, and mark it as owned by a micro-service.
Business logic defined with DEFINE-BUSINESS-LOGIC calls it according to
*CALLING-CONVENTION*.  The arguments and the value of a service function must
be readable data."
  `(progn
     (defun ,name ,lambda-list ,@body)
     (register-service-function ',name)))

(defun call-service (function arguments logic step frame)
  "Call the service FUNCTION with ARGUMENTS on behalf of the business logic LOGIC,
that continues with STEP in the environment FRAME."
  (ecase *calling-convention*
    (:monolith
     (resume-step logic step frame (apply function arguments)))
    (:mesh
     (send-event (make-event (service-event-type function) *run-id*
			     (continuation-payload logic step frame "arguments" arguments)))
     :suspended)))

(defun handle-resume (event)
  "Resume the business logic that EVENT continues, with the value it carries."
  (let ((payload (event-payload event))
	(*run-id* (event-id event)))
    (resume-step (decode-data (gethash "logic" payload))
		 (gethash "step" payload)
		 (decode-data (gethash "frame" payload))
		 (decode-data (gethash "value" payload)))))

(register-event-handler :resume 'handle-resume)

;;;; End of file `continuation.lisp'
