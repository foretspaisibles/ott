;;;; queue.lisp — Send and receive messages on the OTT queue

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
;;;; Parameters and conditions
;;;;

(defvar *aws-program* '("aws")
  "The command, as a list of strings, which runs the AWS CLI.")

(defvar *clock* #'get-universal-time
  "The function returning the universal time, used to refresh credentials.")

(defparameter *role-name* "OTTContinuedFunction"
  "The name of the IAM role under which the AWS CLI sends and receives messages.")

(defparameter *credentials-lifetime* (* 45 60)
  "The time, in seconds, after which assumed role credentials are renewed.")

(define-condition aws-error (error)
  ((message
    :initarg :message
    :reader aws-error-message))
  (:report (lambda (condition stream)
	     (write-string (aws-error-message condition) stream)))
  (:documentation "An invocation of the AWS CLI failed."))


;;;;
;;;; AWS CLI
;;;;

(defun trim (string)
  (string-trim '(#\Newline #\Space) string))

(defun process-environment ()
  "The environment variable assignments of the Lisp process, as a list of strings."
  #+sbcl (sb-ext:posix-environ)
  #-sbcl (error "Cannot enumerate the process environment on this implementation."))

(defun child-environment (additions unset)
  "The environment of the AWS CLI: the one of the Lisp process, plus ADDITIONS,
without the variables named in UNSET.
UIOP passes :ENVIRONMENT to the child as its whole environment, so the
inherited variables (PATH, HOME, AWS_PROFILE...) have to be given explicitly."
  (flet ((variable-name (assignment)
	   (subseq assignment 0 (position #\= assignment))))
    (let ((overridden (append unset (mapcar #'variable-name additions))))
      (append additions
	      (remove-if (lambda (assignment)
			   (member (variable-name assignment) overridden :test #'string=))
			 (process-environment))))))

(defun run-aws (environment arguments &key unset)
  "Run the AWS CLI with ARGUMENTS, adding ENVIRONMENT to the environment of the
Lisp process and removing the variables named in UNSET.
Return the trimmed output and signal an AWS-ERROR when the command fails."
  (multiple-value-bind (output error-output exit-code)
      (handler-case
	  (uiop:run-program (append *aws-program* arguments)
			    :environment (child-environment environment unset)
			    :output :string :error-output :string :input nil
			    :ignore-error-status t)
	(error (condition)
	  (values "" (princ-to-string condition) 127)))
    (unless (zerop exit-code)
      (error 'aws-error
	     :message (format nil "The command `aws~{ ~A~}' failed with exit code ~D: ~A"
			      (subseq arguments 0 (min 2 (length arguments)))
			      exit-code (trim error-output))))
    (trim output)))

(defun aws-account ()
  "The account of the current AWS credentials."
  (run-aws nil '("sts" "get-caller-identity" "--query" "Account" "--output" "text")))

(defun role-arn ()
  (format nil "arn:aws:iam::~A:role/~A" (aws-account) *role-name*))

(defvar *credentials* nil
  "A cons of the time at which the credentials were obtained and the environment
variable assignments that convey them to the AWS CLI.")

(defun assume-role ()
  "Assume the role OTTContinuedFunction with AWS STS and return an environment."
  (let* ((output (run-aws nil `("sts" "assume-role"
				"--role-arn" ,(role-arn)
				"--role-session-name" ,(format nil "ott-~D" (random 1000000 (make-random-state t)))
				"--query" "Credentials"
				"--output" "json")))
	 (credentials (yason:parse output)))
    (flet ((field (name) (gethash name credentials)))
      (list (format nil "AWS_ACCESS_KEY_ID=~A" (field "AccessKeyId"))
	    (format nil "AWS_SECRET_ACCESS_KEY=~A" (field "SecretAccessKey"))
	    (format nil "AWS_SESSION_TOKEN=~A" (field "SessionToken"))))))

(defparameter *default-region* "eu-west-3"
  "The region of the queue, unless AWS_REGION or AWS_DEFAULT_REGION name another.")

(defun aws-region ()
  (or (let ((value (uiop:getenv "AWS_REGION")))
	(and value (plusp (length value)) value))
      (let ((value (uiop:getenv "AWS_DEFAULT_REGION")))
	(and value (plusp (length value)) value))
      *default-region*))

(defun region-assignments ()
  "The assignments that make the AWS CLI use the region of the queue, whatever
the region of the profile."
  (list (format nil "AWS_REGION=~A" (aws-region))
	(format nil "AWS_DEFAULT_REGION=~A" (aws-region))))

(defun role-environment ()
  "The environment under which the AWS CLI runs as the role OTTContinuedFunction."
  (let ((now (funcall *clock*)))
    (unless (and *credentials*
		 (< (- now (car *credentials*)) *credentials-lifetime*))
      (setf *credentials* (cons now (append (assume-role) (region-assignments)))))
    (cdr *credentials*)))

(defun run-aws-as-role (&rest arguments)
  "Run the AWS CLI with ARGUMENTS under the role OTTContinuedFunction.
AWS_PROFILE is unset, so that the credentials of the role are not shadowed by
the ones of the profile, and the region is the one of the queue."
  (run-aws (role-environment) arguments :unset '("AWS_PROFILE")))


;;;;
;;;; Messages
;;;;

(defstruct (message (:constructor make-message (id body receipt-handle receive-count)))
  "A message received from the queue."
  id body receipt-handle receive-count)

(defun trace-event (control &rest arguments)
  "Write a line to *TRACE-OUTPUT* when it is bound to a stream."
  (when *trace-output*
    (format *trace-output* "~&;; ~?~%" control arguments)
    (finish-output *trace-output*)))

(defun send-message (url message)
  "Send MESSAGE, an event or a string, to the queue at URL.  Return its message identifier."
  (let* ((body (if (stringp message) message (event-json message)))
	 (id (run-aws-as-role "sqs" "send-message" "--queue-url" url
			      "--message-body" body
			      "--query" "MessageId" "--output" "text")))
    (trace-event "SEND ~A ~A" id body)
    id))

(defun parse-message (table)
  (let ((attributes (gethash "Attributes" table)))
    (make-message (gethash "MessageId" table)
		  (gethash "Body" table)
		  (gethash "ReceiptHandle" table)
		  (and attributes
		       (parse-integer (gethash "ApproximateReceiveCount" attributes "0"))))))

(defun receive-message (url &key (maximum 1) wait)
  "Receive at most MAXIMUM messages from the queue at URL, a list that may be empty.
The call long polls for the wait time of the queue, or for WAIT seconds."
  (let* ((output (apply #'run-aws-as-role "sqs" "receive-message"
			"--queue-url" url
			"--max-number-of-messages" (princ-to-string maximum)
			"--attribute-names" "ApproximateReceiveCount"
			"--output" "json"
			(when wait
			  (list "--wait-time-seconds" (princ-to-string wait)))))
	 (messages (and (plusp (length output))
			(gethash "Messages" (yason:parse output)))))
    (map 'list (lambda (table)
	      (let ((message (parse-message table)))
		(trace-event "RECEIVE ~A ~A" (message-id message) (message-body message))
		message))
	    messages)))

(defun delete-message (url message)
  "Delete MESSAGE from the queue at URL."
  (run-aws-as-role "sqs" "delete-message" "--queue-url" url
		   "--receipt-handle" (message-receipt-handle message))
  (trace-event "DELETE ~A" (message-id message))
  nil)

;;;; End of file `queue.lisp'
