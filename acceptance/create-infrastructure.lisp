;;;; create-infrastructure.lisp — Acceptance Tests for the Infrastructure

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


;;;;
;;;; Stubs
;;;;

(defun install-stub (script directory name)
  "Copy the stub SCRIPT in DIRECTORY as the executable NAME and return its namestring."
  (let ((stub (merge-pathnames name directory)))
    (uiop:copy-file (system-relative-pathname script) stub)
    (uiop:run-program (list "chmod" "755" (namestring stub)))
    (namestring stub)))

(defun call-with-sqs-stub (function)
  "Call FUNCTION with the AWS CLI of OTT replaced by a queue stub.
The program is set globally, so that threads started by FUNCTION see it."
  (let* ((directory (make-temporary-directory))
	 (saved ott:*aws-program*))
    (unwind-protect
	 (progn
	   (setf ott:*aws-program* (list (install-stub #p"acceptance/sqs-stub.sh" directory "aws"))
		 ott::*credentials* nil
		 ott::*queue-url* nil)
	   (funcall function directory))
      (setf ott:*aws-program* saved
	    ott::*credentials* nil
	    ott::*queue-url* nil)
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))))

(defmacro with-sqs-stub ((directory) &body body)
  `(call-with-sqs-stub (lambda (,directory) (declare (ignorable ,directory)) ,@body)))

(defun stub-directory-files (directory name)
  "The names of the message files of the stub directory NAME, markers excluded."
  (sort (loop :for file :in (uiop:directory-files
			     (merge-pathnames (format nil "state/~A/" name) directory))
	      :for filename = (file-namestring file)
	      :unless (search "." filename :start2 2)
		:collect filename)
	#'string<))

(defun stub-message-bodies (directory name)
  (loop :for filename :in (stub-directory-files directory name)
	:collect (uiop:read-file-string
		  (merge-pathnames (format nil "state/~A/~A" name filename) directory))))

(defun stub-redeliver (directory)
  "Make the messages in flight visible again, as the expiration of the visibility timeout does."
  (dolist (hidden (uiop:subdirectories (merge-pathnames "state/queue/" directory)))
    (uiop:delete-directory-tree hidden :validate t)))

(defun call-with-infrastructure-stub (environment function)
  "Call FUNCTION with the AWS CLI and Terraform of OTT/OPERATION replaced by stubs.
FUNCTION receives the directory of the Terraform stub.
ENVIRONMENT is an alist of environment variables."
  (let* ((directory (make-temporary-directory))
	 (aws (install-stub #p"acceptance/aws-stub.sh" directory "aws"))
	 (terraform-directory (merge-pathnames "terraform/" directory))
	 (terraform (progn
		      (ensure-directories-exist terraform-directory)
		      (install-stub #p"acceptance/terraform-stub.sh" terraform-directory
				    "terraform"))))
    (unwind-protect
	 (let ((ott/operation:*aws-program* (list aws))
	       (ott/operation:*terraform-program* (list terraform))
	       (ott/operation:*clock* (lambda () *stub-time*))
	       (ott/operation:*getenv* (lambda (name)
					 (cdr (assoc name environment :test #'string=)))))
	   (funcall function terraform-directory))
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))))

(defmacro with-infrastructure-stub ((directory &optional environment) &body body)
  `(call-with-infrastructure-stub
    (or ,environment '(("GITHUB_ORG" . "Example-Org") ("GITHUB_REPO" . "example-repo")))
    (lambda (,directory) (declare (ignorable ,directory)) ,@body)))

(defun terraform-log-lines (directory)
  (stub-log-lines directory))


;;;;
;;;; Infrastructure
;;;;

(defparameter *expected-state-bucket* "ott-terraform-123456789012-example-org-example-repo")

(define-testcase create-infrastructure-runs-terraform ()
  "CREATE-INFRASTRUCTURE initialises the S3 backend, applies and returns the queue URL."
  (with-infrastructure-stub (directory)
    (assert-equal "https://sqs.stub/123456789012/ott-continuation"
		  (ott/operation:create-infrastructure))
    (let ((lines (terraform-log-lines directory)))
      (assert-equal '("init" "apply" "output")
		    (mapcar (lambda (line) (subseq line 0 (position #\Space line))) lines))
      (assert-t* (search (format nil "-backend-config=bucket=~A" *expected-state-bucket*)
			 (first lines)))
      (assert-t* (search "-backend-config=key=ott/infrastructure/terraform.tfstate"
			 (first lines)))
      (assert-t* (search "-backend-config=region=eu-west-3" (first lines)))
      (assert-t* (search "-auto-approve" (second lines)))
      (assert-t* (search "-var=region=eu-west-3" (second lines)))
      (assert-t* (search "-var=expiry_date=2026-10-28T12:30:00Z" (second lines)))
      (assert-t* (search "-var=github_repository=example-repo" (second lines))))))

(define-testcase destroy-infrastructure-runs-terraform ()
  "DESTROY-INFRASTRUCTURE initialises the S3 backend and destroys."
  (with-infrastructure-stub (directory)
    (assert-t (ott/operation:destroy-infrastructure))
    (assert-equal '("init" "destroy")
		  (mapcar (lambda (line) (subseq line 0 (position #\Space line)))
			  (terraform-log-lines directory)))
    (assert-t* (search "-auto-approve" (second (terraform-log-lines directory))))))

(define-testcase infrastructure-state-bucket-name ()
  "The state bucket is the one of SETUP-AWS, unless OTT_STATE_BUCKET names it."
  (with-infrastructure-stub (directory)
    (assert-equal *expected-state-bucket* (ott/operation:state-bucket-name))
    (let ((ott/development:*aws-account* "123456789012")
	  (ott/development:*github-organization* "Example-Org")
	  (ott/development:*github-repository* "example-repo"))
      (assert-equal (funcall (find-symbol "AWS-BUCKET-NAME" "OTT/DEVELOPMENT"))
		    (ott/operation:state-bucket-name))))
  (with-infrastructure-stub (directory '(("GITHUB_REPOSITORY" . "Example-Org/example-repo")))
    (assert-equal *expected-state-bucket* (ott/operation:state-bucket-name)))
  (with-infrastructure-stub (directory '(("OTT_STATE_BUCKET" . "my-bucket")))
    (assert-equal "my-bucket" (ott/operation:state-bucket-name))
    (ott/operation:create-infrastructure)
    (assert-t* (search "-backend-config=bucket=my-bucket" (first (terraform-log-lines directory)))))
  (with-infrastructure-stub (directory '(("AWS_REGION" . "eu-central-1")))
    (assert-condition (ott/operation:state-bucket-name) ott/operation:infrastructure-error)))

(define-testcase infrastructure-reports-terraform-failure ()
  "A failure of Terraform is reported and stops the operation."
  (with-infrastructure-stub (directory)
    (stub-create-state directory "fail-apply")
    (assert-condition (ott/operation:create-infrastructure) ott/operation:infrastructure-error)
    (assert-equal '("init" "apply")
		  (mapcar (lambda (line) (subseq line 0 (position #\Space line)))
			  (terraform-log-lines directory)))))

(define-testcase enforce-terraform-configuration ()
  "The Terraform configuration declares the queues, the role and the S3 backend."
  (let ((contents (uiop:read-file-string (system-relative-pathname #p"terraform/main.tf"))))
    (dolist (fragment '("backend \"s3\""
			"name                       = \"ott-continuation\""
			"name = \"ott-continuation-dlq\""
			"maxReceiveCount     = 5"
			"name               = \"OTTContinuedFunction\""
			"\"sqs:SendMessage\""
			"\"sqs:ReceiveMessage\""
			"output \"queue_url\""))
      (assert-t* (search fragment contents)))))


;;;;
;;;; AWS CLI Environment
;;;;

(defun call-with-process-environment (bindings function)
  "Call FUNCTION with the environment variables BINDINGS, an alist, set in the process.
The program is found along the PATH of the process, as a user's program is."
  (let ((saved (loop :for (name . nil) :in bindings
		     :collect (cons name (uiop:getenv name)))))
    (unwind-protect
	 (progn
	   (loop :for (name . value) :in bindings
		 :do (sb-posix:setenv name value 1))
	   (funcall function))
      (loop :for (name . value) :in saved
	    :do (if value
		    (sb-posix:setenv name value 1)
		    (sb-posix:unsetenv name))))))

(defun call-with-env-stub (function)
  "Call FUNCTION with a program named ott-aws, found along the PATH, as the AWS CLI."
  (let* ((directory (make-temporary-directory))
	 (saved ott:*aws-program*))
    (unwind-protect
	 (progn
	   (install-stub #p"acceptance/env-stub.sh" directory "ott-aws")
	   (setf ott:*aws-program* (list "ott-aws")
		 ott::*credentials* nil)
	   (call-with-process-environment
	    `(("PATH" . ,(format nil "~A:~A" (string-right-trim "/" (namestring directory))
				 (uiop:getenv "PATH")))
	      ("AWS_PROFILE" . "test-profile")
	      ("OTT_TEST_HOME" . "test-home")
	      ("AWS_DEFAULT_REGION" . "eu-west-3"))
	    function))
      (setf ott:*aws-program* saved
	    ott::*credentials* nil)
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))))

(define-testcase run-aws-keeps-process-environment ()
  "The AWS CLI is found along the PATH and sees the environment of the Lisp process."
  (call-with-env-stub
   (lambda ()
     (assert-equal "profile=test-profile home=test-home extra="
		   (ott::run-aws nil nil))
     (assert-equal "profile=test-profile home=test-home extra=1"
		   (ott::run-aws '("OTT_TEST_EXTRA=1") nil))
     (assert-equal "profile=test-profile home=test-home extra=2"
		   (ott::run-aws '("OTT_TEST_EXTRA=2") nil))
     (assert-equal "profile= home=test-home extra="
		   (ott::run-aws nil nil :unset '("AWS_PROFILE"))))))

(define-testcase queue-region-is-eu-west-3 ()
  "The queue region is eu-west-3 unless the environment names another."
  (call-with-process-environment
   '(("AWS_REGION" . "") ("AWS_DEFAULT_REGION" . ""))
   (lambda ()
     (assert-equal "eu-west-3" (ott::aws-region))
     (assert-equal '("AWS_REGION=eu-west-3" "AWS_DEFAULT_REGION=eu-west-3")
		   (ott::region-assignments))))
  (call-with-process-environment
   '(("AWS_REGION" . "") ("AWS_DEFAULT_REGION" . "eu-central-1"))
   (lambda ()
     (assert-equal "eu-central-1" (ott::aws-region)))))

(define-testcase run-aws-as-role-unsets-profile ()
  "The credentials of the role are not shadowed by AWS_PROFILE."
  (call-with-env-stub
   (lambda ()
     (let ((ott::*credentials* (cons (get-universal-time) '("OTT_TEST_EXTRA=role"))))
       (assert-equal "profile= home=test-home extra=role"
		     (ott::run-aws-as-role))))))


;;;;
;;;; Messages
;;;;

(defun make-listener-output ()
  (make-string-output-stream))

(define-testcase send-message-runs-under-role ()
  "SEND-MESSAGE and RECEIVE-MESSAGE run the AWS CLI under the role OTTContinuedFunction."
  (with-sqs-stub (directory)
    (let ((url (ott:queue-url)))
      (assert-equal "https://sqs.stub/123456789012/ott-continuation" url)
      (ott:send-message url "{\"type\":\"A\",\"id\":\"1\"}")
      (let ((messages (ott:receive-message url)))
	(assert-equal 1 (length messages))
	(assert-equal "{\"type\":\"A\",\"id\":\"1\"}" (ott:message-body (first messages)))))
    (let ((lines (stub-log-lines directory)))
      (assert-t* (find-if (lambda (line)
			    (and (search "sts assume-role" line)
				 (search "role/OTTContinuedFunction" line)))
			  lines))
      (dolist (line lines)
	(when (search "sqs " line)
	  (assert-t* (search "[role]" line)))))))

(define-testcase send-and-receive-trace-events ()
  "SEND-MESSAGE and RECEIVE-MESSAGE write the events to *TRACE-OUTPUT*."
  (with-sqs-stub (directory)
    (let* ((url (ott:queue-url))
	   (trace (with-output-to-string (*trace-output*)
		    (ott:send-message url "{\"type\":\"A\",\"id\":\"traced\"}")
		    (ott:receive-message url))))
      (assert-t* (search "SEND" trace))
      (assert-t* (search "RECEIVE" trace))
      (assert-equal 1 (count #\Newline (string-trim '(#\Newline) trace)))
      (assert-t* (search "traced" trace)))))

(define-testcase send-a-yields-b-with-same-id ()
  "An A event is handled by sending a B event with the same identifier, which is printed."
  (with-sqs-stub (directory)
    (let* ((url (ott:queue-url))
	   (id (ott:send-a :payload "hello"))
	   (output (with-output-to-string (*standard-output*)
		     (assert-equal 1 (ott:listen-once))
		     (assert-equal 1 (length (stub-directory-files directory "queue")))
		     (assert-equal 1 (ott:listen-once))
		     (assert-equal 0 (ott:listen-once)))))
      (assert-nil (stub-directory-files directory "queue"))
      (assert-t* (search (format nil "B event ~A" id) output))
      (assert-t* (search "hello" output)))))

(define-testcase unknown-event-type-reaches-dead-letter-queue ()
  "An event of unknown type is not printed, it lands in the DLQ after five receives."
  (with-sqs-stub (directory)
    (let* ((url (ott:queue-url))
	   (output (with-output-to-string (*standard-output*)
		     (ott:send-message url "{\"type\":\"C\",\"id\":\"odd\",\"payload\":null}")
		     (dotimes (attempt 5)
		       (assert-condition (ott:listen-once) ott:unknown-event-type)
		       (assert-equal 1 (length (stub-directory-files directory "queue")))
		       (stub-redeliver directory))
		     (assert-equal 0 (ott:listen-once)))))
      (assert-equal "" output)
      (assert-nil (stub-directory-files directory "queue"))
      (assert-equal 1 (length (stub-directory-files directory "dlq"))))))

(define-testcase listen-loop-survives-unknown-event-type ()
  "The listener loop reports an event of unknown type and carries on with the others."
  (with-sqs-stub (directory)
    (let* ((url (ott:queue-url))
	   (count 0)
	   (output (with-output-to-string (*standard-output*)
		     (let ((*error-output* (make-broadcast-stream)))
		       (ott:send-message url "{\"type\":\"C\",\"id\":\"odd\"}")
		       (ott:send-a :id "good")
		       (ott:listen-loop :continue-p (lambda () (< (incf count) 6)))))))
      (assert-t* (search "B event good" output))
      (assert-equal 1 (length (stub-directory-files directory "queue"))))))

(define-testcase listeners-share-the-work ()
  "Two listeners handle ten A events: ten B events are printed, each once, by both listeners."
  (with-sqs-stub (directory)
    (let* ((url (ott:queue-url))
	   (ids (loop :for index :from 1 :to 10
		      :collect (ott:send-a :id (format nil "event-~D" index))))
	   (handled (list 0 0))
	   (lock (sb-thread:make-mutex))
	   (outputs (list (make-listener-output) (make-listener-output)))
	   (threads
	     (loop :for index :from 0 :to 1
		   :collect (let ((index index)
				  (output (nth index outputs)))
			      (sb-thread:make-thread
			       (lambda ()
				 (let ((*standard-output* output)
				       (idle 0))
				   (loop :while (< idle 5)
					 :do (let ((count (ott:listen-once)))
					       (cond
						 ((plusp count)
						  (setf idle 0)
						  (sb-thread:with-mutex (lock)
						    (incf (nth index handled) count)))
						 (t
						  (incf idle)
						  (sleep 0.05))))))))))))
      (mapc #'sb-thread:join-thread threads)
      (let ((text (format nil "~{~A~}" (mapcar #'get-output-stream-string outputs))))
	(dolist (id ids)
	  (assert-equal 1 (count-occurrences (format nil "B event ~A:" id) text))))
      (assert-equal 20 (reduce #'+ handled))
      (assert-t* (plusp (first handled)))
      (assert-t* (plusp (second handled)))
      (assert-nil (stub-directory-files directory "queue")))))

(defun count-occurrences (needle haystack)
  (loop :with start = 0
	:for position = (search needle haystack :start2 start)
	:while position
	:count t
	:do (setf start (1+ position))))


;;;;
;;;; Workflows
;;;;

(defparameter *workflow-fragments*
  '("workflow_dispatch:" "id-token: write" "aws-actions/configure-aws-credentials"
    "hashicorp/setup-terraform" "OTT_STATE_BUCKET" "sbcl")
  "The fragments found in the workflows that create and destroy the infrastructure.")

(define-testcase enforce-create-infrastructure-workflow ()
  "The workflow create-infrastructure.yml runs CREATE-INFRASTRUCTURE."
  (let ((workflow (system-relative-pathname #p".github/workflows/create-infrastructure.yml")))
    (assert-t* (probe-file workflow))
    (let ((contents (uiop:read-file-string workflow)))
      (dolist (fragment (cons "ott/operation:create-infrastructure" *workflow-fragments*))
	(assert-t* (search fragment contents :test #'char-equal))))))

(define-testcase enforce-destroy-infrastructure-workflow ()
  "The workflow destroy-infrastructure.yml runs DESTROY-INFRASTRUCTURE."
  (let ((workflow (system-relative-pathname #p".github/workflows/destroy-infrastructure.yml")))
    (assert-t* (probe-file workflow))
    (let ((contents (uiop:read-file-string workflow)))
      (dolist (fragment (cons "ott/operation:destroy-infrastructure" *workflow-fragments*))
	(assert-t* (search fragment contents :test #'char-equal))))))

(define-testcase create-infrastructure ()
  (create-infrastructure-runs-terraform)
  (destroy-infrastructure-runs-terraform)
  (infrastructure-state-bucket-name)
  (infrastructure-reports-terraform-failure)
  (enforce-terraform-configuration)
  (run-aws-keeps-process-environment)
  (queue-region-is-eu-west-3)
  (run-aws-as-role-unsets-profile)
  (send-message-runs-under-role)
  (send-and-receive-trace-events)
  (send-a-yields-b-with-same-id)
  (unknown-event-type-reaches-dead-letter-queue)
  (listen-loop-survives-unknown-event-type)
  (listeners-share-the-work)
  (enforce-create-infrastructure-workflow)
  (enforce-destroy-infrastructure-workflow))

;;;; End of file `create-infrastructure.lisp'
