;;;; operation.lisp — Create and destroy the OTT infrastructure

;;;; OTT (https://github.com/foretspaisibles/ott)
;;;; This file is part of OTT.
;;;;
;;;; Copyright © 2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:ott/operation
  (:use #:common-lisp)
  (:export
   #:*aws-program*
   #:*clock*
   #:*getenv*
   #:*terraform-program*
   #:create-infrastructure
   #:destroy-infrastructure
   #:infrastructure-error
   #:state-bucket-name))

(in-package #:ott/operation)


;;;;
;;;; Parameters
;;;;

(defvar *getenv* #'uiop:getenv
  "The function returning the value of an environment variable, or NIL.")

(defvar *aws-program* '("aws")
  "The command, as a list of strings, which runs the AWS CLI.")

(defvar *terraform-program* '("terraform")
  "The command, as a list of strings, which runs Terraform.")

(defvar *clock* #'get-universal-time
  "The function returning the universal time at which an operation is performed.")

(defparameter *lease-duration* (* 7 24 60 60)
  "The duration, in seconds, of the lease of the infrastructure.")

(defparameter *state-key* "ott/infrastructure/terraform.tfstate"
  "The object key of the Terraform state in the state bucket.")

(define-condition infrastructure-error (error)
  ((message
    :initarg :message
    :reader infrastructure-error-message))
  (:report (lambda (condition stream)
	     (write-string (infrastructure-error-message condition) stream)))
  (:documentation "The infrastructure cannot be created or destroyed."))

(defun infrastructure-error (control &rest arguments)
  "Signal an INFRASTRUCTURE-ERROR with a message built from CONTROL and ARGUMENTS."
  (error 'infrastructure-error :message (apply #'format nil control arguments)))

(defun environment-parameter (name)
  "The value of the environment variable NAME, or NIL when it is unset or empty."
  (let ((value (funcall *getenv* name)))
    (and value (plusp (length value)) value)))

(defun trim (string)
  (string-trim '(#\Newline #\Space) string))


;;;;
;;;; Naming
;;;;

(defun aws-region ()
  (or (environment-parameter "AWS_REGION")
      (environment-parameter "AWS_DEFAULT_REGION")
      "eu-west-3"))

(defun github-organization-and-repository ()
  "The GitHub organization and repository, from GITHUB_ORG and GITHUB_REPO
or else from GITHUB_REPOSITORY, which GitHub Actions sets to ORG/REPO."
  (let ((organization (environment-parameter "GITHUB_ORG"))
	(repository (environment-parameter "GITHUB_REPO"))
	(slug (environment-parameter "GITHUB_REPOSITORY")))
    (when (and slug (not (and organization repository)))
      (let ((slash (position #\/ slug)))
	(when slash
	  (setf organization (or organization (subseq slug 0 slash))
		repository (or repository (subseq slug (1+ slash)))))))
    (values organization repository)))

(defun run-aws (&rest arguments)
  "Run the AWS CLI with ARGUMENTS and return its trimmed output.
Signal an INFRASTRUCTURE-ERROR when the command fails."
  (multiple-value-bind (output error-output exit-code)
      (handler-case
	  (uiop:run-program (append *aws-program* arguments)
			    :output :string :error-output :string :input nil
			    :ignore-error-status t)
	(error (condition)
	  (values "" (princ-to-string condition) 127)))
    (unless (zerop exit-code)
      (infrastructure-error "The command `aws~{ ~A~}' failed with exit code ~D: ~A"
			    (subseq arguments 0 (min 2 (length arguments)))
			    exit-code (trim error-output)))
    (trim output)))

(defun aws-account ()
  "The account of the current AWS credentials."
  (run-aws "sts" "get-caller-identity" "--query" "Account" "--output" "text"))

(defun sanitize-name (string)
  (map 'string
       (lambda (character)
	 (if (and (alphanumericp character) (< (char-code character) 128))
	     (char-downcase character)
	     #\-))
       string))

(defun state-bucket-name ()
  "The name of the S3 bucket holding the Terraform state.
It is the value of OTT_STATE_BUCKET when set, a GitHub secret can convey it.
Otherwise it is the name SETUP-AWS gives the bucket, inferred from the account
of the AWS credentials and the GitHub organization and repository."
  (or (environment-parameter "OTT_STATE_BUCKET")
      (multiple-value-bind (organization repository)
	  (github-organization-and-repository)
	(unless (and organization repository)
	  (infrastructure-error
	   "Cannot infer the state bucket: set OTT_STATE_BUCKET, or GITHUB_ORG and GITHUB_REPO."))
	(let ((name (sanitize-name (format nil "ott-terraform-~A-~A-~A"
					   (aws-account) organization repository))))
	  (subseq name 0 (min 63 (length name)))))))


;;;;
;;;; Terraform
;;;;

(defun terraform-directory ()
  (merge-pathnames #p"terraform/" (asdf:system-source-directory "ott")))

(defun format-timestamp (universal-time)
  "The ISO 8601 representation in UTC of UNIVERSAL-TIME."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
	    year month day hour minute second)))

(defun terraform (&rest arguments)
  "Run Terraform in the Terraform directory with ARGUMENTS, forwarding its output.
Signal an INFRASTRUCTURE-ERROR when the command fails."
  (multiple-value-bind (output error-output exit-code)
      (handler-case
	  (uiop:run-program (append *terraform-program*
				    (list (format nil "-chdir=~A"
						  (uiop:native-namestring (terraform-directory))))
				    arguments)
			    :output *standard-output* :error-output *error-output*
			    :input nil :ignore-error-status t)
	(error (condition)
	  (values nil (princ-to-string condition) 127)))
    (declare (ignore output))
    (unless (zerop exit-code)
      (infrastructure-error "The command `terraform ~A' failed with exit code ~D~@[: ~A~]"
			    (first arguments) exit-code
			    (and (stringp error-output) (trim error-output))))))

(defun terraform-output (name)
  "The value of the Terraform output NAME."
  (let ((*standard-output* (make-string-output-stream)))
    (terraform "output" "-raw" name)
    (get-output-stream-string *standard-output*)))

(defun terraform-initialize ()
  (terraform "init" "-input=false" "-reconfigure"
	     (format nil "-backend-config=bucket=~A" (state-bucket-name))
	     (format nil "-backend-config=key=~A" *state-key*)
	     (format nil "-backend-config=region=~A" (aws-region))))

(defun terraform-variables ()
  (multiple-value-bind (organization repository)
      (github-organization-and-repository)
    (list (format nil "-var=region=~A" (aws-region))
	  (format nil "-var=github_organization=~A" (or organization ""))
	  (format nil "-var=github_repository=~A" (or repository ""))
	  (format nil "-var=expiry_date=~A"
		  (format-timestamp (+ (funcall *clock*) *lease-duration*))))))

(defun create-infrastructure ()
  "Create or update the SQS queues and the role OTTContinuedFunction with Terraform.
The Terraform state is stored in the S3 state bucket.  Return the URL of the
queue ott-continuation."
  (terraform-initialize)
  (apply #'terraform "apply" "-input=false" "-auto-approve" (terraform-variables))
  (terraform-output "queue_url"))

(defun destroy-infrastructure ()
  "Destroy the resources created by CREATE-INFRASTRUCTURE.  Return true."
  (terraform-initialize)
  (apply #'terraform "destroy" "-input=false" "-auto-approve" (terraform-variables))
  t)

;;;; End of file `operation.lisp'
