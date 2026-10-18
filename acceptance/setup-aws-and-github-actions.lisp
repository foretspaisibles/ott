;;;; setup-aws-and-github-actions.lisp — Acceptance Tests for the AWS Setup

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
;;;; AWS CLI Stub
;;;;

(defparameter *stub-account* "123456789012")

(defparameter *stub-time*
  (encode-universal-time 0 30 12 21 10 2026 0)
  "The time at which the stubbed operations are performed.")

(defparameter *one-week* (* 7 24 60 60))

(defun make-temporary-directory ()
  (let ((directory
	  (uiop:ensure-directory-pathname
	   (string-trim '(#\Newline)
			(uiop:run-program '("mktemp" "-d" "-t" "ott-acceptance")
					  :output :string)))))
    directory))

(defun call-with-aws-stub (function)
  "Call FUNCTION with the AWS CLI replaced by a stub, passing the stub directory."
  (let* ((directory (make-temporary-directory))
	 (stub (merge-pathnames "aws" directory)))
    (unwind-protect
	 (progn
	   (uiop:copy-file (system-relative-pathname #p"acceptance/aws-stub.sh") stub)
	   (uiop:run-program (list "chmod" "755" (namestring stub)))
	   (let ((ott/development:*aws-program* (list (namestring stub)))
		 (ott/development:*aws-account* *stub-account*)
		 (ott/development:*github-organization* "Example-Org")
		 (ott/development:*github-repository* "example-repo")
		 (ott/development:*clock* (lambda () *stub-time*)))
	     (funcall function directory)))
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))))

(defmacro with-aws-stub ((directory) &body body)
  `(call-with-aws-stub (lambda (,directory) ,@body)))

(defun stub-state-file (directory name)
  (merge-pathnames name (merge-pathnames "state/" directory)))

(defun stub-state-p (directory name)
  (probe-file (stub-state-file directory name)))

(defun stub-file-contents (directory name)
  "The contents of the state file NAME of the stub, trimmed, or NIL when it is absent."
  (let ((file (stub-state-file directory name)))
    (when (probe-file file)
      (string-trim '(#\Newline #\Space) (uiop:read-file-string file)))))

(defun stub-create-state (directory name &optional contents)
  (let ((file (stub-state-file directory name)))
    (ensure-directories-exist file)
    (with-open-file (stream file :direction :output :if-exists :supersede)
      (when contents
	(write-line contents stream)))))

(defun stub-log-lines (directory)
  (let ((contents (stub-file-contents directory "log")))
    (and contents (uiop:split-string contents :separator '(#\Newline)))))

(defun stub-resource-names ()
  "The names of the state files the stub keeps for the resources of the setup."
  (list "bucket" "role" "oidc"
	"policy-ott-github-actions-Example-Org-example-repo-state"
	"policy-ott-github-actions-Example-Org-example-repo-provisioning"
	"policy-ott-github-actions-Example-Org-example-repo-guards"))

(defun stub-resources (directory)
  (remove-if-not (lambda (name) (stub-state-p directory name)) (stub-resource-names)))

(defun stub-expiry-dates (directory)
  "The ExpiryDate labels recorded by the stub for the resources of the setup."
  (loop :for name :in (remove "oidc" (stub-resource-names) :test #'string=)
	:when (stub-state-p directory name)
	  :collect (cons name (stub-file-contents directory (format nil "~A.expiry" name)))))

(defparameter *expected-expiry-date* "2026-10-28T12:30:00Z")


;;;;
;;;; Parameters
;;;;

(defun empty-to-nil (string)
  (and string (plusp (length string)) string))

(define-testcase setup-aws-parameters-from-environment ()
  "The parameters of the AWS setup are read from the environment."
  (assert-equal (empty-to-nil (uiop:getenv "AWS_ACCOUNT_ID"))
		(symbol-value (find-symbol "*AWS-ACCOUNT*" "OTT/DEVELOPMENT")))
  (assert-equal (empty-to-nil (uiop:getenv "GITHUB_ORG"))
		(symbol-value (find-symbol "*GITHUB-ORGANIZATION*" "OTT/DEVELOPMENT")))
  (assert-equal (empty-to-nil (uiop:getenv "GITHUB_REPO"))
		(symbol-value (find-symbol "*GITHUB-REPOSITORY*" "OTT/DEVELOPMENT"))))


;;;;
;;;; Setup
;;;;

(define-testcase setup-aws-fails-without-credentials ()
  "SETUP-AWS signals an error and creates no resource without credentials."
  (with-aws-stub (directory)
    (stub-create-state directory "fail-sts")
    (assert-condition (ott/development:setup-aws) ott/development:aws-setup-error)
    (assert-nil (stub-resources directory))
    (assert-equal 1 (length (stub-log-lines directory)))))

(define-testcase setup-aws-requires-github-environment ()
  "SETUP-AWS signals an error when one of the parameters is NIL."
  (with-aws-stub (directory)
    (let ((ott/development:*github-organization* nil))
      (assert-condition (ott/development:setup-aws) ott/development:aws-setup-error))
    (let ((ott/development:*github-repository* nil))
      (assert-condition (ott/development:setup-aws) ott/development:aws-setup-error))
    (let ((ott/development:*aws-account* nil))
      (assert-condition (ott/development:setup-aws) ott/development:aws-setup-error))
    (stub-create-state directory "account" "999999999999")
    (assert-condition (ott/development:setup-aws) ott/development:aws-setup-error)
    (assert-nil (stub-resources directory))))

(define-testcase setup-aws-creates-resources ()
  "SETUP-AWS creates the bucket, the role and the policies and returns the role ARN."
  (with-aws-stub (directory)
    (assert-equal "arn:aws:iam::123456789012:role/ott-github-actions-Example-Org-example-repo"
		  (ott/development:setup-aws))
    (assert-equal (stub-resource-names) (stub-resources directory))
    ;; Running the setup again reuses the existing resources.
    (assert-equal "arn:aws:iam::123456789012:role/ott-github-actions-Example-Org-example-repo"
		  (ott/development:setup-aws))
    (assert-equal 1 (count-if (lambda (line)
				(search "create-bucket" line))
			      (stub-log-lines directory)))
    (assert-equal 1 (count-if (lambda (line)
				(search "create-role" line))
			      (stub-log-lines directory)))))

(define-testcase setup-aws-keeps-existing-oidc-provider ()
  "SETUP-AWS does not create the OIDC provider when the account has one."
  (with-aws-stub (directory)
    (stub-create-state directory "oidc")
    (ott/development:setup-aws)
    (assert-nil (find-if (lambda (line) (search "create-open-id-connect-provider" line))
			 (stub-log-lines directory)))))

(define-testcase setup-aws-trusts-immutable-github-subjects ()
  "The trust policy accepts the name based and the immutable GitHub OIDC subjects."
  (with-aws-stub (directory)
    (ott/development:setup-aws)
    (let ((line (find-if (lambda (line) (search "iam create-role" line))
			 (stub-log-lines directory))))
      (assert-t* (search "repo:Example-Org/example-repo:*" line))
      (assert-t* (search "repo:Example-Org@*/example-repo@*:*" line)))))

(define-testcase setup-aws-honours-github-subject ()
  "An explicit GitHub subject filter replaces the default filters."
  (with-aws-stub (directory)
    (let ((ott/development:*github-subject*
	    "repo:Example-Org/example-repo:ref:refs/heads/main"))
      (ott/development:setup-aws))
    (let ((line (find-if (lambda (line) (search "iam create-role" line))
			 (stub-log-lines directory))))
      (assert-t* (search "repo:Example-Org/example-repo:ref:refs/heads/main" line))
      (assert-nil (search "example-repo@*" line)))))

(define-testcase setup-aws-writes-manifest ()
  "SETUP-AWS stores a manifest of its resources in the state bucket, EXTEND-AWS-LEASE updates it."
  (with-aws-stub (directory)
    (ott/development:setup-aws)
    (let ((manifest (stub-file-contents directory "manifest.json")))
      (assert-t* manifest)
      (assert-t* (search "\"account\":\"123456789012\"" manifest))
      (assert-t* (search "\"role\":{\"name\":\"ott-github-actions-Example-Org-example-repo\""
			 manifest))
      (assert-t* (search "\"expiryDate\":\"2026-10-28T12:30:00Z\"" manifest)))
    (assert-t* (find-if (lambda (line) (search "put-object --bucket" line))
			(stub-log-lines directory)))
    (ott/development:extend-aws-lease 3600)
    (assert-t* (search "\"expiryDate\":\"2026-10-21T13:30:00Z\""
		       (stub-file-contents directory "manifest.json")))))

(define-testcase setup-aws-policies-fit-iam-limit ()
  "The managed policies are under the 6,144 characters IAM limit and guard the role."
  (with-aws-stub (directory)
    (ott/development:setup-aws)
    (let ((documents
	    (loop :for line :in (stub-log-lines directory)
		  :when (search "iam create-policy " line)
		    :collect (subseq line (search "{\"Version\"" line)))))
      (assert-equal 3 (length documents))
      (dolist (document documents)
	(assert-t* (< (length document) 6144)))
      (assert-t* (search "iam:PassedToService" (third documents)))
      (assert-t* (search "ec2:RunInstances" (third documents))))))

(define-testcase setup-aws-labels-expiry-date ()
  "Every resource is labelled with an ExpiryDate one week after the setup time."
  (with-aws-stub (directory)
    (ott/development:setup-aws)
    (let ((dates (stub-expiry-dates directory)))
      (assert-equal 5 (length dates))
      (dolist (date dates)
	(assert-equal *expected-expiry-date* (cdr date))))
    (assert-t* (every (lambda (line)
			(or (not (search "create-" line))
			    (search "create-open-id-connect-provider" line)
			    (search "create-policy-version" line)
			    (and (search "GitHubOrganization" line)
				 (search "GitHubRepository" line)
				 (search "ExpiryDate" line))
			    (search "create-bucket" line)))
		      (stub-log-lines directory)))))


;;;;
;;;; Lease
;;;;

(define-testcase extend-aws-lease-updates-expiry-date ()
  "EXTEND-AWS-LEASE moves the ExpiryDate to the invocation time plus the duration."
  (with-aws-stub (directory)
    (assert-condition (ott/development:extend-aws-lease) ott/development:aws-setup-error)
    (ott/development:setup-aws)
    (let ((ott/development:*clock* (lambda () (+ *stub-time* (* 3 24 60 60)))))
      (assert-equal "2026-10-31T12:30:00Z" (ott/development:extend-aws-lease)))
    (dolist (date (stub-expiry-dates directory))
      (assert-equal "2026-10-31T12:30:00Z" (cdr date)))
    (let ((ott/development:*clock* (lambda () *stub-time*)))
      (assert-equal "2026-10-21T13:30:00Z" (ott/development:extend-aws-lease 3600)))
    (dolist (date (stub-expiry-dates directory))
      (assert-equal "2026-10-21T13:30:00Z" (cdr date)))))


;;;;
;;;; Tear Down
;;;;

(define-testcase tear-down-aws-honours-expiry-date ()
  "TEAR-DOWN-AWS keeps unexpired resources, reclaims expired ones and honours FORCE."
  (with-aws-stub (directory)
    (ott/development:setup-aws)
    (assert-nil (ott/development:tear-down-aws))
    (assert-equal (stub-resource-names) (stub-resources directory))
    (let ((ott/development:*clock* (lambda () (+ *stub-time* *one-week* 1))))
      (assert-t (ott/development:tear-down-aws)))
    (assert-equal '("oidc") (stub-resources directory))
    (assert-nil (ott/development:tear-down-aws))
    (ott/development:setup-aws)
    (assert-t (ott/development:tear-down-aws :force t))
    (assert-equal '("oidc") (stub-resources directory))))


;;;;
;;;; Describe
;;;;

(defun call-with-github-actions-environment (summary function)
  "Call FUNCTION with GITHUB_ACTIONS and GITHUB_STEP_SUMMARY set from SUMMARY, if any."
  (let ((ott/development:*getenv*
	  (lambda (name)
	    (cond
	      ((and summary (string= name "GITHUB_ACTIONS"))
	       "true")
	      ((and summary (string= name "GITHUB_STEP_SUMMARY"))
	       (namestring summary))
	      ((member name '("GITHUB_ACTIONS" "GITHUB_STEP_SUMMARY") :test #'string=)
	       nil)
	      (t
	       (uiop:getenv name))))))
    (funcall function)))

(defun resource-entries (resources)
  (loop :for (nil entry) :on resources :by #'cddr
	:collect entry))

(define-testcase describe-aws-setup-returns-plist ()
  "DESCRIBE-AWS-SETUP returns the resources and writes no job summary outside GitHub Actions."
  (with-aws-stub (directory)
    (ott/development:setup-aws)
    (let* ((summary (stub-state-file directory "summary.md"))
	   (resources
	     (call-with-github-actions-environment
	      nil
	      (lambda () (let ((*standard-output* (make-broadcast-stream)))
			   (ott/development:describe-aws-setup)))))
	   (entries (resource-entries resources)))
      (assert-equal 6 (length entries))
      (dolist (entry entries)
	(assert-t* (getf entry :expiry-date))
	(assert-string-match (getf entry :console-url) "https://*console.aws.amazon.com/*"))
      (assert-equal *expected-expiry-date* (getf (getf resources :github-actions-role)
						 :expiry-date))
      (assert-nil (probe-file summary)))))

(define-testcase describe-aws-setup-writes-job-summary ()
  "DESCRIBE-AWS-SETUP writes a Markdown table to the job summary in GitHub Actions."
  (with-aws-stub (directory)
    (ott/development:setup-aws)
    (let* ((summary (stub-state-file directory "summary.md"))
	   (resources
	     (call-with-github-actions-environment
	      summary
	      (lambda () (let ((*standard-output* (make-broadcast-stream)))
			   (ott/development:describe-aws-setup)))))
	   (contents (uiop:read-file-string summary)))
      (assert-t* (search "| Type | Name | Expiry date | Console |" contents))
      (dolist (entry (resource-entries resources))
	(assert-t* (search (getf entry :name) contents))
	(assert-t* (search (getf entry :expiry-date) contents))
	(assert-t* (search (getf entry :console-url) contents))))))


;;;;
;;;; GitHub Actions Secrets
;;;;

(defun call-with-gh-stub (function)
  "Call FUNCTION with the GitHub CLI replaced by a stub, passing the stub directory."
  (with-aws-stub (directory)
    (let ((stub (merge-pathnames "gh" directory)))
      (uiop:copy-file (system-relative-pathname #p"acceptance/gh-stub.sh") stub)
      (uiop:run-program (list "chmod" "755" (namestring stub)))
      (let ((ott/development:*gh-program* (list (namestring stub))))
	(funcall function directory)))))

(defun stub-gh-lines (directory)
  (let ((contents (stub-file-contents directory "gh-log")))
    (and contents (uiop:split-string contents :separator '(#\Newline)))))

(define-testcase setup-github-action-sets-secrets ()
  "SETUP-GITHUB-ACTION sets the repository secrets from the AWS setup."
  (call-with-gh-stub
   (lambda (directory)
     (assert-condition (ott/development:setup-github-action)
		       ott/development:aws-setup-error)
     (assert-nil (stub-gh-lines directory))
     (ott/development:setup-aws)
     (assert-equal '("AWS_ACCOUNT_ID" "AWS_REGION" "AWS_ROLE_ARN")
		   (let ((*standard-output* (make-broadcast-stream)))
		     (ott/development:setup-github-action)))
     (assert-equal
      `("secret set AWS_ACCOUNT_ID --repo Example-Org/example-repo=123456789012"
	"secret set AWS_REGION --repo Example-Org/example-repo=eu-west-3"
	,(concatenate 'string
		      "secret set AWS_ROLE_ARN --repo Example-Org/example-repo="
		      "arn:aws:iam::123456789012:role/ott-github-actions-Example-Org-example-repo"))
      (stub-gh-lines directory)))))

(define-testcase setup-github-action-reports-gh-failure ()
  "SETUP-GITHUB-ACTION signals an error when the GitHub CLI fails."
  (call-with-gh-stub
   (lambda (directory)
     (ott/development:setup-aws)
     (stub-create-state directory "fail-gh")
     (assert-condition (ott/development:setup-github-action)
		       ott/development:aws-setup-error))))


;;;;
;;;; Workflow
;;;;

(define-testcase enforce-describe-aws-setup-workflow ()
  "The workflow describe-aws-setup.yml is scheduled and runs DESCRIBE-AWS-SETUP."
  (let ((workflow (system-relative-pathname #p".github/workflows/describe-aws-setup.yml")))
    (assert-t* (probe-file workflow))
    (let ((contents (uiop:read-file-string workflow)))
      (assert-t* (search "schedule:" contents))
      (assert-t* (search "workflow_dispatch:" contents))
      (assert-t* (search "id-token: write" contents))
      (assert-t* (search "contents: read" contents))
      (assert-t* (search "aws-actions/configure-aws-credentials" contents))
      (assert-t* (search "describe-aws-setup" contents :test #'char-equal))
      (assert-t* (search "sbcl" contents)))))

(define-testcase setup-aws-and-github-actions ()
  (setup-aws-parameters-from-environment)
  (setup-aws-fails-without-credentials)
  (setup-aws-requires-github-environment)
  (setup-aws-creates-resources)
  (setup-aws-keeps-existing-oidc-provider)
  (setup-aws-policies-fit-iam-limit)
  (setup-aws-labels-expiry-date)
  (extend-aws-lease-updates-expiry-date)
  (tear-down-aws-honours-expiry-date)
  (describe-aws-setup-returns-plist)
  (describe-aws-setup-writes-job-summary)
  (setup-github-action-sets-secrets)
  (setup-github-action-reports-gh-failure)
  (enforce-describe-aws-setup-workflow))

;;;; End of file `setup-aws-and-github-actions.lisp'
