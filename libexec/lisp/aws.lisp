;;;; aws.lisp — AWS Setup for OTT

;;;; OTT (https://github.com/foretspaisibles/ott)
;;;; This file is part of OTT.
;;;;
;;;; Copyright © 2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:ott/development)


;;;;
;;;; Parameters
;;;;

(defvar *getenv* #'uiop:getenv
  "The function returning the value of an environment variable, or NIL.")

(defun environment-parameter (name)
  "The value of the environment variable NAME, or NIL when it is unset or empty."
  (let ((value (funcall *getenv* name)))
    (and value (plusp (length value)) value)))

(defvar *aws-account* (environment-parameter "AWS_ACCOUNT_ID")
  "The identifier of the AWS account hosting the setup.")

(defvar *github-organization* (environment-parameter "GITHUB_ORG")
  "The GitHub organization owning the repository allowed to assume the role.")

(defvar *github-repository* (environment-parameter "GITHUB_REPO")
  "The GitHub repository allowed to assume the role.")

(defvar *github-subject* (environment-parameter "GITHUB_SUBJECT")
  "The OIDC subject filter, a StringLike pattern, the role trusts.
When it is NIL the role trusts every workflow of the repository, see
GITHUB-SUBJECT-FILTERS.  Narrow it for a stricter trust boundary, for instance
repo:ORG/REPO:ref:refs/heads/main or repo:ORG/REPO:environment:production.")

(defvar *aws-program* '("aws")
  "The command, as a list of strings, which runs the AWS CLI.")

(defvar *clock* #'get-universal-time
  "The function returning the universal time at which an operation is performed.")

(defparameter *aws-lease-duration* (* 7 24 60 60)
  "The default duration of a lease, in seconds.")

(defparameter *aws-allowed-regions* '("eu-west-1" "eu-west-3" "eu-central-1" "us-east-1")
  "The regions in which the role is allowed to use regional services.")

(defparameter *aws-passable-services*
  '("lambda.amazonaws.com" "ecs-tasks.amazonaws.com" "events.amazonaws.com"
    "scheduler.amazonaws.com")
  "The service principals to which the role may pass IAM roles.")

(defparameter *aws-attachable-policies*
  '("arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole"
    "arn:aws:iam::aws:policy/service-role/AmazonECSTaskExecutionRolePolicy"
    "arn:aws:iam::aws:policy/AmazonSSMManagedInstanceCore"
    "arn:aws:iam::aws:policy/CloudWatchAgentServerPolicy")
  "The AWS managed policies which the role may attach to roles it creates.")

(defparameter *aws-oidc-provider-host* "token.actions.githubusercontent.com"
  "The host of the GitHub OIDC identity provider.")

(defparameter *aws-oidc-thumbprints*
  '("6938fd4d98bab03faadb97b34396831e3780aea1"
    "1c58a3a8518e8759bf075b76b750d4f2df264fcd")
  "The OIDC signing CA thumbprints of GitHub.
AWS validates the full TLS chain and no longer enforces them, but older versions
of the AWS CLI still require at least one thumbprint.")

(defparameter *aws-policy-kinds* '(:state :provisioning :guards)
  "The kinds of IAM managed policies attached to the role.")


;;;;
;;;; Conditions and AWS CLI
;;;;

(define-condition aws-setup-error (error)
  ((message
    :initarg :message
    :reader aws-setup-error-message))
  (:report (lambda (condition stream)
	     (write-string (aws-setup-error-message condition) stream)))
  (:documentation "The AWS setup cannot be performed."))

(defun aws-setup-error (control &rest arguments)
  "Signal an AWS-SETUP-ERROR with a message built from CONTROL and ARGUMENTS."
  (error 'aws-setup-error :message (apply #'format nil control arguments)))

(defun run-aws (arguments)
  "Run the AWS CLI with ARGUMENTS and return its trimmed output and its exit code.
A failure to launch the program is reported as an exit code of 127."
  (handler-case
      (multiple-value-bind (output error-output exit-code)
	  (uiop:run-program (append *aws-program* arguments)
			    :output :string
			    :error-output :string
			    :input nil
			    :ignore-error-status t)
	(values (string-trim '(#\Newline #\Space) output) exit-code error-output))
    (error (condition)
      (values "" 127 (princ-to-string condition)))))

(defun aws (&rest arguments)
  "Run the AWS CLI with ARGUMENTS and return its trimmed output.
Signal an AWS-SETUP-ERROR when the command fails."
  (multiple-value-bind (output exit-code error-output)
      (run-aws arguments)
    (unless (zerop exit-code)
      (aws-setup-error "The command `aws~{ ~A~}' failed with exit code ~D: ~A"
		       (subseq arguments 0 (min 2 (length arguments)))
		       exit-code
		       (string-trim '(#\Newline #\Space) error-output)))
    output))

(defun aws-succeeds-p (&rest arguments)
  "True when the AWS CLI exits succesfully with ARGUMENTS."
  (zerop (nth-value 1 (run-aws arguments))))


;;;;
;;;; Time
;;;;

(defun current-universal-time ()
  (funcall *clock*))

(defun format-timestamp (universal-time)
  "The ISO 8601 representation in UTC of UNIVERSAL-TIME."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
	    year month day hour minute second)))

(defun parse-timestamp (string)
  "The universal time denoted by the ISO 8601 UTC timestamp STRING."
  (flet ((field (start end)
	   (parse-integer string :start start :end end)))
    (handler-case
	(progn
	  (unless (and (= (length string) 20) (char= #\Z (char string 19)))
	    (error "Invalid timestamp."))
	  (encode-universal-time (field 17 19) (field 14 16) (field 11 13)
				 (field 8 10) (field 5 7) (field 0 4) 0))
      (error ()
	(aws-setup-error "Cannot parse ~S as an ISO 8601 UTC timestamp." string)))))


;;;;
;;;; Names
;;;;

(defun sanitize-name (string &key (allowed "-_") (case :downcase))
  "Replace in STRING the characters which are neither alphanumeric nor in ALLOWED by a dash."
  (map 'string
       (lambda (character)
	 (cond
	   ((and (alphanumericp character) (< (char-code character) 128))
	    (if (eq case :downcase) (char-downcase character) character))
	   ((find character allowed) character)
	   (t #\-)))
       string))

(defun truncate-name (string length)
  (subseq string 0 (min length (length string))))

(defun aws-bucket-name ()
  "The name of the S3 bucket holding the Terraform state."
  (truncate-name
   (sanitize-name (format nil "ott-terraform-~A-~A-~A"
			  *aws-account* *github-organization* *github-repository*)
		  :allowed "-")
   63))

(defun aws-role-name ()
  "The name of the IAM role assumed by GitHub Actions."
  (truncate-name
   (sanitize-name (format nil "ott-github-actions-~A-~A"
			  *github-organization* *github-repository*)
		  :allowed "-_" :case :preserve)
   64))

(defun aws-role-arn ()
  (format nil "arn:aws:iam::~A:role/~A" *aws-account* (aws-role-name)))

(defun aws-policy-name (kind)
  (format nil "~A-~(~A~)" (aws-role-name) kind))

(defun aws-policy-arn (kind)
  (format nil "arn:aws:iam::~A:policy/~A" *aws-account* (aws-policy-name kind)))

(defun aws-bucket-arn ()
  (format nil "arn:aws:s3:::~A" (aws-bucket-name)))

(defun aws-oidc-provider-arn ()
  (format nil "arn:aws:iam::~A:oidc-provider/~A" *aws-account* *aws-oidc-provider-host*))

(defun aws-region ()
  (or (environment-parameter "AWS_REGION")
      (environment-parameter "AWS_DEFAULT_REGION")
      "eu-west-3"))

(defun aws-labels (expiry-date)
  "The labels applied to every resource of the setup, as an alist."
  `(("GitHubOrganization" . ,*github-organization*)
    ("GitHubRepository" . ,*github-repository*)
    ("ExpiryDate" . ,expiry-date)))

(defun iam-tag-arguments (expiry-date)
  (loop :for (key . value) :in (aws-labels expiry-date)
	:collect (format nil "Key=~A,Value=~A" key value)))

(defun s3-tagging-argument (expiry-date)
  (format nil "TagSet=[~{~A~^,~}]"
	  (loop :for (key . value) :in (aws-labels expiry-date)
		:collect (format nil "{Key=~A,Value=~A}" key value))))


;;;;
;;;; JSON
;;;;

(defun json (value)
  "The compact JSON representation of VALUE.
A list (:OBJECT KEY VALUE ...) is an object, a vector is an array, a string is
a string and :TRUE and :FALSE are the booleans."
  (with-output-to-string (stream)
    (labels ((emit-string (string)
	       (write-char #\" stream)
	       (loop :for character :across string
		     :do (when (find character "\"\\")
			   (write-char #\\ stream))
			 (write-char character stream))
	       (write-char #\" stream))
	     (emit (value)
	       (etypecase value
		 (string (emit-string value))
		 ((eql :true) (write-string "true" stream))
		 ((eql :false) (write-string "false" stream))
		 (vector
		  (write-char #\[ stream)
		  (loop :for element :across value
			:for first = t :then nil
			:do (unless first (write-char #\, stream))
			    (emit element))
		  (write-char #\] stream))
		 (cons
		  (assert (eq :object (first value)))
		  (write-char #\{ stream)
		  (loop :for (key element) :on (rest value) :by #'cddr
			:for first = t :then nil
			:do (unless first (write-char #\, stream))
			    (emit-string key)
			    (write-char #\: stream)
			    (emit element))
		  (write-char #\} stream)))))
      (emit value))))

(defun json-array (list)
  (coerce list 'vector))

(defun json-object (&rest keys-and-values)
  (list* :object keys-and-values))

(defun policy-statement (effect &key actions not-actions resources condition)
  "A statement of an IAM policy document."
  (apply #'json-object
	 "Effect" effect
	 (append (when actions (list "Action" (json-array actions)))
		 (when not-actions (list "NotAction" (json-array not-actions)))
		 (when resources (list "Resource" (json-array resources)))
		 (when condition (list "Condition" condition)))))

(defun policy-document (&rest statements)
  (json (json-object "Version" "2012-10-17"
		     "Statement" (json-array statements))))


;;;;
;;;; Policy Documents
;;;;

(defun github-subject-filters ()
  "The list of OIDC subject patterns the role trusts.
Without an explicit *GITHUB-SUBJECT* these are the name based subject
repo:ORG/REPO:* and the immutable subject repo:ORG@ID/REPO@ID:*, which GitHub
issues for the repositories and organizations that enable immutable subjects."
  (if *github-subject*
      (list *github-subject*)
      (list (format nil "repo:~A/~A:*" *github-organization* *github-repository*)
	    (format nil "repo:~A@*/~A@*:*" *github-organization* *github-repository*))))

(defun trust-policy-document ()
  "The trust policy of the role assumed by GitHub Actions."
  (let ((host *aws-oidc-provider-host*))
    (policy-document
     (json-object
      "Effect" "Allow"
      "Principal" (json-object "Federated" (aws-oidc-provider-arn))
      "Action" "sts:AssumeRoleWithWebIdentity"
      "Condition"
      (json-object
       "StringEquals" (json-object (format nil "~A:aud" host) "sts.amazonaws.com")
       "StringLike" (json-object (format nil "~A:sub" host)
				 (json-array (github-subject-filters))))))))

(defun bucket-policy-document ()
  "The bucket policy which denies requests that do not use TLS."
  (policy-document
   (json-object
    "Sid" "DenyInsecureTransport"
    "Effect" "Deny"
    "Principal" "*"
    "Action" "s3:*"
    "Resource" (json-array (list (aws-bucket-arn) (format nil "~A/*" (aws-bucket-arn))))
    "Condition" (json-object "Bool" (json-object "aws:SecureTransport" "false")))))

(defun state-policy-document ()
  (policy-document
   (policy-statement "Allow"
		     :actions '("s3:ListBucket" "s3:GetBucketLocation"
				"s3:GetBucketVersioning")
		     :resources (list (aws-bucket-arn)))
   (policy-statement "Allow"
		     :actions '("s3:GetObject" "s3:PutObject" "s3:DeleteObject")
		     :resources (list (format nil "~A/*" (aws-bucket-arn))))))

(defun provisioning-policy-document ()
  "The actions Terraform needs to create resources.
Extend this list when the Terraform configuration uses other AWS services."
  (policy-document
   (policy-statement "Allow"
		     :actions '("s3:*")
		     :resources '("*"))
   (policy-statement "Allow"
		     :actions '("iam:CreateRole" "iam:DeleteRole" "iam:GetRole"
				"iam:UpdateRole" "iam:UpdateAssumeRolePolicy"
				"iam:TagRole" "iam:UntagRole" "iam:ListRoleTags"
				"iam:PutRolePolicy" "iam:GetRolePolicy" "iam:DeleteRolePolicy"
				"iam:ListRolePolicies" "iam:ListAttachedRolePolicies"
				"iam:DetachRolePolicy" "iam:ListInstanceProfilesForRole"
				"iam:CreatePolicy" "iam:DeletePolicy" "iam:GetPolicy"
				"iam:GetPolicyVersion" "iam:ListPolicyVersions"
				"iam:CreatePolicyVersion" "iam:DeletePolicyVersion"
				"iam:TagPolicy" "iam:UntagPolicy" "iam:ListPolicyTags"
				"iam:GetOpenIDConnectProvider" "iam:ListOpenIDConnectProviders")
		     :resources '("*"))
   (policy-statement "Allow"
		     :actions '("ec2:*Vpc*" "ec2:*Subnet*" "ec2:*SecurityGroup*"
				"ec2:*InternetGateway*" "ec2:*RouteTable*" "ec2:*Route"
				"ec2:*NatGateway*" "ec2:*Address*" "ec2:*Tags"
				"ec2:Describe*")
		     :resources '("*"))
   (policy-statement "Allow"
		     :actions '("logs:*" "lambda:*" "events:*" "scheduler:*" "sqs:*"
				"sns:*" "dynamodb:*" "kms:*")
		     :resources '("*"))))

(defun guards-policy-document ()
  "The statements limiting what the role can do with the provisioning policy."
  (let ((protected-arns (list* (aws-role-arn)
			       (mapcar #'aws-policy-arn *aws-policy-kinds*))))
    (policy-document
     (policy-statement "Allow"
		       :actions '("iam:PassRole")
		       :resources '("*")
		       :condition (json-object
				   "StringEquals"
				   (json-object "iam:PassedToService"
						(json-array *aws-passable-services*))))
     (policy-statement "Allow"
		       :actions '("iam:AttachRolePolicy")
		       :resources '("*")
		       :condition (json-object
				   "ArnEquals"
				   (json-object "iam:PolicyARN"
						(json-array *aws-attachable-policies*))))
     (policy-statement "Deny"
		       :not-actions '("iam:*" "sts:*" "organizations:*" "route53:*"
				      "cloudfront:*" "support:*")
		       :resources '("*")
		       :condition (json-object
				   "StringNotEquals"
				   (json-object "aws:RequestedRegion"
						(json-array *aws-allowed-regions*))))
     (policy-statement "Deny"
		       :actions '("ec2:RunInstances")
		       :resources '("*"))
     (policy-statement "Deny"
		       :actions '("iam:*User*" "iam:*AccessKey*" "iam:*LoginProfile*"
				  "organizations:*" "aws-portal:*" "billing:*")
		       :resources '("*"))
     (policy-statement "Deny"
		       :actions '("iam:AttachRolePolicy" "iam:DetachRolePolicy"
				  "iam:PutRolePolicy" "iam:DeleteRolePolicy"
				  "iam:DeleteRole" "iam:UpdateRole"
				  "iam:UpdateAssumeRolePolicy" "iam:TagRole" "iam:UntagRole"
				  "iam:CreatePolicyVersion" "iam:DeletePolicyVersion"
				  "iam:SetDefaultPolicyVersion" "iam:DeletePolicy"
				  "iam:TagPolicy" "iam:UntagPolicy")
		       :resources protected-arns))))

(defun policy-document-for-kind (kind)
  (ecase kind
    (:state (state-policy-document))
    (:provisioning (provisioning-policy-document))
    (:guards (guards-policy-document))))


;;;;
;;;; Verification
;;;;

(defun verify-aws-environment ()
  "Verify the AWS credentials and the parameters of the setup, signal an error otherwise."
  (multiple-value-bind (identity exit-code error-output)
      (run-aws '("sts" "get-caller-identity" "--query" "Account" "--output" "text"))
    (unless (zerop exit-code)
      (aws-setup-error "The AWS CLI has no valid credentials: ~A"
		       (string-trim '(#\Newline #\Space) error-output)))
    (loop :for (value name) :in `((,*aws-account* "*AWS-ACCOUNT* (AWS_ACCOUNT_ID)")
				  (,*github-organization* "*GITHUB-ORGANIZATION* (GITHUB_ORG)")
				  (,*github-repository* "*GITHUB-REPOSITORY* (GITHUB_REPO)"))
	  :unless value
	    :do (aws-setup-error "The parameter ~A is not set." name))
    (unless (string= identity *aws-account*)
      (aws-setup-error "The AWS account ~A reported by STS differs from *AWS-ACCOUNT* ~A."
		       identity *aws-account*))
    identity))


;;;;
;;;; Resources
;;;;

(defun bucket-exists-p ()
  (aws-succeeds-p "s3api" "head-bucket" "--bucket" (aws-bucket-name)))

(defun role-exists-p ()
  (aws-succeeds-p "iam" "get-role" "--role-name" (aws-role-name)))

(defun policy-exists-p (kind)
  (aws-succeeds-p "iam" "get-policy" "--policy-arn" (aws-policy-arn kind)))

(defun oidc-provider-exists-p ()
  (aws-succeeds-p "iam" "get-open-id-connect-provider"
		  "--open-id-connect-provider-arn" (aws-oidc-provider-arn)))

(defun ensure-bucket (expiry-date)
  (let ((bucket (aws-bucket-name))
	(region (aws-region)))
    (unless (bucket-exists-p)
      (apply #'aws "s3api" "create-bucket" "--bucket" bucket "--region" region
	     (unless (string= region "us-east-1")
	       (list "--create-bucket-configuration"
		     (format nil "LocationConstraint=~A" region)))))
    (aws "s3api" "put-bucket-versioning" "--bucket" bucket
	 "--versioning-configuration" "Status=Enabled")
    (aws "s3api" "put-bucket-encryption" "--bucket" bucket
	 "--server-side-encryption-configuration"
	 (json (json-object
		"Rules" (json-array
			 (list (json-object
				"ApplyServerSideEncryptionByDefault"
				(json-object "SSEAlgorithm" "AES256")))))))
    (aws "s3api" "put-public-access-block" "--bucket" bucket
	 "--public-access-block-configuration"
	 "BlockPublicAcls=true,IgnorePublicAcls=true,BlockPublicPolicy=true,RestrictPublicBuckets=true")
    (aws "s3api" "put-bucket-policy" "--bucket" bucket "--policy" (bucket-policy-document))
    (aws "s3api" "put-bucket-tagging" "--bucket" bucket
	 "--tagging" (s3-tagging-argument expiry-date))))

(defun ensure-oidc-provider ()
  "Create the GitHub OIDC identity provider unless the account already has one."
  (unless (oidc-provider-exists-p)
    (apply #'aws "iam" "create-open-id-connect-provider"
	   "--url" (format nil "https://~A" *aws-oidc-provider-host*)
	   "--client-id-list" "sts.amazonaws.com"
	   "--thumbprint-list" *aws-oidc-thumbprints*)))

(defun ensure-role (expiry-date)
  (let ((role (aws-role-name)))
    (cond
      ((role-exists-p)
       (aws "iam" "update-assume-role-policy" "--role-name" role
	    "--policy-document" (trust-policy-document))
       (apply #'aws "iam" "tag-role" "--role-name" role
	      "--tags" (iam-tag-arguments expiry-date)))
      (t
       (apply #'aws "iam" "create-role" "--role-name" role
	      "--assume-role-policy-document" (trust-policy-document)
	      "--tags" (iam-tag-arguments expiry-date))))))

(defun delete-non-default-policy-versions (arn)
  "Delete the non default versions of the policy ARN, IAM keeps at most five versions."
  (let ((versions (aws "iam" "list-policy-versions" "--policy-arn" arn
		       "--query" "Versions[?IsDefaultVersion==`false`].VersionId"
		       "--output" "text")))
    (dolist (version (uiop:split-string versions :separator '(#\Space #\Tab #\Newline)))
      (unless (member version '("" "None") :test #'string=)
	(aws "iam" "delete-policy-version" "--policy-arn" arn "--version-id" version)))))

(defun ensure-policy (kind expiry-date)
  (let ((arn (aws-policy-arn kind))
	(document (policy-document-for-kind kind)))
    (cond
      ((policy-exists-p kind)
       (delete-non-default-policy-versions arn)
       (aws "iam" "create-policy-version" "--policy-arn" arn
	    "--policy-document" document "--set-as-default")
       (apply #'aws "iam" "tag-policy" "--policy-arn" arn
	      "--tags" (iam-tag-arguments expiry-date)))
      (t
       (apply #'aws "iam" "create-policy" "--policy-name" (aws-policy-name kind)
	      "--policy-document" document
	      "--tags" (iam-tag-arguments expiry-date))))
    (aws "iam" "attach-role-policy" "--role-name" (aws-role-name) "--policy-arn" arn)))

(defun aws-manifest (expiry-date)
  "The JSON manifest recording the resources of the setup and its trust configuration."
  (json
   (json-object
    "account" *aws-account*
    "region" (aws-region)
    "allowedRegions" (json-array *aws-allowed-regions*)
    "expiryDate" expiry-date
    "github" (json-object "organization" *github-organization*
			  "repository" *github-repository*
			  "subjectFilters" (json-array (github-subject-filters)))
    "stateBucket" (json-object "name" (aws-bucket-name) "arn" (aws-bucket-arn))
    "oidcProvider" (json-object "arn" (aws-oidc-provider-arn))
    "role" (json-object "name" (aws-role-name) "arn" (aws-role-arn))
    "policies" (json-array
		(loop :for kind :in *aws-policy-kinds*
		      :collect (json-object "name" (aws-policy-name kind)
					    "arn" (aws-policy-arn kind)))))))

(defun write-manifest (expiry-date)
  "Store the manifest of the setup in the state bucket under bootstrap/manifest.json.
Previous versions survive thanks to the versioning of the bucket."
  (uiop:with-temporary-file (:stream stream :pathname pathname :keep nil
			     :external-format :utf-8)
    (write-string (aws-manifest expiry-date) stream)
    (finish-output stream)
    (aws "s3api" "put-object" "--bucket" (aws-bucket-name)
	 "--key" "bootstrap/manifest.json" "--body" (namestring pathname))))

(defun setup-aws ()
  "Prepare the AWS account for GitHub Actions and Terraform, and return the role ARN.
The setup creates a Terraform state bucket, the GitHub OIDC identity provider,
an IAM role GitHub Actions can assume and the IAM managed policies attached to
the role.  Resources are labelled with an ExpiryDate one week from now.
Existing resources are reused, so that the function can be run again safely.
A manifest of the resources is stored in the state bucket.

Run SETUP-AWS again whenever its permission statements change: editing the
source does not touch an account which has already been set up."
  (verify-aws-environment)
  (let ((expiry-date (format-timestamp (+ (current-universal-time) *aws-lease-duration*))))
    (ensure-bucket expiry-date)
    (ensure-oidc-provider)
    (ensure-role expiry-date)
    (dolist (kind *aws-policy-kinds*)
      (ensure-policy kind expiry-date))
    (write-manifest expiry-date)
    (aws-role-arn)))


;;;;
;;;; Lease
;;;;

(defun tag-value-or-nil (&rest arguments)
  "The ExpiryDate reported by the AWS CLI called with ARGUMENTS, NIL if it is missing."
  (multiple-value-bind (output exit-code)
      (run-aws arguments)
    (and (zerop exit-code)
	 (plusp (length output))
	 (string/= output "None")
	 output)))

(defun bucket-expiry-date ()
  (tag-value-or-nil "s3api" "get-bucket-tagging" "--bucket" (aws-bucket-name)
		    "--query" "TagSet[?Key==`ExpiryDate`].Value" "--output" "text"))

(defun role-expiry-date ()
  (tag-value-or-nil "iam" "list-role-tags" "--role-name" (aws-role-name)
		    "--query" "Tags[?Key==`ExpiryDate`].Value" "--output" "text"))

(defun policy-expiry-date (kind)
  (tag-value-or-nil "iam" "list-policy-tags" "--policy-arn" (aws-policy-arn kind)
		    "--query" "Tags[?Key==`ExpiryDate`].Value" "--output" "text"))

(defun extend-aws-lease (&optional (duration *aws-lease-duration*))
  "Set the ExpiryDate of the resources created by SETUP-AWS to now plus DURATION.
The DURATION is expressed in seconds, it defaults to one week.
Return the new expiry date."
  (verify-aws-environment)
  (unless (role-exists-p)
    (aws-setup-error "The AWS setup does not exist, run SETUP-AWS first."))
  (let ((expiry-date (format-timestamp (+ (current-universal-time) duration))))
    (when (bucket-exists-p)
      (aws "s3api" "put-bucket-tagging" "--bucket" (aws-bucket-name)
	   "--tagging" (s3-tagging-argument expiry-date)))
    (apply #'aws "iam" "tag-role" "--role-name" (aws-role-name)
	   "--tags" (iam-tag-arguments expiry-date))
    (dolist (kind *aws-policy-kinds*)
      (when (policy-exists-p kind)
	(apply #'aws "iam" "tag-policy" "--policy-arn" (aws-policy-arn kind)
	       "--tags" (iam-tag-arguments expiry-date))))
    (when (bucket-exists-p)
      (write-manifest expiry-date))
    expiry-date))


;;;;
;;;; Tear Down
;;;;

(defun empty-bucket ()
  "Delete every object version and delete marker of the state bucket."
  (let ((listing (aws "s3api" "list-object-versions" "--bucket" (aws-bucket-name)
		      "--query" "[Versions,DeleteMarkers][].[Key,VersionId]"
		      "--output" "text")))
    (dolist (line (uiop:split-string listing :separator '(#\Newline)))
      (let ((fields (uiop:split-string line :separator '(#\Tab))))
	(when (= 2 (length fields))
	  (aws "s3api" "delete-object" "--bucket" (aws-bucket-name)
	       "--key" (first fields) "--version-id" (second fields)))))))

(defun delete-policy (kind)
  (let ((arn (aws-policy-arn kind)))
    (aws-succeeds-p "iam" "detach-role-policy" "--role-name" (aws-role-name)
		    "--policy-arn" arn)
    (delete-non-default-policy-versions arn)
    (aws "iam" "delete-policy" "--policy-arn" arn)))

(defun tear-down-aws (&key force)
  "Reclaim the resources created by SETUP-AWS when their lease has expired.
When FORCE is true the resources are reclaimed unconditionally.  The shared
GitHub OIDC identity provider is kept.  Return true when resources have been
reclaimed and false otherwise."
  (verify-aws-environment)
  (let* ((role-p (role-exists-p))
	 (bucket-p (bucket-exists-p))
	 (policies (remove-if-not #'policy-exists-p *aws-policy-kinds*))
	 (expiry-date (or (and role-p (role-expiry-date))
			  (and bucket-p (bucket-expiry-date)))))
    (when (and (or role-p bucket-p policies)
	       (or force
		   (null expiry-date)
		   (< (parse-timestamp expiry-date) (current-universal-time))))
      (dolist (kind policies)
	(delete-policy kind))
      (when role-p
	(aws "iam" "delete-role" "--role-name" (aws-role-name)))
      (when bucket-p
	(empty-bucket)
	(aws "s3api" "delete-bucket" "--bucket" (aws-bucket-name)))
      t)))


;;;;
;;;; Describe
;;;;

(defun console-url-encode (string)
  (with-output-to-string (stream)
    (loop :for character :across string
	  :do (if (or (alphanumericp character) (find character "-_.~"))
		  (write-char character stream)
		  (format stream "%~2,'0X" (char-code character))))))

(defun aws-setup-resources ()
  "The existing resources of the AWS setup as a plist of entries.
Each entry is a plist with the keys :TYPE, :NAME, :EXPIRY-DATE and :CONSOLE-URL."
  (let ((resources nil))
    (flet ((add (key type name expiry-date url)
	     (setf resources
		   (append resources
			   (list key (list :type type :name name
					   :expiry-date expiry-date :console-url url))))))
      (when (bucket-exists-p)
	(add :state-bucket "S3 bucket" (aws-bucket-name)
	     (or (bucket-expiry-date) "unknown")
	     (format nil "https://s3.console.aws.amazon.com/s3/buckets/~A?region=~A"
		     (aws-bucket-name) (aws-region))))
      (when (role-exists-p)
	(add :github-actions-role "IAM role" (aws-role-arn)
	     (or (role-expiry-date) "unknown")
	     (format nil "https://console.aws.amazon.com/iam/home#/roles/details/~A"
		     (console-url-encode (aws-role-name)))))
      (dolist (kind *aws-policy-kinds*)
	(when (policy-exists-p kind)
	  (add (intern (format nil "~A-POLICY" kind) "KEYWORD")
	       "IAM policy" (aws-policy-arn kind)
	       (or (policy-expiry-date kind) "unknown")
	       (format nil "https://console.aws.amazon.com/iam/home#/policies/details/~A"
		       (console-url-encode (aws-policy-arn kind))))))
      (when (oidc-provider-exists-p)
	(add :oidc-provider "IAM identity provider" (aws-oidc-provider-arn)
	     "none (shared)"
	     (format nil "https://console.aws.amazon.com/iam/home#/providers/~A"
		     (console-url-encode (aws-oidc-provider-arn))))))
    resources))

(defun github-actions-p ()
  (equal "true" (environment-parameter "GITHUB_ACTIONS")))

(defun write-job-summary (resources)
  "Append a Markdown table describing RESOURCES to the GitHub job summary."
  (let ((summary (environment-parameter "GITHUB_STEP_SUMMARY")))
    (when summary
      (with-open-file (stream summary :direction :output :if-exists :append
				      :if-does-not-exist :create
				      :external-format :utf-8)
	(format stream "~&## AWS setup~%~%")
	(format stream "| Type | Name | Expiry date | Console |~%")
	(format stream "|------|------|-------------|---------|~%")
	(loop :for (nil entry) :on resources :by #'cddr
	      :do (format stream "| ~A | `~A` | ~A | [Open](~A) |~%"
			  (getf entry :type) (getf entry :name)
			  (getf entry :expiry-date) (getf entry :console-url)))
	(terpri stream)))))

(defun describe-aws-setup ()
  "List the resources of the AWS setup and return them as a plist.
Each entry has a type, a name, an expiry date and a link to the AWS Console.
When running in GitHub Actions the resources are also written to the job summary."
  (verify-aws-environment)
  (let ((resources (aws-setup-resources)))
    (loop :for (nil entry) :on resources :by #'cddr
	  :do (format t "~&~A ~A expires ~A~%  ~A~%"
		      (getf entry :type) (getf entry :name)
		      (getf entry :expiry-date) (getf entry :console-url)))
    (when (github-actions-p)
      (write-job-summary resources))
    resources))


;;;;
;;;; GitHub Actions
;;;;

(defvar *gh-program* '("gh")
  "The command, as a list of strings, which runs the GitHub CLI.")

(defun set-github-secret (name value)
  "Set the secret NAME of the GitHub repository to VALUE using the GitHub CLI.
The value is passed on the standard input so that it does not appear in the
process list.  Signal an AWS-SETUP-ERROR when the command fails."
  (multiple-value-bind (output error-output exit-code)
      (handler-case
	  (uiop:run-program (append *gh-program*
				    (list "secret" "set" name
					  "--repo" (format nil "~A/~A"
							   *github-organization*
							   *github-repository*)))
			    :input (make-string-input-stream value)
			    :output :string
			    :error-output :string
			    :ignore-error-status t)
	(error (condition)
	  (values "" (princ-to-string condition) 127)))
    (declare (ignore output))
    (unless (zerop exit-code)
      (aws-setup-error "The command `gh secret set ~A' failed with exit code ~D: ~A"
		       name exit-code
		       (string-trim '(#\Newline #\Space) error-output)))))

(defun setup-github-action ()
  "Create the repository secrets needed by the GitHub Actions workflows.
The secrets AWS_ACCOUNT_ID, AWS_REGION and AWS_ROLE_ARN are derived from the
output of DESCRIBE-AWS-SETUP.  Signal an AWS-SETUP-ERROR, and create nothing,
when the setup has no GitHub Actions role.  Return the list of secret names."
  (let* ((role (getf (let ((*standard-output* (make-broadcast-stream)))
		       (describe-aws-setup))
		     :github-actions-role))
	 (secrets (list (cons "AWS_ACCOUNT_ID" *aws-account*)
			(cons "AWS_REGION" (aws-region))
			(cons "AWS_ROLE_ARN" (getf role :name)))))
    (unless role
      (aws-setup-error "The AWS setup does not exist, run SETUP-AWS first."))
    (loop :for (name . value) :in secrets
	  :do (set-github-secret name value)
	  :collect name)))

;;;; End of file `aws.lisp'
