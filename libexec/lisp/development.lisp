;;;; development.lisp — Development System for OTT

;;;; OTT (https://github.com/foretspaisibles/ott)
;;;; This file is part of OTT.
;;;;
;;;; Copyright © 2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:ott/development
  (:use #:common-lisp)
  (:export
   #:*aws-account*
   #:*aws-program*
   #:*clock*
   #:*getenv*
   #:*gh-program*
   #:*github-organization*
   #:*github-repository*
   #:*github-subject*
   #:aws-setup-error
   #:configure
   #:describe-aws-setup
   #:extend-aws-lease
   #:install-pre-commit-hook
   #:lint
   #:make-documentation
   #:setup-aws
   #:setup-github-action
   #:tear-down-aws
   #+quicklisp
   #:reload))

(in-package #:ott/development)

(defun system-relative-pathname (&rest pathnames)
  (labels ((system-source-directory ()
	     (asdf:system-source-directory "ott"))
	   (merge-one (pathname)
	     (merge-pathnames pathname (system-source-directory))))
    (mapcar #'merge-one pathnames)))

(defparameter *parameter-bindings*
  '((:copyright-holder . "Michaël Le Barbier")
    (:copyright-year . "2026")
    (:project-filename . "ott")
    (:project-name . "OTT")
    (:project-description
     . "OBI Meetup talk: centralised business logic in a decentralized architecture")
    (:project-long-description . "Lab example for the OBI Meetup presentation.")
    (:homepage . "https://github.com/foretspaisibles/ott")
    (:license . :MIT)))

(defun lint ()
  "Lint the OTT sources and return T when no anomaly is found.
The vendored file libexec/lisp/docstrings.lisp is not linted."
  (let ((atelier:*parameter-bindings* *parameter-bindings*)
	(atelier:*linter-interactive-p* t))
    ;; Anomalies are collected as hints, the linter continues on the next one.
    (handler-bind ((atelier::anomaly
		     (lambda (anomaly)
		       (declare (ignore anomaly))
		       (invoke-restart 'continue))))
      (atelier:lint
       (system-relative-pathname
	#p"ott.asd"
	#p"acceptance"
	#p"doc"
	#p"libexec/git"
	#p"libexec/lisp/development.lisp"
	#p"libexec/lisp/aws.lisp"
	#p"libexec/lisp/operation.lisp"
	#p"src"
	#p"testsuite")))))

(defparameter *required-tools*
  '(("gh" "gh" "--version")
    ("aws" "aws" "--version")
    ("terraform" "terraform" "version")
    ("makeinfo" "makeinfo" "--version")
    ("texi2pdf" "texi2pdf" "--version")
    ("pdftex" "pdftex" "--version"))
  "The command line tools OTT requires, with the command reporting their version.")

(defun report-tool-version (name command)
  "Run COMMAND and report its first output line for tool NAME on the diagnostic output.
Return T when the command runs and exits succesfully."
  (handler-case
      (multiple-value-bind (output error-output exit-code)
	  (uiop:run-program command
			    :output :string
			    :error-output :string
			    :ignore-error-status t)
	(let ((first-line
		(first (uiop:split-string
			(string-trim '(#\Newline #\Space)
				     (if (plusp (length output)) output error-output))
			:separator '(#\Newline)))))
	  (cond
	    ((zerop exit-code)
	     (format *error-output* "~&~A: ~A~%" name first-line)
	     t)
	    (t
	     (format *error-output* "~&~A: command ~S failed with exit code ~D~%"
		     name command exit-code)
	     nil))))
    (error (condition)
      (format *error-output* "~&~A: cannot run ~S: ~A~%" name command condition)
      nil)))

(defun configure ()
  "Verify that the Lisp environment can run the OTT command line tools.
This includes the Texinfo material required to produce the documentation:
makeinfo, texi2pdf and a TeX engine.
Versions are reported on the diagnostic output. Return T when every tool runs."
  (let ((results
	  (loop :for (name . command) :in *required-tools*
		:collect (report-tool-version name command))))
    (every #'identity results)))

(defun run-documentation-command (directory &rest command)
  "Run COMMAND in DIRECTORY and signal a WARNING for every warning it reports.
Return T when the command exits succesfully."
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program command
			:directory directory
			:output :string
			:error-output :output
			:input nil
			:ignore-error-status t)
    (declare (ignore error-output))
    (dolist (line (uiop:split-string output :separator '(#\Newline)))
      (when (search "warning" line :test #'char-equal)
	(warn "~A: ~A" (first command) line)))
    (unless (zerop exit-code)
      (format *error-output* "~&~A: command ~S failed with exit code ~D~%~A~%"
	      (first command) command exit-code output))
    (zerop exit-code)))

(defun generate-documentation-includes (include-directory)
  "Extract the docstrings of the OTT package into INCLUDE-DIRECTORY and patch them.
Each patch doc/patch/NAME.sed is applied to the include NAME.texinfo."
  (unless (find-package "SB-TEXINFO")
    (with-compilation-unit ()
      (load (first (system-relative-pathname #p"libexec/lisp/docstrings.lisp")))))
  (funcall (find-symbol "GENERATE-INCLUDES" "SB-TEXINFO")
	   include-directory (list '#:ott) :base-package '#:ott)
  (dolist (patch (directory (first (system-relative-pathname #p"doc/patch/*.sed"))))
    (let ((include (merge-pathnames (make-pathname :name (pathname-name patch)
						   :type "texinfo")
				    include-directory)))
      (when (probe-file include)
	(let ((patched (uiop:run-program
			(list "sed" "-f" (namestring patch) (namestring include))
			:output :string)))
	  (with-open-file (stream include :direction :output :if-exists :supersede
				  :external-format :utf-8)
	    (write-string patched stream)))))))

(defun make-documentation ()
  "Produce the INFO, HTML and PDF documentation of OTT in the directory obj/makedoc.
A WARNING is signaled for every warning reported by the Texinfo tools.
Return T when all formats are produced succesfully."
  (let* ((master (first (system-relative-pathname #p"doc/ott.texinfo")))
	 (directory (first (system-relative-pathname #p"obj/makedoc/")))
	 (include-directory (merge-pathnames "include/" directory))
	 (master-name (namestring master)))
    (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)
    (ensure-directories-exist include-directory)
    (generate-documentation-includes include-directory)
    (every #'identity
	   (list (run-documentation-command directory "makeinfo" master-name)
		 (run-documentation-command directory "makeinfo" "--html" "--no-split"
					    master-name)
		 (run-documentation-command directory "texi2pdf" "--batch" "--quiet"
					    master-name)))))

(defun install-pre-commit-hook ()
  "Install the git pre-commit hook running the OTT acceptance tests.
Return the pathname of the installed hook."
  (let* ((source
	   (first (system-relative-pathname #p"libexec/git/pre-commit.sh")))
	 (hooks-directory
	   (uiop:ensure-directory-pathname
	    (string-trim '(#\Newline)
			 (uiop:run-program
			  (list "git" "-C" (namestring (first (system-relative-pathname #p"")))
				"rev-parse" "--git-path" "hooks")
			  :output :string))))
	 (target
	   (merge-pathnames "pre-commit"
			    (merge-pathnames hooks-directory
					     (first (system-relative-pathname #p""))))))
    (ensure-directories-exist target)
    (uiop:copy-file source target)
    (uiop:run-program (list "chmod" "755" (namestring target)))
    target))

#+quicklisp
(defun reload ()
  (ql:quickload '("ott"
		  "ott/testsuite"
		  "ott/development")))


;;;;
;;;; Command Stock
;;;;

#+nil
(ott/development:reload)

;;;; End of file `development.lisp'
