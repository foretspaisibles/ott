;;;; utilities.lisp — Utilities for OTT acceptance tests

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

(defun system-relative-pathname (pathname)
  "The PATHNAME relative to the directory of the OTT system."
  (merge-pathnames pathname (asdf:system-source-directory "ott")))

(defun run-sbcl (&rest forms)
  "Evaluate FORMS in a fresh SBCL image whose registry knows the OTT systems.
Return the exit code and the combined standard output and error output."
  (let ((command
	  `("sbcl" "--noinform" "--disable-debugger"
		   "--eval" ,(format nil "(pushnew ~S asdf:*central-registry* :test #'equal)"
				     (system-relative-pathname #p""))
		   ,@(loop :for form :in forms
			   :append (list "--eval"
					 (let ((*package* (find-package "CL-USER")))
					   (prin1-to-string form))))
		   "--eval" "(uiop:quit 0)")))
    (multiple-value-bind (output error-output exit-code)
	(uiop:run-program command
			  :output :string
			  :error-output :output
			  :ignore-error-status t)
      (declare (ignore error-output))
      (values exit-code output))))

(defun warning-lines (output)
  "The lines of OUTPUT which mention a warning."
  (remove-if-not (lambda (line) (search "WARNING" line))
		 (uiop:split-string output :separator '(#\Newline))))

;;;; End of file `utilities.lisp'
