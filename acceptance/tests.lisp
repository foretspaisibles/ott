;;;; tests.lisp — Acceptance tests for OTT

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

(define-testcase build-ott-system-without-warnings ()
  "The OTT and OTT/TESTSUITE systems build from scratch without warnings."
  (multiple-value-bind (exit-code output)
      (run-sbcl '(ql:quickload '("alexandria" "org.melusina.confidence") :silent t)
		'(asdf:load-system "ott" :force t)
		'(asdf:load-system "ott/testsuite" :force t))
    (assert-t (zerop exit-code))
    (assert-nil (warning-lines output))))

(define-testcase lint-ott-system-without-warnings ()
  "The OTT linter runs without producing warnings."
  (let ((warnings nil)
	(clean nil))
    (handler-bind ((warning (lambda (condition)
			      (push condition warnings)
			      (muffle-warning condition))))
      (setf clean (funcall (find-symbol "LINT" "OTT/DEVELOPMENT"))))
    (assert-nil warnings)
    (assert-t clean)))

(define-testcase build-ott-documentation-without-warnings ()
  "The INFO, PDF and HTML documentation of the OTT system builds without warnings."
  (let ((warnings nil)
	(built nil))
    (handler-bind ((warning (lambda (condition)
			      (push condition warnings)
			      (muffle-warning condition))))
      (setf built (funcall (find-symbol "MAKE-DOCUMENTATION" "OTT/DEVELOPMENT"))))
    (assert-nil warnings)
    (assert-t built)
    (dolist (file '(#p"obj/makedoc/ott.info"
		    #p"obj/makedoc/ott.html"
		    #p"obj/makedoc/ott.pdf"))
      (assert-t* (probe-file (system-relative-pathname file))))))

(define-testcase test-ott-system-succesfully ()
  "The OTT test suite runs succesfully."
  (funcall (find-symbol "RUN-ALL-TESTS" "OTT/TESTSUITE")))

(defun readme-h1-headings ()
  "The level 1 headings of the OTT README file, ignoring fenced code blocks."
  (with-open-file (stream (system-relative-pathname #p"README.md"))
    (loop :with fenced = nil
	  :for line = (read-line stream nil)
	  :while line
	  :when (and (>= (length line) 3) (string= "```" line :end2 3))
	    :do (setf fenced (not fenced))
	  :when (and (not fenced) (>= (length line) 2) (string= "# " line :end2 2))
	    :collect (string-right-trim " " (subseq line 2)))))

(define-testcase review-ott-readme ()
  "The OTT README file has a H1 section `Technical onboarding'."
  (assert-t* (member "Technical onboarding" (readme-h1-headings) :test #'string=)))

(define-testcase enforce-ott-sibling-systems ()
  "The systems OTT/TESTSUITE, OTT/ACCEPTANCE and OTT/DEVELOPMENT exist."
  (dolist (name '("ott/testsuite" "ott/acceptance" "ott/development"))
    (assert-t* (asdf:find-system name nil))))

(define-testcase enforce-ott-acceptance-run-all-tests ()
  "The system OTT/ACCEPTANCE exports a function RUN-ALL-TESTS."
  (multiple-value-bind (symbol status)
      (find-symbol "RUN-ALL-TESTS" "OTT/ACCEPTANCE")
    (assert-t* symbol)
    (assert-t (eq status :external))
    (assert-t* (fboundp symbol))))

;;;; End of file `tests.lisp'
