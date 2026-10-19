;;;; cps.lisp — Rewrite business logic in continuation-passing style

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

;;;; DEFINE-BUSINESS-LOGIC rewrites its body so that every call to a service
;;;; function is a tail call that carries its continuation.  The continuation
;;;; is a STEP, a function of the values of the variables in scope (the
;;;; frame) and of the value of the call.  Steps are closed, so that a frame
;;;; and a step number are all it takes to resume the logic in another process.
;;;;
;;;; The rewriting handles PROGN, IF, WHEN, UNLESS, COND, CASE, LET, LET*,
;;;; function calls and macros that expand into these.  A form that does not
;;;; call a service function is left alone.  Variables must not be shadowed.

(define-condition cannot-rewrite (error)
  ((form
    :initarg :form
    :reader cannot-rewrite-form)
   (reason
    :initarg :reason
    :reader cannot-rewrite-reason))
  (:report (lambda (condition stream)
	     (format stream "Cannot rewrite ~S in continuation-passing style: ~A."
		     (cannot-rewrite-form condition)
		     (cannot-rewrite-reason condition))))
  (:documentation "A form of business logic calls a service function in a
position the rewriting does not support."))

(defvar *logic-name* nil
  "The name of the business logic being rewritten.")

(defvar *steps* nil
  "The forms of the steps of the business logic being rewritten, an adjustable vector.")

;;;;
;;;; Compile-time continuations
;;;;

(defstruct (kont (:constructor make-kont (function &optional small)))
  "What remains to be done with a value: FUNCTION takes a form for the value and
the variables in scope, and returns a form.  A SMALL continuation can be called
many times, because the form it returns is a jump."
  function small)

(defmacro lambda-kont ((form env) &body body)
  `(make-kont (lambda (,form ,env)
		(declare (ignorable ,env))
		,@body)))

(defun kall (kont form env)
  (funcall (kont-function kont) form env))

(defun add-step (env result make-body)
  "Add a step to the logic and return its number.  The step binds the variables
of ENV from its frame, and RESULT to its value.  MAKE-BODY returns its body."
  (let* ((index (vector-push-extend nil *steps*))
	 (frame (gensym "FRAME"))
	 (body (funcall make-body)))
    (setf (aref *steps* index)
	  `(lambda (,frame ,result)
	     (declare (ignorable ,result))
	     (destructuring-bind ,env ,frame
	       (declare (ignorable ,@env))
	       ,body)))
    index))

(defun call-with-join (kont join-env function)
  "Call FUNCTION with a small continuation that does what KONT does.
When KONT is not small it becomes a step, the join point of branches.
The variables of JOIN-ENV are in scope at every jump to the join point."
  (if (kont-small kont)
      (funcall function kont)
      (let* ((result (gensym "R"))
	     (step (add-step join-env result
			     (lambda () (kall kont result (adjoin result join-env))))))
	(funcall function
		 (make-kont (lambda (form env)
			      (declare (ignore env))
			      `(resume-step ',*logic-name* ,step (list ,@join-env) ,form))
			    t)))))

;;;;
;;;; Rewriting
;;;;

(defun service-call-p (form)
  "True when FORM contains a call to a service function."
  (and (consp form)
       (not (eq (first form) 'quote))
       (or (service-function-p (first form))
	   (loop :for tail :on form
		 :thereis (service-call-p (car tail))))))

(defun cps-unsupported (form reason)
  (error 'cannot-rewrite :form form :reason reason))

(defun cps (form env k)
  "A form that evaluates FORM and passes its value to the continuation K.
ENV is the list of the variables in scope."
  (if (not (service-call-p form))
      (kall k form env)
      (let ((operator (first form)))
	(cond
	  ((service-function-p operator) (cps-service-call form env k))
	  ((eq operator 'progn) (cps-progn (rest form) env k))
	  ((eq operator 'if)
	   (destructuring-bind (test then &optional else) (rest form)
	     (cps-if test then else env k)))
	  ((eq operator 'when)
	   (cps `(if ,(second form) (progn ,@(cddr form))) env k))
	  ((eq operator 'unless)
	   (cps `(if ,(second form) nil (progn ,@(cddr form))) env k))
	  ((eq operator 'cond) (cps (expand-cond (rest form)) env k))
	  ((eq operator 'case) (cps (expand-case (second form) (cddr form)) env k))
	  ((eq operator 'let) (cps-let (second form) (cddr form) env k))
	  ((eq operator 'let*) (cps (expand-let* (second form) (cddr form)) env k))
	  ((not (symbolp operator))
	   (cps-unsupported form "the operator is not a symbol"))
	  ((special-operator-p operator)
	   (cps-unsupported form (format nil "~A is not supported" operator)))
	  ((macro-function operator) (cps (macroexpand-1 form) env k))
	  (t (cps-call operator (rest form) env k))))))

(defun cps-progn (forms env k)
  (cond ((null forms) (kall k nil env))
	((null (rest forms)) (cps (first forms) env k))
	(t (cps (first forms) env
		(lambda-kont (value env)
		  (let ((rest (cps-progn (rest forms) env k)))
		    (if (atom value) rest `(progn ,value ,rest))))))))

(defun cps-if (test then else env k)
  (call-with-join k env
    (lambda (k)
      (cps test env
	   (lambda-kont (value env)
	     `(if ,value ,(cps then env k) ,(cps else env k)))))))

(defun bind-value (value env function)
  "A form that calls FUNCTION with an atom for VALUE and the variables in scope."
  (if (atom value)
      (funcall function value env)
      (let ((variable (gensym "V")))
	`(let ((,variable ,value))
	   ,(funcall function variable (adjoin variable env))))))

(defun cps-args (forms env k)
  "Evaluate FORMS from left to right and pass forms for their values to K.
A value computed before a service call is bound to a variable."
  (if (notany #'service-call-p forms)
      (kall k forms env)
      (labels ((walk (forms env done)
		 (if (null forms)
		     (kall k (reverse done) env)
		     (cps (first forms) env
			  (lambda-kont (value env)
			    (if (some #'service-call-p (rest forms))
				(bind-value value env
					    (lambda (atom env)
					      (walk (rest forms) env (cons atom done))))
				(walk (rest forms) env (cons value done))))))))
	(walk forms env nil))))

(defun cps-call (function arguments env k)
  (cps-args arguments env
	    (lambda-kont (atoms env)
	      (kall k `(,function ,@atoms) env))))

(defun cps-service-call (form env k)
  (cps-args (rest form) env
	    (lambda-kont (atoms env)
	      (let* ((result (gensym "R"))
		     (step (add-step env result
				     (lambda () (kall k result (adjoin result env))))))
		`(call-service ',(first form) (list ,@atoms) ',*logic-name* ,step
			       (list ,@env))))))

(defun normalize-binding (binding)
  (if (consp binding)
      (list (first binding) (second binding))
      (list binding nil)))

(defun cps-let (bindings body env k)
  (let ((variables (mapcar #'first (mapcar #'normalize-binding bindings)))
	(inits (mapcar #'second (mapcar #'normalize-binding bindings))))
    (let ((shadowed (intersection variables env)))
      (when shadowed
	(cps-unsupported `(let ,bindings ,@body)
			 (format nil "~{~A~^, ~} would shadow a variable in scope" shadowed))))
    (cps-args inits env
	      (lambda-kont (atoms env)
		`(let ,(mapcar #'list variables atoms)
		   ,(cps-progn body (append variables env) k))))))

(defun expand-let* (bindings body)
  (if (null bindings)
      `(progn ,@body)
      `(let (,(first bindings)) ,(expand-let* (rest bindings) body))))

(defun expand-cond (clauses)
  (if (null clauses)
      nil
      (destructuring-bind (test &rest body) (first clauses)
	(if body
	    `(if ,test (progn ,@body) ,(expand-cond (rest clauses)))
	    (let ((variable (gensym "C")))
	      `(let ((,variable ,test))
		 (if ,variable ,variable ,(expand-cond (rest clauses)))))))))

(defun expand-case (key clauses)
  (let ((variable (gensym "K")))
    `(let ((,variable ,key))
       ,(expand-cond
	 (loop :for (keys . body) :in clauses
	       :collect (list (cond ((member keys '(t otherwise)) t)
				    ((listp keys) `(member ,variable ',keys))
				    (t `(eql ,variable ',keys)))
			      `(progn ,@body)))))))


;;;;
;;;; Business logic
;;;;

(defmacro define-business-logic (name lambda-list &body body)
  "Define the function NAME, that runs business logic made of calls to service
functions, defined with DEFINE-SERVICE-FUNCTION.

The function returns two values, the one of the logic and the identifier of the
run.  When *CALLING-CONVENTION* is :MESH, the value is :SUSPENDED once a call
to a service has been sent on the continuation queue, and the result is later
found with RUN-RESULT in the process that handles the last event of the run."
  (when (intersection lambda-list lambda-list-keywords)
    (error "The business logic ~S has only required parameters." name))
  (let* ((documentation (when (and (stringp (first body)) (rest body))
			  (list (pop body))))
	 (*logic-name* name)
	 (*steps* (make-array 0 :adjustable t :fill-pointer t))
	 (code (cps-progn body lambda-list
			  (make-kont (lambda (form env)
				       (declare (ignore env))
				       `(complete-run ,form))
				     t))))
    `(progn
       (setf (gethash ',name *logic-steps*)
	     (vector ,@(coerce *steps* 'list)))
       (defun ,name ,lambda-list
	 ,@documentation
	 (start-run (lambda () ,code))))))

;;;; End of file `cps.lisp'
