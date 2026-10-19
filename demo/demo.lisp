;;;; demo.lisp — The demo Task of the OBI Meetup talk

;;;; The Task: check out the demo orders of the retail example, then
;;;; return the happy order.  Run in the :MONOLITH calling convention
;;;; by demo/run_monolith, and sent to the :MESH by demo/run_mesh.

(defpackage #:ott/demo
  (:use #:common-lisp)
  (:export #:run-monolith #:run-mesh))

(in-package #:ott/demo)

(defun banner (name convention)
  (format t "~&;; Demo:       ~A~%;; PID:        ~D~%;; Convention: ~S~%"
	  name (ott::process-id) convention)
  (finish-output))

(defun run-monolith ()
  "Perform the demo Task in this process, every service is a plain function call."
  (banner "monolith" :monolith)
  (let ((ott:*calling-convention* :monolith))
    (dolist (kind '(:happy :out-of-stock :no-credit :bad-address :absent-recipient))
      (format t "~&~%;; Checkout of the ~(~A~) order~%=> ~S~%"
	      kind (business-logic:run-checkout kind :convention :monolith)))
    (dolist (reason '(:defective :changed-mind))
      (format t "~&~%;; Return of the happy order, ~(~A~)~%=> ~S~%"
	      reason (business-logic:run-return reason :convention :monolith)))))

(defun run-mesh (&optional (kind :happy))
  "Send the checkout of the demo order KIND to the mesh and return.
The micro-services pick up the events from the continuation queue, the
value shows in the output of the BUSINESS-LOGIC service."
  (banner "mesh" :mesh)
  (let ((ott:*calling-convention* :mesh)
	(*trace-output* *standard-output*))
    (handler-case (ott::preflight-check)
      (ott:aws-error (condition)
	(format *error-output* "~&;; Pre-flight FAILED: ~A~%" condition)
	(sb-ext:exit :code 1)))
    (business-logic:reset-demo-data)
    (multiple-value-bind (value id)
	(business-logic:checkout (business-logic:demo-order kind))
      (format t "~&;; Checkout of the ~(~A~) order: ~S, run ~A~%;; See the output of the business-logic service for its result.~%"
	      kind value id))))

;;;; End of file `demo.lisp'
