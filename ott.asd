;;;; ott.asd — System definition for OTT

;;;; OTT (https://github.com/foretspaisibles/ott)
;;;; This file is part of OTT.
;;;;
;;;; Copyright © 2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(asdf:defsystem #:ott
  :description "OBI Meetup talk: centralised business logic in a decentralized architecture"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:alexandria
	       #:uiop
	       #:yason)
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "utilities")
		 (:file "queue")
		 (:file "events")
		 (:file "entrypoint")))))

(asdf:defsystem #:ott/testsuite
  :description "OBI Meetup talk: centralised business logic in a decentralized architecture"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:alexandria
	       #:org.melusina.confidence
	       #:ott)
  :components
  ((:module "testsuite"
    :components ((:file "package")
		 (:file "utilities")
		 (:file "entrypoint")))))

(asdf:defsystem #:ott/development
  :description "Development tools for OTT"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:alexandria
	       #:org.melusina.atelier)
  :components
  ((:module "libexec/lisp"
    :components ((:file "development")
		 (:file "aws")))))

(asdf:defsystem #:ott/operation
  :description "Create and destroy the OTT infrastructure"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:ott)
  :components
  ((:module "libexec/lisp"
    :components ((:file "operation")))))

(asdf:defsystem #:ott/acceptance
  :description "Acceptance tests for OTT"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:alexandria
	       #:org.melusina.confidence
	       #+sbcl (:require "sb-posix")
	       #:ott/testsuite
	       #:ott/development
	       #:ott/operation)
  :components
  ((:module "acceptance"
    :components ((:file "package")
		 (:file "utilities")
		 (:file "bootstrap")
		 (:file "setup-aws-and-github-actions")
		 (:file "create-infrastructure")
		 (:file "entrypoint")))))

;;;; End of file `ott.asd'
