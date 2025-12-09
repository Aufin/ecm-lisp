;;; -*- lisp -*-
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :maxclaims/asdf-system)
    (defpackage :maxclaims/asdf-system
      (:documentation "ASDF System package for meta-model.")
      (:use :common-lisp :asdf)
      (:export #:project-relative-pathname))))

(in-package :maxclaims/asdf-system) 

(defun project-relative-pathname (path)
  (merge-pathnames path (component-pathname (find-system :maxclaims))))

(asdf:defsystem :maxclaims-src
  :description "MaxHacky!"
  :long-description "This is an attempt to see how the frig this was ever working"
  :class :package-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:maxclaims))


(defsystem :maxclaims
  :components 
  ((:static-file "maxclaims.asd")
   (:module :src
	    :serial t
	    :components
	    (
	     
	     
	     ;; This file is for code that belongs in rofl.
	     (:file "relational-objects-for-lisp/relational-objects-for-lisp")
	     
	     (:file "packages")
	     (:file "globals")
	     (:file "configuration")

	     (:module :lib
		      :components
		      ((:file "db-utils")
		       (:file "db-classes")))
	     (:file "utilities")

	     (:file "login")
	     
	     (:module :core
		      :serial t
		      :components
		      (
		       (:file "detail-attribute-classes")
		       (:module :database-bindings
				:serial t
				:components
				((:file "risk")
				 (:file "contract")
				 (:file "claim")
				 (:file "claim-transaction")
				 (:file "policy")
				 (:file "person")
				 (:file "deleted")
				 (:file "merge")
				 (:file "history")
				 (:file "user")
				; (:file "app-user-message")
				 (:file "timecard")
				 (:file "timecard-interim")
				 (:file "attachment")
				 (:file "diary")
				 (:file "defer-diary")
				 (:file "app-adjuster")))
		       (:file "claim")
		       (:file "risk")
		       (:file "person")
		       (:file "policy")
		       (:file "user")
		       (:file "history")
		       (:file "merge")
		       (:file "deleted")))
	     (:module :search
		      :components
		      ((:file "engine")
		       ))

	     (:file "user-manager")

	     (:file "start")
	     

	     (:file "loggers" :depends-on (:lib :search :core))

	     )
	    :serial t))
  :serial t
  :depends-on
   (:ecm/postgresql/connection
    :ecm/hack
    :hunchentoot
	:quux-hunchentoot
	:simple-date
	:split-sequence
	:drakma
	:relational-objects-for-lisp
	:yaclml
    :postmodern
	:ironclad
	:cl-postgres
	:swank)
  :version "3.0.14")



