;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(asdf:defsystem :max-ecm
  :description "Max Effective Claims Manager"
  :long-description ""
  :class :package-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:pwap
			   :simple-date
			   :simple-date/postgres-glue
			   :s-sql
			   :cl-postgres
			   :postmodern))

(asdf:register-system-packages 
 :cxml 
 '(:sax :runes))

(asdf:register-system-packages 
 :st-json
 '(:st-json))

(asdf:register-system-packages 
 :cxml-stp 
 '(:cxml-stp :stp))


