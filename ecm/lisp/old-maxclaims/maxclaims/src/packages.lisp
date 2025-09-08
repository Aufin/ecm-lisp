(uiop:define-package :maxclaims
  (:use :common-lisp 
	:alexandria	
	:relational-objects-for-lisp
	:postmodern
	:split-sequence
        :maxclaims/asdf-system)
  (:import-from :sb-ext #:quit)
  (:shadow :instance :standard-attribute)
  (:nicknames :maxclaims-ecm)
  (:import-from :arnesi #:defgeneric/cc #:defmethod/cc
		#:dolist*
		#:if-bind #:when-bind))







