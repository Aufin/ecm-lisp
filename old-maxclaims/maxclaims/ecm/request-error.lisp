(defpackage :ecm/request-error
  (:use :cl)
  ()
(in-package :ecm/request-error)

(defun <error-page> (condition)
  
(defun call-using-request-error-handler (thunk)
  (let ((method (hunchentoot:request-method*)))
    (block error
      (handler-bind
	  ((error (lambda (c s))
	     (
