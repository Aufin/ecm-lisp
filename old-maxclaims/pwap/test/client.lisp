#+quicklisp '#.(ql:quickload '("cxml-stp" "drakma"
			       "closure-html"))

(defpackage :pwap/test/client
  (:documentation 
   "Test Client : A client-side test framework")
  (:use :cl)
  (:import-from :drakma #:http-request)
  (:import-from :closure-html #:parse)
  (:import-from :cxml-stp #:make-builder)
  (:export))

(in-package :pwap/test/client)

(defun test-request (uri &key &allow-other-keys)
  (http-request uri))

(defun test-document (uri &key &allow-other-keys)
  (multiple-value-call (lambda (doc &rest others)
			 (apply #'values (parse doc (make-builder)) others))
      (test-request uri)))







