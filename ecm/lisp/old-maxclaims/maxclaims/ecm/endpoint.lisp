(cl:defpackage :ecm/endpoint
  (:use :cl)
  (:export
   #:endpoint
   #:define-endpoint
   #:endpoint-dispatcher
   #:endpoint-not-found
   #:*endpoints*)
  (:documentation " 

\"A WSDL document defines services as collections of network endpoints,
or ports. In WSDL, the abstract definition of endpoints and messages
is separated from their concrete network deployment or data format
bindings.\" -- http://www.w3.org/TR/wsdl.html

The endpoints are matched by way of functions or regular expressions
and then dispatch on the method to find a named handler
function." ))

(cl:in-package  :ecm/endpoint)

(defvar *endpoints* (list)
  "The endpoints that DEFINE-ENDPOINT and DISPATCH-ENDPOINT use.")

(defclass endpoint ()
  ((name :initarg name :reader endpoint-name)
   (match :initarg match :reader endpoint-match)))

(defun endpoint-scan (endpoint string)
  "=> /true or NIL/, /arguments to ENDPOINT-FUNCTION/"
  (let ((match (endpoint-match endpoint)))
    (etypecase match
    (string 
     (multiple-value-bind
	   (match registers)
	 (cl-ppcre:scan-to-strings match string)
       (when match
	 (return-from endpoint-scan
	   (apply #'values t (coerce registers 'list))))))
    (function (return-from endpoint-scan (funcall match string))))))

(defmethod print-object ((endpoint endpoint) stream)
  (print-unreadable-object (endpoint stream :type t)
    (print (endpoint-name endpoint) stream)
    (princ #\Space stream)
    (print (endpoint-match endpoint) stream)))

(defun make-endpoint (name match)
  (make-instance 'endpoint
		 'name name
		 'match match))

(defun endpoint-function-name (endpoint-name method)
  (let ((function-name-name (format nil "~A/~A" endpoint-name method)))
    (intern function-name-name (symbol-package endpoint-name))))

(defun endpoint-function (endpoint method)
  (or (ignore-errors
	(symbol-function (endpoint-function-name (endpoint-name endpoint) method)))
      (error "No ~A method handler defined for endpoint ~A"
	     method (endpoint-name endpoint))))


;;; Declarative endpoint definition.

(defun %define-endpoint (name match)
  ;; The load-time magic for endpoint definition... or redefinition.
  ;; Add the endpoint to our dispatch list if necessary, otherwise
  ;; just update the regexp.
  (let ((endpoint (find name *endpoints* :key #'endpoint-name)))
    (unless endpoint
      (push (setf endpoint (make-endpoint name match))
	    *endpoints*))
    (setf (slot-value endpoint 'match) match)))

(defmacro define-endpoint (name match)
  "Define an endpoint for the server protocol. MATCH defines the URL
scheme for the endpoint, NAME provides the base for the handler
function names."
  `(progn
     (%define-endpoint ',name ,match)
     ',name))


;;; Endpoint dispatch.

(define-condition endpoint-not-found ()
  ((endpoint :initarg :endpoint :reader endpoint-not-found-endpoint))
  (:report (lambda (condition stream)
	     (format stream "The endpoint ~S is not defined"
		     (endpoint-not-found-endpoint condition)))))

(defun endpoint-dispatcher (string method)
  (dolist (endpoint *endpoints*)
    (destructuring-bind (match . args)
	(multiple-value-list (endpoint-scan endpoint string))
      (when match
	(return-from endpoint-dispatcher
	  (apply (endpoint-function endpoint method)
		 args)))))
      (error 'endpoint-not-found
	     :endpoint string))


