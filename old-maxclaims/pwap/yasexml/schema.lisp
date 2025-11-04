(defpackage :pwap/yasexml/schema
  (:documentation 
   "Schema YASEXML: create correct tags using XSD")
  (:use :cl)
  (:import-from 
   :pwap/yasexml/tag)
  (:export #:schema-file-symbols
	   #:define-schema-functions
	   #:make-schema
	   #:schema-name
	   #:schema-elements))

(in-package :pwap/yasexml/schema)

(defparameter *namespace-map*
  '(("xs" . "http://www.w3.org/2001/XMLSchema")))

(defun of-name (name-or-qname 
		&key (namespace-prefix nil)
		  (namespace-uri "http://www.w3.org/2001/XMLSchema"))
  (let* ((|:| (position #\: name-or-qname)))
    (cond (|:|
	   (let ((prefix (subseq name-or-qname 0 |:|))
		 (local-name (subseq name-or-qname (1+ |:|))))
	     (stp:of-name local-name (cdr (assoc prefix *namespace-map*
						 :test #'string=)))))
	  (namespace-prefix 
	   (stp:of-name name-or-qname 
			(cdr (assoc namespace-prefix *namespace-map*
				    :test #'string=))))
	  (:else (stp:of-name name-or-qname namespace-uri)))))

(defun make-schema (input)
  (cxml:parse input (cxml-stp:make-builder)))

(defun schema-elements (node)
  (stp:filter-recursively 
   (of-name "xs:element") 
   node))

(defun list-attributes (node)
  (let* ((type (stp:attribute-value node "type"))
	 (attributes   (stp:filter-recursively 
			(of-name "xs:attribute") 
			node))
	 (complex-type 
	  (when type 
	    (first 		   
	     (stp:filter-recursively 
	      (lambda (e)
		(and (funcall (of-name "xs:complexType") e)
		     (string= 
		      (concatenate 
		       'string "gnm:" (stp:attribute-value e "name"))
		      type)))
	      (stp:document node)))))
	 (others (when complex-type 
		   (stp:filter-recursively 
		    (of-name "xs:attribute") 
		    complex-type))))
    #+(or)(if (and type (not complex-type))
	(break "~A ~A" type complex-type))
    (nconc attributes others)))
		   
(defun schema-name (node)
  (stp:attribute-value node "name"))

(defun schema-symbols (schema)
  (remove-duplicates (mapcar #'make-symbol
			     (mapcar #'schema-name (list-elements schema)))
		     :test #'string=))

(defun call-with-schema-from-file (function pathname 
				   &optional (defaults *default-pathname-defaults*))
  (let* ((filename (merge-pathnames pathname defaults))
	 (schema (cxml:parse filename (cxml-stp:make-builder))))
    (funcall function schema)))

(defun schema-file-symbols (pathname)
  (call-with-schema-from-file #'schema-symbols pathname))
  
(defun make-element-function
    (element 
     &key (package *package*)
       (export nil))
  (let* ((name (schema-name element))
	 (symbol (intern name package))
	 (attributes (list-attributes element))
	 (at-keys (remove-duplicates 
		   (mapcar (lambda (a) 
			     (intern (schema-name a) package))
			   attributes))))
    (eval `(defun ,symbol
	 (&rest attributes 
	  &key ,@at-keys)
       (declare (ignorable ,@at-keys))
       (lambda (&optional (children-function (constantly "")))
	 (pwap/yasexml/tag:<> `(,,name ,@attributes) 
	   (funcall children-function)))))
    (when export (export symbol package))))

(defmacro define-schema-functions (schema &optional (package *package*))
  (let* ((elements (list-elements schema)))
     `(progn
	,@(mapcar (lambda (e) 
		    (make-element-defun-sexp 
		     e :package package))
		  elements))))
	 
(defmacro define-schema-package ((package-name pathname) &rest options)
  (declare (ignore options))
  (let* ((filename (merge-pathnames pathname (or *compile-file-pathname*
						 *load-pathname*
						 *default-pathname-defaults*)))
	 (schema (cxml:parse filename (cxml-stp:make-builder)))
	 (elements (list-elements schema))
	 (export-symbols (mapcar #'make-symbol
				 (mapcar #'schema-name elements)))
	 (package-exists? (find-package package-name))
	 (package (or package-exists? (make-package package-name))))    
    (prog1 `(eval-when (:compile-toplevel :load-toplevel :execute)
	      (cl:defpackage ,package-name 
		(:use) 
		(:export ,@export-symbols))
	      ,@(mapcar (lambda (e) 
			  (make-element-defun-sexp 
			   e :package package))
			elements))
      (unless package-exists? (delete-package package-name)))))

		
