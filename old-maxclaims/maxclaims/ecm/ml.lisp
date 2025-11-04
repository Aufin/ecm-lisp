(uiop:define-package :ecm/ml
    (:use :cl :sexpml :sexpml/html5)
  (:shadowing-import-from :sexpml/html5 #:<>)
  (:import-from :sexpml/html5)
  (:reexport :sexpml :sexpml/html5)
  (:export #:attribute-value #:<>))
(in-package :ecm/ml)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf sexpml:*sexpml-indent* nil))

(defun attribute-value (name &key (attributes sexpml:*sexpml-attributes*)
			       (default NIL))
  (let* ((null (gensym))
	 (value (getf attributes
		      (typecase name
			(keyword name)
			(symbol (intern (symbol-name name) :keyword))
			(string (intern (string-upcase name) :keyword)))
		  null)))
    (values (if (eq value null)
		default
		value)
	    (not (eq value null)))))
	    
  
