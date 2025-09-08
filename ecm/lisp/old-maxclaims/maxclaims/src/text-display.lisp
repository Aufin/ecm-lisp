(defpackage :maxclaims/text-display
  (:use :cl)
  (:import-from :maxclaims/ecm-description
		#:ecm-attributes
		 #:attribute-label
		 #:attribute-value)
  (:export #:display))
(in-package :maxclaims/text-display)

(defgeneric display (object layers  &rest args &key &allow-other-keys)
  (:method (object layers &rest args)
    (declare (ignore args))
    (princ-to-string object))
  (:method ((object list) layers &rest args)
    (with-output-to-string (s)
      (dolist (o object)
	(princ (apply #'display o layers args) s)
	(terpri s))))
  (:method ((object standard-object) layers &rest args)
    (with-output-to-string (s)
      (flet ((ps (thing)
	       (when thing 
		 (princ (display thing :inline) s)
		 (princ " " s))))
	(let ((attributes (apply #'ecm-attributes object layers args)))
	  (dolist (a attributes)	    
	    (ps (attribute-label a))
	    (ps (attribute-value a))
	    ))))))
    

#+(or)(defmethod display :around (object layer 
					 &key activate label active
					 &allow-other-keys))
	  
