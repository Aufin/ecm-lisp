(in-package :lisp-on-lines)

(define-layered-method attribute-value-using-object (object (attribute slot-definition-attribute))
   (handler-case (slot-value object (attribute-slot-name attribute))
     (unbound-slot ()  +unbound-slot+)))

(defun funcall-with-attribute-context (attribute thunk)
  (funcall-with-layer-context 
   (modify-layer-context (current-layer-context)
			 :activate (attribute-active-descriptions attribute)
			 :deactivate (attribute-inactive-descriptions attribute))
   (lambda ()
     (with-special-symbol-access
       (contextl::funcall-with-special-initargs
	(without-special-symbol-access
	  
	  (let ((foo (mappend (lambda (desc)
				(when (consp desc)
				  (let ((description (find-description (car desc))))
				    (loop 
				      :for (key val) :on (cdr desc) :by #'cddr
				      :collect (list (find key (description-attributes description) 
							   :key #'attribute-keyword)
						     :value val)))))
			      (attribute-active-descriptions attribute))))
	    #+nil(break "~A ~A" (attribute-active-descriptions attribute) 
			foo)
	    (remove NIL foo :key #'car)))
	(lambda ()  
	  (without-special-symbol-access
	    (funcall thunk))))))))

#+NIL(defmethod initialize-attribute-for-description (description-class (attribute simple-plist-attribute) layer-name &rest args)

  "Define a method on the PROPERTY-ACCESS-FUNCTION to associate
slots (named by their :initarg) with values in layer LAYER-NAME."
  (let* ((class (class-of attribute))
	 (slotds (class-slots class)))    
    (setf (slot-value attribute 'description-class) description-class)
    (ensure-layered-method  
     (ensure-property-access-function attribute)
     `(lambda (description-class)
	',(alexandria:remove-from-plist  
	   (loop 
	      :for (key val) :on args :by #'cddr 
	      :nconc (list 
		      (loop 
			 :for slotd :in slotds 
			 :do (when (find key (slot-definition-initargs slotd))
			       (return  (slot-definition-name slotd))))
		      val))
	   nil)) 
     :specializers (list description-class)
     :qualifiers '(append)
     :in-layer layer-name)))

