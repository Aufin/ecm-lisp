(in-package :maxclaims)

(defcomponent object-editor-component (object-viewer-component)
  ((attributes :initarg :attributes :initform nil :accessor editor-attributes))
  (:metaclass described-component-class))


(defun render-buttons (self)
  (with-udb
    (<:submit :value "Continue")
    (<ucw:a :action (progn (reload-object (object self)) ; discard changes
			   (answer nil))
	    "Cancel")))

(defmethod render-actions ((Self object-editor-component))
  (<:br))

(defmethod render ((self object-editor-component))
  (with-active-descriptions (object-editor-component validate)
    (let ((submit-action 
	   (make-action 
	    (lambda ()
	      (with-udb 
		(arnesi:with-call/cc
		  (when (lol:validp (object self))
		    (update-object-action self))))))))
     
      (<ucw:form :action* submit-action 
		 :method "POST"
		 (render-buttons self)
		 (apply #'read-object (object self) 
			(when (editor-attributes self)
			  (list :attributes (editor-attributes self))))
		 (render-buttons self)))))

(defaction update-object-action (self)
  (let* ((class (class-of (object self))) 
	 (foreign-objects (find-foreign-objects (object self))))
    (maxclaims.info "Preparing to Update object ~A" (object self))
    (let ((it-be t))
      (dolist (o foreign-objects)
	(unless (or (not (rofl::db-access-object-p o))
		    (primary-key-boundp o))	
	  (unless (create-object o)
	    (setf it-be nil))))
      (if (and it-be (notany #'consp foreign-objects))      
	  (if (primary-key-boundp (object self))
	      (progn  
		(maxclaims.info "Updating persistent object ~A" (object self))
		(answer (update-with-history (object self))))
	      (progn  
		(maxclaims.info "Inserting new object from update: ~A" (object self))
		(insert-object-action self)))))))
					 
(defaction edit-object (object)
  (call 'object-editor-component :object object))

(defaction edit-object-attributes (object attributes)
  (call 'object-editor-component :object object :attributes attributes))

(define-description link-to-editor ()
  ())

;;; This needs to be exported from lol either via defdisplay or ??

(define-layered-method lol::display-using-description
    :in-layer #.(lol::defining-description 'link-to-editor)
    :around (description d o &rest args)
    (with-inactive-descriptions (link-to-editor) 
      (<ucw:a :action (edit-object o) 
	      (call-next-method))))

(define-description link-object-to-editor ()
  ())

;;; This needs to be exported from lol either via defdisplay or ??

(define-layered-method lol::display-attribute
    :in-layer #.(lol::defining-description 'link-object-to-editor)
    :around (attribute)
    (let ((object (attribute-object attribute)))
      (with-inactive-descriptions (link-object-to-editor) 
	(<ucw:a :action (edit-object object) (<:as-html object) 
		(call-next-method)))))


(define-description link-label-to-editor ()
  ())

;;; This needs to be exported from lol either via defdisplay or ??
#+nil(define-layered-method lol::display-attribute-label
  :in-layer #.(lol::defining-description 'link-to-editor)
  :around (description d o &rest args)
 (with-inactive-descriptions (link-to-editor) 
   (<ucw:a :action (edit-object o) 
	   (call-next-method))))
