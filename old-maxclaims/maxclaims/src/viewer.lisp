(in-package :maxclaims)

(defun log-viewer-object (&key object log-type)
  ;(break "log-viewer-started")
  (ignore-errors 
   (yaclml:with-yaclml-output-to-string
    (app-user-log :log-type (or log-type 
			      "VIEW")
		:log-info (log-info-object object)
		:row-type (object-table-name object)
		:row-id (if (c2mop:slot-boundp-using-class 
			     (class-of object)
			     object (rofl::class-id-slot-definition 
				     (class-of object)))
			    (object-id object)
			    0))))
   ;(break "log-viewer-ended")
  )

(defcomponent object-viewer-component (lol-component)
  ((object :accessor object 
	   :initarg :object))
  (:metaclass described-component-class))

(defcomponent object-viewer-component/select-function (object-viewer-component)
  ()
  (:metaclass described-component-class))

(defmethod object :around ((obv object-viewer-component/select-function))
  (let ((o (call-next-method)))
    (typecase o
      (function 
       (let ((no (funcall o)))
	 (prog1 no (setf (object obv) no))))
      (t o))))
  
(defun render-title-wrapper (self function)
 (<:div 
   :class "tabs"
   :style "display:inline;margin-bottom:5px;"      
   (<:ul 
    (<:li 
     :class "current"
     :style "display:inline;border-bottom:1px solid grey" 
     (<:a :class "inline" 
	  :href "#"     
	  (<:span 
	   :class "space" 
	   (<:h1 (funcall function)))))     
    (render-actions self)
    (<:br)))) 
  
(defmethod render-title ((self object-viewer-component) object)
  (render-title-wrapper 
   self
   (lambda () (display-inline 
	       (object self) 
	       :attributes '((label :attributes (identity)))))))

(defaction close-claim (self claim)
  (when (yes-or-no-p-dialog 
	 (format nil "Closing claim ~A?~%" (claim.claim-id claim)))
    (log-viewer-object :object claim :log-type "CLOSE")
    (setf (claim.status-key claim) 
	  "Closed")
  
  (update-with-history claim)))



(defmethod ucw-core::answer-component* :after ((source object-viewer-component) target value)
  (maxclaims.info "Reset foreign relations for ~A" (object source))
  (rofl::foreign-relations-makunbound (object source)))

(defmethod user-can-edit-p :around (object)
  (if (persistentp object) 
      (or (app-user.can-edit $app-user)
	  (let* ((tn (s-sql:sql-escape 
		      (rofl::class-table-name (class-of object))))
		 (id (object-id object)))
	    (and 
	     (cadr (select-only 1 `(:history.user-can-update ,tn ,id)))
	     (call-next-method))))
      (call-next-method)))
  
  
(defmethod render-actions ((self object-viewer-component))
  (<:as-html "  ")

    (when (user-can-edit-p (object self))
      (<:li (<ucw:a :action (when (edit-object (object self))
				   (log-viewer-object 
				    :object (object self)
				    :log-type "EDIT"))
			 (<:as-html "edit"))))
  (when (user-can-delete-p (object self))
    (<:li (<ucw:a :action (when (yes-or-no-p-dialog "Really Delete?")
		      (delete-object (object self))
		      (answer t))
	    (<:as-html "delete")))))

(defmethod render :around ((self object-viewer-component))
  (<:div :style "padding-top:10px;"	 
	   (render-title self (object self))
	   (call-next-method)))

(defaction view-object (object)
  (log-viewer-object 
   :object object)
  (call 'object-viewer-component :object object))

(defaction view-object-history (object)
  (log-viewer-object  
   :object object :log-type "HISTORY")
  (call 'object-history-component :object object))

(defmethod render-object-viewer (self object)
  (display self (object self))
  (if (ignore-errors 
       (ucw::component.calling-component self))
      (<ucw:form :action (answer nil) 
		 (<:submit :value "Back"))
      (<:submit :onclick "history.go(-1)"
		:value "Back"))
  (when (persistentp object)
    (when (app-user.admin $app-user)
      (<ucw:a :action (view-object-history object)
	      (<:as-html "View History"))
      (<:br))     
    (<:as-html (object-id object))))

(defmethod render ((self object-viewer-component))
  (render-object-viewer self (object self)))
					 
(defaction view-object (object)
  (log-viewer-object 
   :object object)
  (call 'object-viewer-component :object object))

(define-description link-to-viewer ()
  ())

;;; This needs to be exported from lol either via defdisplay or ??

(define-layered-method lol::display-using-description
    :in-layer #.(lol::defining-description 'link-to-viewer)
    :around (description d object &rest args)
  (declare (ignorable args))
    (if (layer-active-p (defining-description 'html-description))
	(with-inactive-descriptions (link-to-viewer) 
	  (let* ((name (string-downcase (class-name (class-of object))))
		 (pkeys (remove-if-not #'rofl::slot-definition-primary-key-p 
				       (c2mop:class-slots (class-of object))))
		 (value (ignore-errors (c2mop:slot-value-using-class 
					(class-of object) object (first pkeys)))))
	  (<:a :href (format nil "/maxclaims/view?~A=~A" name value)
	       (call-next-method))))
	(call-next-layered-method)))

(define-description link-object-to-viewer ()
  ())

;;; This needs to be exported from lol either via defdisplay or ??

(define-layered-method lol::display-attribute-value
    :in-layer #.(lol::defining-description 'link-object-to-viewer)
    :around (attribute)
    (if (layer-active-p (defining-description 'html-description))
	(let ((object (attribute-object attribute)))
	  (with-inactive-descriptions (link-object-to-viewer) 
	    (<ucw:a :action (view-object object) (<:as-html object) 
		    (call-next-layered-method))))
	(call-next-layered-method)))


(define-description link-label-to-viewer ()
  ())

;;; This needs to be exported from lol either via defdisplay or ??
#+nil(define-layered-method lol::display-attribute-label
  :in-layer #.(lol::defining-description 'link-to-viewer)
  :around (description d o &rest args)
 (with-inactive-descriptions (link-to-viewer) 
   (<ucw:a :action (view-object o) 
	   (call-next-method))))

(defcomponent user-timecard-viewer-component (object-viewer-component)
  ((user :accessor user
	 :initarg :user))
  (:metaclass described-component-class))


(defmethod render :wrapping ((c user-timecard-viewer-component))
  (with-active-descriptions (user-timecards)
    (let ((*timecard-user* (user c)))
    (<:div 
     :style "border:solid 1px ;"
     (<:h2 :style "text-indent:3em"
	   (<:as-html (format nil "Timecards for ~A:" 
			      (app-user.username (user c)))))
    (call-next-method)))))

(defaction view-user-timecards (claim user)
  (log-viewer-object 
   :object claim
   :log-type (format nil "VIEW TIMECARDS FOR ~A"
		     (app-user.username user)))
  (with-active-descriptions (user-timecards)
    (call 'user-timecard-viewer-component 
	:object claim
	:user user)))



