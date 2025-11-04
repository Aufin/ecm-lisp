(in-package #:maxclaims)

(define-layered-class diary-entry-processed-attribute (slot-definition-attribute)
  ())

(define-layered-class diary-entry-defer-attribute (has-many-attribute)
  ())

(define-layered-class diary-note-attribute (slot-definition-attribute)
  ())

(define-description diary-entry (description-for-diary-entry)
  ((user :validate (boundp)
	 :editp nil
	 :attributes ((username :label nil)))
   (claim :validate (boundp)
	  :activate (link-to-viewer)
	  :deactivate (list-as-table))
   (action-date :input (:type simple-date)
		:validate (boundp)
		:label "Date")
   (note :input (:type textarea) :value-formatter augment-newline-with-<br/>
	 :attribute-class diary-note-attribute
	 :validate (boundp))
   (defered 
       :attribute-class diary-entry-defer-attribute
       :label "Defered"
     )
   (processed :input (:type bool)
	      :validate (boundp)
	      :attribute-class diary-entry-processed-attribute)
   (active-attributes :value '(user claim action-date note))))

(defmethod render-title ((self object-viewer-component) (object diary-entry))
  (render-title-wrapper 
   self
   (lambda () 
     (<:as-html "Diary Entry for Adjuster"))))

(defaction create-object :around ((diary diary-entry))
  (if (slot-boundp diary 'claim)
      (progn 
	(rofl::mark-instance-as-persistent 
	 (claim.adjuster (diary-entry.claim diary)))
	(let ((id  
		(adjuster-user-id 
		 (claim.adjuster (diary-entry.claim diary)))))
	  ;(break "~A ~A" id diary)
	  (if id 
	      (progn 
		(setf (diary-entry.app-user-id diary) id)
		(call-next-method diary))
	      (error "Adjuster ~A does not exist as a user" (claim.adjuster (diary-entry.claim diary))))))
      (error "No Adjuster as user, cannot add a diary")))


(define-description diary-entry (description-for-diary-entry)
  ((active-attributes :value '((user :deactivate (editable)) action-date note )))
  (:in-description editable))

(defun note-summary-formatter (note)
  (if (< (length note) 65)
      note
      (arnesi:strcat (subseq note 0 64)
		     "…")))

(define-description diary-entry (description-for-diary-entry)
  ((active-attributes :value '((action-date)
			       user
			       claim
			       (note :value-formatter note-summary-formatter)
			       (defered )
			       processed)))
  (:in-description inline))

(defcomponent diary-entry-creator (object-creator-component)
  ())

(defmethod render :around ((self diary-entry-creator))
  (<:h1 "Open claims must have at least one active diary date")
  (call-next-method))

(defaction process-diary-entry ((entry diary-entry) slot-writer)
  ;; fixme: hack
  (rofl::foreign-relations-makunbound (diary-entry.claim entry))
  (let ((claim (diary-entry.claim entry)))
    (if (or (claim-closed-p claim)
	    (some (lambda (e) (not (or (= (diary-entry.diary-entry-id e)
				     (diary-entry.diary-entry-id entry))
				  (diary-entry.processedp e))))
		  (claim.diary-entries claim))
	    (call 'diary-entry-creator
		  :object (make-object 'diary-entry :claim claim :user $app-user)))
	(funcall slot-writer t)
	(info-message-alert "WARNING: Diary entry not marked done."))) )

(define-layered-method lol::display-attribute-value
    :in-layer #.(defining-description 'html-description) ((attribute diary-entry-processed-attribute))
  (flet ((form-body ()
	   (with-active-descriptions (editable)
	     (let ((writer (dlambda (val)
			     (setf (attribute-value attribute) val)
			     (update-object (attribute-object attribute)))))
	       (if (not (attribute-value attribute))
		   (let ((diary-entry (attribute-object attribute)))
		     (<ucw:submit :action (process-diary-entry diary-entry writer)
				  "Mark as Done"))
		   (<:ah "Already Processed"))))))
    
    (if ucw::*in-form*
	(form-body)
	(<ucw:form :action (refresh-component self)
		   :method "POST"
	  (form-body)))))

(defmethod diary-entry-defered ((diary-entry-id integer))
  (query (:select 'defer-date 
	  :from 'defer-diary-entry 
	  :where (:and (:= 'diary-entry-id diary-entry-id)
			(:> 'defer-date (:now))))
	 :single))

(define-layered-method attribute-active-p :around ((attribute diary-entry-defer-attribute))
 t)

(defmethod display-has-many-attribute-p (has-many (attribute diary-entry-defer-attribute))
  (attribute-active-p attribute))

(define-layered-method lol::display-attribute-value
    :in-layer #.(defining-description 'html-description) ((attribute diary-entry-defer-attribute))
  (flet ((form-body ()
	   (with-active-descriptions (editable)
	     (let ((defered (diary-entry-defered 
			     (diary-entry.diary-entry-id
			      (attribute-object attribute)))))
	       
	       (if (not (diary-entry.processedp (attribute-object attribute)))
		   (if (not defered)
		       (<ucw:submit :action (create-new-has-many 
					     (attribute-object attribute)
					 attribute)
				    
				:value "Defer")
		       (<:ah "Defered until " (print-date defered)))
		   (<:ah "Done"))))))
    
    (if ucw::*in-form*
	(form-body)
	(<ucw:form :action (refresh-component self)
		   :method "POST"
	  (form-body)))))

(define-layered-method lol::display-attribute-value
  :in-layer #.(defining-description 'html-description)
  :around ((attribute diary-note-attribute))
  (<:div :style "max-width: 30em; word-wrap: break-word;"
	 (call-next-layered-method)))
