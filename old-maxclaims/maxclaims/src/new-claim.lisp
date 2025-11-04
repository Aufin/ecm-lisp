(in-package :maxclaims)

;; The new claim object has some contraints that are best computed rather than managed manually
;; especially given the multiple possible paths (standard, ajax, and select vs submit).
;; Unfortunately, the mess you see below does it manually, and poorly.
;; It works though, ish.

(deftype select-risk () '(or null risk-type risk))

(defclass select-risk-attribute-editor (select-db-object-attribute-editor)
  ())

(define-layered-method display-select-db-object-value :around (value 
						   (editor select-risk-attribute-editor) 
						   writer
						   lisp-on-lines:attribute 
						   selected-value
						   reset)
  (if (or (lol::unbound-slot-value-p value) (null value))
      (let (new (object (attribute-object attribute)))
	(<ucw:select 
	 :writer (lambda (val)
		   (when val 
		     (setf new (cond ((typep val 'risk-type)
				      (make-instance 'risk :risk-type val))
				     ((riskp val) val)
				     (t (error "Neither risk-type nor risk: ~A"
					       val))))
		     (funcall writer new)))
	 :reader value
	 (if (policy-exists-p (ignore-errors (policy object)))
	     (lol::with-described-object ((policy object) nil)
	       (let ((risks (attribute-value 
				      (find-attribute 
				       lol::*description* 
				       'risks))))
		 (unless (lol::unbound-slot-value-p risks)
		   (arnesi:dolist* (risk risks)
		     (<ucw:option :value risk
				  (with-inactive-descriptions (editable) 
				    (display-inline-attribute attribute risk)))))))
	     (<ucw:option :value nil))

	 (arnesi:dolist* (val (risk-types))
	   (<ucw:option :value val (<:as-html "New " (risk-type.type-name val)))))
	(<ucw:submit :action (find-db-object-using-attribute-editor-action attribute editor new)
		     "select"))
      (call-next-method)))

(defun setup-new-risk-from-component (object)
  (let ((policy (ignore-errors (policy *source-component*)))
	(contract (ignore-errors (contract *source-component*))))
    (when policy 
      (setf (risk.policy object) policy))
    (when contract 
      (setf (risk.contract object) contract))))

(defaction find-db-object-using-attribute-editor-action
    (attribute (editor select-risk-attribute-editor) object)
  (cond ((and (typep object 'standard-db-access-object) 
	      (primary-key-boundp object))
	 t)
	(t 
	 (setup-new-risk-from-component object)
	 (create-object object))))

(defmethod display-html-attribute-editor (attribute (editor select-risk-attribute-editor))
  (call-next-method))


(defclass new-claim-policy-select-db-object (select-db-object-attribute-editor)
  ())

(defun insured-or-nil (component)
  (ignore-errors (insured component)))

(defaction select-db-object-using-editor (attribute (editor new-claim-policy-select-db-object) value-thunk)
  (let ((insured (insured-or-nil *source-component*))
	(policy (funcall value-thunk)))
;;    (break "insured policy ~A ~A" insured policy)
    (cond 
      ((and (typep policy 'policy)
	    insured 
	    (not (primary-key-boundp policy)))
       (setf (policy.insured policy) insured)
       (call-next-method attribute editor value-thunk))
      (t
       (call-next-method attribute editor value-thunk)))))

(defmethod display-html-attribute-editor :around (attribute (editor new-claim-policy-select-db-object))  
  ;(break "hah ~A" (slot-value *source-component* 'insured))
  (when (and (slot-boundp *source-component* 'insured)
	     (typep (slot-value *source-component* 'insured) 'standard-db-access-object)
	     (not (slot-boundp *source-component* 'policy))
	     (slot-boundp (slot-value *source-component* 'insured) 
			       'person-id))
  
    (setf (slot-value *source-component* 'policy)
	  (cons  
	   (make-object 'policy :insured-id (person.person-id  
					     (slot-value *source-component* 'insured))) 
	   (select-objects 'policy :where `(:= insured-id 
					       ,(person.person-id  
						 (slot-value *source-component* 'insured)))))))
  (call-next-method))

(defclass new-claim-insured-select-db-object (select-db-object-attribute-editor)
  ())

(defmethod display-html-attribute-editor :around (attribute (editor new-claim-insured-select-db-object))
  (if (and (slot-boundp *source-component* 'policy)
	   (policy-exists-p (policy *source-component*)))
      (when (slot-boundp (policy *source-component*) 'insured)
      (with-inactive-descriptions (editable)
	(with-active-descriptions (link-to-viewer)
	  (setf (insured *source-component*) (policy.insured (policy *source-component*)))
	  (display-inline (policy.insured (policy *source-component*)))))
      (call-next-method))))

(defun policy-or-nil (component)
  (ignore-errors (policy component)))

(defaction select-db-object-using-editor (attribute (editor new-claim-insured-select-db-object) value-thunk)

  (let ((insured (funcall value-thunk))
	(policy (policy-or-nil *source-component*)))

    (cond 
      ((and (typep policy 'standard-db-access-object)
	    insured 
	    (not (primary-key-boundp policy)))
       (setf (policy.insured policy) insured)
       (call-next-method attribute editor value-thunk))
      (t
       (call-next-method attribute editor value-thunk)))))


(defclass new-claim-contract-select-db-object (select-db-object-attribute-editor)
  ())

(defmethod display-html-attribute-editor :around (attribute (editor new-claim-contract-select-db-object))
  (if (and (slot-boundp *source-component* 'risk)
	   (risk-exists-p (risk *source-component*)))
      (with-inactive-descriptions (editable)
	(with-active-descriptions (link-to-viewer)
	  (when (ignore-errors (risk.contract (risk *source-component*)))
	    (setf (contract *source-component*) (risk.contract (risk *source-component*)))
	    (display-inline (risk.contract (risk *source-component*))))))
      (call-next-method)))

(defun risk-or-nil ()
  (ignore-errors(risk *source-component*)))


(defaction select-db-object-using-editor (attribute (editor new-claim-contract-select-db-object) value-thunk)
  (call-next-method attribute editor value-thunk)
  (let ((risk (risk-or-nil)))
    (when risk
      (setup-new-risk-from-component (risk *source-component*)))))


  
(defaction process-person-value-type (value type)
  (typecase value 
    (string (cons value (string-search-person value)))
    (list (cond ((eql (first value) :person)
		 (destructuring-bind (f &optional (l "")) 
		     (split-sequence:split-sequence #\Space (second value))
		   (create-object (make-object type :first-name f :last-name l))))))
		 
    (t value)))

(defun slot-value-if-boundp (object slot)
  (when (slot-boundp object slot)
    (slot-value object slot)))

(defaction process-new-claim (new-claim)
  (flet ((bound? (slot)
	   (slot-boundp new-claim slot))
	 (bound-in-db (slot)
	   (and (slot-boundp new-claim slot)
		(typep (slot-value new-claim slot) 'standard-db-access-object)
		(primary-key-boundp (slot-value new-claim slot))))
	 (val (slot)
	   (slot-value new-claim slot)))
    
    (when (and (bound? 'policy) (bound? 'insured) 
	       (not (bound-in-db 'policy)))
      (if (consp (val 'policy))
	  (setf (policy.insured (car  (val 'policy)))
		(val 'insured))
	  (setf (policy.insured (val 'policy)) (val 'insured))))

    (when (bound? 'risk)
      (setup-new-risk-from-component (val 'risk))
      (when (and (bound? 'policy)
		 (not (slot-boundp (val 'risk) 'policy)))
	(setf (risk.policy (val 'risk)) (val 'policy)))

      
      (when (and (bound? 'contract)
		 (not (slot-boundp (val 'risk) 'contract)))
	(setf (risk.contract (val 'risk)) (val 'contract)))


      
      (let ((slots (mapcar (lambda (s)
			     (when (slot-boundp new-claim s)
			       (slot-value new-claim s)))
			   '(policy insured risk contract adjuster))))
	(dolist (i slots)
	  (if (and i 
		   (typep i 'standard-db-access-object)
		   (not (primary-key-boundp i)))
	      (create-object i)))
	  
	
	(if (and  (bound? 'risk) (primary-key-boundp (val 'risk)))
	    (setf (slot-value new-claim 'policy)
		  (risk.policy (val  'risk))))

	(when (and (bound-in-db 'risk) 
		   (bound-in-db 'contract)
		   (bound-in-db 'policy)
		   (bound-in-db 'adjuster)
		   (bound? 'date-of-loss)
		   (notany #'consp slots))
	  (if (or NIL  #+nil (policy-has-claim-with-date-of-loss-p (policy.policy-id (val 'policy))
							   (val 'date-of-loss)))
	      (info-message-alert "The policy already has a claim with this date of loss. Duplicate claims are not currently allowed.")
	      (let* ((claim (or (new-claim new-claim) 
				(rofl::make-object 
				 'claim 
				 :risk-id (risk.risk-id (val 'risk))
				 :adjuster-id (person.person-id (val 'adjuster))
				 :date-of-loss (val 'date-of-loss)
				 :deductible (val 'deductible)
				 :open-date (simple-date:universal-time-to-timestamp 
					     (get-universal-time))
				 :status "Open" ; FIXME: hardcoded claim_status
				 ))))
		(update-with-history (val 'risk))
		(mapcar  (lambda (detail)
			   (setf (claim-claim-detail.claim detail) claim)
			   ;; If the user has hit back the claim and
			   ;; detail may exist, but also maybe not (if
			   ;; e.g. he forgot to enter the detail
			   (if (and (new-claim new-claim) (persistentp detail))
			       (update-foreign-object claim 'details detail)
			       (insert-foreign-object claim 'details detail)))

			 (remove-if-not  (lambda (detail)
					   ;; filter any non-applicable
					   ;; claim-claim-details. Doing
					   ;; this here because it is
					   ;; simplest (the details
					   ;; editor filters out any
					   ;; non-applicable details)
					   (db= (claim-detail-type.risk-type
						 (claim-claim-detail.claim-detail-type detail))
						(risk.type (claim.risk claim))))
					 (new-details new-claim)))
		
		(cond ((new-claim new-claim)
		       ;; potentially remove details that were deleted
		       (mapcar (curry #'delete-foreign-object claim 'details)
			       (remove-if (rcurry #'member (new-details new-claim))
					  (claim.details (new-claim new-claim))))
		       (answer (update-with-history (new-claim new-claim))))
		      (t 
		       (answer (insert-with-history (setf (new-claim new-claim) claim))))))))))))
  
(defclass new-claim-component ()
  ((insured :input (:type select-db-object
			  :class new-claim-insured-select-db-object
			  :db-type person
			  :size 10)
            :accessor insured
	    :initform nil
            :attributes ((first-name :active :when)
			 (last-name :active :when)
			 (company-name :active :when)))
   (policy :input (:type select-db-object
		   :size 20	 
                   :class new-claim-policy-select-db-object
                   :db-type policy
                   :attributes (policy-number effective-date expiry-date))
           :attributes (policy-number 
			effective-date
			(insured :active :when 
				 :attributes 
				 ((first-name :active :when)
				  (last-name :active :when)
				  (company-name :active :when))))
           :accessor policy)
   (risk :input (:type select-risk :db-type risk)
         :accessor risk
         :attributes (type (contract
			    :label nil
                            :attributes
                            ((contract-number :label "Contract Number"))
                            :active :when)
			   (claim-number :label "Number of Claims")))
   (contract :input (:type select-db-object
			   :db-type contract
			   :class new-claim-contract-select-db-object
			   :size 20
			   :attributes (contract-number)
			   )
	     :editp t
	     :attributes (contract-number agency)
	     :accessor contract)
   (date-of-loss :input (:type simple-date)
                 :accessor date-of-loss)
   (initial-reserve :input (:type currency))
   (deductible :input (:type currency)
	       :accessor deductible)
   (plaintiff :input (:type select-db-object :db-type person)
	      :attributes (first-name last-name company-name))
   (adjuster :input (:type select-db-object :db-type person :size 1)
             :attributes (first-name last-name company-name)
	     :activate ((prepopulated-select :select-function active-adjusters))
	     :deactivate (writeable-select))
   (details :attribute-class claim-details-attribute
	    :accessor new-details
	    :initform nil)
   (new-claim :accessor new-claim :initform nil))

  (:metaclass described-component-class)
  (:attributes policy insured risk contract date-of-loss deductible adjuster details))

;;; Details editor helpers

(defmethod attribute-object-risk-type ((attribute claim-details-attribute)
				       (object new-claim-component))
  (risk.type (risk object)))

(defmethod make-object-detail ((object new-claim-component))
  (make-instance 'claim-claim-detail))

(defmethod notify-detail-changed ((object new-claim-component)
				  detail-detail
				  old-value
				  new-value)
  #+nil(break "ndc: claim = ~A" (new-claim object))
  (cond ((and (null old-value) new-value)
	 (push detail-detail (new-details object)))
	((and old-value new-value)
	 ;; do nothing -- the claim-detail-detail was mutated
	 )
	((and old-value (null new-value))
	 (setf (new-details object)
	       (remove detail-detail (new-details object))))))

(defmethod render ((self new-claim-component))
  (<:fieldset 
   (<:div :class "notes"
	  (<:h4 "Important Info")
	  (<:as-html "If adding a claim to an existing policy, there is no need to find the insured (this will happen automatically)."))
   (<ucw:form 
    :action (process-new-claim self)
    (<:legend  (<:h2 :style "display:inline"
		     (<:as-html "Add New Claim")))
    
    (<:submit :value  "Continue")       
    (<ucw:submit :action  (answer nil) "Cancel")
    (with-inactive-descriptions (inline)
      (read-object self)
      (<:submit :value "Continue")
      (<ucw:submit :action  (answer nil) "Cancel")))))

