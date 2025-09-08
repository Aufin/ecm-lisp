(in-package :maxclaims)

(define-description claim-status (description-for-claim-status)
  ((claim-status-type :label nil)
   (active-attributes :value '(claim-status-type)))
  (:in-description inline))

(define-description claim-cause (description-for-claim-cause)
  ((claim-cause-type :label nil)
   (active-attributes :value '(claim-cause-type)))
  (:in-description inline))

(define-description claim-detail-type (description-for-claim-detail-type)
  ((description :label nil)
   (active-attributes :value '(description)))
  (:in-description inline))

(define-description claim-detail (description-for-claim-detail)
  ((claim-detail-type :label nil)
   (code :label nil)
   (description :label nil)
   (attribute-delimiter :value ": ")
   (active-attributes :value '(claim-detail-type description)))
  (:in-description inline))

(define-description claim-claim-detail (description-for-claim-claim-detail)
  ((claim-detail :label nil)
   (active-attributes :value '(claim-detail)))
  (:in-description inline))

(define-layered-class claim-bordereau-attribute (standard-attribute)
  ())

(define-layered-class claim-bordereau-by-heading-attribute (claim-bordereau-attribute)
  ())

(define-layered-class claim-bordereau-totals-attribute (claim-bordereau-attribute)
  ())

(define-layered-class claim-deductible-totals-attribute (standard-attribute)
  ())


(define-layered-class claim-transaction-has-many (has-many-attribute)
 ())

(defvar *can-edit-cheque-number* nil)

(defun user-can-edit-cheque-number-p (user)
  (declare (ignore user))
  (or *can-edit-cheque-number*
      (second (select-only 1 '(:pg-has-role "mr_update" "member")))))

(define-layered-method display-html-attribute-value 
   :around (object (attribute claim-transaction-has-many))
  (let ((*can-edit-cheque-number* (user-can-edit-cheque-number-p $app-user)))
   (call-next-method)))
							    
(define-description claim (description-for-claim)
  ((deductible :value-formatter $-formatter
	       :input (:type currency)
	       :label "Deductible to be Recovered")
   (total-deductible :value-formatter $-formatter
	       :input (:type currency))
   (timecards :attribute-class timecard-totals-attribute
	      :sort (simple-date:time< :key timecard.date)
	      :active t)
   (attachments :attribute-class has-many-attribute
		:active t)
   (details :attribute-class claim-details-attribute
	    :active :when)
   (plaintiff :active :when
	      :input (:type select-db-object 
		      :db-type person
		      :attributes (first-name last-name company-name)
		      :size 20))
   (adjuster :input (:type select-db-object :db-type person :size 10)
	     :attributes ((first-name
			   :label nil)
			  (last-name :label nil)
			  (company-name :label " "))
	     :activate ((prepopulated-select :select-function active-adjusters))
	     :deactivate (writeable-select)
	     :validate (boundp))
   (adjuster-office :input (:type select-db-object 
				  :db-type person
				  :attributes (company-name)))
   (defense-counsel
       :input (:type select-db-object 
	       :db-type person
	       :attributes (first-name last-name company-name)
	       :size 20)
     :active :when)
   (coverage-counsel 
    :input (:type select-db-object 
	    :db-type person
	    :attributes (first-name last-name company-name)
	    :size 20)
    :active :when)
   (date-of-loss
    :input (:type simple-date)
    :validate (boundp))
   (status :input (:type select-db-object
			 :db-type claim-status
			 :attributes (claim-status-type)
			 :size 1)
	   :deactivate (writeable-select)
	   :activate (prepopulated-select)
	   :validate (boundp))
   (insured :function (lambda (claim)
			(ignore-errors
			  (policy.insured
			   (risk.policy (claim.risk claim)))))
	    :editp nil
	    :label "Insured")
   (claim-bordereau-by-heading
    :editp nil
    :function 'claim-info-by-heading
    :attribute-class claim-bordereau-by-heading-attribute
    :active :when
    :label "Reserve Information")
   (claim-bordereau-totals
    :editp nil
    :function 'claim-info-totals
    :attribute-class claim-bordereau-totals-attribute    
    :label "Total Reserve Information")
   (claim-deductible-totals
    :editp nil
    :function 'claim-deductible-totals
    :attribute-class claim-deductible-totals-attribute   
    :label "Total Deductible Information")
   (transactions 
    :attribute-class claim-transaction-has-many
    :sort (simple-date:time<
	   :key claim-transaction.transaction-date))
   (risk :editp nil :label "Risk" :validate (boundp)
	 :input (:type select-db-object
		       :db-type risk
		       :attributes ((policy :attributes (policy-number))
				    (contract :attributes (contract-number)))))
   (policy :label "Policy"
	   :function (lambda (c) (ignore-errors (risk.policy (claim.risk c)))))
   (contract :function 'find-contract  
	     :editp nil :label "Contract"
	     :activate (link-to-viewer))
   (diary-entries :attribute-class has-many-attribute
		  :active t
		  :filter filter-diary-entry)
   (active-attributes
    :value '((policy :activate (link-to-viewer)
	      :attributes 
	      ((policy-number :label nil)
	       effective-date 
	       expiry-date)
	      :active :when)		; fixme: NULL risk-id HACK
	     (risk :attributes (type)
	      :activate (link-to-viewer)
	      :active :when)		; fixme: NULL risk-id HACK
	     (contract
	      :active :when)		; fixme: NULL risk-id HACK
	     date-of-loss
	     claim-deductible-totals
	     claim-bordereau-by-heading
	     claim-bordereau-totals
	     status
	     details
	     plaintiff 
	     defense-counsel
	     coverage-counsel
	     (adjuster :activate (link-to-viewer))
	     (adjuster-office :activate (link-to-viewer) :active :when)
	     (transactions 
	      :attributes (transaction-date
			   transaction-type
			   transaction-heading
			   (cheque-number :active t
					  :editable t)
			   amount))
	     (timecards :attributes (date
				     (app-user :attributes ((username :label nil))
					       :activate (link-to-timecards-viewer)
					       :label "User")
				    notes 
				     (minutes
				      :active :when)
				     mileage-km
				     disbursements))
	     (attachments  
	      :attributes ((file-name  )
			   (date)
			   (file-description ) 
			   (file :label "Download")))
	     (diary-entries)))))

(defmethod render-title  ((self object-viewer-component) (object claim))
  (<:div 
   :class "tabs"
   :style "display:inline;margin-bottom:5px;"      
   (<:ul 
    (<:li 
     :class "current"
     :style "display:inline;border-bottom:1px solid grey"
     
     (<:a :href "#" 

	  (<:h1    :style "display: inline; font-size:200%"
	 (display self (object self) 
		  :activate '(inline)
		  :attributes '((label :attributes (identity))
				(claim-id :label "#")
				(status :label nil :active :when))))))
    
     (unless (or (user-read-only-p $app-user)
		 (and (slot-boundp object 'status)
		      (string-equal (claim-status.claim-status-type 
				     (claim.status object))
				    "Closed")))
       (<:li 
	(<ucw:a :action (close-claim self object)
		(<:as-html "close claim"))))
    
  (render-actions self)
  
  (when (user-is-adjuster-p $app-user)
    (<:li (<ucw:a :action (edit-object-attributes object '(status))
	    (<:as-html "edit status"))))))
  
  (<:h2 (display self (object self) 
		  :activate '(inline)
		  :attributes '((insured :activate (link-to-viewer))))))

(defun filter-diary-entry (entry)
  (not (or (user-read-only-p $app-user)
	   #|(diary-entry.processedp entry)|#)))

(define-description claim (description-for-claim)
  ((lol::active-attributes 
    :value '(claim-id 
	    insured)))
  (:in-description inline))

(define-description claim (description-for-claim)
  ((lol::active-attributes 
    :value '((timecards :attributes (date
				     (app-user :attributes ((username :label nil))
					       :activate (link-to-timecards-viewer)
					       :label "User")
				    notes 
				     (minutes
				      :active :when)
				     mileage-km
				     disbursements)))))
  (:in-description user-timecards))


(define-layered-class edit-claim-externals-attribute (standard-attribute) ())

(defmethod edit-the-value ((edited (eql 'claim))
			   (attribute-name (eql 'adjuster)))
  (if (or (app-user.admin $app-user)
	  (user-is-adjuster-p $app-user))
      t 
      nil))

(define-description claim (description-for-claim)
  ((edit-externals :attribute-class edit-claim-externals-attribute :value t)
   (active-attributes 
    :value '(claim-id 
	     edit-externals
	     (plaintiff :active t)
	     (adjuster :active t)
	     (adjuster-office :active t)
	     (deductible :active t
	      :editp t)
	     (total-deductible :active t
	      :editp t)
	     details
	     date-of-loss
	     status #|fixme: was status-code|#
	     (defense-counsel :active t)
	     (coverage-counsel :active t))))
  (:in-description editable))

(defmethod find-contract ((claim claim))
  (ignore-errors  (risk.contract (claim.risk claim))))

;;; Edit Claim Externals
(define-layered-method 
    lol::display-html-attribute-value (claim (attribute edit-claim-externals-attribute))
    (<:td
     (if (ignore-errors (cdr (risk.claims (claim.risk claim))))
	 (progn 
	   (<:as-html "Multiple claims have this risk. Changing it here is not allowed, please click on Risk in the claim itself to view/change details.")
	   (<:br)
	   (let ((self *current-component*))	     
	     (<ucw:a 
	      :action (let* ((risk (claim.risk claim))
			     (new (make-instance 
				   'risk 
				   :risk-type (risk.risk-type-name risk)
				   :policy (risk.policy risk)
				   :contract (risk.contract risk)))
			     (details (mapcar 
				       (lambda (c)
					 (make-object 
					  (class-name (class-of c))
					  :risk new
					  :risk-detail
					  (risk-risk-detail.risk-detail c)))
				       (relation.details risk))))
			       (mapcar (lambda (d)
					 (insert-foreign-object new 'details d))
				       details)
			       (let ((o (create-object new)))
				 (setf (claim.risk-id claim)
				       (risk.risk-id o))))
	      (<:as-html "Create New Risk for this Claim"))))
	 
	 ;; fixme: Might want to add y-or-n-p dialog when changing risk
	 ;; type as it is currently
	 (<:ul (<:li (<ucw:a :action (select-risk-type claim)
			     "Change Risk Type"))
	       (<:li (<ucw:a :action
			     (update-object
			      (edit-object-attributes
			       (claim.risk claim) '(contract)))
			     "Change Contract for Risk"))
	       (<:li (<ucw:a :action
			     (let ((o (edit-object-attributes
				       (claim.risk claim) '(policy))))

			     (update-object o))
			     "Change Policy for Risk"))))))


;;; Modification of claim.risk risk-type
(defun change-risk-type (claim new-risk)
  (let ((old-risk (claim.risk claim)))
    (ensure-transaction ()
      (if (persistentp new-risk)
	  (update-object new-risk)
	  (insert-object new-risk))
      (setf (claim.risk claim) new-risk)
      (dolist (detail (claim.details claim))
	(delete-object detail))
      (setf (claim.details claim) nil)
      (update-object claim)
      ;; fixme: broken when other claims reference risk
      (when (and old-risk (not (risk.claims old-risk)))
	(delete-object old-risk)))))

(defclass select-risk-type-component-2 ()
  ((claim :accessor claim :initarg :claim :editp nil) 
   (new-type :accessor new-type :initarg :type
	     :input (:type select-db-object :db-type risk-type :size 1)
	     :activate (prepopulated-select)
	     :deactivate (writeable-select)))
  (:metaclass described-component-class))

(defmethod render ((self select-risk-type-component-2))
  (<ucw:form :action (select-risk-type-action self)
	     (read-object self :attributes '((claim :deactivate (editable))
					     (new-type)))
	     (render-buttons self)))

(defaction select-risk-type (claim)
  (call 'select-risk-type-component-2 :claim claim))

(defun claim.policy (claim)
  (when-bind risk (claim.risk claim)
    (risk.policy risk)))

(defaction select-risk-type-action (self)
  (answer (when (yes-or-no-p-dialog (format nil "Are you sure you want to change the risk type?"))
	    (let* ((old-risk (claim.risk (claim self)))
		   (new-risk (create-object
			      (apply #'make-instance
				     'risk
				     :risk-type (new-type self)
				     (cond (old-risk 
					    `(:policy ,(risk.policy old-risk)
						      :contract ,(risk.contract old-risk)))
					   ((claim.policy (claim self))
					    `(:policy ,(claim.policy (claim self)))))))))
	      (when new-risk
		(change-risk-type (claim self) new-risk))))))







;;; todo: would it instead be possible to set the
;;; :attribute-description of the outstanding-reserve attribute and
;;; write a display function for this instead?

(define-layered-method attribute-active-p :around ((attribute claim-bordereau-by-heading-attribute))		       
 (let ((active? (slot-value attribute 'lol::activep)))
   (if (and (eq :when active?)
	    (let ((v (attribute-value attribute)))
	      (every (lambda (seq)
		       (every #'zerop (cdr seq)))
		     v)))
		     
       NIL       
       (call-next-method))))

(define-layered-method lol::display-html-attribute-value (claim (attribute claim-bordereau-by-heading-attribute))
  (<:td 
   ;; todo: the bolding should probably be done in CSS, but the
   ;; stylesheet is not in the source repo!
   (<:table :class "claim-info-by-heading"
	    :width "100%"
	    (<:thead :style "background-color: #EEEEEE"
	     (<:tr :style "text-align: center"
		   (<:td  :width "33%"
			  (<:strong "Heading"))
		   (<:td  :width "33%"
			  (<:strong "Outstanding Reserve"))
		   (<:td  :width "33%"
			  (<:strong "Total Paid"))))
	    
	    
	    (<:tbody
	     (dolist (row 
		      (remove-if 
		       (lambda (v &optional (c (cdr v)))
			 (and (zerop (first c))
			      (zerop (second c))))
			 (attribute-value attribute)))
	       (<:tr (<:td :style "text-align: center"
			   (<:ah (first row)))
		     (dolist (col (cdr row))
		       ;; todo: *really* should be in the stylesheet!
		       (<:td :style "text-align: center"
			     (<:ah ($-formatter col))))))))))

(define-layered-method lol::display-html-attribute-value (claim (attribute claim-bordereau-totals-attribute))
  (<:td 
   ;; todo: the bolding should probably be done in CSS, but the
   ;; stylesheet is not in the source repo!
   (<:table :class "claim-info-totals"
	    :width "100%"
	    (<:thead :style "background-color: #EEEEEE"
	     (<:tr :style "text-align: center"
		   (<:td :width "33%"
			 (<:strong "Outstanding Reserve"))
		   (<:td :width "33%"
			 (<:strong "Total Paid"))
		   (<:td :width "33%"
			 (<:strong "Incurred"))))
	    (<:tbody
	     (<:tr (dolist (col (attribute-value attribute))
		     ;; todo: *really* should be in the stylesheet!
		     (<:td :style "text-align: center"
			   (<:ah ($-formatter col)))))))))

(define-layered-method lol::display-html-attribute-value (claim (attribute claim-deductible-totals-attribute))
  (<:td 
   ;; todo: the bolding should probably be done in CSS, but the
   ;; stylesheet is not in the source repo!
	(<:table :class "claim-info-totals"
		 :width "100%"
		 :style "text-align: center"
	    (<:thead 
	     :style "background-color: #EEEEEE"
	     (<:tr (<:td 
		    :width "33%"
		    (<:strong "Deductible"))
		   (<:td  
		    :width "33%"
		    (<:strong "Recovered Deductible"))
		   (<:td 
		    :width "33%"
		    (<:strong "Outstanding Deductible"))))
	    (<:tbody
	     (<:tr (dolist (col (attribute-value attribute))
		     ;; todo: *really* should be in the stylesheet!
		     (<:td :style "text-align: center"
			   (<:ah ($-formatter col)))))))))
