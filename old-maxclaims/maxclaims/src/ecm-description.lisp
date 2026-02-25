(defpackage :maxclaims/ecm-description
  (:use :cl)
  (:export #:ecm-attributes
	   #:update-object
	   #:insert-object
	   #:attribute-layers
	   #:attribute-label
	   #:attribute-type
	   #:attribute-value
	   #:attribute-required
	   #:attribute-name

	   ;; * Activate 
	   #:view-link
	   #:diary-link
	   #:modify-timecard
	   #:download-timecard
	   #:contract-bordereau
	   #:link-to-viewer
	   #:search-link
	   #:download-attachment
	   #:view-a-btn
	   )
  
  (:import-from :max-ecm/gnumeric/ssconvert
		#:ssconvert-export-types
		#:ssconvert-type)
  (:import-from :maxclaims
		#:open-for-recovery
		#:login
		#:unbillable-hours 
		#:ibc-code
		#:external-adjuster
		#:active
		#:app-user-rolename
		#:diary-entry-id
		#:contract-id
		#:app-user.app-user-id 
		#:sha1-digest
		#:approved
		;; * Claim
		#:claim
		#:claim.cause
		#:claim-cause
		#:claim-cause-type
                #:claim-cause-codes
		#:cause
    #:denial
    #:syndicates
    #:app-user-syndicate
    #:date-of-denial
    #:reason-for-denial
    #:refer-to-underwriters
    #:line-of-business
    #:coverage
    #:event-category
    #:basis-of-settlement
    #:subscription-percent
    #:claim.claim-id
    #:claim.adjuster-id
    #:claim.adjuster
    #:claim.open-date
    #:claim.close-date
    #:claim.status
    #:claims
    #:claim-id
    #:insured
    #:label
    #:claim.label
    #:date-of-loss
    #:date-claim-made
    #:claim.risk
    #:open-date
    #:close-date
    #:rev-date
    #:peer-reviewed-date
    #:recovery-subrogation-date
    #:claim-received-time
    #:claim-acknowledged-time
    #:loss-detail
    #:loss-details 
    #:loss-detail-key
    
    #:driver-detail
    #:driver-details 
    #:driver-detail-key
    
    #:vehicle-detail
    #:vehicle-details 
    #:vehicle-detail-key
    
    #:claim-status-detail
    #:claim-status-details 
    #:claim-status-detail-key




		;; ** Claim Authority
		#:claim-authority
		#:authority
		
		
		;; ** Claim Status
		#:claim.status
		#:status		
		#:claim-status
		#:claim-status-type
		#:claim-status.claim-status-type

		#:claim-detail
		#:detail.description
		#:claim-claim-detail
		#:detail-text
		#:adjuster
		#:transactions
		
		;; * Diary 
		#:diary-entry
		#:diary-entry.processed
		#:diary-entry.claim
		#:diary-entries
		#:diary-entry
		#:action-date 
		#:user 
		#:defered 
		#:processed 
		#:note
		#:defer-date
		#:defer-diary-entry

		#:update
		#:updates
		#:claim-update
		#:active-adjusters
		#:risk-id
		#:deductible
		#:plaintiff
		#:coverage-counsel
		#:defense-counsel

			
	

		#:attachments
		#:attachment
		#:attachment-id
		
		#:file-name
		#:file-description
		;; #:file
		#:sha1-digest
		#:file-type

		
	
		
		#:claim-transaction
		#:transaction-date
		#:transaction-type
		#:transaction-type-id
		#:claim-transaction-expense-type
		#:claim-transaction-expense-type-name
		#:expense-type
		#:claim-transaction-type
		#:description 
		#:transaction-type
		#:transaction-heading
		#:claim-transaction-heading
		#:claim-transaction-heading-name
		#:reference-number
		#:schemes-advance-number
		#:payee
		#:timecards
		#:timecard
		#:timecard-interim
		#:timecard.attachment
		#:notes

		#:app-user
		#:app-user-id
		#:username
		#:password
		#:can-edit
		#:admin
		#:contracts
		
		;; App Adjuster
		#:app-adjuster
		#:full-time

		;; App User Access
		#:app-user-contract
		#:app-user-agency
		#:app-user-claim
		#:access 
		
		

		#:minutes
		#:mileage-km
		#:disbursements
		#:cheque-number
		#:amount
		#:risk
		#:risks
		#:risk-type
		#:type-name
		#:risk.policy
		#:risk.contract
		#:details
		#:detail
		#:risk-risk-detail
		#:risk-risk-detail-detail
		#:risk-risk-detail-id	; needed for foreign-insert
		
		#:risk-detail
		#:code
		
		#:policy
		#:policy-number
		#:policy-detail
		#:policy-detail-key
		#:key
		#:value
		#:text
		#:policy-id
		#:effective-date
		#:expiry-date
		#:agent
		#:company
		#:sub-agent
		#:branch
		#:underwriter

		#:person 
		#:first-name
		#:last-name
		#:company-name
		#:policies-as-insured
		#:contracts-as-agency
		#:contracts-as-syndicate
		#:address1
		#:address2
		#:city
		#:province/state
		#:postal-zip-code
		#:home-phone 
		#:work-phone 
		#:fax-phone 
		#:cell-phone 
		#:email-address 
		#:birth-date
		#:date-added 
		#:date-modified 
		#:postal-code
		#:policy.insured
		#:date
		#:loss-fund
		#:contract
		#:settlement-type
		#:contract-number
		#:contract.contract-id
		#:contract-authority
		#:syndicate
		#:agency
		#:london-broker
		#:merge-attachment-pathname
		#:string-search-person
		)
  (:import-from :simple-date 
		#:timestamp
		)

  (:import-from :maxclaims/data-entity/app-user-message
		#:app-user-message
		#:app-user-message.app-user-message-id
		#:app-user-message.parent-message
		#:app-user-message.from
		#:app-user-message.to
		#:app-user-message.subject
		#:app-user-message.body
		#:app-user-message.unread
		
		#:app-user-message-id
		#:parent-message-id
		#:parent-message 
		#:from
		#:from-id
		#:to
		#:to-id
		#:subject
		#:body
		#:unread
		)
  (:export #:object-attribute-value))



(in-package :maxclaims/ecm-description)



(defvar *query-offset* 0)

(defun query-offset ()
  *query-offset*)

(defgeneric ecm-description (description-name layer))

(defun ensure-description (description-name layer attributes)
  (eval `(defmethod ecm-description ((n (eql ',description-name))
				     (l (eql ',layer)))
	   ',attributes)))

(defmacro define-description (description-name layer &body attributes)
  `(ensure-description ',description-name ',layer ',attributes))

(defmacro define-descriptions (name &body layer-and-attributes)
  `(progn ,@(loop for (layer . attributes) :in layer-and-attributes
		 :collect `(define-description ,name ,layer ,@attributes))))

(defgeneric insert-object (object)
  (:method (object)
    (maxclaims::insert-object object)))

(defgeneric update-object (object)
  (:method (object)
    (maxclaims::update-object object)))

(defmethod insert-object ((object attachment))
  (prog1  (call-next-method)
    (let* ((pn (merge-attachment-pathname object))
	   (fn (file-namestring pn))
	   (tmpfn (pathname (concatenate 'string "/tmp/" fn))))
      (ensure-directories-exist pn)
      (open pn :direction :probe :if-does-not-exist :create)
      (cl-fad:copy-file tmpfn 
			pn
			:overwrite t)
      (delete-file tmpfn))))

(define-descriptions null
 (:view identity)
  (:app-user-tab identity)
  (:view-tab identity)
  (:claim-tab identity)
  (:overdue identity)
  (:claim identity)
  (:policy-view-tab identity)
  (:contract-view-tab identity)
  (:diary-entry-tab identity)
  (:default identity)
  (:inline identity))

(define-descriptions cons 
  (:view identity)
  (:app-user-tab identity)
  (:view-tab identity)
  (:default identity)
  (:inline identity))

(defclass view-link ()
  ((object :initarg object)))

(define-descriptions view-link
  (:default)
  (:inline identity))

(defmethod object-attribute-value (object (a (eql 'view-a-btn))
				   &rest args)
  (declare (ignore args))
  (make-instance 'view-link 'object object))

(local-time:reread-timezone-repository)

(defun print-date-to-string (timestamp)
  (local-time:format-timestring 
   nil 
   (local-time:universal-to-timestamp timestamp) 
   :format '(:year "-" :short-month "-" :day)
   :timezone (local-time:find-timezone-by-location-name "America/Vancouver")))

(define-descriptions timestamp
  (:default date-time)
  (:date-time date-time)
  (:edit-date-time (date-time :timezone t))
  (:date date)
  (:inline date))

(defmethod object-attribute-value ((time timestamp) 
				   (a (eql 'date)) &rest args)
  (declare (ignore args))
  (local-time:format-timestring
   nil 
   (local-time:universal-to-timestamp 
    (simple-date:timestamp-to-universal-time time)) 
   :format '(:year "-" :short-month "-" :day)
   :timezone local-time:+utc-zone+))

(defmethod object-attribute-value ((time timestamp) 
				   (a (eql 'date-time)) 
				   &rest args &key (timezone t) &allow-other-keys)
  (destructuring-bind  (&key (timezone t))
      args
;;;ht    (break "~A" args)
    (local-time:format-timestring
     nil 
     (local-time:universal-to-timestamp 
      (simple-date:timestamp-to-universal-time time)) 
     :format `( :year "-" :month "-" :day 
		      " " (:hour 2) 
		      ":" (:min 2) 
		      ":" (:sec 2)
		      ,@ (when timezone
			   (list " "   :GMT-OFFSET-OR-Z)))
     :timezone (local-time:find-timezone-by-location-name "America/Vancouver"))))

(define-descriptions simple-date:date
  (:default date)
  (:date-time date)
  (:date date)
  (:inline date))

(defmethod object-attribute-value ((date simple-date:date) 
				   (a (eql 'date)) &rest args)
  (declare (ignore args))
  (multiple-value-bind (year month day) (simple-date:decode-date date)
    (print-date-to-string (encode-universal-time 0 0 0 day month year))))


(define-descriptions risk-risk-detail        
  (:inline risk-detail)
  (:default (risk-detail :activate ((p :class "lead")))))

(define-descriptions risk-detail        
  (:inline description (code :label t))
  (:default code description))


(define-descriptions claim-status
  (:inline claim-status-type))

(define-descriptions risk-type
  (:inline type-name)
  (:default type-name))


(defgeneric ecm-description-attributes (description layers)
  (:method :around ((d symbol) (l null))
    (call-next-method d :default))
  (:method ((d symbol) (l symbol))    
    (loop :for a :in (ecm-description d l)
       :collect 
       (let ((default (if (eq l :default)
			      nil
			      (ecm-description-attributes d :default)))
	     (newa (typecase a
			   (list a)
			   (t (list a)))))
	 (append newa
		 (rest (assoc (first newa) default)))))))

(defun attribute-name (attribute)
  (first attribute))

(defun attribute-value (attribute)
  (getf (rest attribute) :value))

(defun (setf attribute-value) (value attribute)
  (setf (getf (rest attribute) :value) value))

(defun attribute-label (attribute)
  (let ((l (getf (rest attribute) :label)))
    (if (eq l t)
	(string-capitalize 
	 (substitute 
	  #\Space #\- 
	  (substitute 
	   #\Space #\_ 
	   (symbol-name (first attribute)))))
	l)))

(defun attribute-layers (attribute)
  (or (getf (rest attribute) :layers)
	:inline))

(defun attribute-active (attribute)
  (getf (rest attribute) :active t))

(defun attribute-type (attribute)
  (getf (rest attribute) :type))

(defun attribute-required (attribute)
  (getf (rest attribute) :required))

(defgeneric object-attribute-value (object attribute-name &rest args)
  (:method (object attribute-name &rest args)
    (declare (ignore args))
    (slot-value object attribute-name))
  (:method (object (attribute-name (eql :value)) &rest args)
	   (getf args :value))
  (:method :around (object attribute-name &rest args &key (active nil active-provided))
    (declare (ignore args active))
     (handler-case (call-next-method)
       (unbound-slot () (if active-provided 
			    nil
			    "None"))
       (error (c) (format nil "~A Error: ~A" attribute-name c)))))

(defmethod (setf object-attribute-value) (value object attribute-name &rest args)
  (declare (ignorable args))
  (setf (slot-value object (etypecase attribute-name
			     (symbol attribute-name)
			     (string (intern (string-upcase attribute-name) 
					     :maxclaims/ecm-description)))) 
	(or value nil)))


(defgeneric ecm-attributes (object layers &key  &allow-other-keys)
  (:method (object layers &key (attributes NIL) &allow-other-keys)
    (flet ((ensure-list (v) (typecase v (list v) (t (list v)))))
      (setf attributes (mapcar #'ensure-list attributes)))
    (let* ((attr (ecm-description-attributes 
		 (class-name (class-of object)) layers))
	   (final-attributes 
	    (loop for a in attr
	       :when (or (not attributes)
			 (find (first a) attributes :key #'first
			       :test #'string-equal))
	       :collect (list* (first a) 
			      :value 
			      (apply #'object-attribute-value object a)
			      (rest a)))))
      (if attributes 
	   (sort final-attributes #'< :key (lambda (a) (or (position a attributes) -1)))
	  final-attributes))))



(defmethod object-attribute-value (o (a (eql 'identity)) &rest args)
  (declare (ignore args))
  (identity o))

(defmethod object-attribute-value (o (a (eql 'tab)) &rest args)
  (getf args :tab))

(defmethod object-attribute-value (o (a (eql 'tab2)) &rest args)
  (getf args :tab))

(defmethod object-attribute-value (o (a (eql :inline-block)) &rest args)
  "inline blokc")




	

  
