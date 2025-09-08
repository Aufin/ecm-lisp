(defpackage :maxclaims/ecm-description/claim
  (:use))
(in-package :maxclaims/ecm-description)

(defmethod insert-object ((claim claim))
  (prog1  (call-next-method)
    (insert-claim-diary claim "Claim Opened")
    (let ((industry (maxclaims::claim.ibc-code claim))
	  (claim-id (maxclaims::claim.claim-id claim)))
      (when industry
	(postmodern:query 
	 (:insert-into 
	  'claim-ibc-code 
	  :set 'claim-id claim-id 
	  'industry industry))))))

(defmethod update-object ((claim claim))
  (prog1  (call-next-method)
    (let ((industry (maxclaims::claim.ibc-code claim))
	  (claim-id (maxclaims::claim.claim-id claim)))
      (when industry
	(multiple-value-bind (res count) 
	    (postmodern:query 
	     (:update
	      'claim-ibc-code 
	      :set 'industry industry
	      :where (:= 'claim-id claim-id)))
	  (unless (or res (> count 0))
	    (postmodern:query 
	     (:insert-into 
	      'claim-ibc-code 
	    :set 'claim-id claim-id 
	    'industry industry))))))))



(defmethod object-attribute-value ((claim claim) 
				   (a (eql 'ibc-code)) 
				   &key &allow-other-keys)
  (let ((ibc-code (call-next-method)))
    (or ibc-code 
	(maxclaims::query-objects 
	 'maxclaims/data-entity/ibc-code:ibc-code
	 (lambda (table fields)
	   `(:select 
	     ,@fields :from ,table
	     :where (:= industry (:select 
				  industry :from claim-ibc-code
				  :where (:= claim-id ,(maxclaims::claim.claim-id claim))))))))))




(defun industry-codes ()
  (let ((list (maxclaims::select-objects 'maxclaims/data-entity/ibc-code:ibc-code)))
    (sort list #'<
          :key (lambda (c)
                 (slot-value c 'maxclaims/data-entity/ibc-code::industry)))))

(defun risk-policy-risks (policy)
  (object-attribute-value policy 'risks))

(defmethod object-attribute-value ((claim claim) 
				   (a (eql 'deductible)) 
				   &key &allow-other-keys)
  (let ((num (call-next-method)))
    (format nil "~$" num)))

(defmethod object-attribute-value ((claim claim) 
				   (a (eql 'authority)) 
				   &key &allow-other-keys)
  (let ((num (call-next-method)))
    (format nil "~$" num)))



(define-descriptions line-of-business
  (:default maxclaims::lob)
  (:inline maxclaims::lob))

(define-descriptions coverage
  (:default coverage)
  (:inline coverage))


(defmethod object-attribute-value ((claim claim) 
				   (a (eql 'thread)) 
				   &key &allow-other-keys)
  claim)

(defmethod object-attribute-value ((claim claim) 
				   (a (eql 'diary)) 
				   &key &allow-other-keys)
  claim)

(symbol-macrolet 
    ((|claim edit create|
       '(
         (date-of-loss
	        :label t
	        :layers :date
	        :active t
	        :required t
	        :type simple-date:date)
	       (date-claim-made
	        :label t
	        :layers :date
	        :active t
	        :required t
	        :type simple-date:date)
         (cause
	        :active t
	        :label "Cause Code"
	        :type  claim-cause
	        :select-objects (claim-cause-codes))
         (adjuster :type person
	                 :select-objects (active-adjusters))
	       (line-of-business
	        :active t
	        :label t
	        :type  maxclaims:line-of-business
	        :select-objects maxclaims:line-of-business)
         (coverage
	        :active t
	        :label t
	        :type  maxclaims:coverage
	        :select-objects coverage)
         (event-category
          :active t
          :label t)
         (basis-of-settlement
          :active t
          :label t)
         (subscription-percent
          :active t
          :label "Subscription %")
         (denial
          :active t
          :label t
          :type boolean)
         (date-of-denial
	        :label t
	        :layers :date
	        :active t
	        :required t
	        :type simple-date:timestamp)
         (reason-for-denial :label t :active t)
	       (open-for-recovery
	        :active t
	        :label t :type boolean)
         (refer-to-underwriters
          :active t
          :label t
          :type boolean)
	       (claim-received-time
	        :label t
	        :active t
	        :required t
          :layers :date-time
	        :type simple-date:timestamp
          :timezone t)
         (claim-acknowledged-time
	        :label t
	        :active t
	        :required t
	        :type simple-date:timestamp
          :timezone t)
         (peer-reviewed-date
	        :label t
	        :active t
	        :type simple-date:date)
	       (external-adjuster
	        :active t
	        :label "External Adjuster"
	        :type person
	        :select-objects (:search maxclaims::string-search-person :existing))
	       (deductible :label t
	                   :prepend "$"
	                   :required t
	                   :active t)
	       (authority :label t
	                  :prepend "$"
	                  :required t
	                  :active t)
	       (plaintiff
	        :active t
	        :label t
	        :type person
	        :select-objects (:search maxclaims::string-search-person))
	       (coverage-counsel
	        :active t
	        :label t
	        :type person
	        :select-objects (:search maxclaims::string-search-person
			                     :existing))
	       (defense-counsel
	        :active t
	        :label t
	        :type person
	        :select-objects (:search maxclaims::string-search-person
			                     :existing))
	       (ibc-code
	        :active t
	        :label "Industry Code"
	        :type  maxclaims/data-entity/ibc-code:ibc-code
	        :select-objects (industry-codes))
	       (risk-id :label "Risk Id #"
	                :edit nil)
	       (open-date :label "Open Date" :edit nil)
	       #+(or)(risk :edit nil
                     :label "Risk"
                     :layer :inline)
	
	       )))
 
  (macrolet 
      ((def ()
	       `(define-descriptions claim
	          (:edit
	           (status :label t
		                 :type claim-status
		                 :select-objects claim-status)

	           (risk :type risk
		               :select-objects (:object risk-policy-risks policy))
	           ,@|claim edit create|
	           (recovery-subrogation-date :label t
                                        :active t
                                        :type simple-date:date)
	           )
	          (:create  ,@|claim edit create|)
	          (:default
		         (close-date :label t)
		         (view-a-btn
		          :label (:create claim
				                      risk-id)
		          :activate (view-link))
	           (CLAIM-ID :label "Claim #")
      
	           (status :label "Status"
		                 :active t)
	           (cause :label "Cause"
		                :active t)
	           (policy
	            :attributes ((policy-number :label nil) EFFECTIVE-DATE EXPIRY-DATE)
	            :ACTIVE :WHEN)
	           (adjuster :label "Examiner"
			                 :activate (link-to-viewer))
	           (external-adjuster :label t
				                        :activate (link-to-viewer)
				                        :active :when)
	           (RISK
	            :ATTRIBUTES (risk-type)
	            :ACTIVATE (LINK-TO-VIEWER)
	            :ACTIVE :WHEN)
	           (CONTRACT :ACTIVE :WHEN)
	           (date-of-loss :label t :layers :date)
	           (date-claim-made :label t :layers :date)
	           (plaintiff)
             (line-of-business
              :active t
              :label t
              :type  maxclaims:line-of-business
              :select-objects maxclaims:line-of-business)
             (coverage
              :active t
              :label t
              :type  maxclaims:coverage
              :select-objects coverage)
             (event-category
              :label t)
             (basis-of-settlement
              :label t)
             (subscription-percent
              :active t
              :label "Subscription %")
             (denial
              :active t
              :label t)
             (refer-to-underwriters
              :active t
              :label t)
	           (open-for-recovery
              :active t
              :label t)
	           (close-date :label t)
	           (rev-date :label "Re-Open Date")
             (recovery-subrogation-date :label t :active :when)
	           (coverage-counsel :label t :active :when)
	           (defense-counsel :label t :active :when)
	           (claim-authority)
	           (ADJUSTER-OFFICE :ACTIVATE (LINK-TO-VIEWER) :ACTIVE :WHEN)
	           (TRANSACTIONS)
	           (TIMECARDS)
	           (attachments )
	           (DIARY-ENTRIES))
 	          (:heading (CLAIM-ID :label "Claim #"
				                        :activate ((h1 :style "display:inline-block;")))
		                  (status :label "status"
			                        :active t
			                        :activate ((h2 :style "padding-left: 10px;display:inline")))
	       
		                  (INSURED
		                   :activate
		                   (LINK-TO-VIEWER
			                  (h2 ))
		                   :label "Insured"
		                   :layers :claim-heading))
	    
	          (:create-heading (:value :label "Create new"
				                             :value "Claim"
				                             :activate ((h2 :style "display:inline-block;")))
			                       (risk
			                        :attributes (risk-type
					                                 (details :label nil
						                                        :attributes
						                                        ((risk-detail
						                                          :label nil)
						                                         (detail ))))
			                        :activate ((h4 :style "display:inline-block;"))))

	          (:view (app-user-overdue-diaries
		                :label "Overdue Diary"
		                :active :when)
		               (risk :label t)
		               (policy :label t
			                     :activate (link-to-viewer))
		               (contract :label t
			                       :activate (link-to-viewer))
		               (date-of-loss :label t :layers :date)
		               (date-claim-made :label t :layers :date)
		               (cause :active :when)
		               (details
		                :label "Loss Code"
		                :layers :inline
		                :active :when)
                   (line-of-business
                    :active :when
                    :label t)
                   (coverage
                    :active :when
                    :label t)
                   (event-category
                    :active :when
                    :label t)
                   (basis-of-settlement
                    :active :when
                    :label t)
                   (subscription-percent
                    :active :when
                    :label "Subscription %")
                   (denial 
                    :active :when
                    :label t)
		               (open-for-recovery
		                :active :when
		                :label t)
                   (refer-to-underwriters 
                    :active :when
                    :label t)
		               (close-date :active :when)
                   (rev-date :active :when)
                   (recovery-subrogation-date :label t)
		               (updates :label "Update"
			                      :layers :inline
			                      :active :when
			                      :attributes ((update :label nil)))
		               (ibc-code :label "Industry Code"
			                       :active :when)
		               (adjuster :activate (link-to-viewer))
		               (external-adjuster :activate (link-to-viewer))
		               (defense-counsel :activate (link-to-viewer))
		               (coverage-counsel :activate (link-to-viewer))
		               (claim-authority :label "Authority"
				                            :as-table t #+(or)(:create claim-authority
                                                               claim-id)
				                            :single t
				                            :active :when))
	          (:view-tab (tab
			                  :label "Deductible, Reserve and Transactions"
			                  :tab
			                  :deductible-and-reserve-and-transactions)
		                   (tab
			                  :label "Thread"
			                  :tab
			                  :thread)
		                   (tab
			                  :label "Diary"
			                  :tab
			                  :diary)
		                   (tab
			                  :label "Timecards"
			                  :tab :timecards
			                  :layers :claim-tab)
		                   (tab
			                  :label "Attachments"
			                  :tab :attachments)
		                   (tab
			                  :label "Diary Entries"
			                  :tab :diary-entries)
		                   (tab
			                  :label "Loss Details"
			                  :tab
			                  :details)
		                   (tab
			                  :label "Claim Status"
			                  :tab
			                  :status))
	          (:thread (thread :layers :thread :value :thread))
	          (:diary (diary :layers :diary :value :diary))
	          (:claim-view-tab
	           (tab
	            :label "Transactions"
	            :tab
	            :transactions)
	           (tab
	            :label "Timecards"
	            :tab :timecards
	            :layers :claim-tab)
	           (tab
	            :label "Attachments"
	            :tab :attachments)
	           (tab
	            :label "Diary Entries"
	            :tab :diary-entries)
	           (tab
	            :label "Loss Details"
	            :tab
	            :details)
	           (tab
	            :label "Claim Status"
	            :tab
	            :status))

	          (:details
	           (details
	            :label "Details"
	            :layers :default
	            :as-table (:create claim-claim-detail claim-id)
	            :attributes ((detail-text :label "Detail Text"
					                              :active :when)
			                     (claim-detail :label "Loss Code"
					                               :active :when)
			   
;;;; (claim-detail :label "Details")
			   
			                     (view-a-btn
			                      :label (:create claim-claim-detail
					                                  claim-id)
			                      :activate (view-link))))
	           (loss-details
	            :label "Loss Details"
	            :layers :default
	            :as-table (:create loss-detail claim-id))
	           (driver-details
	            :label "Driver Details"
	            :layers :default
	            :as-table (:create driver-detail claim-id))
	           (vehicle-details
	            :label "Vehicle Details"
	            :layers :default
	            :as-table (:create vehicle-detail claim-id))
	           (updates
	            :label "Update"
	            :layers :default
	            :as-table (:create claim-update claim-id)
	            :attributes (
			                     update
			                     (view-a-btn
			                      :label (:create claim-update
					                                  claim-id)
			                      :activate (view-link)))))
	          (:status
	           (status)
	           (updates
	            :label "Update (Claim Status Notes)"
	            :layers :default
	            :as-table (:create claim-update claim-id)
	            :attributes (
			                     update
			                     (view-a-btn
			                      :label (:create claim-update
					                                  claim-id)
			                      :activate (view-link))))

	           (claim-status-details
	            :label "Status Details"
	            :layers :default
	            :as-table (:create claim-status-detail claim-id)))
	      
	          (:diary-entries
	           (diary-entries
	            :as-table (:create diary-entry
				                         claim-id)
	            :layers :claim))
	          (:attachments
	           (attachments
	            :as-table (:create attachment
				                         claim-id)))
	          (:timecards
	           (timecards :as-table (:create timecard claim-id)
			                  :layers :claim-tab)
	           (timecard-interim
	            :layers :claim-tab
	            :as-table (:create timecard-interim claim-id)
	            :label t))
	          (:transactions
	           (transactions
	            :as-table (:create claim-transaction claim-id)
	            :label nil
	            :layers :claim
	            :attributes
	            ((view-a-btn)
	             (transaction-date :label "Date")
	             (transaction-type :label "Type")
	             (transaction-heading :label "Heading")
	             (amount-string :label "Amount")
	             (cheque-number :label "Cheque Number and Payee")
	             (payee :label t)
	             (approved))))
	          (:deductible-and-reserve-and-transactions
	           (claim-deductible-totals
	            :label "Deductible"
	            :as-table t)
	           (claim-reserve-information
	            :label "Reserve and Paid"
	            :as-table t)
	           (claim-info-total
	            :label "Total"
	            :as-table t)
	           (:value
	            :value "Transactions"
	            :activate (h3)
	            :label nil)
	           (transactions
	            :as-table (:create claim-transaction claim-id)
	            :label nil
	            :layers :claim
	            :attributes
	            ((view-a-btn)
	             (transaction-date :label "Date")
	             (transaction-type :label "Type")
	             (transaction-heading :label "Heading")
	             (amount-string :label "Amount")
	             (cheque-number :label "Cheque Number and Payee")
	             (payee :label t)
	       
	             (approved))))
	          (:inline-block risk
			                     claim-id
			                     (date-of-loss :label t
					                               :layers :date)
			                     (status :label "Status")
			                     (policy :label t))
	          (:inline
	           #+(or)(view-a-btn
		                :label (:create claim-transaction	claim-id)
		                :activate (view-link))
	           (CLAIM-ID)
	           (INSURED :label t)
	           (plaintiff :label "Claimant")
	           (date-of-loss :label t
			                     :layers :date)
	           (status :label "Status")
	           (policy :label t)
	           (contract :label t)
	           (date-of-loss :label t
			                     :layers :date)
	           (adjuster :label t))
	    
	          (:person-tab
	           (view-a-btn
	            :label nil
	            :activate (view-link))
	           (CLAIM-ID :label "Claim #")
	           (date-of-loss :label t
			                     :layers :date)
	           (INSURED :label t
		                  :activate (link-to-viewer))
	           (risk :label t
		               :activate (link-to-viewer))
	           (status)
	           (contract
	            :label t
	            :activate (link-to-viewer))
	           (policy
	            :label t
	            :activate (link-to-viewer))
	           (adjuster :label "Examiner")
	           ))))
    (def)))

(defmethod object-attribute-value ((claim claim)
				   (a (eql 'app-user-overdue-diaries))
				   &key (limit 50)
				     (offset 0))
  (maxclaims::query-objects
   'diary-entry
   (lambda (n fs)
     (let ((d.names
	    (loop :for f
	       :in (remove nil fs)
	       :collect `(:as ,(intern (format nil "D.~A" f)) ,f))))
       `(:limit
	 (:order-by
	  (:select
	   ,@fs :distinct
	   :from  (:as
		   (:select
		    ,@(list* '(:as  (:max dd.defer-date) defer-date)
			      d.names)
		     :from (:as ,n d)
		     :left-join (:as defer-diary-entry dd)
		     :on (:= dd.diary-entry-id
			     d.diary-entry-id)
		     :where
		     (:and (:= d.claim-id ,(claim.claim-id claim))
			   (:not d.processed)
			   (:> (:now) d.action-date)
			   (:= app-user-id ,(app-user.app-user-id maxclaims::$app-user)))
		     :group-by ,@(mapcar #'second d.names))

		   overdue)
	   :where  (:or (:is-null defer-date)
		       (:> (:now) defer-date)))


	  action_date)
	 ,limit , (typecase offset
		    (symbol (funcall offset))
		    (T offset)))))))

(defmethod object-attribute-value ((claim claim)
				   (a (eql 'transactions))
				   &key (limit 1000)
				     (offset 0))
  (maxclaims::query-objects
    'claim-transaction
    (lambda (&rest _)
      (declare (ignorable _))
      `(:limit
	(:order-by
	 (:select
	  * :from claim-transaction
	  :where
	  (:and (:= claim-id ,(maxclaims::claim.claim-id claim))))
	 'transaction-date)
	,limit , (typecase offset
		   (symbol (funcall offset))
		   (T offset))))))


(defmethod object-attribute-value ((claim claim)
				   (a (eql 'transactions-with-number-has-payee))
				   &key (limit 1000)
				     (offset 0))
  (let ((ts (object-attribute-value claim 'transactions :limit limit :offset offset)))
    (mapc (lambda (tr)
	      (setf (maxclaims::claim-transaction.cheque-number tr)
		    (remove ""
			    (remove nil (list (maxclaims::claim-transaction.cheque-number tr)
					      (maxclaims::claim-transaction.payee tr)))
			    :test #'equalp)))
	    ts)
    ts))

(defmethod object-attribute-value ((claim claim)
				   (a (eql 'recovered-deductibles))
				   &key (limit 1000)
				     (offset 0))
  (maxclaims::query-objects
    'claim-transaction
    (lambda (&rest _)
      (declare (ignorable _))
      `(:limit
	(:order-by
	 (:select
	  * :from claim-transaction
	  :where
	  (:and (:= claim-id ,(maxclaims::claim.claim-id claim))
		(:in transaction_type_id
		      (:select claim_transaction_type_id
			       :from claim-transaction-type
			       :where (:= description
					  "Recovered Deductible")))))
	 'transaction-date)
	,limit , (typecase offset
		   (symbol (funcall offset))
		   (T offset))))))

(defmethod object-attribute-value
    ((claim claim)
     (a (eql 'timecards))
     &key (limit 1000)
       (offset 0))
  (maxclaims/ecm-description/timecard:select-timecards
   :where `(:= timecard.claim-id ,(maxclaims::claim.claim-id claim))
   :interim t
   :limit limit
   :offset offset))

(defmethod object-attribute-value
    ((claim claim)
     (a (eql 'timecard-interim))
     &key (limit 1000)
       (offset 0))
     (maxclaims::query-objects
    'timecard-interim
    (lambda (&rest _)
      `(:limit
	(:order-by
	 (:union (:select
		  ,@(cadr _)
		  :from 'timecard-interim
		  :where (:= claim-id ,(maxclaims::claim.claim-id claim))))
	 date)
	,limit , (typecase offset
		   (symbol (funcall offset))
		   (T offset))))))

(defmethod object-attribute-value ((claim claim)
				   (a (eql 'attachments))
				   &key (limit 1000)
				     (offset 0))
  (maxclaims::filter-objects
   (maxclaims::query-objects
    'attachment
    (lambda (&rest _)
      (declare (ignorable _))
      `(:limit
	(:order-by
	 (:select * :from  'attachment
		  :where (:= claim-id ,(maxclaims::claim.claim-id claim)))
	 date)
	,limit , (typecase offset
		   (symbol (funcall offset))
		   (T offset)))))))

(defmethod object-attribute-value ((claim claim) (a (eql 'insured)) &rest args)
  (declare (ignore args))
  (policy.insured (risk.policy (claim.risk claim))))

(defmethod object-attribute-value ((claim claim) (a (eql 'policy)) &rest args)
  (declare (ignore args))
  (risk.policy (claim.risk claim)))

(defmethod object-attribute-value ((claim claim) (a (eql 'contract)) &rest args)
  (declare (ignore args))
  (risk.contract (claim.risk claim)))


(define-descriptions attachment

  (:heading
     (file-name
      :activate ((h2 :style "display:inline-block")))
   (claim
    :label "#"
    :attibutes (claim-id)
    :activate ((h4 :style "display:inline-block")
	       link-to-viewer)))
  (:default)
  (:create-heading
   (:value :value "Create Attachment"
	   :activate (h1)))
  (:create
   (file :input (:type "file")
	 :label "File to Attach")
   (claim-id :label t
	     :edit nil
	     :parse parse-integer)



   (file-description
    :label t
    :textarea (:rows 7 :cols 30)
	       :active t)
   (maxclaims::date :label t
		    :type simple-date:date
		    :edit nil)
   (file-name :label t
	      :edit nil
	      :active t)
   (sha1-digest :edit nil
		:label t
		:active :when)
   (file-type :edit nil
	      :label t
	      :active :when))

  (:view
   (claim :label t)
   (file-name :label t)
   (file-description :label t)
   (maxclaims::date :label t)
   (file :label "Download"
	 :activate (download-attachment)))
  (:view-tab)
   (:inline
    (view-a-btn
     :label (:create attachment
		     claim-id)
     :activate (view-link))
    (file-name :label t)
    (file-description :label t)
    (maxclaims::date :label t)
    (file :label "Download"
	  :activate (download-attachment))))

(defclass attachment-download ()
  ((attachment :initarg attachment)))

(defmethod object-attribute-value ((att attachment) (a (eql 'file))
				   &rest args)
  (declare (ignore args))
  (make-instance 'attachment-download 'attachment att))

(define-descriptions attachment-download
  (:default)
  (:inline identity))

(defun string-search-person/exact (term)
  (string-search-person term :exact t))

(symbol-macrolet
    ((transaction-edit/create
      '((claim-id :label t
	 :edit nil
	 :parse parse-integer)
	(amount :label "Amount"
	 :prepend "$"
	 :active t)
	(transaction-date
	 :label "Date"
	 :type simple-date:date)
	(transaction-type
	 :label "Type"
	 :type claim-transaction-type
	 :select-objects claim-transaction-type
	 :parse parse-integer)
	(transaction-heading
	 :label "Heading"
	 :type claim-transaction-heading
	 :select-objects claim-transaction-heading)
	(cheque-number :label t
	 :active t)
	(expense-type
	 :label t
	 :active t
	 :allow-null t)
	(payee :type person
	 :label t
	 :allow-null? "Some Transactions have no Payee. Marking it NULL says \"There is no payee for this one\"."
	 :select-objects (:search string-search-person/exact))
	(reference-number :label t
	 :active t)
	(schemes-advance-number :label t :active t)
	(approved :label "Approved?" :type boolean :active t))))
  (macrolet
      ((def ()
	  `(define-descriptions claim-transaction
	     (:create ,@transaction-edit/create)
	     (:edit ,@transaction-edit/create)
	     (:default
		 (claim )
		 (view-a-btn
		  :label (:create claim-transaction
				  claim-id)
		  :activate (view-link))
	       (transaction-date :label "Date")
	       (transaction-type :label "Type")
	       (transaction-heading :label "Heading")
	       (cheque-number :label t)
	       (payee :label t)
	       (amount-string :label "Amount")
	       (amount :label t)
	       (expense-type
		:label t
		:type claim-transaction-expense-type
		:select-objects claim-transaction-expense-type)
	       (approved :label "Approved?"
		:active t))
	     (:claim (view-a-btn
		  :label (:create claim-transaction
				  claim-id)
		  :activate (view-link))
	       (transaction-date :label "Date")
	       (transaction-type :label "Type")
	       (transaction-heading :label "Heading")
	       (amount-string :label "Amount" :prepend "$")
	       (cheque-number :label t :inline-edit t)
	       (payee :label t :inline-edit t)
	       (approved :label "Approved?"
			 :active t
			 :inline-edit t))
	     (:create-heading
	      (:value :label "create"
		      :value "Claim Transaction"
		      :activate (h1)))

	     (:heading
	      (claim
	       :activate
	       ((h3)
		link-to-viewer)
	       :attributes (claim-id insured))
	      (transaction-type
	       :label nil
	       :activate
	       ((h2 :style "padding-left: 10px;display:inline")))
	      (transaction-heading
	       :label nil
	       :activate
	       ((h2 :style "padding-left: 5px;display:inline")))
	      (amount :label "$"
		      :activate
		      ((h2 :style "padding-left: 5px;display:inline"))))
	     (:view (transaction-date :label "Date")

		    (cheque-number :label t
				   :active :when)
		    (payee :active :when
			   :label t
			   :activate (link-to-viewer))
		    (reference-number :label t
				      :active :when)
		    (schemes-advance-number :label t
					    :active :when)

		    (expense-type
		     :active :when
		     :label t)
		    (approved :label "Approved?"))
	     (:view-tab)
	     (:inline
	      (transaction-date :label "Date")
	      (transaction-type :label "Type")
	      (transaction-heading :label "Heading")
	      (cheque-number :label t)
	      (payee)
	      (amount :label t)
	      (approved :label "Approved?"
			:active t)))))
    (def)))

(defmethod object-attribute-value ((ct claim-transaction)
				   (a (eql 'amount))
				   &rest args)
  (declare (ignore args))
  (ecm/print:princ-money-to-string (maxclaims::claim-transaction.amount ct)
                                   :prefix ""))

(define-descriptions claim-transaction-expense-type
  (:default claim-transaction-expense-type-name)
  (:inline claim-transaction-expense-type-name))

(define-descriptions claim-transaction-type
  (:default )
  (:inline
   description))

(define-descriptions claim-transaction-heading
  (:default )
  (:inline
   claim-transaction-heading-name))

(defmethod object-attribute-value ((ct claim-transaction) (a (eql 'amount-string)) &rest args)
  (declare (ignore args))
  (ecm/print:princ-money-to-string (maxclaims::claim-transaction.amount ct)
                                   :prefix "$"))

(define-descriptions claim-status
  (:inline claim-status-type)
  (:default claim-status-type))



(defclass claim-deductible-totals ()
  ((deductible :initarg deductible)
   (recovered-deductible :initarg recovered-deductible)
   (outstanding-deductible :initarg outstanding-deductible)))

(define-descriptions claim-deductible-totals
  (:inline (deductible :label t)
	   (recovered-deductible :label t)
	   (outstanding-deductible :label t))
  (:default ))

(defmethod object-attribute-value ((claim claim) (a (eql 'claim-deductible-totals)) &rest args)
  (declare (ignore args))
  (destructuring-bind (deductible recovered-deductible outstanding-deductible)
      (maxclaims::claim-deductible-totals claim)
    (list (make-instance
	   'claim-deductible-totals
	   'deductible (format nil "$~$" deductible)
	   'recovered-deductible (format nil "$~$"recovered-deductible)
	   'outstanding-deductible
	   (if (string-equal
		(maxclaims::claim.status-key claim) "Closed")
	       "$0"
	       (format nil "$~$" outstanding-deductible))))))

(defclass claim-reserve-information ()
  ((heading :initarg heading)
   (outstanding-reserve :initarg outstanding-reserve)
   (total-paid :initarg total-paid)))

(define-descriptions claim-reserve-information
  (:inline (heading :label t)
	   (outstanding-reserve :label t)
	   (total-paid :label t))
  (:default ))

(defmethod object-attribute-value ((claim claim) (a (eql 'claim-reserve-information))
				   &rest args)
  (declare (ignore args))
  (loop :for (heading outstanding-reserve total-paid)
     :in (remove 0
		 (maxclaims::claim-info-by-heading claim)
		 :key (lambda (i) (+ (second i)
				     (third i))))

     :collect (make-instance
	       'claim-reserve-information
	       'heading heading
	       'outstanding-reserve (format nil "$~$" outstanding-reserve)
	       'total-paid (format nil "$~$" total-paid))))

(defclass claim-info-total ()
  ((outstanding-reserve :initarg outstanding-reserve)
   (total-paid :initarg total-paid)
   (incurred :initarg incurred)))

(define-descriptions claim-info-total
  (:inline (outstanding-reserve :label t)
	   (total-paid :label t)
	   (incurred :label t))
  (:default ))

(defmethod object-attribute-value ((claim claim) (a (eql 'claim-info-total))

				   &rest args)
  (declare (ignore args))
  (destructuring-bind (outstanding-reserve total-paid incurred)
      (maxclaims::claim-info-totals claim)

    (list (make-instance
	   'claim-info-total
	   'outstanding-reserve (format nil "$~$" outstanding-reserve)
	   'total-paid (format nil "$~$" total-paid)
	   'incurred (format nil "$~$" incurred)))))

(defmethod object-attribute-value ((claim claim) (a (eql 'incurred))

				   &rest args)
  (declare (ignore args))
  (destructuring-bind (args a incurred)
      (maxclaims::claim-info-totals claim)
    (parse-number:parse-number incurred)))
