(defpackage :maxclaims/ecm-description/person (:use))
(in-package :maxclaims/ecm-description)

(symbol-macrolet 
    ((|person edit create|
      '((first-name :label t 
	 :active t)
	(last-name :label t
	 :active t)
	(company-name :label t :active t)
	(email-address :active t
	 :label t)
	(address1 :active t
	 :label "Address")
	(address2 :active t
	 :label t) 
	(city :active t
	 :label t) 
	(province/state 
	 :active t
	 :label t
	 :type provice/state
	 :select-objects province/state)
	(postal-zip-code :active t
	 :label t) 
	(home-phone :active t
	 :label t)
	(work-phone :active t
	 :label t) 
	(fax-phone :active t
	 :label t)
	(cell-phone :active t
	 :label t) 
	(birth-date 
	 :active t
	 :label t
	 :layers :date	  
	 :type simple-date:date))))
  (macrolet 
      ((def ()
	 `(define-descriptions person
	    (:edit ,@|person edit create|)
	    (:create  ,@|person edit create|)
	    (:create-heading 
	     (:value :value "Person / Company"
		     :label "Create"
		     :activate (h3)))
	    (:person-name 
	     first-name last-name)
	    (:default 
		(ecm-users :label t :active :when
			   :attributes (username)
			   :activate (link-to-viewer))
		(first-name :label nil 
			    :active :when) 
		(last-name :label nil 
			   :active :when)
	      (company-name :label nil :active :when)
	      (address1 :active :when
			:label "Address")
	      (address2 :active :when) 
	      (city :active :when) 
	      (province/state :active :when)
	      (postal-zip-code :active :when) 
	      (home-phone :active :when)
	      (work-phone :active :when) 
	      (fax-phone :active :when)
	      (cell-phone :active :when) 
	      (email-address :active :when)
	      (birth-date :active :when)
	      (contracts-as-agency :active :when)
	      (contracts-as-syndicate :active :when)
	      (policies-as-insured))
	    (:claim-heading 
	     first-name 
	     last-name 
	     (company-name :label "company")
	     (province/state :label nil 
			     :active :when))
  
	    (:inline first-name last-name 
		     (company-name :label "company"
				   :active :when))
	    (:select-option first-name last-name 
			    (company-name :active :when))
	    (:heading (first-name 
		       :activate ((h1 :style "display:inline")))
		      (last-name :activate ((h1 :style "display:inline")))
		      (company-name 
		       :label "company"
		       :activate ((h1 :style "display:inline:padding-bottom:1em;"))))
	    (:view (first-name :label t :active :when) 
		   (last-name :label t :active :when)
		   (company-name :label t :active :when)
		   (address1 :active :when)
		   (address2 :label t :active :when) 
		   (city :label t :active :when) 
		   (province/state :label t :active :when)
		   (postal-zip-code :label t :active :when)
		   (postal-code :label t :active :when)
		   (home-phone :label t :active :when)
		   (work-phone :label t :active :when) 
		   (fax-phone :label t :active :when)
		   (cell-phone :label t :active :when) 
		   (email-address :label t :active :when)
		   (birth-date :label t :active :when)
		   (ecm-users :label t :active :when
			      :attributes (username)
			      :activate (link-to-viewer)))
	    (:view-tab 
	     (tab 
	      :active :when
	      :label "Open Claims as Examiner"
	      :tab open-claims-as-examiner)
	     (tab 
	      :active :when
	      :label "Claims as Examiner"
	      :tab claims-as-adjuster)
	      (tab 
	      :active :when
	      :label "Claims as Insured"
	      :tab claims-as-insured)
	    
	     (tab  
	      :label "Policies as Underwriter"
	      :active :when
	      :tab policies-as-underwriter)
	     (tab  
	      :label "Contracts as Agency"
	      :active :when
	      :tab contracts-as-agency)
	     (tab  
	      :label "Contracts as Syndicate"
	      :active :when
	      :tab contracts-as-syndicate)
	     (tab  
	      :label "Policies as Insured"
	      :active :when
	      :tab policies-as-insured)
		       
	     (tab  
	      :label "Transactions as Payee"
	      :active :when
	      :tab transactions-as-payee)
		       
	     (tab  
	      :label "ECM Users"
	      :active :when
	      :tab ecm-users)
	     (tab 
	      :active :when
	      :label "Policies as Company"
	      :tab policies-as-insurance-company)
	     (tab  
	      :active :when
	      :label "Policies as Agent"
	      :tab policies-as-agent))
	    (claims-as-insured
	     (claims-as-insured
	      :layers :person-tab
	      :attributes (view-a-btn
			   claim-id
			   date-of-loss
			   risk
			   status
			   contract
			   policy
			   (adjuster :label "Examiner"))
	      :as-table t

	      
	      :limit 25 
	      :activate (link-to-viewer)
	      :offset query-offset))
	    (ecm-users
	     (ecm-users 
	      :limit 25
	      :as-table t
	      :offset query-offset
	      :layers :person-tab))
	    (claims-as-adjuster 
	     (claims-as-adjuster
	      :as-table t
	      :layers :person-tab
	      :limit 25 
	      :activate (link-to-viewer)
	      :offset query-offset))

	    (open-claims-as-examiner 
	     (open-claims-as-examiner
	      :as-table t
	      :layers :person-tab
	      :limit 25 
	      :activate (link-to-viewer)
	      :offset query-offset))
	    (transactions-as-payee 
	     (transactions-as-payee
	      :activate (link-to-viewer)
	      :limit 25
	      :offset query-offset))
	    (policies-as-agent
	     (policies-as-agent
	      :limit 25
	      :offset query-offset
	      :activate (link-to-viewer)))
	    (contracts-as-agency
	     (contracts-as-agency
	      :limit 25
	      :offset query-offset
	      :activate (link-to-viewer)))
	    (contracts-as-syndicate
	     (contracts-as-syndicate
	      :limit 25
	      :offset query-offset
	      :activate (link-to-viewer)))
	    (policies-as-insurance-company
	     (policies-as-insurance-company
	      :limit 25
	      :activate (link-to-viewer)
	      :offset query-offset))
	    (policies-as-insured
	     (policies-as-insured
	      :limit 25
	      :offset query-offset
	      :activate (link-to-viewer)))
	    (policies-as-underwriter
	     (policies-as-underwriter
	      :limit 25
	      :offset query-offset
	      :activate (link-to-viewer)))
	    
    
	    (:inline-block 
	     (claims-as-insured
	      :limit 25
	      :offset query-offset
	      :activate (link-to-viewer)
	      )))
	 ))
    (def)))

(define-descriptions province/state 
  (:default maxclaims::short-name
      maxclaims::long-name)
  (:inline maxclaims::short-name
	   (maxclaims::long-name :label "-")))

(defmethod person-ecm-users ((person person)
			     &key (limit 25)
			       (offset 0) &allow-other-keys)
  (maxclaims::query-objects 
   'app-user
   (lambda (name cols)
     `(:limit 
       (:order-by 
	(:select 
	 ,@cols :from ,name
	 :where (:= person-id 
		    ,(maxclaims::person.person-id person)))
	app-user-id)
       ,limit , (typecase offset
		  (symbol (funcall offset))
		  (T offset))))))

(defmethod object-attribute-value ((person person) 
				   (a (eql 'ecm-users)) 
				   &rest args
				   &key &allow-other-keys)
  (maxclaims::filter-objects 
   (apply #'person-ecm-users person args)))

(defmethod object-attribute-value ((person person) 
				   (a (eql 'transactions-as-payee)) 
				   &key (limit 25)
				     (offset 0))
  (maxclaims::filter-objects 
   (maxclaims::query-objects 
    'claim-transaction
    (lambda (&rest _)
      (declare (ignorable _))
      `(:limit 
	(:order-by 
	 (:select * :from claim-transaction 
		  :where (:= payee-id ,(maxclaims::person.person-id person)))
	 (:desc claim-id))
	,limit , (typecase offset
		   (symbol (funcall offset))
		   (T offset)))))))

(defmethod object-attribute-value ((person person) 
				   (a (eql 'claims-as-adjuster)) 
				   &key (limit 25)
				     (offset 0))
  (maxclaims::filter-objects 
   (maxclaims::query-objects 
    'claim 
    (lambda (&rest _)
      (declare (ignorable _))
      `(:limit 
	(:order-by 
	 (:select * :from claim
		  :where (:and (:= adjuster-id ,(maxclaims::person.person-id person))))
	 (:desc claim-id))
	,limit , (typecase offset
		   (symbol (funcall offset))
		   (T offset)))))))

(defmethod object-attribute-value ((person person) 
				   (a (eql 'open-claims-as-examiner)) 
				   &key (limit 25)
				     (offset 0))
  (maxclaims::filter-objects 
   (maxclaims::query-objects 
    'claim 
    (lambda (&rest _)
      (declare (ignorable _))
      `(:limit 
	(:order-by 
	 (:select * :from claim
		  :where (:and (:= adjuster-id ,(maxclaims::person.person-id person))
			       (:= status "Open")))
	 (:desc claim-id))
	,limit , (typecase offset
		   (symbol (funcall offset))
		   (T offset)))))))

(defmethod object-attribute-value ((person person) 
				   (a (eql 'claims-as-insured))
				   &key (limit 25)
				     (offset 0))
  (maxclaims::filter-objects 
   (maxclaims::query-objects 
    'claim 
    (lambda (name fields)
      (setf fields 
	    (remove-duplicates (remove nil fields)))
      (let ((c.fields (mapcar (lambda (f)
				(intern (string-downcase (format nil "claim.~A" f)))) 
			      fields)))
	`(:limit 
	  (:order-by
	   (:select ,@(loop for f in fields
			   for c.f in c.fields
			   :collect `(:as ,c.f ,f)) :from  
		    (:as  (:select ,@c.fields :from claim
				   :left-join risk
				   :on (:= claim.risk-id
					   risk.risk-id)
				   :left-join policy
				   :on (:and (:= policy.policy-id
						 risk.policy-id))
				   :where (:= insured-id ,(maxclaims::person.person-id person))
				   
				   
				   )
			  ,name))
	   (:desc claim-id))
	  ,limit  ,(typecase offset
			(symbol (funcall offset))
			(T offset))))))))

(defmethod object-attribute-value ((person person) 
				   (a (eql 'policies-as-agent)) 
				   &key (limit 25)
				     (offset 0))
  (maxclaims::filter-objects 
   (maxclaims::query-objects 
    'policy
    (lambda (&rest _)
      (declare (ignorable _))
      `(:limit 
	(:order-by 
	 (:select * :from 'policy
	  :where (:= agent-id ,(maxclaims::person.person-id person)))
	 (:desc policy-id))
	,limit , (typecase offset
		   (symbol (funcall offset))
		   (T offset)))))))

(defmethod object-attribute-value ((person person) 
				   (a (eql 'policies-as-underwriter)) 
				   &key (limit 25)
				     (offset 0))
  (maxclaims::filter-objects 
   (maxclaims::query-objects 
    'policy
    (lambda (&rest _)
      (declare (ignorable _))
      `(:limit 
	(:order-by 
	 (:select * :from 'policy
	  :where (:= underwriter-id ,(maxclaims::person.person-id person)))
	 (:desc policy-id))
	,limit , (typecase offset
		   (symbol (funcall offset))
		   (T offset)))))))


(defmethod object-attribute-value 
    ((person person) 
     (a (eql 
	 'policies-as-insurance-company))
     &key 
       (limit 25)
       (offset 0))
  (maxclaims::filter-objects 
   (maxclaims::query-objects 
    'policy
    (lambda (&rest _)
      (declare (ignorable _))
      `(:limit 
	(:order-by 
	 (:select * :from 'policy
		  :where (:= company-id ,(maxclaims::person.person-id person)))
	 (:desc policy-id))
	,limit , (typecase offset
		   (symbol (funcall offset))
		   (T offset))))))) 

(defmethod object-attribute-value 
    ((person person) 
     (a (eql 
	 'contracts-as-agency))
     &key 
       (limit 25)
       (offset 0))
  (maxclaims::filter-objects 
   (maxclaims::query-objects 
    'contract
    (lambda (&rest _)
      (declare (ignorable _))
      `(:limit 
	(:order-by 
	 (:select * :from 'contract
		  :where (:= agency-id ,(maxclaims::person.person-id person)))
	 (:desc contract-id))
	,limit , (typecase offset
		   (symbol (funcall offset))
		   (T offset)))))))

(defmethod object-attribute-value 
    ((person person) 
     (a (eql 
	 'contracts-as-syndicate))
     &key 
       (limit 25)
       (offset 0))
  (maxclaims::filter-objects 
   (maxclaims::query-objects 
    'contract
    (lambda (&rest _)
      (declare (ignorable _))
      `(:limit 
	(:order-by 
	 (:select * :from 'contract
		  :where (:= syndicate-id ,(maxclaims::person.person-id person)))
	 (:desc contract-id))
	,limit , (typecase offset
		   (symbol (funcall offset))
		   (T offset)))))))

(macrolet ((define-people-policy (name policy-person-id)
	     `(defun ,name ()
		(maxclaims::with-db  
		  (maxclaims::query-objects 
		   'person (lambda (table fields) 
			     `(:order-by
				 (:select 
				  ,@(loop for f in fields 
				       :when f 
				       :collect (intern (format nil "person.~A" f)))
				  :distinct-on ,(first fields)
				  :from ,table policy 
				  :where (:= person.person-id ,',policy-person-id))
				 (:desc person.person-id)
				 (:desc policy.policy-id))))))))
  (define-people-policy people-as-policy-agent policy.agent-id)
  (define-people-policy people-as-policy-company  policy.company-id)
  (define-people-policy people-as-policy-sub-agent  policy.agency-office-id)
  (define-people-policy people-as-policy-branch  policy.branch-id)
  (define-people-policy people-as-policy-underwriter  policy.underwriter-id))

(macrolet ((define-people-contract (name contract-person-id)
	     `(defun ,name ()
		(maxclaims::with-db  
		  (maxclaims::query-objects 
		   'person (lambda (table fields) 
			     `(:order-by
				 (:select 
				  ,@(loop for f in fields 
				       :when f 
				       :collect (intern (format nil "person.~A" f)))
				  :distinct-on ,(first fields)
				  :from ,table policy 
				  :where (:= person.person-id ,',contract-person-id))
				 (:desc person.person-id)
				 (:desc contract.contract-id))))))))
  (define-people-contract people-as-contract-agency contract.agency-id))
  
	

  
