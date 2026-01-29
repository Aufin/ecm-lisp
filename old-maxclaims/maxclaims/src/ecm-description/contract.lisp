(defpackage :maxclaims/ecm-description/contract (:use))
(in-package :maxclaims/ecm-description)

(symbol-macrolet 
    ((|contract edit create| 
      '((contract-number :label t
	 :active t
	 :required t)
	(effective-date 
	 :label t
	 :active t
	 :type simple-date:date
	 :required t)
	(expiry-date 
	 :label t
	 :active t
	 :type simple-date:date
	 :required t)
	(agency :label t
	 :active t
	 :select-objects (:search maxclaims::string-search-person
			  :existing)
	 :type person)
	(syndicate :label t
	 :select-objects (:search maxclaims::string-search-person
			  :existing)
	 :type person
	 :active t)
	(london-broker :label t
	 :select-objects (:search maxclaims::string-search-person
			  :existing)
	 :type person
				   :active t)
		(loss-fund :label "Loss Fund Limit"
				   :active t)
	)))
  
  (macrolet 
      ((def ()
	     
	 `(define-descriptions contract 
	    (:edit ,@|contract edit create| )
	    (:create ,@|contract edit create| )
	    (:create-heading (:value :label "Create"
				     :value "Contract"))
	    (:inline contract-number
		     (effective-date :label t)
		     (expiry-date :label t)
		     (agency :label t)
		     (syndicate :label t))
	    (:view 
	     (effective-date :label t)
	     (expiry-date :label t)
	     (agency :label t
		     :type person
		     :select-objects (people-as-contract-agency)
		     :activate (link-to-viewer))
	     (syndicate :label t
			:active :when
			:activate (link-to-viewer))
	     (london-broker :label t
			:active :when
			:activate (link-to-viewer))
	     
	     (contract-authority :label "Authority"				    
				 :as-table (:create contract-authority
						    contract-id)
							 :single t)
		 (loss-fund
		  :label "Loss Fund Limit",
		  :active :when)
		 (loss-fund-balance
		  :label "Loss Fund balance",
		  :active :when)
	     (view-a-btn
	      :label "Reports"
	      :activate (contract-bordereau))
	     )
  
	    (:view-tab (tab :tab :risks
			    :label "Risks"))
	    (:risks
	     (risks
	      :as-table (:create risk
				 contract-id)
	      :layers :contract-view-tab
	      :activate (link-to-viewer)
	      :offset query-offset
	     
	      :label nil))
	    (:default contract-number
		(effective-date :label t)
	      (expiry-date :label t)
	      (syndicate :label t)
	      (agency :label t)
	      (contract-authority))
	    (:heading (contract-number :label t
				       :activate (h1)))
	    (:inline-block
	     (number-of-risks
	      :label t))
	     )))
    (def))
  )

(define-description risk :contract-view-tab
   (view-a-btn
    :label (:create risk
		    contract-id)
    :activate (view-link))
   (risk-type :label t)
   (policy
    :label t
    :activate (link-to-viewer))
   (claims 
    :label t
    :layers :default
    :activate (link-to-viewer)
    :attributes (claim-id date-of-loss status)))

(defmethod object-attribute-value ((contract contract) 
				   (a (eql 'risks)) 
				   &key (limit 25)
				     (offset 0))
  (maxclaims::filter-objects 
   (maxclaims::query-objects 
    'risk
    (lambda (&rest _)
      (declare (ignorable _))
      `(:limit 
	(:order-by 
	 (:select * :from 'risk
	  :where (:= contract-id ,(maxclaims::contract.contract-id contract)))
	 (:desc risk-id))
	,limit , (typecase offset
		   (symbol (funcall offset))
		   (T offset)))))))

(defmethod object-attribute-value ((contract contract) 
				   (a (eql 'number-of-risks)) 
				   &key &allow-other-keys)
  (length 
   (maxclaims::filter-objects 
   (maxclaims::query-objects 
    'risk
    (lambda (&rest _)
      (declare (ignorable _))      
      `(:select * :from 'risk
		:where (:= contract-id ,(maxclaims::contract.contract-id contract))))))))

(defmethod object-attribute-value ((contract contract) 
								   (a (eql 'loss-fund)) 
								   &key &allow-other-keys)
  (format nil "$~,vf~%" 2 (call-next-method)))

(defmethod object-attribute-value ((contract contract) 
								   (a (eql 'loss-fund-balance)) 
								   &key &allow-other-keys)
  (format nil "$~,vf~%" 2
		  (cadar (maxclaims::select
		   `(:contract-loss-fund-balance
			 ,(maxclaims::contract.contract-id contract))))))
