(defpackage :maxclaims/ecm-description/risk (:use))
(in-package :maxclaims/ecm-description)

(defmethod object-attribute-value ((risk risk) 
				   (a (eql 'risk-type))
				   &key &allow-other-keys)
  (let ((type (object-attribute-value risk 'type)))
    (or type "None Chosen")))

(define-descriptions risk  
  (:default 
      (view-a-btn
       :label (:create risk
		       policy-id)
       :activate (view-link))
      (risk-type :label t)
    (contract :label t)
    (policy :label t)
    (claims))
  (:inline (type :label t)
	   (risk-type :label "Type")
	   (contract :label t)
	   (policy :label t)
	   (claims)
	   (details :label nil
		    :attributes
		    ((risk-detail 
		      :label nil)
		     detail)))
  (:edit 
   (type 
    :type risk-type
    :label t
    :select-objects risk-type
    :required t)
   (policy 
    :required t
    :label t 
    :active t
    :type policy
    :select-objects (:search maxclaims::string-search-policy))
   (contract 
    :required t
    :label t
    :active t
    :type contract
    :select-objects (:search maxclaims::string-search-contract)))

  (:heading (risk-type :label "Risk: Type"
		       :activate (h1)))

  (:create-heading (:value 
		    :label "Create New" 
		    :value "Risk"
		    :activate (h3)))
  (:create 
   (type
    :type risk-type
    :label t
    :select-objects risk-type
    :required t)
   (policy 
    :required t
    :label t 
    :active t
    :type policy
    :select-objects (:search maxclaims::string-search-policy))
   (contract 
    :required t
    :label t
    :active t
    :type contract
    :select-objects (:search maxclaims::string-search-contract)))
  (:view 
   (policy :label t 
	   :activate ((link-to-viewer)))
   (contract :label t :attributes (contract-number
				   effective-date
				   expiry-date)
	     :activate ((link-to-viewer)))
   (details
    :layers :inline
    :attributes ((risk-detail :label nil)
		 (detail :label nil))))
  (:view-tab
   (tab 
    :label "Claims" 
    :tab :claims)
   (tab 
    :label "Details"
    :tab :details))
  (:claims (claims 
	    :as-table (:create claim risk-id)
	    :layers :default
	    :attributes ((view-a-btn
			  :label (:create claim
					  risk-id)
			  :activate (view-link))
			 (claim-id 
			  :label
			  "claim #")
			 (adjuster
			  :activate (link-to-viewer)))))
  (:details (details
	     :as-table (:create risk-risk-detail risk-id)
	     :layers :default
	     :attributes ((view-a-btn
			   :label (:create risk-risk-detail risk-id)
			   :activate (view-link))
			  (risk-id)
			  (risk-detail))))
  (:policy-view-tab
   (view-a-btn
    :label (:create risk
		    policy-id)
    :activate (view-link))
   (risk-type :label t)
   (claims 
    :label t
    :layers :default
    :activate (link-to-viewer)
    :attributes ((claim-id :activate nil) date-of-loss status))
   (contract 
    :label t
    :activate (link-to-viewer)))

  (:claim-tab
   (view-a-btn
    :label (:create risk
		    policy-id)
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

  (:contract-view-tab
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
    :attributes (claim-id date-of-loss status))))


(define-descriptions risk-risk-detail        
  (:inline risk-id 
	   (risk-detail :label nil)
	   (detail :active :when 
		   :label t))
  (:heading (risk :label "Risk Detail for"
		  :activate (h2)
		  :attributes (type risk-id)))
  
  (:view (risk :label "Risk"
	       :attributes (policy contract)
	       :activate (link-to-viewer))
	 (risk-detail)
	 detail)
  (:view-tab 
   (tab 
    :label "detail" 
    :tab :detail))
  (:detail 
   (detail 
    :as-table (:create risk-risk-detail-detail 
		       risk-risk-detail-id)
    :layers :default
    :attributes ((view-a-btn
		  :label (:create  risk-risk-detail-detail 
				   risk-risk-detail-id)
		  :activate (view-link))
		 (detail
		  :label t))))
  (:create-heading 
   (:value 
    :label "Create New" 
    :value "Risk detail"
    :activate (h3))
   (risk-id))
  (:create (risk-detail
	    :type risk-detail
	    :label t
	    :select-objects (:object possible-risk-details risk)
	    :required t)
	   (risk-id))
  (:default 
      (risk-detail :label t :activate ((p :class "lead")))
      (view-a-btn
       :label (:create risk-risk-detail risk-id)
       :activate (view-link))
    detail
    risk-id))


(defun possible-risk-details (risk)
  (maxclaims::query-objects 
    'risk-detail 
    (lambda (&rest _)
      (declare (ignorable _))
      `(:select 
       risk-detail.* :from risk-detail
       risk-detail-type
       
       :where (:and (:= risk-detail-type.risk_detail_type_id
			risk-detail.risk_detail_type_id)
		    (:= risk-type-name ,(maxclaims::risk.risk-type-name risk)))))))

(define-descriptions risk-risk-detail-detail        
  (:inline detail)
  (:heading (risk-risk-detail :label "Risk Detail for"
			      :activate (h2)
			      ))
  
  (:view detail)
  (:view-tab )
  (:create-heading 
   (:value 
    :label "Create New" 
    :value "Risk detail"
    :activate (h3))
   (risk-risk-detail)
   risk-risk-detail-id)
  (:create (detail
	    :label t
	    :value ""
	    :required t)
	   
	   ( risk-risk-detail-id
	     :edit nil))

  (:default 
      risk-risk-detail-id
      detail))

(define-descriptions risk-detail        
  (:inline description (code :label t
			     :active :when))
  (:default code description))

(define-descriptions risk-type
  (:inline type-name)
  (:default type-name))

	

  
