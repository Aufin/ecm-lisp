(defpackage :maxclaims/ecm-description/policy (:use))
(in-package :maxclaims/ecm-description)


(symbol-macrolet ((|policy-edit/create| 
		   '((policy-number 
		      :label t
		      :required t
		      :active t)
		     (insured 
		      :label t
		      :required t
		      :active t
		      :type person
		      :select-objects 
		      (:search maxclaims::string-search-person))
		     (effective-date 
		      :label t
		      :required t
		      :type simple-date:date
		      :active t)
		     (expiry-date 
		      :label t
		      :required t
		      :type simple-date:date
		      :active t)
		     (agent :type person
		      :active t
		      :allow-null t
		      :select-objects 
		      (:search maxclaims::string-search-person :existing))
		     (underwriter
		      :allow-null t
		      :type person
		      :select-objects (:search maxclaims::string-search-person
				       :existing)
		      :active t)		      
		     (company 
		      :active t
		      :type person
		      :allow-null t
		      :select-objects (:search maxclaims::string-search-person
				       :existing))
		     (sub-agent 
		      :active t
		      :allow-null t
		      :type person
		      :select-objects (:search maxclaims::string-search-person
				       :existing))
		     (branch
		      :active t
		      :allow-null t
		      :type person
		      :select-objects (:search maxclaims::string-search-person
				       :existing)))))
  (macrolet 
      ((def()
	 `(define-descriptions policy 
	    (:edit ,@|policy-edit/create|)
	    (:create ,@|policy-edit/create|)
	    (:details 
	     (details 
	      :label t
	      :layers :default
	      :as-table (:create policy-detail policy-id)
	      :attributes ((view-a-btn
			    :label (:create policy-detail
					    policy-id)
			    :activate (view-link))
			   (key :label "Type")
			   (value :label "Value")))
	     )
	    (:default policy-number 
		(effective-date :label t)
	      (expiry-date :label t)
	      (agent 
	       :label t
	       :activate (link-to-viewer))
	      (company
	       :label "Insurance Company"
	       :activate (link-to-viewer))
	      (agent
	       :label t
	       :activate (link-to-viewer))
	      (sub-agent 
	       :label "Retail Broker"
	       :activate (link-to-viewer))
	      (branch
	       :label "Local Branch"
	       :activate (link-to-viewer))
	      (underwriter 
	       :label t
	       :activate (link-to-viewer))
	      (contract :label t)
	      (risks :attributes (type (contract 
					:label t)			     
				       (claims 
					:activate (link-to-viewer)
					:attributes (claim-id date-of-loss status)))
	  
		     :label t))
	    (:create-heading (:value 
			      :label "Create New" 
			      :value "Policy"
			      :activate ((h3 :style "display:inline-block;"))))
  
	    (:inline policy-number 
		     (insured :label t)
		     effective-date 
		     expiry-date)
	    (:heading (policy-number 
		       :label "Policy #"
		       :activate ((h1 ))))

	    (:view   
	     (insured :label t
		      :activate (link-to-viewer))
	     effective-date 
	     expiry-date
	     (agent 
	      :label t
	      :active :when
	      :activate (link-to-viewer))
	     (company
	      :active :when

	      :label "Insurance Company"
	      :activate (link-to-viewer))
	     (sub-agent 
	      :active :when

	      :label "Retail Broker"
	      :activate (link-to-viewer))
	     (branch
	      :active :when

	      :label "Local Branch"
	      :activate (link-to-viewer))
	     (underwriter
	      :active :when
	      :label t
	      :activate (link-to-viewer))
	     (details :active :when
		      :label t))
	    (:view-tab
	     (tab :label "Risks"
		  :tab :risks)
	     (tab :label "Details"
		  :tab :details))
	    (:risks
	     (risks 
	      :as-table (:create risk
				 policy-id)
	      :layers :policy-view-tab
	      :offset query-offset
	      :attributes 
	      ((type :label t) 
	       (view-a-btn
		:label (:create risk
				policy-id)
		:activate (view-link))
	       (claims 
		:label t
		:attributes (claim-id date-of-loss status))
	       (contract 
		:label t))
	      :activate (link-to-viewer)
	      :label nil))
	    
  
	    (:inline-block
	     (:value :value "")))))
    (def)))
       
       
(defmethod object-attribute-value ((policy policy) 
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
	  :where (:= policy-id ,(maxclaims::policy.policy-id policy)))
	 (:desc policy-id))
	,limit , (typecase offset
		   (symbol (funcall offset))
		   (T offset)))))))


(define-descriptions policy-detail
  (:create-heading (:value 
		    :label "Create New" 
		    :value "Policy Detail"
		    :activate ((h3 :style "display:inline-block;"))))
  (:heading (key
	     :label "Policy Detail"
	     :activate ((h3 :style "display:inline-block;"))))
  (:inline (key :label nil
		:activate ((<:span :class "muted")))
	   (value :label nil))
  (:view (key :label "type")
	 (value :label "value"))
  (:view-tab )
  (:default (key :label "type")
      (value :label "value")
    (view-a-btn
     :label (:create policy-detail
		     policy-id)
     :activate ((view-link :edit t))))
  
  (:create (key :label "Type"
		:type policy-detail-key
		:select-objects policy-detail-key
		:required t
		)
	   (value :label "Value" 
		  :value "" 
		  :active t
		  :required t)
	   (policy-id :label t :edit nil))
  (:edit (key :label "Type"
		:type policy-detail-key
		:select-objects policy-detail-key
		:required t
		)
	   (value :label "Value" 
		  :active t
		  :required t)
	   (policy-id :label t :edit nil)))

(define-descriptions policy-detail-key
  (:create-heading "text")
  (:inline text)
  (:default text))
