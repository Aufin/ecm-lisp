(in-package :maxclaims)

(define-description policy (description-for-policy)
  ((policy-number :validate (boundp))
   (insured :input (:type select-db-object :db-type person 
			  :size 20)
	    :attributes ((first-name :label nil :active :when) 
			 (last-name :label nil :active :when) 
			 (company-name :label nil :active :when))


	    :validate (boundp))
   (agent :input (:type select-db-object :db-type person)
	  :attributes ((first-name :label nil :active :when) 
		       (last-name :label nil :active :when) 
		       (company-name :label nil :active :when))
	  :validate (boundp))
   (company :input (:type select-db-object :db-type person)
	    :attributes ((first-name :label nil :active :when) 
			 (last-name :label nil :active :when) 
			 (company-name :label nil :active :when))
	    :label "Insurance Company")
   (sub-agent :input (:type select-db-object :db-type person)
	      :attributes ((first-name :label nil :active :when) 
			   (last-name :label nil :active :when) 
			   (company-name :label nil :active :when))
	      :label "Retail Broker")
   (branch :input (:type select-db-object :db-type person)
	   :attributes ((first-name :label nil  :active :when) 
			(last-name :label nil  :active :when) 
			(company-name :label nil  :active :when))
	   :label "Local Branch")
   (underwriter :input (:type select-db-object :db-type person)
		:attributes ((first-name :label nil  :active :when) 
			     (last-name :label nil  :active :when) 
			     (company-name :label nil  :active :when)))
   (effective-date :input (:type simple-date)
		   :value-formatter print-date
		   :active :when)
   (expiry-date  :input (:type simple-date))
   (deductible :value-formatter $-formatter
	       :input (:type currency))
   ;; fixme: was policy-risk-attribute... copying active attributes
   ;; from that here (it looks like risk-attributes existed mostly to
   ;; nconc the results of fetching risks from the disjunct tables
   ;; that no longer exist)
   (risks :attribute-class has-many-attribute
	  :attributes ((type :deactivate (editable)) policy contract))
   (claims :function 'find-policy-claims
	   :label "Claims"
	   :attribute-class list-attribute
	   :active :when
	   :item (:activate (inline link-to-viewer)))
   (active-attributes :value '(policy-number 
			      (insured :activate (link-to-viewer) :active :when)
			      effective-date expiry-date deductible 
			      (agent :activate (link-to-viewer )
				     :active :when)
			      (branch :activate (link-to-viewer)
				      :active :when)
			      (underwriter :activate (link-to-viewer)
					   :active :when)
			      (sub-agent :activate (link-to-viewer)
					 :active :when)
			      (company :activate (link-to-viewer)
				       :active :when)
			       claims))))

(defmethod find-risks ((policy policy))
  (select-objects 'risk
		  :where `(:= policy-id ,(policy.policy-id policy))))

(defun find-policy-claims (policy)
  (alexandria:mappend (lambda (risk) (ignore-errors (risk.claims risk)))
	  (policy.risks policy)))


(define-description policy (description-for-policy)
  ((active-attributes 
    :value '((policy-number :label nil) 
	     (insured :deactivate (link-to-viewer)
	              :active :when 
	              :attributes ((first-name 
				    :active :when)
				   (last-name :active :when)
				   (company-name :active :when))))))
  (:in-description inline))

(define-description policy (description-for-policy)
  ((active-attributes 
    :value '(policy-number 
	     (insured :deactivate (link-to-viewer)
	      :attributes ((first-name :active :when)
			   (last-name  :active :when)
			   (company-name :active :when))
	      :validate (boundp))
	     effective-date expiry-date deductible
	     (agent :deactivate (link-to-viewer))       
	     (branch :deactivate (link-to-viewer))
	     (underwriter :deactivate (link-to-viewer))
	     (sub-agent :deactivate (link-to-viewer))
	     (company :deactivate (link-to-viewer))
	     (risks))))
  (:in-description editable))
