(defpackage :maxclaims/ecm-description/claim/detail (:use))
(in-package :maxclaims/ecm-description)

(define-descriptions claim-status-detail
  (:create-heading (:value 
		    :label "Create New" 
		    :value "Claim-Status Detail"
		    :activate ((h3 :style "display:inline-block;"))))
  (:heading (key
	     :label "Claim-Status Detail"
	     :activate ((h3 :style "display:inline-block;"))))
  (:inline (key :label nil
		:activate ((<:span :class "muted")))
	   (value :label nil))
  (:view (key :label "Type")
	 (value :label "Value"))
  (:view-tab )
  (:default (key :label "Type")
      (value :label "Value")
    (view-a-btn
     :label (:create claim-status-detail
		     claim-id)
     :activate ((view-link :edit t))))  
  (:create (key :label "Type"
		:type claim-status-detail-key
		:select-objects claim-status-detail-key
		:required t
		)
	   (value :label "Value" 
		  :value "" 
		  :active t
		  :required t)
	   (claim-id :label t :edit nil))
  (:edit (key :label "Type"
		:type claim-status-detail-key
		:select-objects claim-status-detail-key
		:required t
		)
	   (value :label "Value" 
		  :active t
		  :required t)
	   (claim-id :label t :edit nil)))

(define-descriptions claim-status-detail-key
  (:create-heading "text")
  (:inline text)
  (:default text))

(define-descriptions driver-detail
  (:create-heading (:value 
		    :label "Create New" 
		    :value "Driver Detail"
		    :activate ((h3 :style "display:inline-block;"))))
  (:heading (key
	     :label "Driver Detail"
	     :activate ((h3 :style "display:inline-block;"))))
  (:inline (key :label nil
		:activate ((<:span :class "muted")))
	   (value :label nil))
  (:view (key :label "Type")
	 (value :label "Value"))
  (:view-tab )
  (:default (key :label "Type")
      (value :label "Value")
    (view-a-btn
     :label (:create driver-detail
		     claim-id)
     :activate ((view-link :edit t))))  
  (:create (key :label "Type"
		:type driver-detail-key
		:select-objects driver-detail-key
		:required t
		)
	   (value :label "Value" 
		  :value "" 
		  :active t
		  :required t)
	   (claim-id :label t :edit nil))
  (:edit (key :label "Type"
		:type driver-detail-key
		:select-objects driver-detail-key
		:required t
		)
	   (value :label "Value" 
		  :active t
		  :required t)
	   (claim-id :label t :edit nil)))

(define-descriptions driver-detail-key
  (:create-heading "text")
  (:inline text)
  (:default text))

(define-descriptions vehicle-detail
  (:create-heading (:value 
		    :label "Create New" 
		    :value "Vehicle Detail"
		    :activate ((h3 :style "display:inline-block;"))))
  (:heading (key
	     :label "Vehicle Detail"
	     :activate ((h3 :style "display:inline-block;"))))
  (:inline (key :label nil
		:activate ((<:span :class "muted")))
	   (value :label nil))
  (:view (key :label "Type")
	 (value :label "Value"))
  (:view-tab )
  (:default (key :label "Type")
      (value :label "Value")
    (view-a-btn
     :label (:create vehicle-detail
		     claim-id)
     :activate ((view-link :edit t))))  
  (:create (key :label "Type"
		:type vehicle-detail-key
		:select-objects vehicle-detail-key
		:required t
		)
	   (value :label "Value" 
		  :value "" 
		  :active t
		  :required t)
	   (claim-id :label t :edit nil))
  (:edit (key :label "Type"
		:type vehicle-detail-key
		:select-objects vehicle-detail-key
		:required t)
	   (value :label "Value" 
		  :active t
		  :required t)
	   (claim-id :label t :edit nil)))

(define-descriptions vehicle-detail-key
  (:create-heading "text")
  (:inline text)
  (:default text))

(define-descriptions loss-detail
  (:create-heading (:value 
		    :label "Create New" 
		    :value "Loss Detail"
		    :activate ((h3 :style "display:inline-block;"))))
  (:heading (key
	     :label "Loss Detail"
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
     :label (:create loss-detail
		     claim-id)
     :activate ((view-link :edit t))))  
  (:create (key :label "Type"
		:type loss-detail-key
		:select-objects loss-detail-key
		:required t
		)
	   (value :label "Value" 
		  :value "" 
		  :active t
		  :required t)
	   (claim-id :label t :edit nil))
  (:edit (key :label "Type"
		:type loss-detail-key
		:select-objects loss-detail-key
		:required t
		)
	   (value :label "Value" 
		  :active t
		  :required t)
	   (claim-id :label t :edit nil)))

(define-descriptions loss-detail-key
  (:create-heading "text")
  (:inline text)
  (:default text))

(defun possible-claim-details (claim)
  (maxclaims::query-objects 
   'claim-detail 
   (lambda (&rest _)
     (declare (ignorable _))
     `(:select 
       claim-detail.* :from claim-detail
       claim-detail-type
       
       :where (:and (:= claim-detail-type.claim_detail_type_id
			claim-detail.claim_detail_type_id)
		    (:= risk-type-name ,(maxclaims::risk.risk-type-name (claim.risk claim))))))))

;;; * claim-claim-detail
#.`(define-descriptions claim-claim-detail
     (:inline  
      (detail-text :label nil)
      (claim-detail :label nil))
     (:default 
	 (detail-text :label "Detail Text"
		      :active t)
	 (claim-detail :label "Detail") 
	 (view-a-btn
	  :label (:create claim-claim-detail
			  claim-id)
	  :activate (view-link))
       claim claim-id)
     (:view-tab)
     (:view
      (claim-detail :label "Details"
		    :active :when)
      (detail-text :label "Detail Text"
		   :active :when)
	 
	
      (claim :activate (link-to-viewer)))
     ,@(loop for n in '(:create-heading :heading)
	  :collect `(,n (claim-id :label "Claim Detail for "
				  :activate ((h2)))))
     ,@(mapcar 
	(lambda (n)
	  `(,n 
	    (detail-text :label "Detail Text"
			 :active t)
	    (claim-detail
	     :type claim-detail
	     :label t
	     :select-objects (:object possible-claim-details claim)
	     :required t)
	       
	    (claim-id :edit nil
		      :label "Claim #")))
	'(:create :edit)))

(define-descriptions claim-detail
  (:inline maxclaims::description (maxclaims::code :label t))
  (:default maxclaims::description (maxclaims::code :label t)))
 



	

      
  
