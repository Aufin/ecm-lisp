(defpackage :maxclaims/ecm-description/claim/authority (:use))
(in-package :maxclaims/ecm-description)

;;; * claim-authority
;;; is now contract authority

(defmethod object-attribute-value ((claim claim) 
				   (a (eql 'claim-authority)) 
				   &key &allow-other-keys)
  (let* ((contract (object-attribute-value claim 'contract))
	 (contract-authority (object-attribute-value  contract 'contract-authority))
	 (incurred (object-attribute-value claim 'incurred))
	 (claim-authority (slot-value claim 'maxclaims::authority))
	 (contract-authority-number
	  (ignore-errors 
	    (parse-number:parse-number 
	     (slot-value (first contract-authority) 'maxclaims::authority)
	     :start 1)))
	 (authority 	   (or
			    claim-authority
			    contract-authority-number))
	 (indemnity (postmodern:query (:select 
				       (:claim-paid (:raw "'Indemnity'")
						    (claim.claim-id claim)
						    (:type (:now) 
							   timestamp-without-time-zone)))
				      :single))
	 (over-authority (postmodern:query
			  (:select 'over-authority
				   :from 'claim
				   :where (:= 'claim_id (claim.claim-id claim)))
			  :single)))
   (when authority
     (list (make-instance
	    'claim-authority
	    :claim-authority (unless (and (numberp claim-authority)
					  (= 0 claim-authority))
			       (format nil "~$" claim-authority))
	    :claim-authority-over
	    (if (and (numberp claim-authority)
		     (not (= 0 claim-authority))
		     (> incurred claim-authority))
		(format nil "Yes (by $~$)" (-  incurred claim-authority))
		"No")
	    :contract-authority contract-authority
	    :contract-authority-over
	    (if (and (numberp contract-authority-number)
		     (> incurred contract-authority-number))
		(format nil "Yes (by $~$)" (-  incurred contract-authority-number))
		"No")
	    :over-authority-date over-authority
	    :indemnity (format nil "$~$" indemnity))))))

(defclass claim-authority ()
  ((contract-authority :initarg :contract-authority)
   (contract-authority-over :initarg :contract-authority-over)
   (claim-authority :initarg :claim-authority)
   (claim-authority-over :initarg :claim-authority-over)
   (over-authority-date :initarg :over-authority-date)
   (indemnity :initarg :indemnity)))

(define-descriptions claim-authority
  (:default
      claim-id
      (contract-authority
       :label t
       )
    (contract-authority-over 
     :label "Over Contract Authority?"
     :active t)
    (claim-authority
     :label t
     :active :when
     )
    (claim-authority-over 
     :label "Over Claim Authority?"
     :active :when)
    (over-authority-date 
     :label "Over Authority Date"
     :active :when)
    (indemnity :active t
	       :label t))
  (:inline (contract-authority
	    :label "Contract Authority") 
	   (contract-authority-over 
	    :label "Over Authority?"
	    :active t)
	   (claim-authority
	    :label t
	    :active :when
	    )
    (claim-authority-over 
     :label "Over Claim Authority?"
     :active t)
    over-authority-date
    (indemnity :active t))
  
  (:heading )
  (:view 
   (contract-authority
    :label nil)
    (contract-authority-over 
     :label "Over Authority?"
     :active t)
      (claim-authority
       :label t
       :active :when
       )
    (claim-authority-over 
     :label "Over Claim Authority?"
     :active t)
    (indemnity :active t)
    (over-authority-date :active t))
  (:view-tab))




	

      
  
