(defpackage :maxclaims/ecm-description/contract/authority 
  (:use))
(in-package :maxclaims/ecm-description)


(defmethod object-attribute-value ((contract contract) 
				   (a (eql 'contract-authority)) 
				   &key &allow-other-keys)
  (let ((auth (maxclaims::query-objects 
	       'contract-authority
	       (lambda (n fs)
		 `(:select 
		   ,@fs :distinct
		   :from ,n
		   :where (:= contract-id ,(contract.contract-id contract)))))))
    (loop for a in auth do 
	 (setf (maxclaims::contract-authority.authority a)
	       (format nil "$~$" (maxclaims::contract-authority.authority a))))
    auth))

(define-descriptions contract-authority
  (:default
      contract-id
      (authority
	    :label "Amount"
	    :prepend "$"
	    :prefix "$")
      (view-a-btn
       :label (:create contract-authority
		       contract-id)
       :activate ((view-link)))
      contract authority )
  (:inline (authority
	    :label "Amount"
	    :prepend "$"
	    :prefix "$") 

	   (view-a-btn
	    :label (:create contract-authority
			    contract-id)
	    :activate ((view-link :edit t))))
  (:heading 
   (contract-id :label "Authority for Contract #"
	     :activate ( 
			(h2 :style "display:inline-block"))))
  (:create-heading 
   (contract-id :label "Authority for Contract #"
	     :activate ( 
			(h2 :style "display:inline-block"))))
  (:view (contract  :activate (link-to-viewer))
		 authority )
  (:view-tab)
  (:edit 
   authority )
  (:create 
   (contract-id :edit nil
	     :label "Contract #"
	     )
   authority))
