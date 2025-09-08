(defpackage :maxclaims/ecm-description/claim/update (:use))
(in-package :maxclaims/ecm-description)

;;; * Claim Update
(define-descriptions claim-update
  (:default 
      claim-id 
      
      (update :label t)
    (view-a-btn :activate (view-link))

    
     
    (claim :label "For Claim"))
  (:inline claim-id (update :label nil))
  (:heading 
   (claim :label "Update (narrative) for "
	  :activate ((h2))))
  (:view (update :label t))
  (:create-heading 
   (claim :label "Update (narrative) for "
	  :activate ((h2))))
  
  (:create 
   (update 
    :textarea (:rows 7 :cols 30)
    :active t)
   (claim-id :edit nil
	     :label "For Claim")))
