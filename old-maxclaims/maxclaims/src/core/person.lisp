(in-package :maxclaims)

(defun person-name (person)
  (if (slot-boundp person 'person-id)
      (query (:select (:person-name (person.person-id person)))
	     :single!)
      "NEW"))

(defmethod agency-contracts (person)
  (select-objects 'contract
		  :where `(:= contract.agency-id ,(person.person-id person))))

(defun active-adjusters (&optional editor)
  "Set of all active adjuster PERSON instances"
  (declare (ignore editor))
  (mapcar #'app-adjuster.person 
          (query-objects 
           'app-adjuster 
           (lambda (tbl clmns)
             `(:order-by 
               (:select ,@clmns
                :from ,tbl)
               (:desc 
                (:SELECT 
                 (count *) 
                 :from claim 
                 :where (:and (:= adjuster-id person-id)
                              (:= status "Open")))))))))

(defun claim-cause-codes (&optional editor)
  "Set of all active adjuster PERSON instances"
  (declare (ignore editor))
  (query-objects 
   'claim-cause
   (lambda (tbl clmns)
     `(:order-by 
       (:select ,@clmns
                :from ,tbl)
       (:desc 
        (:SELECT 
         (count *) 
         :from claim 
         :where (:and (:= claim-cause-type claim.cause)
                      (:= status "Open"))))))))

