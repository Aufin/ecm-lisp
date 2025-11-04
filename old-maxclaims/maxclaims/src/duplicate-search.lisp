(in-package :maxclaims)

(defgeneric search-for-duplicates (object)
  (:method (object)
    nil))

(defmethod search-for-duplicates ((policy policy))
  nil
  #+nil
  (let (id date)
    (when (primary-key-boundp policy)
      (setf id `((:not (:= 'policy-id
			  ,(policy.policy-id policy))))))
    (when (slot-boundp policy 'effective-date)
     (setf date 
	   `((:= (:extract :year 'effective-date)
		(:extract :year ,(policy.effective-date policy))))))
    (remove-duplicates* 
     (append
      (select-objects 'policy :where `(:and 
				       ,@id				
				       ,@date
				       (:= policy-number
					   ,(policy.policy-number policy))
				       (:= insured-id 
					   ,(policy.insured-id policy))))
      (select-objects 'policy :where `(:and 
				       ,@id				
				       (:= policy-number
					   ,(policy.policy-number policy))
				       (:= insured-id 
					   ,(policy.insured-id policy))))
      (select-objects 'policy :where `(:= policy-number
					  ,(policy.policy-number policy)))
      (select-objects 'policy
		      :where `(:and ,@id				
				    ,@date
				    (> (similarity 'policy-number
						   ,(policy.policy-number policy))
				       0.74))))
     nil)))