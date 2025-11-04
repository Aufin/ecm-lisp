(in-package :maxclaims)

(defmethod claim-claim-detail.claim-detail-type ((ccd claim-claim-detail))
  (claim-detail.claim-detail-type (claim-claim-detail.claim-detail ccd)))

(defmethod make-object-detail ((object claim))
  (make-instance 'claim-claim-detail :claim object))

(defmethod object-detail-type ((object claim))
  'claim-claim-detail)

(defmethod claim-closed-p ((claim claim))
  (string= "Closed" (claim-status.claim-status-type (claim.status claim))))

#|(defun policy-has-claim-with-date-of-loss-p (policy-id date-of-loss)
  (select '* :from 'claim :where `(:and (:= 'policy-id ,policy-id)
					(:= 'date-of-loss ,date-of-loss))))

(defun transaction-type (transaction)
  (flet ((body ()
	   (ignore-errors (code-select 17 (claimtransaction.transaction-type-code transaction)))))
    (if postmodern:*database*
	(body)
	(with-db 
	  (body)))))

(defaction insert-object-action-using-object (self (object claimtransaction))
  (call-next-method self object)
 (let ((reserve (claim-outstanding-reserve (claimtransaction.claim-id object))))
   (when (> 0 reserve)

     (if (yes-or-no-p-dialog "Illegal transaction : Reserve below 0. would you like to try again?")
	 (create-object object)
	 (delete-object object)))))

(defmethod user-can-edit-p :around ((object claimtransaction))
  (null (app-user.contracts $app-user)))

(define-symbol-macro +initial-reserve+ "1")
(define-symbol-macro +reserve-adjustment+ "2")
(define-symbol-macro +check-loss+ "3")
(define-symbol-macro +check-expense+ "4")
(define-symbol-macro +check-in-house+ "5")
(define-symbol-macro +subrogation+ "6")
(define-symbol-macro +salvage+ "7")


(defun claim-risk-type (claim)
  (risk-code->type (claim.policy-type-code claim)))

(defmethod find-risk ((claim claim))
  (ignore-errors 
    (select-only-n-objects 1 (claim-risk-type claim) 
			 :where `(:and 
				  (:= risk-number ,(claim.risk-number claim))
				  (:= policy-id ,(claim.policy-id claim))))))



(defmethod find-contract-id ((claim claim))
  (cadar  (select 'risk.contract-id :from 'claim 
		  `(:as ,(claim-risk-type claim) risk)
		  :where `(:and 
			   (:= claim.claim-id ,(claim.claim-id claim))
			   (:= risk.risk-number ,(claim.risk-number claim))
			   (:= risk.policy-id ,(claim.policy-id claim))))))

(defun claim-adjuster (clid)
	     (apply #'format nil "~A ~A ~A"
		    (or (first (query (:select 'first-name 'last-name 'company-name
					   :from 'person 'claim
					   :where (:and 
						   (:= 'claim-id clid)
						   (:= 'person-id 'adjuster-id)) )))
			(make-list 3 :initial-element ""))))

|#