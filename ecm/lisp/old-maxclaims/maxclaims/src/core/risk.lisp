(in-package #:maxclaims)

(defgeneric riskp (risk)
  (:method (risk)
    nil)
  (:method ((risk risk))
    risk))

(defun risk-exists-p (risk)
  (and (riskp risk)
       (primary-key-boundp risk)))

(defun risk-claim-numbers (risk)
  (mapcar #'cadr 
	  (select 'claim-id 
		  :from 'claim 
		  :where `(:= risk-id 
			      ,(risk.risk-id risk)))))

(defun risk-types ()
  (select-objects 'risk-type))

(defmethod risk-risk-detail.risk-detail-type ((ccd risk-risk-detail))
  (risk-detail.risk-detail-type (risk-risk-detail.risk-detail ccd)))

(defmethod make-object-detail ((object risk))
  (make-instance 'risk-risk-detail :risk object))