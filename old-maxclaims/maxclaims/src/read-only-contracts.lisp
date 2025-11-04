(in-package :maxclaims)

;;; Read only contract support

;;; Returns T/:NULL (-> wrapper)
(defprepared %contract-read-only-p
    (:select (:contract-read-only-p '$1))
  :single!)

(defgeneric contract-writeable-p (object))

(defmethod contract-writeable-p ((contract-id integer))
  (let ((ro (%contract-read-only-p contract-id)))
    (cond ((eq ro t)  nil)
	  ((eq ro :null)  t)
	  (t (error "Invalid contract-read-only-p result")))))


;;; Contracts
(defmethod contract-writeable-p ((object contract))
  (or (not (persistentp object))
	   (contract-writeable-p (contract.contract-id object))))

(defmethod user-can-edit-p ((object contract))
  (and (contract-writeable-p object)
       (call-next-method)))

;;; Risks
(defmethod contract-writeable-p ((object risk))
  (or (not (slot-boundp object 'contract))
      (contract-writeable-p (risk.contract object))))

(defmethod user-can-edit-p ((object risk))
  (and (contract-writeable-p object)
       (call-next-method)))

;;;; risk-risk-detail
(defmethod contract-writeable-p ((object risk-risk-detail))
  ;; fixme: is this necessary (r-r-d should always have a risk)
  (or (not (slot-boundp object 'risk))
      (contract-writeable-p (risk-risk-detail.risk object))))

(defmethod user-can-edit-p ((object risk-risk-detail))
  (and (contract-writeable-p object)
       (call-next-method)))

;;;; risk-risk-detail-detail
(defmethod contract-writeable-p ((object risk-risk-detail-detail))
  ;; fixme: is this necessary (r-r-d-d should always have a r-r-d)
  (or (not (slot-boundp object 'risk-risk-detail))
      (contract-writeable-p (risk-risk-detail-detail.risk-risk-detail object))))

(defmethod user-can-edit-p ((object risk-risk-detail-detail))
  (and (contract-writeable-p object)
       (call-next-method)))

;;; Claims
(defmethod contract-writeable-p ((object claim))
  (or (not (and (slot-boundp object 'risk)
		;; fixme: workaround for malformed NIL risk records,
		;; remove when those are deal with
		(claim.risk object)))
      (contract-writeable-p (claim.risk object))))

(defmethod user-can-edit-p ((object claim))
  (and (contract-writeable-p object)
       (call-next-method)))

;;;; claim-transaction
(defmethod contract-writeable-p ((object claim-transaction))
  ;; fixme: is this necessary (transaction should always have a claim)
  (or (not (slot-boundp object 'claim))
      (contract-writeable-p (claim-transaction.claim object))))

(defmethod user-can-edit-p ((object claim-transaction))
  (and (contract-writeable-p object)
       (call-next-method)))

;;;; attachment
(defmethod contract-writeable-p ((object attachment))
  (or (not (slot-boundp object 'claim))
      (contract-writeable-p (attachment.claim object))))

(defmethod user-can-edit-p ((object attachment))
  (and (contract-writeable-p object)
       (call-next-method)))

;;;; timecard
(defmethod contract-writeable-p ((object timecard))
  (or (not (slot-boundp object 'claim))
      (contract-writeable-p (timecard.claim object))))

(defmethod user-can-edit-p ((object timecard))
  (and (contract-writeable-p object)
       (call-next-method)))

;;;; claim-claim-detail
(defmethod contract-writeable-p ((object claim-claim-detail))
  ;; fixme: is this necessary (c-c-d should always have a claim
  (or (not (slot-boundp object 'claim))
      (contract-writeable-p (claim-claim-detail.claim object))))

(defmethod user-can-edit-p ((object claim-claim-detail))
  (and (contract-writeable-p object)
       (call-next-method)))