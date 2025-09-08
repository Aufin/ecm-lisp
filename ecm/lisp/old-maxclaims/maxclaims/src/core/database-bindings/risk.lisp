(in-package :maxclaims)

(defclass risk-type ()
  ((type-name :primary-key t)
   description)
  (:metaclass described-db-access-class))

(defclass risk-detail-category ()
  ((category-name :primary-key t))
  (:metaclass described-db-access-class))

(defclass risk-detail-type ()
  ((risk-detail-type-id :primary-key t)
   (risk-type :references risk-type
	      :column risk-type-name)
   (risk-detail-category :references risk-detail-category
			 :column risk-detail-category-name)
   (details :referenced-from risk-detail
	    :reader detail-type.details)
   type-name
   (description :accessor detail-type.description))
  (:metaclass described-db-access-class))

(defclass risk-detail (relation-detail)
  ((risk-detail-id :primary-key t)
   (risk-detail-type :references risk-detail-type
		     :column risk-detail-type-id)
   code
   (description :accessor detail.description)
   detail-detail-type)
  (:metaclass described-db-access-class))

(defmethod detail.detail-detail-type-p ((detail risk-detail))
  (slot-boundp detail 'detail-detail-type))

(defclass risk (relation-with-details-mixin)
  ((risk-id :primary-key t :initarg :risk-id)
   (type :references risk-type
	 :column risk-type-name
	 :initarg :risk-type)
   (policy :references policy
	   :column policy-id
	   :initarg :policy)
   (contract :references contract
	     :column contract-id
	     :initarg :contract)
   (details :referenced-from risk-risk-detail :on risk-id
	    :accessor relation.details)
   (claims :referenced-from claim :on risk-id)
   contract-id
   policy-id
   risk-type-name)
  (:metaclass described-db-access-class))

(defclass risk-risk-detail (relation-relation-detail)
  ((risk-risk-detail-id :primary-key t)
   (risk :references risk
	 :column risk-id
	 :initarg :risk)
   (risk-detail :references risk-detail
		:column risk-detail-id
		:initarg :risk-detail)
   (detail :referenced-from risk-risk-detail-detail
	   :on risk-risk-detail-id)
   risk-id)
  (:metaclass described-db-access-class))

(defclass risk-risk-detail-detail ()
  ((risk-risk-detail-detail-id :primary-key t)
   (risk-risk-detail :references risk-risk-detail
		     :column risk-risk-detail-id
		     :initarg :risk-risk-detail)
   risk-risk-detail-id ; needed for foreign-insert
   (detail)) ; fixme: should use risk-detail.detail-detail-type and
	     ; cast to appropriate lisp type and verify that anything
	     ; setfed will cast to the correct sql type
  (:metaclass described-db-access-class))
