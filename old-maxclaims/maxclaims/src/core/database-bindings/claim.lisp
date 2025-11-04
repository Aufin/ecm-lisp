(in-package :maxclaims)

(export '(loss-detail loss-detail-key
          coverage
          line-of-business
          CONTRACTS-AS-SYNDICATE
          date-claim-made
          group-leader-id))

(defclass coverage ()
  ((coverage :primary-key t))
  (:metaclass described-db-access-class))

(defclass line-of-business ()
  ((lob :primary-key t))
  (:metaclass described-db-access-class))

(defclass loss-detail ()
  ((claim-id :primary-key t)
   (key :initarg :key
	:primary-key t)
   (value :initarg :value))
  (:metaclass described-db-access-class))

(defclass loss-detail-key ()
  ((text :primary-key t))
  (:metaclass described-db-access-class))

(defclass claim-status-detail ()
  ((claim-id :primary-key t)
   (key :initarg :key
	:primary-key t)
   (value :initarg :value))
  (:metaclass described-db-access-class))

(defclass claim-status-detail-key ()
  ((text :primary-key t))
  (:metaclass described-db-access-class))

(defclass driver-detail ()
  ((claim-id :primary-key t)
   (key :initarg :key
	:primary-key t)
   (value :initarg :value))
  (:metaclass described-db-access-class))

(defclass driver-detail-key ()
  ((text :primary-key t))
  (:metaclass described-db-access-class))


(defclass vehicle-detail-key ()
  ((text :primary-key t))
  (:metaclass described-db-access-class))

(defclass vehicle-detail ()
  ((claim-id :primary-key t)
   (key :initarg :key
	:primary-key t)
   (value :initarg :value))
  (:metaclass described-db-access-class))



(defclass claim-cause ()
  ((claim-cause-type :primary-key t))
  (:metaclass described-db-access-class))

(defclass claim-status ()
  ((claim-status-type :primary-key t))
  (:metaclass described-db-access-class))

(defclass claim-detail-category ()
  ((category-name :primary-key t))
  (:metaclass described-db-access-class))

(defclass claim-detail-type ()
  ((claim-detail-type-id :primary-key t)
   (risk-type :references risk-type
	      :column risk-type-name)
   (claim-detail-category :references claim-detail-category
			  :column claim-detail-category-name)
   (details :referenced-from claim-detail
	    :reader detail-type.details)
   type-name
   (description :accessor detail-type.description))
  (:metaclass described-db-access-class))

(defclass claim-detail (relation-detail)
  ((claim-detail-id :primary-key t)
   (claim-detail-type :references claim-detail-type
		      :column claim-detail-type-id)
   code
   (description :accessor detail.description))
  (:metaclass described-db-access-class))

(defclass claim-claim-detail (relation-relation-detail)
  ((claim-claim-detail-id :primary-key t)
   (claim :references claim
	  :column claim-id
	  :initarg :claim)
   (claim-detail :references claim-detail
		 :column claim-detail-id
		 :initarg :claim-detail)
   (detail-text)
   claim-id)
  (:metaclass described-db-access-class))

(defclass claim-update (standard-db-access-object)
  ((claim-id :primary-key t)
   (claim :references claim
	  :column claim-id
	  :initarg :claim)
   (update :initarg :update))
  (:metaclass described-db-access-class))

(defclass claim (relation-with-details-mixin standard-db-access-object)
  ((claim-id :primary-key t)
   (updates :referenced-from claim-update)
   (transactions :referenced-from claim-transaction)
   (timecards :referenced-from timecard)
   (attachments :referenced-from attachment)
   (details :referenced-from claim-claim-detail
	    :accessor relation.details)
   (risk :references risk :column risk-id)
   (diary-entries :referenced-from diary-entry)
   (ibc-code :transient t
	     :initform nil)
   (loss-details :referenced-from loss-detail :on claim-id
	    :accessor claim.loss-details)
   (driver-details :referenced-from driver-detail :on claim-id
		 :accessor claim.driver-details)
   (vehicle-details :referenced-from vehicle-detail :on claim-id
		   :accessor claim.vehicle-details)
   (claim-status-details :referenced-from claim-status-detail :on claim-id
		    :accessor claim.claim-status-details)
   risk-id
   adjuster-id
   external-adjuster-id
   adjuster-office-id
   plaintiff-id
   group-leader-id
   (adjuster :references person
	     :column adjuster-id)
   (external-adjuster :references person
	     :column external-adjuster-id)
   (adjuster-office :references person
		    :column adjuster-office-id)
   (plaintiff :references person
	     :column plaintiff-id)
   date-of-loss 
   (status :references claim-status)
   (cause :references claim-cause) 
   modified 
   (open-date :initform (simple-date:universal-time-to-timestamp
			 (get-universal-time)))
   (claim-received-time
    :initform :null)
   (claim-acknowledged-time
    :initform :null)
   (peer-reviewed-date 
    :initform :null)
   close-date
   authority
   rev-date 
   recovery-subrogation-date
   
   (status-key :column status)
   (cause-key :column cause)
   deductible
   total-deductible
   defense-counsel-id
   coverage-counsel-id
   line-of-business
   coverage
   event-category
   basis-of-settlement
   subscription-percent
   denial
   date-of-denial
   reason-for-denial
   refer-to-underwriters
   open-for-recovery
   (defense-counsel
       :references person
       :column defense-counsel-id)
   (coverage-counsel
    :references person
    :column coverage-counsel-id)
   date-claim-made
   label
   )
  (:metaclass described-db-access-class))

;;; Outstanding Reserves

(defgeneric claim-info-by-heading (claim))
(defgeneric claim-info-totals (claim))

(defmethod claim-info-by-heading ((claim claim))
  (claim-info-by-heading (claim.claim-id claim)))

(defmethod claim-info-by-heading ((claim-id integer))
  (with-db
    (query
     (:select '* :from (:claim-info-by-heading claim-id)))))

(defmethod claim-info-totals ((claim claim))
   (claim-info-totals (claim.claim-id claim)))

(defmethod claim-info-totals ((claim integer))
  ;; todo: references claims_bordereau_totals
  (with-db
    (query 
     (:select 
      (:type (:claim-outstanding-reserve claim) string)
      (:type  (:claim-paid claim)
	      string)
      (:type (:claim-incurred
		  claim)
	     string))
     :row)))

(defmethod claim-deductible-totals ((claim integer))
  (with-db
    (query (:select (:select 'deductible :from 'claim 
			     :where (:= 'claim-id claim))
		    (:claim-recovered-deductible claim)
		    (:claim-deductible claim))
     :row)))

(defmethod claim-deductible-totals ((claim claim))
  (claim-deductible-totals (claim.claim-id claim)))
