(in-package :maxclaims)

(defclass claim-transaction-expense-type ()
  ((claim-transaction-expense-type-name :primary-key t))
  (:metaclass described-db-access-class))

(defclass claim-transaction-heading ()
  ((claim-transaction-heading-name :primary-key t
				   :accessor claim-transaction-heading.name))
  (:metaclass described-db-access-class))

(defclass claim-transaction-type ()
  ((claim-transaction-type-id :primary-key t)
   description)
  (:metaclass described-db-access-class))

(defclass claim-transaction ()
  ((transaction-id :primary-key t) 
   (payee :column payee-id :references person)
   (transaction-type
    :references claim-transaction-type
    :column transaction-type-id)
   (transaction-heading
    :references claim-transaction-heading
    :column transaction-heading)
   (approved)
   (transaction-date :initform (today))
   (claim
    :references claim
    :column claim-id)
   claim-id 
   payee-id 
   cheque-number 
   amount
   schemes-advance-number reference-number
   (expense-type
    :references claim-transaction-expense-type)
   transaction-type-id
   (transaction-heading-name :column transaction-heading))
  (:metaclass described-db-access-class))

(defun today ()
  (multiple-value-bind (second minute hour date month year)
      (get-decoded-time)
    (declare (ignore second minute hour))
    (simple-date:encode-date year month date)))
