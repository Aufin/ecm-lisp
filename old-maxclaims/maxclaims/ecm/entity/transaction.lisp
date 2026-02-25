(defpackage :ecm/entity/transaction
  (:use :cl)
  (:import-from :ecm/json
		#:getjso)
  (:export #:toggle-approved
	   #:find-transaction
	   #:update-transaction
	   #:create-transaction
	   #:transaction-types
	   #:transaction-headings
	   #:transaction-expense-types))
(in-package :ecm/entity/transaction)

(defun create-transaction
	(claim-id
	 &key
	   (date nil)
	   (date-paid nil)
	   (type nil)
	   (heading nil)
	   (expense-type :null expense-type-provided?)
	   (payee-id :null payee-id-provided?)
	   (recipient-id :null recipient-id-provided?)
	   (amount nil)
	   (limit-of-cover nil)
	   (interim-id :null)
	   (approved :false approved-provided?)
	   (cheque-number nil cheque-number-provided?)
	   (reference-number nil reference-number-provided?)
	   (schemes-advance-number nil schemes-advance-number-provided?))
  #+(or) (break "~A" (postmodern:query (:select 'claim-transaction-type-id
					     :from 'claim-transaction-type
									   :where (:= 'description type))))

  (let ((tid (ecm/database:query
			  (s-sql:sql-compile
			   `(:insert-into claim-transaction
				 :set
				 'claim-id ,claim-id
				 ,@(when date `(transaction-date ,date))
				 ,@(when date-paid `(date-paid ,date-paid))
				 ,@(when heading `(transaction-heading ,heading))
				 ,@(when amount `(amount ,amount))
				 ,@(when limit-of-cover `(limit-of-cover ,limit-of-cover))
				 ,@(when cheque-number-provided? `(cheque-number ,(or cheque-number "")))
				 ,@(when reference-number-provided? `(reference-number ,reference-number))
				 ,@(when schemes-advance-number-provided? `(schemes-advance-number ,schemes-advance-number))
				 ,@(when approved-provided? `(approved ,approved))
				 ,@(when expense-type-provided? `(expense-type ,(or expense-type :null)))
				 ,@(when payee-id-provided? `(payee-id ,(or payee-id :null)))
				 ,@(when recipient-id-provided? `(recipient-id ,(or recipient-id :null)))
				 ,@(when type `(transaction-type-id (:select 'claim-transaction-type-id
													 :from claim-transaction-type
													 :where (:= description ,type))))
				 :returning transaction-id)) :single)))
	(setf interim-id (when interim-id
					   (if (numberp interim-id)
						   interim-id
						   (ignore-errors (parse-integer interim-id)))))
	(when (numberp interim-id)
	  (ecm/database:query
	   "UPDATE timecard_interim
                SET invoice_id = $1 WHERE timecard_interim_id = $2"
	   tid interim-id))
	(ecm/database:query/return-jso
	  (s-sql:sql-compile 
	  `(:SELECT (:jsi.transaction (:select claim-transaction :from claim-transaction :WHERE (:= transaction_id ,tid))))))))

(defun update-transaction
	(transaction-id
	 &key
	   (date nil)
	   (date-paid nil)
	   (type nil)
	   (heading nil)
	   (expense-type :null expense-type-provided?)
	   (payee-id :null payee-id-provided?)
	   (recipient-id :null recipient-id-provided?)
	   (amount nil)
	   (limit-of-cover nil)
	   (approved :false approved-provided?)
	   (interim-id nil) 
	   (cheque-number nil cheque-number-provided?)
	   (reference-number nil reference-number-provided?)
	   (schemes-advance-number nil schemes-advance-number-provided?))
  (setf interim-id (when interim-id
					 (if (numberp interim-id)
						 interim-id
						 (ignore-errors (parse-integer interim-id)))))
  ;;(break "Interim id ~A ~A ~A" (numberp interim-id) interim-id transaction-id)
  
  (when (numberp interim-id)
	(postmodern:execute "UPDATE timecard_interim
        SET invoice_id = $1::int    
        WHERE timecard_interim_id = $2::int"
						transaction-id interim-id )
	)
  (ecm/database:query/return-jso
    (s-sql:sql-compile
     `(:update claim-transaction
	       :set
	       ,@(when date `(transaction-date ,date))
	       ,@(when date-paid `(date-paid ,date-paid))
	       ,@(when heading `(transaction-heading ,heading))
	       ,@(when amount `(amount ,amount))
	       ,@(when limit-of-cover `(limit-of-cover ,limit-of-cover))
	       ,@(when cheque-number-provided? `(cheque-number ,(or cheque-number "")))
	       ,@(when reference-number-provided? `(reference-number ,reference-number))
	       ,@(when schemes-advance-number-provided? `(schemes-advance-number ,schemes-advance-number))
	       ,@(when approved-provided? `(approved ,approved))
	       ,@(when expense-type-provided? `(expense-type ,(or expense-type :null)))
	       ,@(when payee-id-provided? `(payee-id ,(or payee-id :null)))
	       ,@(when recipient-id-provided? `(recipient-id ,(or recipient-id :null)))
	       ,@(when type `(transaction-type-id (:select 'claim-transaction-type-id
					     :from claim-transaction-type
					     :where (:= description ,type))))
	       :where (:= 'transaction-id ,transaction-id)
	       :returning (:jsi.transaction claim-transaction.*)))))
	     
  
(defun find-transaction (transaction-id) 
 (ecm/database:query/return-jso
    (:select (:jsi.transaction :t)
	     :from (:as 'claim-transaction :t)
	     :where (:= transaction-id 'transaction-id))))

(defun transaction-types ()
  (postmodern:query
   "SELECT description from claim_transaction_type;"
   :column))

(defun transaction-headings ()
  (postmodern:query
   "SELECT claim_transaction_heading_name from claim_transaction_heading;"
   :column))

(defun transaction-expense-types ()
  (postmodern:query
   "SELECT claim_transaction_expense_type_name from claim_transaction_expense_type;"
   :column))

(defun toggle-approved (transaction-id)
  "=> boolean"
  (postmodern:query
   (format nil "UPDATE claim_transaction 
SET approved = NOT approved 
WHERE transaction_id = ~A 
RETURNING approved" transaction-id)
   :single))
