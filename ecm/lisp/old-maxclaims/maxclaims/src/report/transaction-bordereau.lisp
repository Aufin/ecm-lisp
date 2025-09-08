(defpackage :maxclaims/report/transaction-bordereau
  (:use :cl)
  (:import-from :rofl 
		#:select)
  (:import-from :maxclaims
		  #:with-adb
		  #:risk-type.type-name)
  (:import-from :maxclaims/report/bordereau
                #:*bordereau-headings*
                #:heading-as
                #:bordereau-heading-type)
  (:export #:s-sql/select-transaction-bordereau
           #:execute-sql/create-function-transaction-bordereau-for-contract))
(in-package :maxclaims/report/transaction-bordereau)

(defparameter *transaction-bordereau-headings*
  `(("Claim Number" claim.claim_id nil nil :numeric)
    ("Cheque Number" claim_transaction.cheque_number nil nil :text)
    ("Program" "????")
    ("Status" (:claim-status claim (:+ $2 $3)) nil nil text)
    ("Date of Loss" claim.date-of-loss
                    nil nil date)
    ("Claim #" "????")
    ("Policy Number" policy.policy-number 
                     nil)
    ("Policy Effective" policy.effective-date
                        nil nil date)
    ("Policy Expiry" policy.expiry-date
                     nil nil date)
    ("Date Reported"
     (:type (:coalesce claim.claim-received-time
                       claim.open-date) 
            date)
     nil nil date)
    ("Transaction Authorization Date"
     claim-transaction.transaction_date
     nil nil date)
    ("Loss Description" 
     (:limit
      (:select 
       (:string-agg d " - ") 
       :from 
       (:as 
	(:select 
	 (:as  cd.detail_text 
	       d)
	 :from 
	 (:as claim-detail d) 
	 :left-join
	 (:as claim-claim-detail cd) 
	 :on (:= d.claim-detail-id cd.claim-detail-id)
	 :where (:= cd.claim-id 
		    claim.claim-id)) 
	d))
      1))
    ("Cause of Loss"
     (:limit
      (:select (:\|\| cause_code " - " claim_cause_type)
	       :from claim_cause 
	       :where (:= claim.cause claim_cause_type))
      1))
    ("Kind of Loss Description/Code"
     (:limit (:select 
	      (:\|\| d.code " - " d.description)
	      :from 
	      (:as claim-detail d) 
	      :left-join
	      (:as claim-claim-detail cd) 
	      :on (:= d.claim-detail-id cd.claim-detail-id)
	      :where (:= cd.claim-id 
			 claim.claim-id)) 
      1))
    ("Event Category" claim.event-category)
    ("Basis of Settlement" claim.basis-of-settlement)
    ("LOB (Line of Business)" claim.line-of-business)
    ("Coverage" claim.coverage)
    ("Transaction Type" claim-transaction-type.description)
    ("Heading" claim-transaction.transaction-heading)
    ("Payment Type" claim-transaction.expense-type)
    ("Transaction Amount" claim-transaction.amount nil nil money)
    ("Pay to (payee)" (person_name claim-transaction.payee-id))
    ("Total Current Loss Reserve"
     (:- (claim-outstanding-reserve 
          claim.claim-id claim-transaction.transaction-date)
         (:+ (claim-outstanding-reserve 
              "Expert Expense" 
              claim.claim-id claim-transaction.transaction-date)
             (claim-outstanding-reserve 
              "Legal" 
              claim.claim-id claim-transaction.transaction-date)
             (claim-outstanding-reserve 
              "TPA" claim.claim-id claim-transaction.transaction-date)
             (claim-outstanding-reserve 
              "Adjusting" claim.claim-id claim-transaction.transaction-date)))
     :total NIL money)
    ("Total Current Expense Reserve"
     (:+ (claim-outstanding-reserve "Expert Expense" claim.claim-id claim-transaction.transaction-date)
      (claim-outstanding-reserve "Legal" claim.claim-id claim-transaction.transaction-date)
      ;;(claim-outstanding-reserve "TPA" claim.claim-id claim-transaction.transaction-date)
      (claim-outstanding-reserve "Adjusting" claim.claim-id claim-transaction.transaction-date))
     :total NIL money)
    ("Subscription %" claim.subscription_percent)
    ("Paid Loss" 
     (claim-loss claim.claim-id claim-transaction.transaction-date) 
     :total nil money)
    ("Paid Expense" 
     (:- (claim-cheque-expense claim.claim-id claim-transaction.transaction-date)
	 (claim-cheque-expense "TPA" claim.claim-id claim-transaction.transaction-date))
     
     :total nil money)
    ("Recovery" 
     (:+ 
      (claim-subrogation claim.claim-id claim-transaction.transaction-date)
      (claim-salvage claim.claim-id claim-transaction.transaction-date)
      #+(or)(claim-recovered-deductible claim.claim-id claim-transaction.transaction-date)
      )
      :total nil money)
    ("Incurred"
      (:+ (claim-outstanding-reserve claim.claim-id claim-transaction.transaction-date)
		 (:-  (:+ (claim-cheque-loss claim.claim-id claim-transaction.transaction-date)
			  (claim-cheque-expense claim.claim-id claim-transaction.transaction-date)
			  (claim-cash-call claim.claim-id claim-transaction.transaction-date))
		      (claim-outstanding-reserve 
		       "TPA" claim.claim-id claim-transaction.transaction-date)
		      (claim-cheque-expense "TPA" claim.claim-id claim-transaction.transaction-date)
		      (claim-salvage claim.claim-id claim-transaction.transaction-date)
		      (claim-subrogation claim.claim-id claim-transaction.transaction-date)
		      (claim-recovered-deductible claim.claim-id claim-transaction.transaction-date)
		      ))
      :total nil money)
    ("Loss Street Address" 
     (:limit
      (:select value 
      :from loss-detail
      :where (:and (:= loss-detail.claim-id
		       claim.claim-id)
		   (:= key "Loss Location")))
      1)
     nil)
    ("Loss Street Address #2" 
     (:select value 
      :from loss-detail
      :where (:and (:= loss-detail.claim-id
		       claim.claim-id)
		   (:= key "Loss Location #2"))) nil)
    ("Loss City" 
     (:limit
      (:select value 
	       :from loss-detail
	       :where (:and (:= loss-detail.claim-id
				claim.claim-id)
			    (:= key "Loss City")))
      1)
     nil)
    ("Loss Country" 
     (:limit 
      (:select value 
	       :from loss-detail
      :where (:and (:= loss-detail.claim-id
		       claim.claim-id)
		   (:= key "Loss Country")))
      1)
     nil)
    ("Loss Postal Code"
     (:limit
      (:select value 
	       :from loss-detail
	       :where (:and (:= loss-detail.claim-id
				claim.claim-id)
			    (:= key "Loss Postal Code")))
      1)
     nil)
    ,@*bordereau-headings*))
    
(defparameter *transaction-bordereau-fields* 
  '("Claim Number" "Policy Number" "Insured" 
    "Policy Effective" "Policy Expiry"
    "Status" "Date of Loss" "Date Reported" "Close Date" "Reopen Date" 
    "Transaction Authorization Date" 
    "Loss Description" "Cause of Loss"
    "Loss Street Address" "Loss Street Address #2"
    "Loss City" "Loss Province"
    "Loss Country"
    "Loss Postal Code"
    "Kind of Loss Description/Code"
 
    "Event Category"
    "Basis of Settlement"
    "LOB (Line of Business)"
    "Coverage"
    "Transaction Type"
    "Heading"
    "Payment Type"
    "Transaction Amount"
    "Subscription %"
    "Pay to (payee)"
    "Total Current Loss Reserve"
    "Total Current Expense Reserve"
    "Paid Loss"
    "Paid Expense"
    "Recovery"
    "Incurred"
    "Cheque Number"))

(defun s-sql/transaction-bordereau-headings ()
  (loop :for f :in *transaction-bordereau-fields*
     :collect 
     (let* ((foo (assoc f *transaction-bordereau-headings*
                        :test #'string=))
            (type (bordereau-heading-type foo))
            (as  (heading-as foo)))
       (unless foo (error "Cannot find ~A in headings" f)) 
       (when type 
         (setf (cadr as)
               `(:type ,(cadr as) ,type)))
       as)))

(defun s-sql/select-transaction-bordereau ()
  `(:SELECT 
    ,@(s-sql/transaction-bordereau-headings)
    :from claim_transaction
    :left-join claim_transaction_type 
    :on (:= transaction_type_id claim_transaction_type_id)
                            :left-join claim :using (claim-id)
                            :left-join risk :using (risk-id)
                            :left-join policy :using (policy-id)
                            :left-join contract :using (contract-id)))

(defun s-sql/CREATE-VIEW-transaction-bordereau ()
  `(:create-view 
    transaction-bordereau
    ,(s-sql/select-transaction-bordereau)))

(defun s-sql/create-function-transaction-bordereau-for-contract ()
  (list :raw 
        (concatenate 
         'string 
         "CREATE OR REPLACE FUNCTION transaction_bordereau (contract, date, interval)
   RETURNS SETOF transaction_bordereau AS $$"
         (s-sql:sql-compile 
          `(:order-by 
            ,(append (s-sql/select-transaction-bordereau)
                     `(:where
                       (:and (:= contract.contract-id $1.contract-id)
                             (:> claim-transaction.transaction_date 
                                 $2)
                             (:< claim-transaction.transaction_date 
                                 (+ $2 $3)))))
            claim-transaction.transaction_date
            claim-transaction.transaction_id
            ))
         " $$ LANGUAGE SQL")))

(defun execute-sql/create-function-transaction-bordereau-for-contract ()
  (maxclaims::with-adb 
    (postmodern:execute "DROP VIEW IF EXISTS transaction_bordereau CASCADE;")
    (postmodern:execute 
     (s-sql:sql-compile 
      (s-sql/create-view-transaction-bordereau)))
    (postmodern:execute
(s-sql:sql-compile 
      (s-sql/create-function-transaction-bordereau-for-contract))
     )))
                         

(defun migration-transaction-bordereau-form (number)
  `(,(intern "DEFINE-MIGRATION") ,number
       ("DROP VIEW IF EXISTS transaction_bordereau CASCADE;"
        ,(s-sql:sql-compile (s-sql/create-view-transaction-bordereau))
        ,(s-sql:sql-compile (s-sql/create-function-transaction-bordereau-for-contract)))
     #+(or),(s-sql:sql-compile 
       `(:drop-view :if-exists transaction-bordereau))))
