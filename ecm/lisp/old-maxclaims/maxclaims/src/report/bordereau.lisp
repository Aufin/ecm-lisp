(defpackage :maxclaims/report/bordereau
  (:use :cl)
  (:import-from :rofl
		#:select)
  (:import-from :maxclaims
		  #:with-adb
		  #:risk-type.type-name)
  (:export #:find-bordereau
	   #:find-bordereau/payee
	   #:lloyds-v5-bordereau-fields
	   #:Ascot-lloyds-v5-bordereau-fields
	   #:default-bordereau-fields
	   #:inter-hannover-bordereau-fields
	   #:agency-bordereau-fields
	   #:claim-bordereau-fields
	   #:white-oak-bordereau-fields
     #:llyods-bordereau-fields
	   #:HUB-Dale-bordereau-fields
	   #:arch-bordereau-fields
     #:hiscox-bordereau-fields))

(in-package :maxclaims/report/bordereau)

(defun create-or-replace-bordereau-functions ()
  (let ((fns
	  (list
	   "CREATE OR REPLACE FUNCTION paid_this_period_indemnity(claim_id INT, start DATE, _end DATE)
RETURNS NUMERIC LANGUAGE SQL STABLE AS $$
 SELECT claim_indemnity_paid($1,$3,$2)::money;
$$;"
	   "CREATE OR REPLACE FUNCTION paid_this_period_adjusting(claim_id INT, start DATE, _end DATE)
RETURNS NUMERIC LANGUAGE SQL STABLE AS $$
 SELECT claim_paid('Adjusting',$1,$3,$2)::money;
$$;"
	   "CREATE OR REPLACE FUNCTION paid_this_period_tpa(claim_id INT, start DATE, _end DATE)
RETURNS NUMERIC LANGUAGE SQL STABLE AS $$
 SELECT claim_paid('TPA',$1,$3,$2)::money;
$$;"
	   "CREATE OR REPLACE FUNCTION paid_this_period_legal(claim_id INT, start DATE, _end DATE)
RETURNS NUMERIC LANGUAGE SQL STABLE AS $$
 SELECT claim_paid('Legal',$1,$3,$2)::money;
$$;"
	   "CREATE OR REPLACE FUNCTION paid_this_period_other_fee_and_expenses
  (claim_id INT, start DATE, _end DATE) RETURNS NUMERIC LANGUAGE SQL STABLE AS $$
 SELECT (  claim_paid($1,$3,$2) -
   (  paid_this_period_indemnity($1, $2, $3)
     + paid_this_period_adjusting($1, $2, $3)
     +  paid_this_period_tpa($1, $2, $3)
     +  paid_this_period_legal($1, $2, $3)
   ))::money;
$$;"

	   
 "CREATE OR REPLACE FUNCTION paid_this_period(claim_id INT, start DATE, _end DATE)
RETURNS NUMERIC LANGUAGE SQL STABLE AS $$
 SELECT claim_paid($1,$3,$2)::money;
$$;"
		 "CREATE OR REPLACE FUNCTION salvage_this_period(claim_id INT, start DATE, _end DATE)
RETURNS NUMERIC LANGUAGE SQL STABLE AS $$
 SELECT claim_salvage($1,$3,$2)::money;
$$;" 
	 "CREATE OR REPLACE FUNCTION subrogation_this_period(claim_id INT, start DATE, _end DATE)
RETURNS NUMERIC LANGUAGE SQL STABLE AS $$
 SELECT claim_subrogation($1,$3,$2)::money;
$$;"   
 "CREATE OR REPLACE FUNCTION recovered_deductible_this_period(claim_id INT, start DATE, _end DATE)
RETURNS NUMERIC LANGUAGE SQL STABLE AS $$
 SELECT claim_recovered_deductible($1,$3,$2)::money;
$$;"  

 "CREATE OR REPLACE FUNCTION indemnity_reserve_movement_this_period(claim_id INT, start DATE, _end DATE)
RETURNS NUMERIC LANGUAGE SQL STABLE AS $$
   SELECT (claim_indemnity_reserve($1,$3)
   - claim_indemnity_reserve($1,$2)
 )::money;
$$;"
 "CREATE OR REPLACE FUNCTION adjusting_reserve_movement_this_period(claim_id INT, start DATE, _end DATE)
RETURNS NUMERIC LANGUAGE SQL STABLE AS $$
   SELECT (claim_reserve('Adjusting',$1,$3)
   - claim_reserve('Adjusting',$1,$2)
 )::money;
$$;"
 "CREATE OR REPLACE FUNCTION tpa_reserve_movement_this_period(claim_id INT, start DATE, _end DATE)
RETURNS NUMERIC LANGUAGE SQL STABLE AS $$
   SELECT (claim_reserve('TPA',$1,$3)
   - claim_reserve('TPA',$1,$2)
 )::money;
$$;"
 "CREATE OR REPLACE FUNCTION legal_reserve_movement_this_period(claim_id INT, start DATE, _end DATE)
RETURNS NUMERIC LANGUAGE SQL STABLE AS $$
   SELECT (claim_reserve('Legal',$1,$3) - claim_reserve('Legal',$1,$2)
 )::money;
$$;"
"CREATE OR REPLACE FUNCTION reserve_movement_this_period(claim_id INT, start DATE, _end DATE)
RETURNS NUMERIC LANGUAGE SQL STABLE AS $$
   SELECT (claim_reserve($1,$3) - claim_reserve($1,$2))::money;
$$;"
 
 "CREATE OR REPLACE FUNCTION other_fee_and_expenses_reserve_movement_this_period(claim_id INT, start DATE, _end DATE)
RETURNS NUMERIC LANGUAGE SQL STABLE AS $$
  SELECT (reserve_movement_this_period($1, $2, $3)
   - indemnity_reserve_movement_this_period($1, $2, $3)
   - tpa_reserve_movement_this_period($1, $2, $3)
   - adjusting_reserve_movement_this_period($1, $2, $3)
   - legal_reserve_movement_this_period($1, $2, $3)
 )::money;
$$;"

	   
	   )))
	      
    (loop :for fn :in fns :do
      (print fn)
      (with-adb (postmodern:execute fn)))))
	       
		  
  

   ; ("Outstanding Reserve Adjusting" (claim-outstanding-reserve "Adjusting" claim.claim-id $2) :total NIL numeric)
(defstruct (bordereau-heading (:type list))
  ""
  name s-sql total group type)

(defparameter *bordereau-headings*
  ;; $1 = start-date
  ;; $2 = end-date
  `(("Paid this month - Indemnity"
     (:paid-this-period-indemnity
      claim.claim_id (:type $1 :date) (:type $2 :date)) :total NIL numeric)
    ("Paid this month - Defence Fees" (:raw "''"))
    ("Paid this month - Adjusters Fees"
     (:paid-this-period-adjusting claim.claim_id (:type $1 :date) (:type $2 :date))
     :total NIL numeric)
    ("Paid this month - TPA Fees"
     (:paid-this-period-tpa
      claim.claim_id (:type $1 :date) (:type $2 :date))
     :total NIL numeric)
    ("Paid this month - Attorney Coverage Fees"
     (:paid-this-period-legal
      claim.claim_id (:type $1 :date) (:type $2 :date))
     :total NIL numeric)
    ("Paid this month - Other Fees and Expenses"
     (:paid-this-period-other-fees-and-expenses
      claim.claim_id (:type $1 :date) (:type $2 :date))
     :total NIL numeric)

    ("TOTAL PAID MOVEMENTS THIS MONTH"
     (:paid-this-period claim.claim_id (:type $1 :date) (:type $2 :date))
     :total NIL numeric)

    ("Paid This Period Total"
     (:paid-this-period claim.claim_id (:type $1 :date) (:type $2 :date))
     :total NIL numeric)
    ("Salvage This Period"
     (:salvage-this-period claim.claim_id (:type $1 :date) (:type $2 :date))
     :total NIL numeric)
    ("Salvage Received this Month"
     (:salvage-this-period claim.claim_id (:type $1 :date) (:type $2 :date))
     :total NIL numeric) 
    ("Subrogation This Period"
     (claim-subrogation claim.claim-id $2 $1)
     :total NIL numeric)
    ("Recovery Received this Month"
     (:subrogation-this-period claim.claim_id (:type $1 :date) (:type $2 :date))
     :total NIL numeric)
    ("Recovered Deductible This Period"
     (:recovered-deductible-this-period claim.claim_id (:type $1 :date) (:type $2 :date))
     :total NIL numeric)
    ("TOTAL SALVAGE + RECOVERY RECEIVED MOVEMENTS THIS MONTH"
     (:+ 
      (:salvage-this-period claim.claim_id (:type $1 :date) (:type $2 :date))
      (:recovered-deductible-this-period claim.claim_id (:type $1 :date) (:type $2 :date)))
     :total NIL numeric)

    ("Indemnity Reserve movement this month"
     (:indemnity-reserve-movement-this-period claim.claim_id (:type $1 :date) (:type $2 :date))
     :total NIL numeric)
    ("Defence Fee Reserve movement this month" (:raw "''") nil nil :text)

    ("Adjuster Fee Reserve Movement this Month"
     (:adjusting-reserve-movement-this-period claim.claim_id (:type $1 :date) (:type $2 :date))
     :total NIL numeric)
    ("TPA Fee Reserve Movement this Month"
     (:tpa-reserve-movement-this-period claim.claim_id (:type $1 :date) (:type $2 :date))
     :total NIL numeric)
    ("Attorney Coverage Fee Reserve Movement this Month"
     (:legal-reserve-movement-this-period claim.claim_id (:type $1 :date) (:type $2 :date))
     :total NIL numeric)
    ("Attorney Monitoring Fee Reserve Movement this Month" (:raw "''") nil nil :text)
    ("Total Reserve Movement This Month"
    
     (:reserve-movement-this-period claim.claim_id (:type $1 :date) (:type $2 :date))
     :total NIL numeric)
    
    ("Salvage Reserve Movement this Month" (:raw "''") nil nil :text)
    ("Recovery Reserve Movement this Month" (:raw "''") nil nil :text)
    ("Total Salvage & Recovery Reserve Movements this Month" (:raw "''") nil nil :text)

    ("Other Fee & Expenses Reserve Movement this Month"
     (:other-fee-and-expenses-reserve-movement-this-period claim.claim_id (:type $1 :date) (:type $2 :date))
     :total NIL numeric)
   

    ,@(let ((change '(:+ (:paid-this-period-indemnity
			  claim.claim_id (:type $1 :date) (:type $2 :date))
		      (:indemnity-reserve-movement-this-period
		       claim.claim_id (:type $1 :date) (:type $2 :date)))))
	`(("Change this month - Indemnity" ,change :total NIL numeric)
	  ("TOTAL INDEMNITY MOVEMENT THIS MONTH" ,change :total NIL numeric)))

    ("TOTAL DEFENCE MOVEMENT THIS MONTH" (:raw "''") nil nil :text)
    ,@(let ((change '(:+ (:paid-this-period-adjusting
			  claim.claim_id (:type $1 :date) (:type $2 :date))
		      (:adjusting-reserve-movement-this-period
		       claim.claim_id (:type $1 :date) (:type $2 :date)))))
	`(("Change this month - Adjusting" ,change :total NIL numeric)
	  ("TOTAL ADJUSTER FEE MOVEMENT THIS MONTH" ,change :total NIL numeric)))
    ,@(let ((change '(:+ (:paid-this-period-tpa
			  claim.claim_id (:type $1 :date) (:type $2 :date))
		      (:tpa-reserve-movement-this-period
		       claim.claim_id (:type $1 :date) (:type $2 :date)))))
	`(("Change this month - TPA" ,change :total NIL numeric)
	  ("TOTAL TPA FEE MOVEMENT THIS MONTH" ,change :total NIL numeric)))
    ,@(let ((change '(:+ (:paid-this-period-legal
			  claim.claim_id (:type $1 :date) (:type $2 :date))
		      (:legal-reserve-movement-this-period
		       claim.claim_id (:type $1 :date) (:type $2 :date)))))
	`(("Change this month - Legal" ,change :total NIL numeric)
	  ("TOTAL ATTORNEY COVERAGE FEE MOVEMENT THIS MONTH" ,change :total NIL numeric)))


    ("TOTAL ATTORNEY MONITORING FEE MOVEMENT THIS MONTH" (:raw "''") nil nil :text)
    ,@(let ((change '(:+ (:paid-this-period-other-fee-and-expenses
			  claim.claim_id (:type $1 :date) (:type $2 :date))
		      (:other-fee-and-expenses-reserve-movement-this-period
		       claim.claim_id (:type $1 :date) (:type $2 :date)))))
	`(("Change this month - Other Fee and Expenses" ,change :total NIL numeric)
	  ("TOTAL OTHER FEE & RESERVES MOVEMENT THIS MONTH"
	   ,change :total NIL numeric)))
    ,@(let ((change '(:- (:+ (:paid-this-period
			      claim.claim_id (:type $1 :date) (:type $2 :date))
			  (:reserve-movement-this-period
			   claim.claim_id (:type $1 :date) (:type $2 :date)))
		      (:+ (:paid-this-period-indemnity
			   claim.claim_id (:type $1 :date) (:type $2 :date))
		       (:indemnity-reserve-movement-this-period
			claim.claim_id (:type $1 :date) (:type $2 :date))) )))
	`(("Change this month - Fees" ,change :total NIL numeric)
	  ("TOTAL FEE MOVEMENT THIS MONTH" ,change :total NIL numeric)))
    ,@(let ((change '(:+ (:paid-this-period
			  claim.claim_id (:type $1 :date) (:type $2 :date))
		      (:reserve-movement-this-period
		       claim.claim_id (:type $1 :date) (:type $2 :date)))))
	`(("Change this month" ,change :total NIL numeric)
	  ("TOTAL MOVEMENT IN PERIOD" ,change :total NIL numeric)))
    ("Change this month - Fees" (:raw "'???'"))


    ("TOTAL SALVAGE AND RECOVERY RECEIVED MOVEMENTS THIS MONTH"
     (:+ 
      (:subrogation-this-period claim.claim_id (:type $1 :date) (:type $2 :date))
      (:salvage-this-period claim.claim_id (:type $1 :date) (:type $2 :date)))
     :total NIL numeric)

    ,@(let ((change '(:paid-this-period-indemnity
		      claim.claim_id (:type "1970-01-01" :date) (:type $2 :date))))
	`(("Total Paid To Date - Indemnity" ,change :total NIL numeric)
	  ("TOTAL INDEMNITY" ,change :total NIL numeric)))

    
    ("Total Paid To Date - Defence Fees" (:raw "''"))

    
    ,@(let ((change ' (:paid-this-period-adjusting
		       claim.claim_id (:type "1970-01-01" :date) (:type $2 :date))))
	`(("Total Paid To Date - Adjusters Fees" ,change :total NIL numeric)
	  ("TOTAL ADUSTING" ,change :total NIL numeric)))
    ,@(let ((change ' (:paid-this-period-tpa
		       claim.claim_id (:type "1970-01-01" :date) (:type $2 :date))))
	`(("Total Paid To Date - TPA Fees" ,change :total NIL numeric)
	  ("TOTAL TPA" ,change :total NIL numeric)))
    ,@(let ((change ' (:paid-this-period-legal
		       claim.claim_id (:type "1970-01-01" :date) (:type $2 :date))))
	`(( "Total Paid To Date - Attorney Coverage Fees",change :total NIL numeric)
	  ("TOTAL LEGAL" ,change :total NIL numeric)))

    ("Total Paid To Date - Attorney Monitoring Fees" (:raw "''") nil nil :text)
    ,@(let ((change ' (:paid-this-period-other-fee-and-expenses
		       claim.claim_id (:type "1970-01-01" :date) (:type $2 :date))))
	`(( "Total Paid To Date -  Other Fees and Expenses",change :total NIL numeric)
	  ("TOTAL OTHER" ,change :total NIL numeric)))

    ,@(let ((change ' (:paid-this-period
		       claim.claim_id (:type "1970-01-01" :date) (:type $2 :date))))
	`(( "Total Paid To Date",change :total NIL numeric)
	  ("TOTAL PAID TO DATE" ,change :total NIL numeric)))


    
    ("Total Received To Date - Salvage"
     (:salvage-this-period claim.claim_id (:type "1970-01-01" :date) (:type $2 :date))
     :total NIL numeric) 
    ("Total Received to Date - Recoveries"
     (:subrogation-this-period
      claim.claim_id (:type "1970-01-01" :date) (:type $2 :date))
     :total NIL numeric)
    ("Total Received to Date"
     (:+ (:salvage-this-period claim.claim_id (:type "1970-01-01" :date) (:type $2 :date))
	 (:subrogation-this-period
	  claim.claim_id (:type "1970-01-01" :date) (:type $2 :date)))
     :total NIL numeric)


    ("Total Reserved To Date - Indemnity"
     (:claim-indemnity-reserve claim.claim-id $2)
     :total NIL numeric)

    ("Total Reserved To Date - Defence Fees" (:raw "''"))



    ("Total Reserved To Date - Adjusters Fees"
     (:claim-reserve "Adjusting" claim.claim_id $2)
     :total)

    ("Total Reserved To Date - TPA Fees"
     (:claim-reserve "TPA" claim.claim_id $2) :total)


    ("Total Reserved To Date - Attorney Coverage Fees"
     (:claim-reserve "Legal" claim.claim_id $2) :total)
     
    ("Total Reserved To Date - Attorney Monitoring Fees" (:raw "''"))

    ("Total Reserved To Date - Other Fees and Expenses"
     (:- (:claim-reserve claim.claim-id $2)
	 (:claim-indemnity-reserve claim.claim-id $2)
	 (:claim-reserve "TPA" claim.claim_id $2)
	 (:claim-reserve "Adjusting" claim.claim_id $2)
	 (:claim-reserve "Legal" claim.claim_id $2))
     :total)
    ("Total To Date - Reserved"
     (:claim-reserve claim.claim-id $2) :total)


    ("Reserved to Date - Salvage" (:raw "''") nil nil :text)
    ("Reserved to Date - Recoveries" (:raw "''") nil nil :text)
    ("Total to Date Salvage & Recovery Reserve" (:raw "''") nil nil :text)

    ("Total Incurred - Indemnity"
     (:+
      (claim-indemnity-paid claim.claim-id $2)
      (claim-indemnity-outstanding-reserve claim.claim-id $2))
     :total NIL numeric)


    ("Total Incurred - Defense Fees" (:raw "''") nil nil :text)


    ("Total Incurred - Adjusters Fees"
     (:+
      (:paid-this-period-adjusting
       claim.claim_id (:type "1970-01-01" :date) (:type $2 :date))
      (claim-outstanding-reserve "Adjusting" claim.claim-id $2))
     :total NIL numeric)
    ("Total Incurred - TPA Fees"
     (:+ (:paid-this-period-tpa
	  claim.claim_id   (:type "1970-01-01" :date)(:type $2 :date))
	 (claim-outstanding-reserve "TPA" claim.claim-id $2))
     :total NIL numeric)
    ("Total Incurred - Attorney Coverage Fees"
     (:+ (:paid-this-period-legal
	  claim.claim_id  (:type "1970-01-01" :date) (:type $2 :date))

	 (claim-outstanding-reserve "Legal" claim.claim-id $2))
     :total NIL numeric)

    ("Total Incurred - Attorney Monitoring Fees" (:raw "''") nil nil :text)
  
    ("Total Incurred - Other Fees and Expenses"
     (:+ (:paid-this-period-other-fees-and-expenses
	  claim.claim_id  (:type "1970-01-01" :date) (:type $2 :date))
	 (:- (claim-outstanding-reserve claim.claim-id $2)
	     (claim-indemnity-outstanding-reserve claim.claim-id $2)
	     (claim-outstanding-reserve "Adjusting" claim.claim-id $2)
	     (claim-outstanding-reserve "Legal" claim.claim-id $2)
	     (claim-outstanding-reserve "TPA" claim.claim-id $2))
	 )
     :total NIL numeric)

    ("TOTAL INCURRED EXCLUDING RECOVERIES"
     (:+
      (claim-outstanding-reserve claim.claim-id $2)
      (:-
       (:+
	(claim-cheque-loss claim.claim-id $2)
	(claim-cheque-expense claim.claim-id $2)
	(claim-cash-call claim.claim-id $2))
      ; (claim-salvage claim.claim-id $2)
      ; (claim-subrogation claim.claim-id $2)
      ; (claim-recovered-deductible claim.claim-id $2)
       ))
     :total)

    ("Total Recoveries Received"
     (:+ (claim-salvage claim.claim-id $2)
	 (claim-subrogation claim.claim-id $2))
     :total)
    ("TOTAL INCURRED INCLUDING RECOVERIES"
     (:+
      (claim-outstanding-reserve claim.claim-id $2)
      (:-
       (:+
	(claim-cheque-loss claim.claim-id $2)
	(claim-cheque-expense claim.claim-id $2)
	(claim-cash-call claim.claim-id $2))
       (claim-salvage claim.claim-id $2)
       (claim-subrogation claim.claim-id $2)
       ))
     :total)
    
    ("Recovery Reserves" (:raw "''") nil nil :text)
    ("Excess Received" (claim-recovered-deductible claim.claim-id $2)
		       :total NIL numeric)
    ("Date Excess Received"
     (:SELECT (:max transaction-date)
      :from (:as claim-transaction ct)
      :WHERE (:and (:= ct.claim_id claim.claim_id)
		   (:= transaction-type-id 8)))
     nil nil :date)
    
;;; aCCELERANT HERE 



    
    
    ,@(let ((change '(:+ (:paid-this-period
			  claim.claim_id (:type $1 :date) (:type $2 :date))
		      (:reserve-movement-this-period
		       claim.claim_id (:type $1 :date) (:type $2 :date)))))
	`(("Change this month" ,change :total NIL numeric)
	  ("TOTAL MOVEMENT IN PERIOD" ,change :total NIL numeric)))

    ("Industry Code" ibc.industry nil nil :int)
    ("IBC Segment" (:concat ibc.industry_classification
			    " :: " ibc.description)
		   nil nil :text)
    ("Year of Account (YOA)"
     (:raw "extract('year' from contract.effective_date)::text")
     nil nil)
    ("Recovery" (:case
                    ((:= claim.open-for-recovery t)  "Yes")
                  (:else "No")) nil nil)
    ("Cash Calls" "$0" nil nil)
    ("Subscription %" claim.subscription_percent NIL NIL)
    ("Last Update" (:type (:select (:max time) :from claim-movement
	                   :where (:= claim-movement.claim-id
			              claim.claim-id))
                    :date) nil nil :date)
    ("Update" (:select update :from claim-update
	       :where (:= claim-update.claim-id
			  claim.claim-id)))

    ("i3 Policy Number" (:select update :from claim-update
			 :where (:= claim-update.claim-id
				    claim.claim-id)))
    ("Date Claim Made"
     (:type claim.date_claim_made :date) nil nil date)
    ("Close Date"
     (:type claim.close_date :date) nil nil date)
    ("Close Date again"
     (:type claim.close_date :date) nil nil date)
    ("Date Claim Closed"
     (:type claim.close_date :date) nil nil date)
    ("Open Date"
     (:type claim.open_date :date) nil nil date)
    ("Reopen Date"
     (:type claim.rev_date :date) nil nil date)

    ("Date First Reserves Established"
     (:type (:SELECT (:min transaction_date)
             :FROM claim-transaction
             :WHERE (:and
	             (:= claim-transaction.claim-id
			 claim.claim-id)
	             (:= transaction_type_id 1)))
      :date)
     NIL NIL date)
    ("Diary Date"
     (:type (:SELECT (:min due_date)
             :FROM diary
             :WHERE (:and
	             (:= diary.claim-id
			 claim.claim-id)
	             (:= processed nil)))
	    'text)
     NIL NIL text)
    ("Peer Review Date"
     (:type claim.peer-reviewed-date 'text) NIL NIL text)
    ("Details"
     (:select
      (:string-agg d " | ")
      :from
      (:as
       (:select
	(:as (:concat
	      cd.detail_text
	      " "
	      d.description
	      " "
	      d.code)
 	     d)
	:from
	(:as claim-detail d)
	:left-join
	(:as claim-claim-detail cd)
	:on (:= d.claim-detail-id cd.claim-detail-id)
	:where (:= cd.claim-id
		   claim.claim-id))
       d)))

    ("Loss Code"
     (:limit (:select
	      d.code
	      :from
	      (:as claim-detail d)
	      :left-join
	      (:as claim-claim-detail cd)
	      :on (:= d.claim-detail-id cd.claim-detail-id)
	      :where (:= cd.claim-id
			 claim.claim-id))
	     1))



    ("Risk Type" risk-type-name :group nil)
    ("Risk Number" risk.risk-number nil nil)
    ("Policy" policy.policy-number
	      nil)
    ("Policy/Certificate Reference" policy.policy-number
				    nil)
    ("Contract Number"
     contract.contract-number
     nil)
    ("Contract Effective"
     (:type contract.effective-date :date)
     nil nil date)
    ("Contract Expiry"
     (:type contract.expiry-date :date)
     nil nil date)
    ("Effective" (:type policy.effective-date :date)
		 nil nil date)
    ("Expiry" (:type policy.expiry-date :date)
	      nil nil date)
    ("Branch" (:raw "person_name(find_person(policy.branch_id))")
	      nil)
    ("Underwriter" (:raw "person_name(find_person(policy.underwriter_id))")
		   nil)
    ("Broker" (:raw "person_name(find_person(policy.agency_office_id))")
	      nil)
    ("Claim" claim.claim-id nil nil integer)
    ("Adjuster" (:raw "person_name(find_person(claim.adjuster_id))")
		nil)
    ("Insured" (:raw "person_name(find_person(policy.insured_id))")
	       nil)
    ("Insured Street" (:raw "(SELECT address1 || CASE
          WHEN (address2 IS NOT NULL AND address2 != '')
          THEN (', ' || address2) ELSE ''
          END
         FROM person WHERE person_id = policy.insured_id)")
		      nil)
    ("Insured Email" (:raw "(select email_address FROM person WHERE person_id =  policy.insured_id)")
		     nil)
    ("Insured Address" (:raw "(select person_address(policy.insured_id))")
		       nil)
    ("Insured Home Phone" (:raw "(select home_phone FROM person WHERE person_id =  policy.insured_id)")
			  nil)
    ("Insured Work Phone" (:raw "(select work_phone FROM person WHERE person_id =  policy.insured_id)")
			  nil)
    ("Insured Fax Phone" (:raw "(select fax_phone FROM person WHERE person_id =  policy.insured_id)")
			 nil)
    ("Insured Cell Phone" (:raw "(select cell_phone FROM person WHERE person_id =  policy.insured_id)")
			  nil)
    ("Province" (:raw "substring(person_province(find_person(policy.insured_id))
                        FOR (position(' - ' in  person_province(find_person(policy.insured_id))) - 1))")
		nil)
    ("Date of Loss" (:type claim.date-of-loss :date) nil nil timestamp)

    ("Paid This Period Total Excluding"
     (- (claim-paid claim.claim-id $2 $1)
	(:raw "(select sum(amount) from claim_transaction WHERE transaction_date >= $1 AND transaction_date <= $2 AND transaction_type_id = 9 AND claim_id = claim.claim_id LIMIT 1)::numeric "))
     :total NIL numeric)
    ("Indemnity Cash Call This Period"
     (:raw "(select sum(amount) from claim_transaction
             WHERE transaction_date >= $1
             AND transaction_date <= $2
             AND transaction_heading = 'Indemnity'
             AND transaction_type_id = 9
             AND claim_id = claim.claim_id LIMIT 1)::numeric ")
     :total NIL numeric)

    ("Fee Cash Call This Period"
     (:raw "(select sum(amount) from claim_transaction
             WHERE transaction_date >= $1
             AND transaction_date <= $2
             AND (transaction_heading != 'Indemnity'
                  OR transaction_heading IS NULL)
             AND transaction_type_id = 9
             AND claim_id = claim.claim_id LIMIT 1)::numeric ")
     :total NIL numeric)
    ("Paid This Period Tow Costs"
     (claim-paid "Tow" claim.claim-id $2 $1)
     :total NIL numeric)
    ("Paid This Period Storage"
     (claim-paid "Storage" claim.claim-id $2 $1)
     :total NIL numeric)
    ("Paid This Period Disbursement"
     (claim-paid "Disbursement" claim.claim-id $2 $1)
     :total NIL numeric)
    ("Paid This Period Adjusting"
     (claim-paid "Adjusting" claim.claim-id $2 $1)
     :total NIL numeric)
    ("Paid This Period TPA"
     (claim-paid "TPA" claim.claim-id $2 $1)
     :total NIL numeric)
    ("Paid This Period Legal"
     (claim-paid "Legal" claim.claim-id $2 $1)
     :total)
    ("Paid This Period Expert Expense"
     (claim-paid "Expert Expense" claim.claim-id $2 $1)
     :total NIL numeric)
    ("Paid This Period - Expert"
     (claim-paid "Expert Expense" claim.claim-id $2 $1)
     :total NIL numeric)
    ("Paid This Period Indemnity"
     (claim-indemnity-paid
      claim.claim-id $2 $1) :total NIL numeric)


    ("Paid This Period - Indemnity"
     (claim-indemnity-paid claim.claim-id $2 $1)
     :total NIL numeric)
    ("Previously Paid - Indemnity"
     (claim-indemnity-paid claim.claim-id $1)

     :total NIL numeric)

    ("Outstanding Reserve - Indemnity"
     (claim-indemnity-outstanding-reserve claim.claim-id $2)
     :total NIL numeric)

    
    ("Paid This Period Total Loss"
     (claim-loss claim.claim-id $2 $1)
     :total)
    ("Paid This Period Fees"
     (:+ (claim-paid "Expert Expense" claim.claim-id $2 $1)
	 (claim-paid "Legal" claim.claim-id $2 $1)
	 (claim-paid "Adjusting" claim.claim-id $2 $1)
	 (claim-paid "TPA" claim.claim-id $2 $1))
     :total NIL numeric)
    ("Paid This Period Total Expense"
     (claim-cheque-expense claim.claim-id $2 $1)
     :total)
    ("Advance Paid This Period"
     "" nil nil text)
    ("Salvage This Period" (claim-salvage claim.claim-id $2 $1) :total NIL numeric)
    ("Subrogation This Period"
     (claim-subrogation claim.claim-id $2 $1)
     :total NIL numeric)
    ("Tow Costs Previously Paid"
     (claim-paid "Tow" claim.claim-id $1)
     :total NIL numeric)
    ("Storage Previously Paid"
     (claim-paid "Storage" claim.claim-id $1)
     :total NIL numeric)
    ("Disbursement Previously Paid"
     (claim-paid "Disbursement" claim.claim-id $1)
     :total NIL numeric)
    ("Indemnity Previously Paid"
     (claim-indemnity-paid claim.claim-id $1)
     :total NIL numeric)

    ("Salvage Previously Received"
     (claim-salvage claim.claim-id $1)
     :total NIL numeric)
    ("Subrogation Previously Received"
     (claim-subrogation claim.claim-id $1)
     :total NIL numeric)
    ("TPA Fees Previously Paid"
     (claim-paid "TPA" claim.claim-id $1)
     :total NIL numeric)
    ("Adjusting Fees Previously Paid"
     (claim-paid "Adjusting" claim.claim-id $1)
     :total NIL numeric)
    ("Legal Fees Previously Paid"
     (claim-paid "Legal" claim.claim-id $1)
     :total NIL numeric)
    ("Expert Fees Previously Paid"
     (claim-paid "Expert Expense" claim.claim-id $1)
     :total NIL numeric)
    ("Total Fees Previously Paid"
     (:+ (claim-paid "Expert Expense" claim.claim-id $1)
	 (claim-paid "Legal" claim.claim-id $1)
	 (claim-paid "Adjusting" claim.claim-id $1)
	 (claim-paid "TPA" claim.claim-id $1))
     :total NIL numeric)

    ("Previously Paid - Fees"
     (:+ (claim-paid "Expert Expense" claim.claim-id $1)
	 (claim-paid "Legal" claim.claim-id $1)
	 (claim-paid "Adjusting" claim.claim-id $1)
	 (claim-paid "TPA" claim.claim-id $1))
     :total NIL numeric)

    ("Total Previously Payments"
     (claim-paid claim.claim-id $1)
     :total NIL numeric)

    ("Tow Costs Paid to Date"
     (claim-paid "Tow" claim.claim-id $2)
     :total NIL numeric)
    ("Storage Paid to Date"
     (claim-paid "Storage" claim.claim-id $2)
     :total NIL numeric)
    ("Disbursement Paid to Date"
     (claim-paid "Disbursement" claim.claim-id $2)
     :total NIL numeric)


    ("Total Paid Adjusting"
     (claim-paid "Adjusting" claim.claim-id $2)
     :total NIL numeric)
    ("Total Paid TPA"
     (claim-paid "TPA" claim.claim-id $2)
     :total NIL numeric)
    ("Total Paid Legal"
     (claim-paid "Legal" claim.claim-id $2)
     :total NIL numeric)
    ("Total Paid Expert Expense"
     (claim-paid "Expert Expense" claim.claim-id $2)
     :total NIL numeric)
    ("Total Paid Indemnity"
     (claim-indemnity-paid claim.claim-id $2)
     :total NIL numeric)
    ("Total Received Salvage"
     (claim-salvage claim.claim-id $2)
     :total NIL numeric)
    ("Total Received Subrogation"
     (claim-subrogation claim.claim-id $2)
     :total NIL numeric)


    ("Total Fees Paid To Date"
     (:+ (claim-paid "Expert Expense" claim.claim-id $2)
	 (claim-paid "Legal" claim.claim-id $2)
	 (claim-paid "Adjusting" claim.claim-id $2)
	 (claim-paid "TPA" claim.claim-id $2))
     :total NIL numeric)

    ("Outstanding Reserve Tow Costs"
     (claim-outstanding-reserve "Tow" claim.claim-id $2)
     :total NIL numeric)
    ("Outstanding Reserve Storage Costs"
     (claim-outstanding-reserve "Storage" claim.claim-id $2)
     :total NIL numeric)
    ("Outstanding Reserve Adjusting" (claim-outstanding-reserve "Adjusting" claim.claim-id $2) :total NIL numeric)
    ("Outstanding Reserve TPA"
     (claim-outstanding-reserve "TPA" claim.claim-id $2)
     :total NIL numeric)
    ("Outstanding Reserve Legal"
     (claim-outstanding-reserve "Legal" claim.claim-id $2)
     :total NIL numeric)
    ("Outstanding Reserve Expert Expense"
     (claim-outstanding-reserve "Expert Expense" claim.claim-id $2)
     :total NIL numeric)
    ("Outstanding Reserve Expert"
     (claim-outstanding-reserve "Expert Expense" claim.claim-id $2)
     :total NIL numeric)
    ("Outstanding Fees Reserve"
     (:+ (claim-outstanding-reserve "Expert Expense" claim.claim-id $2)
	 (claim-outstanding-reserve "Legal" claim.claim-id $2)
	 (claim-outstanding-reserve "TPA" claim.claim-id $2)
	 (claim-outstanding-reserve "Adjusting" claim.claim-id $2))
     :total NIL numeric)

    ("Outstanding Reserve - Fees"
     (:+ (claim-outstanding-reserve "Expert Expense" claim.claim-id $2)
	 (claim-outstanding-reserve "Legal" claim.claim-id $2)
	 (claim-outstanding-reserve "TPA" claim.claim-id $2)
	 (claim-outstanding-reserve "Adjusting" claim.claim-id $2))
     :total NIL numeric)
    ("Outstanding Reserve Indemnity"
     (claim-indemnity-outstanding-reserve claim.claim-id $2)
     :total NIL numeric)

    ("Initial Reserve" (claim-open-reserve claim.claim-id)
		       :total NIL numeric)

    ("Change In Reserve This Period"
     (:- (claim-reserve claim.claim-id
			$2)
	 (claim-reserve claim.claim-id $1))
     :total NIL numeric)

    ("Outstanding Reserve Total" (claim-outstanding-reserve claim.claim-id $2)
				 :total NIL numeric)

    ("Deductible to be Recovered"
     (:- claim.deductible (claim-recovered-deductible claim.claim-id $2))
     :total NIL numeric)

    ("Deductible Amount" claim.deductible :total NIL numeric)

    ("Recovered Deductible" (claim-recovered-deductible claim.claim-id $2)
			    :total NIL numeric)
    ("Outstanding Deductible"
     (:case  ((:= status "Closed") 0)
       (t (claim-deductible claim.claim-id $2)))
     :total NIL numeric)



    ("Loss this period" (claim-loss claim.claim-id $2 $1)
			:total)
    ("Expense this Period" (claim-cheque-expense claim.claim-id $2 $1)
			   :total)
    ("Expense this Period (i3)" (:-
				 (claim-cheque-expense claim.claim-id $2 $1)
				 (claim-paid "TPA" claim.claim-id $2 $1))
				:total NIL numeric)
    ("Expense Previously Paid"
     (claim-cheque-expense claim.claim-id $1)
     :total)
    ("Loss Total" (claim-cheque-loss claim.claim-id $2)
		  :total)
    ("Loss" (claim-cheque-loss claim.claim-id)
	    :total)
    ("Expense Total" (claim-cheque-expense claim.claim-id $2)
		     :total)
    ("Expense Total (i3)" (:- (claim-cheque-expense claim.claim-id $2)
			      (claim-paid "TPA" claim.claim-id $2))
			  :total NIL numeric)
    ("Expense" (claim-cheque-expense claim.claim-id)
	       :total)

    ("Total Paid" (claim-paid claim.claim-id )
		  :total)
    ("Total Paid Gross" (:+ (claim-cheque-loss claim.claim-id $2)
			    (claim-cheque-expense claim.claim-id $2))
			:total)
    ("Total Paid Net" (:-  (:+ (claim-cheque-loss claim.claim-id $2)
			       (claim-cheque-expense claim.claim-id $2))
			   (claim-salvage claim.claim-id $2)
			   (claim-subrogation claim.claim-id $2)

			   (claim-recovered-deductible claim.claim-id $2))

		      :total)
    ("Incurred"
     (:+
      (claim-outstanding-reserve claim.claim-id $2)
      (:-
       (:+
	(claim-cheque-loss claim.claim-id $2)
	(claim-cheque-expense claim.claim-id $2)
	(claim-cash-call claim.claim-id $2))
       (claim-salvage claim.claim-id $2)
       (claim-subrogation claim.claim-id $2)
       (claim-recovered-deductible claim.claim-id $2)
       ))
     :total)
    ("Salvage/Recovery" (:+
			 (claim-salvage claim.claim-id $2)
			 (claim-subrogation claim.claim-id $2))
			:total)
    ("Above Authority"
     (:type (:select (:case (authority-over "Yes")
		       ((:not authority-over) "No"))
	     :from claim_authority
	     :where (:= claim.claim_id claim_authority.claim_id))
      :text)
     NIL)
    ("Within Authority"
     (:type (:select (:case ((<= authority
				 (:+ (claim-outstanding-reserve claim.claim-id $2)
				     (:-  (:+ (claim-cheque-loss claim.claim-id $2)
					      (claim-cheque-expense claim.claim-id $2))
					  (claim-salvage claim.claim-id $2)
					  (claim-subrogation claim.claim-id $2)
					  (claim-recovered-deductible claim.claim-id $2)
					  ))) "No")
		       (t "Yes"))
	     :from contract_authority
	     :where (:= contract_authority.contract-id
			contract.contract-id))
      :text)
     NIL)
    ("Status" (:claim-status claim.claim_id $2)
	      nil)

    ("Cause of Loss" claim.cause nil)


    ;; White Oak
    ("White Oak Cause of Loss"
     (:limit (:select
	      (:concat
	       d.description
	       " "
	       d.code)
	      :from
	      (:as claim-detail d)
	      :left-join
	      (:as claim-claim-detail cd)
	      :on (:= d.claim-detail-id cd.claim-detail-id)
	      :where (:= cd.claim-id
			 claim.claim-id))
	     1) nil)

    ("White Oak Loss Description"
     (:limit (:select
	      (:concat
	       cd.detail_text)
	      :from
	      (:as claim-detail d)
	      :left-join
	      (:as claim-claim-detail cd)
	      :on (:= d.claim-detail-id cd.claim-detail-id)
	      :where (:= cd.claim-id
			 claim.claim-id))
	     1) nil)

    ("Loss Description"
     (:limit (:select
	      (:concat
	       cd.detail_text)
	      :from
	      (:as claim-detail d)
	      :left-join
	      (:as claim-claim-detail cd)
	      :on (:= d.claim-detail-id cd.claim-detail-id)
	      :where (:= cd.claim-id
			 claim.claim-id))
	     1) nil)
    ("Loss Code Description"
     (:limit (:select
	      (:concat
	       d.description
	       " "
	       d.code)
	      :from
	      (:as claim-detail d)
	      :left-join
	      (:as claim-claim-detail cd)
	      :on (:= d.claim-detail-id cd.claim-detail-id)
	      :where (:= cd.claim-id
			 claim.claim-id))
	     1) nil)



    ("Retail Agent" (:raw "person_name(find_person(policy.agency_office_id))")
		    nil)
    ("Policy Form" (:select value
		    :from policy-detail
		    :where (:and (:= policy-detail.policy-id
				     policy.policy-id)
				 (:= key "Policy Form")))
		   nil)
    ("Policy Section" (:select value
		       :from policy-detail
		       :where (:and (:= policy-detail.policy-id
					policy.policy-id)
				    (:= key "Policy Section")))
		      nil)
    ("Class of Business"
     (:select (:coalesce claim.line-of-business "")) nil)
    ("Scheduled Insured Value of Vehicle"
     (:select value
      :from policy-detail
      :where (:and (:= policy-detail.policy-id
		       policy.policy-id)
		   (:= key "Scheduled Insured Value of Vehicle"))) nil)


    ("Paid This Period LAE" (:raw "'???'") nil)
    ("LAE Fees Previously Paid" (:raw "'???'") nil)
    ("LAE Fees Paid To Date" (:raw "'???'") nil)

    ("Paid This Period Disbursement" (:raw "'???'") nil)


    ("Total Fees Previously Paid" (:raw "'???'") nil)

    ("Fee Cash Call This Period" (:raw "'???'") nil)








    ("Insured Value"
     (:raw "'???'") nil)
;;;    ("Date Claim Reported" (:raw "'???'") nil)

    ;;    ("Cat Name" (:raw "'???'") nil)
    ;;    ("Cat Code" (:raw "'???'") nil)

    ("Loss Province"
     (:limit
      (:select value
       :from loss-detail
       :where (:and (:= loss-detail.claim-id
			claim.claim-id)
		    (:= key "Loss Province")))
      1)
     nil)
    ("Loss Country"
     (:coalesce
      (:limit
       (:select value
	:from loss-detail
	:where (:and (:= loss-detail.claim-id
			 claim.claim-id)
		     (:= key "Loss Country")))
       1)
      "Canada")

     nil)
    ("Loss Location"
     (:limit (:select value
	      :from loss-detail
	      :where (:and (:= loss-detail.claim-id
			       claim.claim-id)
			   (:= key "Loss Location")))
	     1)
     nil)
    ("Cat Name"
     (:limit (:select value
	      :from loss-detail
	      :where (:and (:= loss-detail.claim-id
			       claim.claim-id)
			   (:= key "Cat Name")))
	     1)
     nil)
    ("Cat Code"
     (:limit (:select value
	      :from loss-detail
	      :where (:and (:= loss-detail.claim-id
			       claim.claim-id)
			   (:= key "Cat Code")))
	     1)
     nil)
    ("Type of Loss"
     (:limit  (:select value
	       :from loss-detail
	       :where (:and (:= loss-detail.claim-id
				claim.claim-id)
			    (:= key "Type of Loss")))
	      1)
     nil)
    ("Type of Cargo"
     (:limit  (:select value
	       :from loss-detail
	       :where (:and (:= loss-detail.claim-id
				claim.claim-id)
			    (:= key "Type of Cargo")))
	      1)
     nil)
    ;;    ;("Loss Location" (:raw "'???'") nil)
    ;; ("Type of Loss" (:raw "'???'") nil)
    ;;("Type of Cargo" (:raw "'???'") nil)
    ("Number of Units"
     (:select value
      :from loss-detail
      :where (:and (:= loss-detail.claim-id
		       claim.claim-id)
		   (:= key "Number of Units"))) nil)
    ("Total Loss"
     (:select value
      :from loss-detail
      :where (:and (:= loss-detail.claim-id
		       claim.claim-id)
		   (:= key "Total Loss (Y/N)"))) nil)
    ("Driver Name"
     (:select value
      :from driver-detail
      :where (:and (:= driver-detail.claim-id
		       claim.claim-id)
		   (:= key "Driver Name"))) nil)
    ("Driver Age"
     (:select value
      :from driver-detail
      :where (:and (:= driver-detail.claim-id
		       claim.claim-id)
		   (:= key "Driver Age"))) nil)
    ("Vehicle Make"
     (:select value
      :from vehicle-detail
      :where (:and (:= vehicle-detail.claim-id
		       claim.claim-id)
		   (:= key "Vehicle Make")))
     nil
     nil
     text)
    ("Vehicle Model"
     (:select value
      :from vehicle-detail
      :where (:and (:= vehicle-detail.claim-id
		       claim.claim-id)
		   (:= key "Vehicle Model")))
     nil
     nil
     text)
    ("Vehicle Year"
     (:select value
      :from vehicle-detail
      :where (:and (:= vehicle-detail.claim-id
		       claim.claim-id)
		   (:= key "Vehicle Year")))
     nil
     nil
     text)
    ("Vehicle VIN/Serial/Registration"
     (:select value
      :from vehicle-detail
      :where (:and (:= vehicle-detail.claim-id
		       claim.claim-id)
		   (:= key "Vehicle VIN/Serial/Registration Number")))
     nil
     nil
     text)
    ("Date Claim Paid"
     (:raw "(SELECT paid.date
       FROM (SELECT claim_outstanding_reserve(ct.transaction_heading, ct.claim_id)
             AS outr, ct.transaction_date AS date
             FROM claim_transaction AS ct
             WHERE claim_transaction_is_indemnity(ct)
             AND int4range(3,5) @> ct.transaction_type_id
             AND ct.claim_id = claim.claim_id ORDER BY transaction_date DESC)
       AS paid WHERE outr = 0 ORDER BY date DESC limit 1)")
     nil
     nil
     date)
    ("ROR Issued (Y/N)"
     (:select value
      :from claim-status-detail
      :where (:and (:= claim-status-detail.claim-id
		       claim.claim-id)
		   (:= key "ROR Issued (Y/N)")))
     nil
     nil
     text)
    ("Denial (Y/N)"
     (:select value
      :from claim-status-detail
      :where (:and (:= claim-status-detail.claim-id
		       claim.claim-id)
		   (:= key "Denial (Y/N)")))
     nil
     nil
     text)

    ("Denial"
     (:case ((:= claim.denial t) "Yes")
       ((:is-null claim.denial) "")
       (:else "No"))
     nil
     nil
     text)

    ("Refer to Underwriters"
     (:case ((:= claim.refer-to-underwriters t) "Yes")
       ((:is-null claim.refer-to-underwriters) "")
       (:else "No"))
     nil
     nil
     text)
    ("Ex Gratia (Y/N)"
     (:select value
      :from claim-status-detail
      :where (:and (:= claim-status-detail.claim-id
		       claim.claim-id)
		   (:= key "Ex Gratia (Y/N)")))
     nil
     nil
     text)
    ("In Litigation (Y/N)"
     (:select value
      :from claim-status-detail
      :where (:and (:= claim-status-detail.claim-id
		       claim.claim-id)
		   (:= key "In Litigation (Y/N)")))
     nil
     nil
     text)

    ("Successful Subrogation Potential (Y/N)"
     (:select value
      :from claim-status-detail
      :where (:and (:= claim-status-detail.claim-id
		       claim.claim-id)
		   (:= key "Successful Subrogation Potential (Y/N)")))
     nil
     nil
     text)

    ("Subrogation Status Notes"
     (:select value
      :from claim-status-detail
      :where (:and (:= claim-status-detail.claim-id
		       claim.claim-id)
		   (:= key "Subrogation Status Notes")))
     nil
     nil
     text)
    ;; ** Lloyds
    ("Coverholder Name"
     (:person-name contract.agency-id)
     nil
     nil
     text)
    ("TPA Name"
     (:raw "'Maxwell Claims Services Inc.'")
     nil
     nil
     text)
    ("Agreement No"
     (:raw "'???'")
     nil
     nil
     text)
    ("Start Date"
     (:type $1 date) nil nil date)

    ("End Date"
     (:type $2 :date) nil nil date)

    ("Section Number"
     (:raw "'???'")
     nil
     nil
     text)
    ("Original Currency"
     "CAD"
     nil
     nil
     text)
    ("Settlement Currency"
     (:raw "'???'")
     nil
     nil
     text)
    ("Rate of Exchange"
     (:raw "'???'")
     nil
     nil
     text)

    ("Row Number"
     (:raw "(4 + row_number() OVER(ORDER BY claim.claim_id))")
     nil T numeric)

    ("Insured Country"
     "Canada")

    ("Risk Country"
     (:raw "'???'::text")
     nil
     nil
     )

    ("Risk Province" (:raw "'???'::text") nil T text)

    ("Period of Cover"
     (:raw "'???'::text")
     nil
     T
     text)

    ("Lloyd's Loss Location"
     (:coalesce (:select value
		 :from loss-detail
		 :where (:and (:= loss-detail.claim-id
                                  claim.claim-id)
                              (:= key "Loss Location")))
		"Canada") nil)
    ("Advance Payments"
     (:raw "''::text"))
    ("Date of Loss to"
     (:raw "'???'::text"))
    ("Claim First Notification Acknowledgement Date"
     (:type claim_acknowledged_time :date)
     nil nil date)
    ("Date First Advised Notification Date"
     (:type claim_received_time :date)
     nil nil date)
    ("Date Claim Made"
     (:type (:coalesce claim_acknowledged_time claim_received_time open_date) :date)
     nil nil date)

    ("Refer to Underwriters"
     (:raw "'??? We have a number of things that may or may not be this ???'"))
    ("Claimant Name" (:raw "person_name(find_person(claim.plaintiff_id))"))
    ("Loss County"
     (:raw "'???'"))
    ("State of Filing"
     (:raw "'???'"))
    ("PCS Code"
     (:raw "'???'"))
    ("Medicare United States Bodily Injury"
     (:raw "'???'"))
    ("Medicare Eligibility Check Performance"
     (:raw "'???'"))
    ("Medicare Outcome of Eligilibility Status Check"
     (:raw "'???'"))
    ("Medicare Conditional Payments"
     (:raw "'???'"))
    ("Medicare MSP Compliance Services"
     (:raw "'???'"))

    
    ("Total Incurred - Fees"
     (:+ (claim-paid "Expert Expense" claim.claim-id $2)
	 (claim-paid "Legal" claim.claim-id $2)
	 (claim-paid "Adjusting" claim.claim-id $2)
	 (claim-paid "TPA" claim.claim-id $2)
	 (:+ (claim-outstanding-reserve "Expert Expense" claim.claim-id $2)
	     (claim-outstanding-reserve "Legal" claim.claim-id $2)
	     (claim-outstanding-reserve "TPA" claim.claim-id $2)
	     (claim-outstanding-reserve "Adjusting" claim.claim-id $2)))
     :total NIL numeric)

    ("Coverholder PIN"
     (:raw "'???'"))
    ("Type of Insurance (Direct, or Type of RI)"
     (:raw "'???'"))
    ("Policy or Group Ref"
     (:raw "'???'"))
    ("Lloyd's Risk Code"
     risk.risk_code)
    ("Insured Address"
     (:person-address policy.insured-id))

    ("Insured Postal Code" (:select postal_zip_code :from person :where (:= person_id  policy.insured_id)))

    ("Location ID"
     (:raw "'???'"))
    ("Risk Address"
     (:raw "'???'"))
    ("Risk Postal Code"
     (:raw "'???'"))

    ("Deductible Basis"
     (:raw "'???'"))
    ("Sums Insured Amount"
     (:raw "'???'"))

    ("Loss Address"
     (:raw "(SELECT string_agg(value::text, ', ') from json_each_text(jso.loss_location(claim.*)))"))
    ("Loss Postal Code"
     (:SELECT value :FROM loss_detail :where (:and (:= loss_detail.claim_id claim.claim_id)
                                                   (:= key "Loss Postal Code")))
     )

    ("Lloyd's Cat Code"
     (:raw "'???'"))
    ("Catastrophe Name"
     (:raw "'???'"))

    ("Paid this month - Defence Fee" (:raw "''"))

    ("Previously Paid - Defence Fees"
     (:raw "'???'"))
    ("Reserve - Defence Fees"
     (:raw "'???'"))



;;; See Total Expense Reserve
    ("Reserve - Expenses"
     (:raw "'???'"))

    ("Name or Reg No of Aircraft Vehicle, Vessel etc"
     (:raw "'???'"))
    ("% Ceded (Reinsurance)"
     (:raw "'???'"))
    ("A&H Plan"
     (:raw "'???'"))
    ("Patient Name"
     (:raw "'???'"))
    ("Treatment Type"
     (:raw "'???'"))
    ("Country of Treatment"
     (:raw "'???'"))
    ("Date of Treatment"
     (:raw "'???'"))
    ("Expert Role" (:raw "'???'"))
    ("Expert Firm / Company Name"
     (:raw "'???'"))
    ("Expert Reference No etc"
     (:raw "'???'"))
    ("Country of Treatment"
     (:raw "'???'"))
    ("Expert Address"
     (:raw "'???'"))
    ("Expert State, Province, Territory, Canton etc"
     (:raw "'???'"))
    ("Expert Postal Code"
     (:raw "'???'"))
    ("Expert Country"
     (:raw "'???'"))
    ("Expert Notes"
     (:raw "'???'"))

    ("Date Coverage Confirmed"
     (:raw "'???'"))
    ("Date Claim Amount Agreed"
     (:type (:raw "(SELECT transaction_date FROM claim_transaction AS ct
             WHERE claim_transaction_is_indemnity(ct)
             AND int4range(3,5) @> ct.transaction_type_id
             AND ct.claim_id = claim.claim_id
             ORDER BY ct.transaction_date LIMIT 1)") :date)
     nil nil date)
    ("Date Claim Opened"
     (:type claim.open_date :date) nil nil date)
    ("Date Fees Paid"
     (:raw "'???'"))
    ("Date Reopened"
     (:type claim.rev_date :date) nil nil date)
    ("Date of Subrogation"
     (:raw "'???'"))

    ("Claimant name" (:raw "person_name(find_person(claim.plaintiff_id))"))
    ("Claimant Postal Code" (:select postal_zip_code :from person :where (:= person_id  claim.plaintiff-id)))
    ("Claimant Address"
     (person-address claim.plaintiff-id))
    ("Claimant Country"
     "Canada")
    ("Date Claim Denied" (:type (:type claim.date-of-denial :date) :text))
    ("Reason for Denial" claim.reason-for-denial)
    ("Amount Claimed" (:raw "'???'"))
    ("Date claim withdrawn"
     (:raw "'???'"))
    ("Claim not paid as within excess"
     (:raw "'???'"))
    ("Field" (:raw "''"))

    ;;
    ;; Zurich
    ;;
    ;;
    ("Certificate Reference" contract.contract-number)
    ("TPA Claim number" claim.claim_id nil nil integer)
    ("Zurich Claim Number" claim.lineage nil nil)
    ("Policy Number" policy.policy_number)
    ("Policy Effective Date" (:type policy.effective-date :date) nil nil timestamp)
    ("Policy Expiration Date" (:type policy.expiry-date :date) nil nil timestamp)
    ("Risk Inception Date" (:type contract.effective-date :date) nil nil timestamp)
    ("Risk Expiration Date" (:type contract.expiry-date :date) nil nil timestamp)
    ("Date of Loss (DDMMYYYY)" (:raw "TO_CHAR(claim.date_of_loss, 'ddmmyyyy')"))
    ("Date Reported to Zurich (DDMMYYYY)" (:raw "TO_CHAR(claim.claim_received_time, 'ddmmyyyy')"))
    ("Date Claim Opened (DDMMYYYY)" (:raw "TO_CHAR(claim.open_date, 'ddmmyyyy')"))
    ("Deductible" claim.deductible :total NIL numeric)
    ("Type of Claim / LOB" claim.line-of-business)
    ("Location of Loss"
     (:coalesce (:select value
                 :from loss-detail
                 :where (:and (:= loss-detail.claim-id
				  claim.claim-id)
                              (:= key "Loss Location")))
		"Canada")nil)
    ("Location of Risk Country"
     (:coalesce
      (:limit
       (:select value
	:from loss-detail
	:where (:and (:= loss-detail.claim-id
			 claim.claim-id)
		     (:= key "Loss Country")))
       1)
      "Canada")
     nil)
    ( "Location of Risk Country" "Canada" nil)
    ("Location/Division Code"
     (:case ((:= "Property" claim.line-of-business)
             (:coalesce (:select value
			 :from loss-detail
			 :where (:and (:= loss-detail.claim-id
					  claim.claim-id)
				      (:= key "Loss Location"))))
             "Canada"))
     nil)
    ( "Description of Loss"
      (:limit (:select
	       (:concat
	        cd.detail_text)
	       :from
	       (:as claim-detail d)
	       :left-join
	       (:as claim-claim-detail cd)
	       :on (:= d.claim-detail-id cd.claim-detail-id)
	       :where (:= cd.claim-id
			  claim.claim-id))
	      1) nil)

    ("Zurich Cause of Loss"
     (:limit (:select
	      (:concat
	       d.description
	       " "
	       d.code)
	      :from
	      (:as claim-detail d)
	      :left-join
	      (:as claim-claim-detail cd)
	      :on (:= d.claim-detail-id cd.claim-detail-id)
	      :where (:= cd.claim-id
			 claim.claim-id))
	     1) nil)
    ("Claim Status (Open/Closed)" status nil)
    ("Degree of Fault (%)" "" nil)
    ("Loss Party Name" (:case ((:= claim.line-of-business "Property")
                               (:raw "person_name(find_person(policy.insured_id))")))
		       nil)
    ("Coverage" coverage nil)
    ("Total Indemnity Reserve" (:claim-indemnity-reserve claim.claim-id $2) :total)
    ("Total Expense Reserve" (:- (:claim-reserve claim.claim-id $2)
				 (:claim-indemnity-reserve claim.claim-id $2)) :total)
    ("Expense Reserve Type (Legal)" (:claim-reserve "Legal" claim.claim_id $2) :total)
    ("Expense Reserve Type (Adjuster)" (:claim-reserve "Adjusting" claim.claim_id $2) :total)
    ("Expense Reserve Type (TPA fees)" (:claim-reserve "TPA" claim.claim_id $2) :total)
    ("Expense Reserve Type (Miscellaneous)"
     (:- (:claim-reserve claim.claim-id $2)
	 (:claim-indemnity-reserve claim.claim-id $2)
	 (:claim-reserve "TPA" claim.claim_id $2)
	 (:claim-reserve "Adjusting" claim.claim_id $2)
	 (:claim-reserve "Legal" claim.claim_id $2))
     :total)
    ("Recovery Reserve"  "" nil)
    ("Indemnity Payment Amount - This Month" (:claim-indemnity-paid claim.claim-id $2 $1) :total)
    ("Total Indemnity Payments Incurred to Date" (:claim-indemnity-paid claim.claim-id $2) :total)
    ("TPA Expense Payment - This Month" (:claim-paid "TPA" claim.claim-id $2 $1) :total)
    ("Adjuster Expense Payment - This Month" (:claim-paid "Adjusting" claim.claim-id $2 $1) :total)
    ("Legal Expense Payment - This Month" (:claim-paid "Legal" claim.claim-id $2 $1) :total)
    ("Other Expense Paid - This month"
     (:- (:claim-paid claim.claim-id $2 $1)
	 (:claim-indemnity-paid claim.claim-id $2 $1)
	 (:claim-paid "TPA" claim.claim_id $2 $1)
	 (:claim-paid "Adjusting" claim.claim_id $2 $1)
	 (:claim-paid "Legal" claim.claim_id $2 $1))
     :total)
    ("Total Expense Paid Incurred to Date" (:- (:claim-paid claim.claim-id $2)
                                               (:claim-indemnity-paid claim.claim-id $2)) :total)
    ("Recovery Type" "")
    ("Recovery Amount - This Month" (:+ (claim-recovered-deductible claim.claim-id $2 $1)
					(claim-salvage claim.claim-id $2 $1)
					(claim-subrogation claim.claim-id $2 $1))
				    :total)
    ("Recovery Total to date" (:+ (claim-recovered-deductible claim.claim-id $2)
                                  (claim-salvage claim.claim-id $2)
                                  (claim-subrogation claim.claim-id $2))
			      :total)

    ("Payee Name" (:raw "((SELECT person_name(payee_id)
          FROM claim_transaction
          WHERE person_name(payee_id) ILIKE 'Maxwell Claim%'
          AND claim_id = claim.claim_id and transaction_date <@ tsrange($1, $2, '[)')
  LIMIT 1
))"))
    ("Invoice/Reference #" (:raw "((SELECT reference_number
          FROM claim_transaction
          WHERE person_name(payee_id) ILIKE 'Maxwell Claim%'
          AND claim_id = claim.claim_id and transaction_date <@ tsrange($1, $2, '[)') LIMIT 1))"))

    ("Previous Paid - Indemnity" (:-  (:claim-indemnity-paid claim.claim-id $2)
                                      (:claim-indemnity-paid claim.claim-id $2 $1))
				 :total)
    ("OS Indemnity Reserve Balance"
     (claim-indemnity-outstanding-reserve claim.claim-id $2)
     :total NIL numeric)
    ("O/S Adjuster Expense Reserve Balance" (:claim-outstanding-reserve "Adjusting" claim.claim_id $2) :total)
    ("O/S TPA Expense Reserve Balance" (:claim-outstanding-reserve "TPA" claim.claim_id $2) :total)
    ("O/S Legal Expense Reserve Balance" (:claim-outstanding-reserve "Legal" claim.claim_id $2) :total)
    ("O/S Other Expense Reserve Balance"
     (:- (:claim-outstanding-reserve claim.claim-id)
	 (claim-indemnity-outstanding-reserve claim.claim-id $2)
	 (:claim-outstanding-reserve "Adjusting" claim.claim_id $2)
	 (:claim-outstanding-reserve "TPA" claim.claim_id $2)
	 (:claim-outstanding-reserve "Legal" claim.claim_id $2))
     :total)
    ("Notes" (:select update :from claim-update
	      :where (:= claim-update.claim-id
			 claim.claim-id)))

    ("Unique Market Reference" contract.contract-number)

    ;; Accelerant 
    ("Accelerant Unique Claim Ref" claim.lineage)
    ("Line of Business" claim.line-of-business)
    ("Annual Statement Line of Business" claim.line-of-business)
    ("OSFI LOB" (:raw "''"))
    ("Product" (:raw "''"))
    ("Trade" (:raw "''"))
    ("Cause of Loss Code 2"
     (:limit (:select
	      (:concat
	       d.description
	       " "
	       d.code)
	      :from
	      (:as claim-detail d)
	      :left-join
	      (:as claim-claim-detail cd)
	      :on (:= d.claim-detail-id cd.claim-detail-id)
	      :where (:= cd.claim-id
			 claim.claim-id))
	     1) nil)
    ("Accident Date" (:raw "''"))
    ("Jurisdiction of the claim" (:raw "''"))
    ("General nature of loss" (:raw "''"))
    ("Body functions or structures affected" (:raw "''"))
    ("Severity of loss" (:raw "''"))
    ("Heads of damage – past economic loss" (:raw "''"))
    ("Heads of damage – future economic loss" (:raw "''"))
    ("Heads of damage - Past medical, hospital" (:raw "''"))
    ("Heads of damage - Future medical, hospital" (:raw "''"))
    ("Heads of damage - Future caring services" (:raw "''"))
    ("Heads of damage - General damages" (:raw "''"))
    ("Heads of damage – Interest" (:raw "''"))
    ("Paid this month - Attorney Monitoring Fees" (:raw "''"))
    ))





(defparameter *default-bordereau-fields*
  '("Risk Type"
    "Policy"
    "Effective"
    "Expiry"
    "Branch"
    "Underwriter"
    "Broker"
    "Claim"
    "Insured"
    "Province"
    "Date of Loss"
    "Details"
    "Cause of Loss"
    "Loss Code"
    "Industry Code"


    ;;                 -Loss Code or Details of Loss
    "Paid This Period Adjusting"
    "Paid This Period TPA"
    "Paid This Period Legal"
    "Paid This Period Expert Expense"
    "Paid This Period Indemnity"
    "Paid This Period Tow Costs"
    "Paid This Period Storage"
    "Paid This Period Total Loss"
    "Paid This Period Total Expense"
    "Paid This Period Total"
    "Advance Paid This Period"

    "Total Paid Adjusting"
    "Total Paid TPA"
    "Total Paid Legal"
    "Total Paid Expert Expense"
    "Total Paid Indemnity"
    "Tow Costs Paid to Date"
    "Storage Paid to Date"
    "Total Paid Gross"
    "Total Paid Net"

    "Initial Reserve"
    "Outstanding Reserve Adjusting"
    "Outstanding Reserve TPA"
    "Outstanding Reserve Legal"
    "Outstanding Reserve Expert Expense"
    "Outstanding Reserve Indemnity"
    "Outstanding Reserve Tow Costs"
    "Outstanding Reserve Storage Costs"
    "Outstanding Reserve Total"
    "Change In Reserve This Period"

    "Deductible to be Recovered"
    "Recovered Deductible This Period"
    "Recovered Deductible"
    "Outstanding Deductible"

    "Loss this period"
    "Cash Calls"
    "Expense this Period"
    "Loss Total"
    "Expense Total"

    "Incurred"
    "Salvage/Recovery"
    "Above Authority"
    "Status"
    "Close Date"
    "Update"))



(defun default-bordereau-fields ()
  (mapcar (lambda (f)
	    (find f *bordereau-headings*
		  :key #'first
		  :test #'string-equal))
	      *default-bordereau-fields*))

(defparameter *zurich-bordereau-fields*
  '(
    "Insured"
     "Certificate Reference"
     "TPA Claim number" "Zurich Claim Number"
     "Policy Number" "Policy Effective Date" "Policy Expiration Date"
     "Risk Inception Date" "Risk Expiration Date"
    "Date of Loss (DDMMYYYY)""Date Reported to Zurich (DDMMYYYY)""Date Claim Opened (DDMMYYYY)"
    "Deductible""Type of Claim / LOB"

     "Location of Loss""Location of Risk Country""Location/Division Code"

     "Description of Loss""Claim Status (Open/Closed)" "Denial""Degree of Fault (%)"

     "Loss Party Name""Coverage" "Zurich Cause of Loss"
     "Claimant Name"

     "Total Indemnity Reserve""Total Expense Reserve"

      "Expense Reserve Type (Legal)" "Expense Reserve Type (Adjuster)""Expense Reserve Type (TPA fees)"
     "Expense Reserve Type (Miscellaneous)"
     "Recovery Reserve"

      "Indemnity Payment Amount - This Month"
     "Total Indemnity Payments Incurred to Date"

     "TPA Expense Payment - This Month"
     "Adjuster Expense Payment - This Month"
     "Legal Expense Payment - This Month"
     "Other Expense Paid - This month"
     "Total Expense Paid Incurred to Date"

      "Recovery Type"
     "Recovery Amount - This Month""Recovery Total to date"

     "Payee Name" "Invoice/Reference #" "Previous Paid - Indemnity"
     "O/S Adjuster Expense Reserve Balance" "O/S TPA Expense Reserve Balance"
     "O/S Legal Expense Reserve Balance" "O/S Other Expense Reserve Balance"


     "Notes"

    ;; These are here for group and order by
     "Risk Type" "Claim"
))

(defun zurich-bordereau-fields ()
  (let ((*default-bordereau-fields* *zurich-bordereau-fields*))
    (default-bordereau-fields)))
(defparameter *lloyds-v5-bordereau-fields*
  (append
   (list
    "Field"
    "Coverholder Name"
    "TPA Name"
    "Agreement No"
    "Contract Number"
    "Contract Effective"
    "Contract Expiry"
    "End Date"
    "Risk Type"
    "Class of Business"
    "Lloyd's Risk Code"
    "Section Number"
    "Original Currency"
    "Settlement Currency"
    "Rate of Exchange"
    ;"Row Number"

    "Policy"
    "Claim"
    "Insured"
    "Province"
    "Insured Country"

    "Risk Province"
    "Risk Country"
    "Effective"
    "Expiry"
    "Period of Cover"

    "Loss Province"
    "Loss Country"
    "Loss Code"
    "Loss Description"
    "Date of Loss"
    "Date of Loss to"
    "Date Claim Made"

    "Status"
    "Refer to Underwriters"
    "Denial"
                                        ;"Advance Payments"
    "Claimant Name"


    "Paid This Period - Indemnity"
    "Paid This Period Fees"
    "Previously Paid - Indemnity"
    "Previously Paid - Fees"
    "Outstanding Reserve - Indemnity"
    "Outstanding Reserve - Fees"

    "Change this month - Indemnity"
    "Change this month - Fees"

    "Total Incurred - Indemnity"
    "Total Incurred - Fees"

    "Coverholder PIN"
    "Start Date"
    "Type of Insurance (Direct, or Type of RI)"
    "Policy or Group Ref"

    "Insured Address"
    "Insured Postal Code"

    "Risk Address"
    "Risk Postal Code"

    "Deductible Amount"
    "Deductible Basis"
    "Sums Insured Amount"

    "Loss Address"
    "Loss Postal Code"

    "Close Date"
    "Cat Code"
    "Cat Name"

    "Paid This Period Expert Expense"
    "Paid This Period Legal"
    "Paid This Period Adjusting"

    "Paid this month - Defence Fee"
    "Paid This Period TPA"

    "Expert Fees Previously Paid"
    "Legal Fees Previously Paid"
    "Adjusting Fees Previously Paid"
    "Previously Paid - Defence Fees"
    "TPA Fees Previously Paid"

    "Outstanding Reserve Expert"
    "Outstanding Reserve Legal"
    "Outstanding Reserve Adjusting"
    "Reserve - Defence Fees"
    "Outstanding Reserve TPA"
    "Incurred"

    "% Ceded (Reinsurance)"


    "Expert Role"
    "Expert Firm / Company Name"
    "Expert Reference No etc"
    "Expert Address"
    "Expert State, Province, Territory, Canton etc"
    "Expert Postal Code"
    "Expert Country"
    "Expert Notes"

    "Date Claim Opened"
    "Ex Gratia (Y/N)")
	  (list
	   "Claim First Notification Acknowledgement Date"
	   "Date First Reserves Established"
	   "Diary Date"
	   "Peer Review Date"
       "Broker"
       "Date Claim Amount Agreed"
       "Date Claim Paid"
       "Date Reopened"
       "Recovery"
       "Recovery Total to date"
       "Loss Code Description"
       "Last Update"
       "Risk Number"
       "Year of Account (YOA)"
       "Industry Code"
       "Date Claim Denied"
       "Reason for Denial"
       )))

(defun lloyds-v5-bordereau-fields ()
  (let ((*default-bordereau-fields* *lloyds-v5-bordereau-fields*))
    (default-bordereau-fields)))

(defparameter *accellerant-bordereau-fields*
  (append
   (list
    "Field"
    "Start Date"
    "End Date"
    "Policy Effective Date"
    "Policy Expiration Date"
    "Coverholder Name"
    "Coverholder PIN"
    "TPA Name"
    "Policy"
    "Claim"
    "Accelerant Unique Claim Ref"
    "Unique Market Reference"
    "Agreement No"
    "Section Number";; <--- ???
    "Lloyd's Risk Code"
    "Line of Business"
    "Risk Type"
    "Annual Statement Line of Business"
    "Industry Code"
    "IBC Segment"
    "OSFI LOB" "Product" "Trade"
    "Original Currency"
    "Settlement Currency"
    "Rate of Exchange"
    "Risk Inception Date"
    "Risk Expiration Date"
    "Period of Cover"
    "Type of Insurance (Direct, or Type of RI)"
    "Deductible Amount"
    "Deductible Basis"
    "Sums Insured Amount"
    "Name or Reg No of Aircraft Vehicle, Vessel etc"
    "% Ceded (Reinsurance)"
    "Insured"
    "Province"
    "Insured Country"
    "Insured Address"
    "Insured Postal Code"
    "Location ID" ;; <--- ???
    "Risk Address";; <--- ???
    "Risk Province";; <--- ???
    "Risk Postal Code";; <--- ???
    "Risk Country";; <--- ???
    "Loss Country"
    "Loss Address"
    "Loss Province"
    "Loss Postal Code"
    "Loss County"
    "State of Filing";; <--- ???
    "Claimant Name"
    "Cause of Loss"
    "Cause of Loss Code 2"
    "Amount Claimed"
    "Loss Description"
    "Status"
    "Denial"
    "Reason for Denial"
    "Cat Name"
    "Cat Code"
    "Ex Gratia (Y/N)" 
    "Claim not paid as within excess"
    "PCS Code"
    "Medicare United States Bodily Injury"
    "Medicare Eligibility Check Performance"
    "Medicare Outcome of Eligilibility Status Check"
    "Medicare Conditional Payments"
    "Medicare MSP Compliance Services"
    "A&H Plan"
    "Patient Name"
    "Treatment Type"
    "Country of Treatment"
    "Date of Treatment"
    "Expert Role"
    "Expert Firm / Company Name"
    "Expert Reference No etc"
    "Expert Address"
    "Expert State, Province, Territory, Canton etc"
    "Expert Postal Code"
    "Expert Country"
    "Expert Notes"
    "Date of Loss"
    "Date of Loss to"
    "Date Claim Made"
    "Date Claim Opened"
    "Refer to Underwriters"
    "Close Date"
    "Date Coverage Confirmed"
    "Date Claim Amount Agreed"
    "Date Claim Paid"
    "Date Fees Paid"
    "Date Reopened"
    "Date of Subrogation"
    "Date Claim Denied"
    "Date claim withdrawn"
    "Claim First Notification Acknowledgement Date"
    "Date First Reserves Established"
    "Diary Date"
    "Peer Review Date"
    "Year of Account (YOA)"
    "Accident Date"
    "Jurisdiction of the claim"
    "General nature of loss" 
    "Body functions or structures affected"
    "Severity of loss"
    "In Litigation (Y/N)"
    "Heads of damage – past economic loss"
    "Heads of damage – future economic loss"
    "Heads of damage - Past medical, hospital"
    "Heads of damage - Future medical, hospital" 
    "Heads of damage - Future caring services" 
    "Heads of damage - General damages"
    "Heads of damage – Interest"

    "Paid this month - Indemnity" ;; DK
    "Paid this month - Defence Fees" ;; DL
    "Paid this month - Adjusters Fees" ;; DM
    "Paid this month - TPA Fees" ;; DN
    "Paid this month - Attorney Coverage Fees" ;; DO
    "Paid this month - Attorney Monitoring Fees" ;; DP 
    "Paid this month - Other Fees and Expenses" ;; DQ
    "TOTAL PAID MOVEMENTS THIS MONTH" ;; DR

    "Salvage Received this Month" ;; DS
    "Recovery Received this Month" ;; DT
    "TOTAL SALVAGE + RECOVERY RECEIVED MOVEMENTS THIS MONTH" ;; DU

    "Indemnity Reserve movement this month" ;; DV
    "Defence Fee Reserve movement this month" ;; DW
    "Adjuster Fee Reserve Movement this Month" ;; DX
    "TPA Fee Reserve Movement this Month" ;; DY
    "Attorney Coverage Fee Reserve Movement this Month" ;; DZ
    "Attorney Monitoring Fee Reserve Movement this Month" ;; EA
    "Other Fee & Expenses Reserve Movement this Month" ;; EB

    "Total Reserve Movement This Month" ;; EC

    "Salvage Reserve Movement this Month" ;; ED N/A
    "Recovery Reserve Movement this Month" ;; EE N/A
    "Total Salvage & Recovery Reserve Movements this Month" ;; EF N/A

     "TOTAL INDEMNITY MOVEMENT THIS MONTH" ;; EG
     "TOTAL DEFENCE MOVEMENT THIS MONTH" ;;EH N/A
     "TOTAL ADJUSTER FEE MOVEMENT THIS MONTH" ;; EI
     "TOTAL TPA FEE MOVEMENT THIS MONTH" ;; EJ
     "TOTAL ATTORNEY COVERAGE FEE MOVEMENT THIS MONTH"  ;; EK
     "TOTAL ATTORNEY MONITORING FEE MOVEMENT THIS MONTH" ;; EL N/A
     "TOTAL OTHER FEE & RESERVES MOVEMENT THIS MONTH"
     "TOTAL MOVEMENT IN PERIOD"

     "TOTAL SALVAGE AND RECOVERY RECEIVED MOVEMENTS THIS MONTH"

     "Total Paid To Date - Indemnity"
     "Total Paid To Date - Defence Fees"
     "Total Paid To Date - Adjusters Fees"
     "Total Paid To Date - TPA Fees"
     "Total Paid To Date - Attorney Coverage Fees"
     "Total Paid To Date - Attorney Monitoring Fees"
     "Total Paid To Date -  Other Fees and Expenses"
     "Total Paid To Date" ;; EW

     "Total Received To Date - Salvage"
     "Total Received to Date - Recoveries"
     "Total Received to Date"

     "Total Reserved To Date - Indemnity" ;; FA
     "Total Reserved To Date - Defence Fees"
     "Total Reserved To Date - Adjusters Fees"
     "Total Reserved To Date - TPA Fees"
     "Total Reserved To Date - Attorney Coverage Fees" ;; FE
     "Total Reserved To Date - Attorney Monitoring Fees"  ;; FF
     "Total Reserved To Date - Other Fees and Expenses"
     "Total To Date - Reserved"

     "Reserved to Date - Salvage"
     "Reserved to Date - Recoveries" 
     "Total to Date Salvage & Recovery Reserve" ;; FK

     "Total Incurred - Indemnity"
     "Total Incurred - Defense Fees"
     "Total Incurred - Adjusters Fees"
     "Total Incurred - TPA Fees"
     "Total Incurred - Attorney Coverage Fees"
     "Total Incurred - Attorney Monitoring Fees"
     "Total Incurred - Other Fees and Expenses"
     "TOTAL INCURRED EXCLUDING RECOVERIES"
     "Total Recoveries Received"
     "TOTAL INCURRED INCLUDING RECOVERIES"
     "Recovery Reserves"
     "Excess Received"
     "Date Excess Received"
     ;; ;; ACcessetrasdadaer acceller
    
       "Last Update"
  
       )
   (list)))

(defun accellerant-bordereau-fields ()
  (let ((*default-bordereau-fields* *accellerant-bordereau-fields*))
    (default-bordereau-fields)))

(defparameter *Ascot-lloyds-v5-bordereau-fields*
  (append
   (list
    "Field"
    "Coverholder Name"
    "TPA Name"
    "Agreement No"
    "Contract Number"
    "Contract Effective"
    "Contract Expiry"
    "End Date"
    "Risk Type"
    "Class of Business"
    "Lloyd's Risk Code"
    "Section Number"
    "Original Currency"
    "Settlement Currency"
    "Rate of Exchange"
    ;"Row Number"

    "Policy"
    "Claim"
    "Insured"
    "Province"
    "Insured Country"

    "Risk Province"
    "Risk Country"
    "Effective"
    "Expiry"
    "Period of Cover"

    "Loss Province"
    "Loss Country"
    "Loss Code"
    "Loss Description"
    "Date of Loss"
    "Date of Loss to"
    "Date Claim Made"

    "Status"
    "Refer to Underwriters"
    "Denial"
    "Date Claim Denied"
    "Reason for Denial"
    "Claimant Name"


    "Paid This Period - Indemnity"
    "Paid This Period Fees"
    "Previously Paid - Indemnity"
    "Previously Paid - Fees"
    "Outstanding Reserve - Indemnity"
    "Outstanding Reserve - Fees"

    "Change this month - Indemnity"
    "Change this month - Fees"

    "Total Incurred - Indemnity"
    "Total Incurred - Fees"

    "Coverholder PIN"
    "Start Date"
    "Type of Insurance (Direct, or Type of RI)"
    "Policy or Group Ref"

    "Insured Address"
    "Insured Postal Code"

    "Risk Address"
    "Risk Postal Code"

    "Deductible Amount"
    "Deductible Basis"
    "Sums Insured Amount"

    "Loss Address"
    "Loss Postal Code"

    "Close Date"
    "Cat Code"
    "Cat Name"

    "Paid This Period Expert Expense"
    "Paid This Period Legal"
    "Paid This Period Adjusting"

    "Paid this month - Defence Fee"
    "Paid This Period TPA"

    "Expert Fees Previously Paid"
    "Legal Fees Previously Paid"
    "Adjusting Fees Previously Paid"
    "Previously Paid - Defence Fees"
    "TPA Fees Previously Paid"

    "Outstanding Reserve Expert"
    "Outstanding Reserve Legal"
    "Outstanding Reserve Adjusting"
    "Reserve - Defence Fees"
    "Outstanding Reserve TPA"
    "Incurred"

    "% Ceded (Reinsurance)"


    "Expert Role"
    "Expert Firm / Company Name"
    "Expert Reference No etc"
    "Expert Address"
    "Expert State, Province, Territory, Canton etc"
    "Expert Postal Code"
    "Expert Country"
    "Expert Notes"

    "Date Claim Opened"
    "Ex Gratia (Y/N)")
	  (list
	   "Claim First Notification Acknowledgement Date"
	   "Date First Reserves Established"
	   "Diary Date"
	   "Peer Review Date"
       "Broker"
       "Date Claim Amount Agreed"
       "Date Claim Paid"
       "Date Reopened"
       "Recovery""Recovery Total to date"
       "Loss Code Description"
       "Last Update"
       "Risk Number"
       "Year of Account (YOA)"
       "Industry Code"
       )))

(defun Ascot-lloyds-v5-bordereau-fields ()
  (let ((*default-bordereau-fields* *ascot-lloyds-v5-bordereau-fields*))
    (default-bordereau-fields)))

(defparameter *HUB-Dale-bordereau-fields*
  (list
   ;; "Field"
   "Coverholder Name"
   "TPA Name"
   ;;    "Agreement No"
   "Contract Number"
   "Contract Effective"
   "Contract Expiry"
   "End Date"
   "Risk Type"
   ;; "Class of Business"
   ;; "Lloyd's Risk Code"
   ;; "Section Number"
   "Original Currency"
   ;; "Settlement Currency"
   ;; "Rate of Exchange"
   ;; "Row Number"

   "Policy"
   "Claim"
   "Insured"
   "Province"
   "Insured Country"

   ;; "Risk Province"
   ;; "Risk Country"
   "Effective"
   "Expiry"
   ;; "Period of Cover"

   "Loss Province"
   "Loss Country"
   "Loss Code"
   "Loss Description"
   "Date of Loss"
   ;; "Date of Loss to"
   "Date Claim Made"

   "Status"
   "Refer to Underwriters"
   "Denial"
                                        ;"Advance Payments"
   "Claimant Name"


   "Paid This Period - Indemnity"
   "Paid This Period Fees"
   "Previously Paid - Indemnity"
   "Previously Paid - Fees"

   "Change In Reserve This Period"

   "Outstanding Reserve - Indemnity"
   "Outstanding Reserve - Fees"

   ;; "Change this month - Indemnity"
   ;; "Change this month - Fees"

   "Total Incurred - Indemnity"
   "Total Incurred - Fees"

   ;; "Coverholder PIN"
   "Start Date"
   ;; "Type of Insurance (Direct, or Type of RI)"
   ;; "Policy or Group Ref"

   "Insured Address"
   "Insured Postal Code"

   ;; "Risk Address"
   ;; "Risk Postal Code"

   "Deductible Amount"
   ;; "Deductible Basis"
   ;; "Sums Insured Amount"

   ;; "Loss Address"
   ;; "Loss Postal Code"

   "Close Date"
   "Recovery""Recovery Total to date"
   "Cat Code"
   "Cat Name"

   "Paid This Period Expert Expense"
   "Paid This Period Legal"
   "Paid This Period Adjusting"

   ;; "Paid this month - Defence Fee"
   "Paid This Period TPA"

   "Expert Fees Previously Paid"
   "Legal Fees Previously Paid"
   "Adjusting Fees Previously Paid"
   ;; "Previously Paid - Defence Fees"
   "TPA Fees Previously Paid"

   "Outstanding Reserve Expert"
   "Outstanding Reserve Legal"
   "Outstanding Reserve Adjusting"
   ;; "Reserve - Defence Fees"
   "Outstanding Reserve TPA"
   "Incurred"

   ;;"% Ceded (Reinsurance)"


   ;; "Expert Role"
   ;; "Expert Firm / Company Name"
   ;; "Expert Reference No etc"
   ;; "Expert Address"
   ;; "Expert State, Province, Territory, Canton etc"
   ;; "Expert Postal Code"
   ;; "Expert Country"
   ;; "Expert Notes"

   "Date Claim Opened"
   ;;    "Ex Gratia (Y/N)"
   ;; "Claim First Notification Acknowledgement Date"
   "Date First Reserves Established"
   "Diary Date"
   ;; "Peer Review Date"
   "Subscription %"
   ))

(defun HUB-Dale-bordereau-fields ()

  (let ((*default-bordereau-fields* *HUB-Dale-bordereau-fields*))
    (default-bordereau-fields)))

(let* ((eff (copy-list (cddr *default-bordereau-fields*)))
       (eff `(,(nth 0 eff)
	       ,(nth 1 eff)
	       ,(nth 2 eff)
	       ,(nth 3 eff)
	       ,(nth 4 eff)
	       ,(nth 5 eff)
	       ,(nth 6 eff)
	       "Insured Address"
	       "Insured Home Phone"
	       "Insured Work Phone"
	       "Insured Fax Phone"
	       "Insured Cell Phone"
	       "Insured Email"

	       ,@(nthcdr 7 eff))))

  (defparameter *agency-bordereau-fields*
    `(,(first *default-bordereau-fields*)
       ,(second *default-bordereau-fields*)
       "Contract Number"

       ,@eff)))

(defun agency-bordereau-fields ()

  (let ((*default-bordereau-fields* *agency-bordereau-fields*))
    (default-bordereau-fields)))

(defparameter *hiscox-bordereau-fields*
  (append
   (list
    "Start Date"
    "End Date"
    "Contract Number"
    "Section Number";; <--- ???
    "Contract Effective"
    "Contract Expiry"
    "Year of Account (YOA)"
    "Coverholder Name"
    "TPA Name"
    "Lloyd's Risk Code"
    "Class of Business"
    "Risk Type"
    "Policy"
    "Effective"
    "Expiry"

    "Insured"
    "Insured Street"
    "Province"
    "Insured Postal Code"
    "Insured Country"

    "Location ID" ;; <--- ???
    "Risk Address";; <--- ???
    "Risk Province";; <--- ???
    "Risk Postal Code";; <--- ???
    "Risk Country";; <--- ???

    "Deductible Amount"
    "State of Filing";; <--- ???
    "Sums Insured Amount";; <--- ???

    "Claim"
    "Status"
    "Refer to Underwriters"
    "Denial"

    "PCS Code"
                                        ;"Advance Payments"



    "Original Currency"
    "Settlement Currency"
    "Rate of Exchange"
    ;"Row Number"

    "Loss Location"
    "Loss Province"
    "Loss County"
    "Loss Postal Code"
    "Loss Country"
    "Loss Code"
    "Loss Description"
    "Date of Loss"
    "Date of Loss to"
   ;; "Date Claim Made"

    "Claimant Name"


   ;; "Period of Cover"
    "Paid This Period - Indemnity"
    "Paid This Period Fees"
    "Previously Paid - Indemnity"
    "Previously Paid - Fees"
    "Outstanding Reserve - Indemnity"
    "Outstanding Reserve - Fees"


    "Total Incurred - Indemnity"
    "Total Incurred - Fees"

    "Date First Advised Notification Date"
    "Close Date"
    "Cat Code"
    "Cat Name"


    "Paid This Period Expert Expense"
    "Paid This Period Legal"
    "Paid This Period Adjusting"
    "Paid this month - Defence Fee"
    "Paid This Period TPA"

    "Expert Fees Previously Paid"
    "Legal Fees Previously Paid"
    "Adjusting Fees Previously Paid"
    "Previously Paid - Defence Fees"
    "TPA Fees Previously Paid"



    "Outstanding Reserve Expert"
    "Outstanding Reserve Legal"
    "Outstanding Reserve Adjusting"
    "Reserve - Defence Fees"
    "Outstanding Reserve TPA"

    "Incurred"


    "Expert Role"
    "Expert Firm / Company Name"

    "Date Claim Opened"
    "Date Coverage Confirmed"
    "Date Claim Amount Agreed"
    "Date Claim Paid"
    "Date Fees Paid"
    "Date Reopened"
    "Date of Subrogation"


    "Date Claim Denied"
    "Reason for Denial"
    "Ex Gratia (Y/N)"
    "In Litigation (Y/N)"
	  "Diary Date"
    "Date First Reserves Established"
    "Claim First Notification Acknowledgement Date"

    "Recovery Total to date"
    "Outstanding Deductible"
    )
	 (list
    )))

(defun hiscox-bordereau-fields ()
  ;;(error "WTF")
  (let ((*default-bordereau-fields* *hiscox-bordereau-fields*))
    (default-bordereau-fields)))

(defparameter *claim-bordereau-fields*
  '("Risk Type"
    "Policy"
    "Effective"
    "Expiry"
    "Branch"
    "Underwriter"
    "Broker"
    "Claim"
    "Insured"
    "Province"
    "Date of Loss"
    "Loss Code"
    "Industry Code"

    "Outstanding Reserve Total"
    "Change In Reserve This Period"

    "Loss this period"
    "Expense this Period"
    "Loss Total"
    "Expense Total"
    "Total Paid Gross"
    "Total Paid Net"

    "Incurred"
    "Salvage/Recovery"
    "Above Authority"
    "Status"
    "Open Date"
    "Close Date"))

(defun claim-bordereau-fields ()
  (let ((*default-bordereau-fields* *claim-bordereau-fields*))
    (default-bordereau-fields)))

(defparameter *Inter-Hannover-bordereau-fields*
  '("Risk Type"
    "Policy"
    "Effective"
    "Expiry"

    "Claim"
    "Insured"
    "Province"
    "Date of Loss"
    "Loss Code"


    "Outstanding Reserve Adjusting"
    "Outstanding Reserve TPA"
    "Outstanding Reserve Legal"
    "Outstanding Reserve Expert Expense"
    "Outstanding Reserve Indemnity"
    "Outstanding Reserve Total"

    "Change In Reserve This Period"

    "Loss this period"
    "Expense this Period (i3)"
    "Paid This Period TPA"

    "Loss Total"
    "Expense Total (i3)"
    "Total Paid TPA"


    "Total Paid Gross"
    "Total Paid Net"

    "Incurred"
    "Salvage/Recovery"
    "Above Authority"
    "Status"
    "Close Date"
    "i3 Policy Number"
    ))

(defun inter-hannover-bordereau-fields ()
  (let ((*default-bordereau-fields*
	 *inter-hannover-bordereau-fields*))
    (default-bordereau-fields)))

(defparameter *white-oak-bordereau-fields*
  '(
    "Policy/Certificate Reference"
    "Claim"
    "Insured"
    "Class of Business"
    "Policy Form"
    "Policy Section"
    "Effective"
    "Expiry"
    "Province"
    "Broker"
    "Scheduled Insured Value of Vehicle"

    ;; ** Loss Details

    "Date of Loss"
    "Open Date"
    "Loss Province"
    "Loss Location"
    "Cat Name"
    "Cat Code"
    "Type of Loss"
    "White Oak Cause of Loss"
    "White Oak Loss Description"
    "Type of Cargo"
    "Number of Units"
    "Total Loss"

    "Driver Name"
    "Driver Age"

    ;; ** vehicle details
    "Vehicle Make"
    "Vehicle Model"
    "Vehicle Year"
    "Vehicle VIN/Serial/Registration"

   ;;  ** Paid this month

    "Paid This Period Indemnity"
    "Paid This Period Tow Costs"
    "Paid This Period Storage"
    "Salvage This Period"
    "Subrogation This Period"
    "Paid This Period TPA"
    "Paid This Period Adjusting"
    "Paid This Period Disbursement"
    "Paid This Period Legal"
    "Paid This Period Expert Expense"
    "Paid This Period Total"
    "Paid This Period Total Excluding"
    "Indemnity Cash Call This Period"
    "Fee Cash Call This Period"

    ;; Previously Paid
    "Indemnity Previously Paid"
    "Tow Costs Previously Paid"
    "Storage Previously Paid"
    "Salvage Previously Received"
    "Subrogation Previously Received"
    "TPA Fees Previously Paid"
    "Adjusting Fees Previously Paid"
    "Disbursement Previously Paid"
    "Legal Fees Previously Paid"
    "Expert Fees Previously Paid"
    "Total Fees Previously Paid"
    "Total Previously Payments"

    ;; Paid To Date
    "Total Paid Indemnity"
    "Tow Costs Paid to Date"
    "Storage Paid to Date"
    "Total Received Salvage"
    "Total Received Subrogation"
    "Total Paid TPA"
    "Total Paid Adjusting"
    "Disbursement Paid to Date"
    "Total Paid Legal"
    "Total Paid Expert Expense"
    "Total Fees Paid To Date"
    "Total Paid"

    ;; Reserves
    "Outstanding Reserve Indemnity"
    "Outstanding Reserve Tow Costs"
    "Outstanding Reserve Storage Costs"
    "Outstanding Fees Reserve"
    "Outstanding Reserve Total"

    ;; incurred
    "Incurred"

    ;; claim status
    "Date Claim Paid"
    "Status"
    "Update"
    "ROR Issued (Y/N)"
    "Denial (Y/N)"
    "Ex Gratia (Y/N)"
    "In Litigation (Y/N)"
    "Successful Subrogation Potential (Y/N)"
    "Subrogation Status Notes"

    "Within Authority"
    "Risk Type" ))

(defun white-oak-bordereau-fields ()
  (let ((*default-bordereau-fields*
	 *white-oak-bordereau-fields*))
    (default-bordereau-fields)))

(defparameter *llyods-bordereau-fields*
  '(
    ;; ** Contract Details
    "Coverholder Name"
    "TPA Name"
    "Agreement No"
    "Contract Number"
    "Contract Effective"
    "Contract Expiry"
    "End Date"
    "Risk Type"
    "Lloyd's Risk Code"
   "Section Number"
    "Original Currency"
    "Settlement Currency"
    "Rate of Exchange"
    "Row Number"

    "Policy"
    "Claim"
    "Insured"
    "Province"
    "Insured Country"

    "Risk Province"
    "Risk Country"
    "Effective"
    "Expiry"
    "Period of Cover"

    "Loss Province"
    "Lloyd's Loss Location"
    "Loss Code"
    "Loss Description"
    "Date of Loss"
    "Date of Loss to"
    "Date Claim Made"

    "Status"
    "Refer to Underwriters"
    "Denial"
                                        ;"Advance Payments"
    "Claimant Name"

    "Loss County"
    "State of Filing"
    "PCS Code"
    "Medicare United States Bodily Injury"
    "Medicare Eligibility Check Performance"
    "Medicare Outcome of Eligilibility Status Check"
    "Medicare Conditional Payments"
    "Medicare MSP Compliance Services"

    "Paid This Period - Indemnity"
    "Paid This Period Fees"
    "Previously Paid - Indemnity"
    "Previously Paid - Fees"
    "Outstanding Reserve - Indemnity"
    "Outstanding Reserve - Fees"

    "Change this month - Indemnity"
    "Change this month - Fees"

    "Total Incurred - Indemnity"
    "Total Incurred - Fees"

    "Coverholder PIN"
    "Start Date"
    "Type of Insurance (Direct, or Type of RI)"
    "Policy or Group Ref"

    "Insured Address"
    "Insured Postal Code"

    "Location ID"
    "Risk Address"
    "Risk Postal Code"

    "Deductible Amount"
    "Deductible Basis"
    "Sums Insured Amount"

    "Loss Address"
    "Loss Postal Code"

    "Open Date"
    "Close Date"
    "Cat Code"
    "Cat Name"

    "Paid This Period Expert Expense"
    "Paid This Period Legal"
    "Paid This Period Adjusting"
    "Paid this month - Defence Fee"
    "Paid This Period TPA"

    "Expert Fees Previously Paid"
    "Legal Fees Previously Paid"
    "Adjusting Fees Previously Paid"
    "Previously Paid - Defence Fees"
    "TPA Fees Previously Paid"

    "Outstanding Reserve Expert"
    "Outstanding Reserve Legal"
    "Outstanding Reserve Adjusting"
    "Reserve - Defence Fees"
    "Outstanding Reserve TPA"
    "Incurred"

    "Name or Reg No of Aircraft Vehicle, Vessel etc"
    "% Ceded (Reinsurance)"
    "A&H Plan"
    "Patient Name"
    "Treatment Type"
    "Country of Treatment"
    "Date of Treatment"

    "Expert Role"
    "Expert Firm / Company Name"
    "Expert Reference No etc"
    "Expert Address"
    "Expert State, Province, Territory, Canton etc"
    "Expert Postal Code"
    "Expert Country"
    "Expert Notes"

    "Date Claim Opened"
    "Date Coverage Confirmed"
    "Date Claim Amount Agreed"
    "Date Claim Paid"
    "Date Fees Paid"
    "Date Reopened"
    "Date of Subrogation"

    "Claimant Address"
    "Claimant Postal Code"
    "Claimant Country"

    "Date Claim Denied"
    "Reason for Denial"
    "Amount Claimed"
    "Date claim withdrawn"
    "Ex Gratia (Y/N)"
    "Date Claim Closed"
    "Claim not paid as within excess"
   "Recovery""Recovery Total to date"
    ))

(defun llyods-bordereau-fields ()
  (let ((*default-bordereau-fields*
	 *llyods-bordereau-fields*))
    (default-bordereau-fields)))

(defparameter *arch-bordereau-fields*
  '(
    ;; ** Contract Details
    "Coverholder Name"
    "TPA Name"
    "Agreement No"
    "Contract Number"
    "Contract Effective"
    "Contract Expiry"
    "End Date"
    "Risk Type"
    "Branch"
    "Underwriter"

    "Lloyd's Risk Code"
    "Section Number"
    "Original Currency"
    "Settlement Currency"
    "Rate of Exchange"
    "Row Number"

    "Policy"
    "Claim"
    "Insured"
    "Province"
    "Insured Country"

    "Risk Province"
    "Risk Country"
    "Effective"
    "Expiry"
    "Period of Cover"

    "Loss Province"
    "Lloyd's Loss Location"
    "Loss Code"
    "Loss Description"
    "Date of Loss"
    "Date of Loss to"
    "Date Claim Made"

    "Status"
    "Refer to Underwriters"
    "Denial"
                                        ;"Advance Payments"
    "Claimant Name"

    "Loss County"
    "State of Filing"
    "PCS Code"
    "Medicare United States Bodily Injury"
    "Medicare Eligibility Check Performance"
    "Medicare Outcome of Eligilibility Status Check"
    "Medicare Conditional Payments"
    "Medicare MSP Compliance Services"

    "Paid This Period - Indemnity"
    "Paid This Period Fees"
    "Previously Paid - Indemnity"
    "Previously Paid - Fees"
    "Outstanding Reserve - Indemnity"
    "Outstanding Reserve - Fees"

    "Change this month - Indemnity"
    "Change this month - Fees"

    "Total Incurred - Indemnity"
    "Total Incurred - Fees"

    "Coverholder PIN"
    "Start Date"
    "Type of Insurance (Direct, or Type of RI)"
    "Policy or Group Ref"

    "Insured Address"
    "Insured Postal Code"

    "Location ID"
    "Risk Address"
    "Risk Postal Code"

    "Deductible Amount"
    "Deductible Basis"
    "Sums Insured Amount"

    "Loss Address"
    "Loss Postal Code"

    "Open Date"
    "Close Date"
    "Cat Code"
    "Cat Name"

    "Paid This Period Expert Expense"
    "Paid This Period Legal"
    "Paid This Period Adjusting"
    "Paid this month - Defence Fee"
    "Paid This Period TPA"

    "Expert Fees Previously Paid"
    "Legal Fees Previously Paid"
    "Adjusting Fees Previously Paid"
    "Previously Paid - Defence Fees"
    "TPA Fees Previously Paid"

    "Outstanding Reserve Expert"
    "Outstanding Reserve Legal"
    "Outstanding Reserve Adjusting"
    "Reserve - Defence Fees"
    "Outstanding Reserve TPA"
    "Incurred"

    "Name or Reg No of Aircraft Vehicle, Vessel etc"
    "% Ceded (Reinsurance)"
    "A&H Plan"
    "Patient Name"
    "Treatment Type"
    "Country of Treatment"
    "Date of Treatment"

    "Expert Role"
    "Expert Firm / Company Name"
    "Expert Reference No etc"
    "Expert Address"
    "Expert State, Province, Territory, Canton etc"
    "Expert Postal Code"
    "Expert Country"
    "Expert Notes"

    "Date Claim Opened"
    "Date Coverage Confirmed"
    "Date Claim Amount Agreed"
    "Date Claim Paid"
    "Date Fees Paid"
    "Date Reopened"
    "Date of Subrogation"

    "Claimant Address"
    "Claimant Postal Code"
    "Claimant Country"

    "Date Claim Denied"
    "Reason for Denial"
    "Amount Claimed"
    "Date claim withdrawn"
    "Ex Gratia (Y/N)"
    "Date Claim Closed"
    "Claim not paid as within excess"
    ))

(defun arch-bordereau-fields ()
  (let ((*default-bordereau-fields*
	 *arch-bordereau-fields*))
    (default-bordereau-fields)))






;; 3)            information
;;                 -Policy #
;;                 -Policy inception date
;;                 -Branch
;;                 -Underwriters
;;                 -Broker
;;                 -Insured
;;                 -Province
;;                 -Date of Loss
;;                 -Loss Code or Details of Loss
;;                 -Loss this Period
;;                 -Expense this Period
;;                 -Loss Total
;;                 -Expense Total
;;                 -Initial Reserve
;;                 -Outstanding Reserve
;;                 -Total Paid
;;                 -Incurred
;;                 -Status



(defun heading-as (heading)
  `(:as ,(bordereau-heading-s-sql heading)
	,(if (listp (bordereau-heading-name heading))
	     (bordereau-heading-name heading)
	     `(:raw ,(concatenate
		      'string "\""
		      (bordereau-heading-name heading)
		      "\"")))))

;; '("Dolden Wallace Folick"
;;      (:sum dolden.amount)
;;      :total :no-group)
;;  '(:inner-join
;; 			    (:as claim-transaction dolden)
;; 			    :on (:and (:= claim.claim-id
;; 				       dolden.claim-id)
;;				 (:ilike (:raw "person_name(find_person(payee_id))")
;;				  "Dolden Wallace Folick%"
;;	))


(defun sql-query-fn (&key (row-reader
			      'cl-postgres:alist-row-reader))
  (lambda (query &rest args)
    (cl-postgres:prepare-query
     postmodern:*database*
     "" (etypecase query
	  (string query)
	  (cons (s-sql:sql-compile query))))
    (cl-postgres:exec-prepared
     postmodern:*database* "" args
     row-reader)))

(defun maxclaims/report/bordereau:find-bordereau
    (contract-id
     &key
       (run t)
       (prefix-currency t)
       (show-totals t)
       (where `(:= contract.contract-id ,contract-id))
       (fields  (default-bordereau-fields))
       (start-date "2012-01-01")
       (end-date "2012-12-31")
       risk-type       (extra-with NIL)
       (extra-from NIL)
       (select "SELECT *
                 FROM bordereau_and_totals
                 AS b ORDER BY \"Claim\" ")
       (query-fn (sql-query-fn)))
  (assert (or (not risk-type) (stringp risk-type)))
  (let ((report
	  (format
	   nil "WITH
                    bordereau AS (~A),
                    bordereau_totals AS (~A),
                    bordereau_text AS (~A),
                    bordereau_and_totals AS
                     (SELECT * FROM bordereau_text
                       UNION SELECT * FROM bordereau_totals)
                   ~{,~A~} ~A"
	   ;; BORDEREAU AS (
	   (s-sql:sql-compile
	    `(:limit
	      (:select
	       ,@(mapcar #'heading-as fields)
	       :from (:as contract contract)
	       :inner-join risk
	       :on (:= risk.contract-id contract.contract-id)
	       :inner-join claim
	       :on (:and (:= risk.risk-id claim.risk-id)
			 (:<= claim.date-of-loss
			      $2))
	       :left-join (:as policy policy)
	       :on (:= risk.policy-id policy.policy-id)
	       :left-join (:as claim-ibc-code cibc)
	       :on (:= claim.claim-id cibc.claim-id)
	       :left-join (:as ibc-code ibc)
	       :on (:= cibc.industry ibc.industry)
	       ,@extra-from
	       :where (:and (< claim.open_date $2) ,where))
	      :null))
	   ;;, BORDEREAU_TOTALS AS (
	   (s-sql:sql-compile
	    (if (or (not show-totals) (not prefix-currency))
		'(:limit (:select * :from bordereau) 0) 
	    (let ((group-by '()))

	      `(:select
		,@(loop
		    for h in fields
		    :collect
		    (if (bordereau-heading-total h)
			(if (eq (bordereau-heading-total h) :group)
			    (let ((f `(:raw ,(format nil "bordereau.\"~A\"" (bordereau-heading-name h)))))
			      (push f group-by)
			      `(:type ,f text))
			    `(:\|\| "$" (:type
					 (:sum (:raw ,(concatenate
						       'string "\""
						       (bordereau-heading-name h)
						       "\"")))
					 text)))
			(let ((type (or (bordereau-heading-type h)
					'text)))
			  (if type
			      `(:type :NULL ,type)
			      :null))))
		:from bordereau
		:group-by ,@(nreverse group-by)))))

	   ;; For getting things into spreadsheets it is/was needed to
	   ;; prefix currency with a dollar sign.

	   ;; BORDEREAU_TEXT AS (

	   
	   (s-sql:sql-compile
	    `(:select
	      ,@(loop for h in fields
		      :collect
		      (if (and prefix-currency
			       (bordereau-heading-total h)
			       (not  (eq (bordereau-heading-total h) :group)))
			  `(:AS  (:\|\| "$" (:type  (:raw ,(concatenate
							    'string "\""
							    (bordereau-heading-name h)
							    "\""))
								text))
					     (:raw ,(concatenate
						     'string "\""
						     (bordereau-heading-name h)
						     "\"")))
				      `(:raw ,(concatenate
					       'string "\""
					       (bordereau-heading-name h)
					       "\""))
				      ))
			   :from bordereau))
			(when extra-with
			  (list extra-with))
			select
			)))
    (if run
        (with-adb
          ()
          (funcall query-fn
	                 (identity report)
	                 start-date
	                 end-date))
        (list query-fn report start-date end-date))))

(defun find-bordereau/payee (contract
			     &rest args
			     &key
			       (start-date "2012-01-01")
			       (end-date "2012-12-31")
			       risk-type)
  (if (listp contract)
      (let ((*bordereau-headings*
	     (cons '("Contract Number"
		     contract.contract-number)
		   *bordereau-headings*)))
	(mapcan (lambda (c)
		  (let ((b  (apply #'find-bordereau/payee c args)))
		    (unless (eq b :null) b)))
		contract))
      (let* ((*bordereau-headings*
	      (cons '("Payee"
		      (:hstore  payee-claim-transaction)
		      nil nil hstore)
		    *bordereau-headings*))
	     (bordereau
	      (find-bordereau
	       contract
	       :start-date start-date
	       :end-date end-date
	       :risk-type risk-type
	       :extra-from
	       '(:left-join
		 (:as claim-transaction payee-claim-transaction)
		 :on (:and (:= claim.claim-id
			    payee-claim-transaction.claim-id)
		      (:not (:is-null payee-id))))
	       :extra-with "
payee AS (SELECT *,
       person_name(find_person((\"Payee\" -> 'payee_id')::integer)) AS payee_name,
        (\"Payee\" -> 'amount')::numeric AS amount
FROM bordereau_text),

payee_claim_totals AS (SELECT DISTINCT \"Claim\",
                       payee_name,
                       '$' || SUM(amount) AS amount
                 FROM payee GROUP BY payee_name, \"Claim\"),
total_claim_totals AS
  (SELECT DISTINCT \"Claim\",
          array_agg(payee_claim_totals.payee_name)
           AS payee_names,
          array_agg(payee_claim_totals.amount)
           AS payee_amounts
          FROM payee_claim_totals GROUP BY \"Claim\"),
payee_totals AS (SELECT DISTINCT payee_name,
                 '$' || SUM(amount) AS amount
                 FROM payee GROUP BY payee_name),
total_totals AS (SELECT DISTINCT NULL::INTEGER AS \"Claim\",
                      array_agg(payee_totals.payee_name)
                        AS payee_names,
                      array_agg(payee_totals.amount)
                        AS payee_amounts
                 FROM payee_totals GROUP BY \"Claim\"),
ok_totals AS
  (SELECT * from total_claim_totals
           UNION SELECT * FROM total_totals
)"
	       :select "
SELECT DISTINCT ON (b.\"Claim\") b.*, t.payee_names,t.payee_amounts
FROM bordereau_and_totals AS b
    LEFT JOIN ok_totals AS t
    ON b.\"Claim\" = t.\"Claim\"
    OR (b.\"Claim\" IS NULL
    AND t.\"Claim\" IS NULL)
    ORDER BY b.\"Claim\"
    ")))

	(loop :for row :in bordereau
	   :collect
	   (unless (eql row :NULL)
	     (let* ((l (length row))
		    (report (rest (butlast row 2)))
		    (arrays (nthcdr (- l 2) row)))
	       (append report
		       (let (payees)
			 (dotimes
			     (n (length (let ((a (rest
						  (first arrays))))
					  (unless (eql a :null)
					    a))))
			   (let ((n1 (aref (rest (first arrays)) n))
				 (n2 (aref (rest (second arrays)) n)))
			     (unless (eq :null n2)
			       (push (list n1 n2)
				     payees))))
			 (list (cons "Payee Amount" (coerce (nreverse payees) 'vector))))
		       )))))))
