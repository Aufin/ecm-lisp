CREATE OR REPLACE FUNCTION claim_indemnity_reserve
(claim_id integer,
 end_date timestamp without time zone DEFAULT 'infinity'::timestamp without time zone
)
 -- note this has no start_date because there is no sensible per-period calculation for the reserve           
 RETURNS numeric(20,2) AS $$
SELECT SUM (claim_reserve(heading, $1, $2))
 FROM UNNEST(ARRAY[
 'Crime', 'Contents', 'Building', 'Indemnity', 'Business Interruption',
 'Additional Living Expense', 'Equipment', 'Stock', 'Additional Coverages', 
 'Loss of Rent', 'Tow', 'Storage'
]) AS heading ;

$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION claim_indemnity_paid
(claim_id integer, end_date timestamp without time zone DEFAULT 'infinity'::timestamp without time zone, 
                   start_date timestamp without time zone DEFAULT '-infinity'::timestamp without time zone)
 RETURNS numeric(20,2) AS $$
SELECT SUM (claim_paid(heading, $1, $2, $3))
 FROM UNNEST(ARRAY[
 'Crime', 'Contents', 'Building', 'Indemnity', 'Business Interruption',
 'Additional Living Expense', 'Equipment', 'Stock', 'Additional Coverages', 
 'Loss of Rent', 'Tow', 'Storage'
]) AS heading ;

$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION claim_transaction_is_paid(claim_transaction)
RETURNS BOOLEAN AS $$
WITH ttype AS (
 SELECT description , ARRAY[
'Cheque - Loss', 'Cheque - Expense', 
-- 'Cheque - In House Payout', 
'Cash Call/Scheme Advance'] AS paid
  FROM claim_transaction_type 
  WHERE claim_transaction_type_id = $1.transaction_type_id
) 

SELECT (description = ANY (paid)) FROM ttype;
$$ LANGUAGE SQL;                 

CREATE OR REPLACE FUNCTION claim_transaction_is_indemnity (claim_transaction)
 RETURNS BOOLEAN AS $$
SELECT ($1.transaction_heading = ANY (ARRAY[
 'Crime', 'Contents', 'Building', 'Indemnity', 'Business Interruption',
 'Additional Living Expense', 'Equipment', 'Stock', 'Additional Coverages', 
 'Loss of Rent', 'Tow', 'Storage'
])) ;

$$ LANGUAGE SQL;
