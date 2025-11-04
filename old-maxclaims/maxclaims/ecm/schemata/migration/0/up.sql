ALTER TABLE contract ADD COLUMN syndicate_number TEXT;
ALTER TABLE contract ADD COLUMN year_of_account date;

CREATE OR REPLACE FUNCTION claim_status(claim, timestamp)
 RETURNS TEXT LANGUAGE SQL AS $$
   SELECT CASE 
     WHEN (($1.open_date <= $2 OR $1.open_date::date >= $2)
            AND ($1.close_date IS NULL OR $1.close_date > $2))
     THEN 'Open'
     WHEN (($1.rev_date <= $2 OR $1.rev_date::date  <= $2)
            AND ($1.close_date IS NULL OR $1.close_date < $1.rev_date))
     THEN 'Reopened'
     WHEN ($1.close_date::date <= $2)
     THEN 'Closed'
  END ;
$$;

CREATE OR REPLACE FUNCTION claim_status(integer, timestamp)
 RETURNS TEXT LANGUAGE SQL AS $$
SELECT claim_status(claim.*, $2) FROM claim WHERE claim_id = $1 ;
$$; 


CREATE OR REPLACE FUNCTION public.claim_outstanding_reserve(claim_id integer, end_date timestamp without time zone DEFAULT 'infinity'::timestamp without time zone)
 RETURNS numeric
 LANGUAGE sql AS $function$
   -- note this has no start_date because there is no sensible per-period calculation for the reserve
   SELECT 
   CASE WHEN (claim_status($1, $2) = 'Closed')
         THEN 0
         ELSE
         -- fixme: this calc and the one in claim_transaction_bordereau differ
         claim_reserve ($1, $2)
         -- fixme: should this calculation be claim_??_expense?
         - ((claim_cheque_loss ($1, $2) + claim_cheque_expense ($1, $2) 
             + claim_cheque_in_house ($1, $2) + claim_cash_call($1, $2))
              - (claim_salvage ($1, $2) + claim_subrogation ($1, $2)))
        END AS outstanding_reserve;
$function$ ;

CREATE OR REPLACE FUNCTION public.claim_outstanding_reserve(heading text, claim_id integer, end_date timestamp without time zone DEFAULT 'infinity'::timestamp without time zone)
 RETURNS numeric
 LANGUAGE sql
 STABLE
AS $function$
   -- note this has no start_date because there is no sensible per-period calculation for the reserve
   SELECT 
   CASE WHEN (claim_status($2, $3) = 'Closed')
         THEN 0
         ELSE
         -- fixme: this calc and the one in claim_transaction_bordereau differ
         claim_reserve ($1, $2, $3)
         -- fixme: should this calculation be claim_??_expense?
         - ((claim_cheque_loss ($1, $2, $3) + claim_cheque_expense ($1, $2, $3) 
            + claim_cheque_in_house ($1, $2, $3) + claim_cash_call($1, $2, $3))
              - (claim_salvage ($1, $2, $3) + claim_subrogation ($1, $2, $3)))
        END AS outstanding_reserve;
$function$ ;


 CREATE OR REPLACE FUNCTION pillar_3 
   (syndicate_id integer, start_date timestamp, end_date timestamp)
  RETURNS TABLE ("Syndicate Number" text, "Claim Reference" int, "UMR" text,
                 "Risk Code" text, 
                 "Year of account" double precision, "Original Currency" text, 
                 "Claim status at beginning of period " text ,  "Claim status at end of period" text,
                  "Outstanding Claims Amount as at beginning of period" numeric,
                  "Paid in Year amount" numeric,  
                  "Outstanding Claims Amount as at end of period" numeric) 
  LANGUAGE sql AS $$

  SELECT 
  coalesce(syndicate_number, person_name(syndicate_id)) AS "Syndicate Number", 
   claim_id AS "Claim Reference", 
   contract_number AS "UMR", 
   risk_code AS "Risk Code",
   extract('year' FROM COALESCE(year_of_account, effective_date)) AS "Year of account",
   'CAD'::text AS "Original Currency",
   claim_status(claim.*, $2)  AS "Claim status at beginning of period ", 
   claim_status(claim.*, $3)  AS "Claim status at end of period ", 
   claim_outstanding_reserve(claim_id, $2) AS "Outstanding Claims Amount as at beginning of period", 
   claim_paid(claim_id, $3, $2) AS "Paid in Year amount",
   claim_outstanding_reserve(claim_id, $3) AS "Outstanding Claims Amount as at end of period"

   FROM claim 
    LEFT JOIN risk USING (risk_id) 
    LEFT JOIN contract USING (contract_id) 
    WHERE syndicate_id = $1;

$$; 
