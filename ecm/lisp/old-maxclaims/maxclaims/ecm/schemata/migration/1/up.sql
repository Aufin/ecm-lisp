DROP FUNCTION pillar_3(integer,timestamp without time zone,timestamp without time zone);

CREATE OR REPLACE FUNCTION pillar_3 
   (syndicate_id integer, start_date timestamp, end_date timestamp)
  RETURNS TABLE  ("Syndicate Number" text, "Claim Reference" int, "UMR" text,
                 "Risk Code" text, 
                 "Year of account" integer, "Original Currency" text, 
                 "Claim status at beginning of period " text ,  "Claim status at end of period" text,
                  "Outstanding Claims Amount as at beginning of period" text,
                  "Paid in Year amount" text,  
                  "Outstanding Claims Amount as at end of period" text)
  LANGUAGE sql AS $$

  SELECT 
  coalesce(syndicate_number, person_name(syndicate_id)) AS "Syndicate Number", 
   claim_id AS "Claim Reference", 
   contract_number AS "UMR", 
   risk_code AS "Risk Code",
   extract('year' FROM COALESCE(year_of_account, effective_date))::integer AS "Year of account",
   'CAD'::text AS "Original Currency",
   claim_status(claim.*, $2)  AS "Claim status at beginning of period ", 
   claim_status(claim.*, $3)  AS "Claim status at end of period ", 
   claim_outstanding_reserve(claim_id, $2)::text AS "Outstanding Claims Amount as at beginning of period", 
   claim_paid(claim_id, $3, $2)::text AS "Paid in Year amount",
   claim_outstanding_reserve(claim_id, $3)::text AS "Outstanding Claims Amount as at end of period"

   FROM claim 
    LEFT JOIN risk USING (risk_id) 
    LEFT JOIN contract USING (contract_id) 
    WHERE syndicate_id = $1
    ORDER BY claim_id;

$$; 
