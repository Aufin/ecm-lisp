CREATE OR REPLACE FUNCTION claim_authority (claim)
RETURNS numeric(20,2) AS $$
 SELECT CASE WHEN ($1.authority = 0.00)
             THEN (SELECT contract_authority(c)
                   FROM contract AS c
                   JOIN risk USING (contract_id)
                   WHERE risk_id = $1.risk_id)
             ELSE $1.authority
        END 
$$ LANGUAGE SQL;                

CREATE OR REPLACE 
  FUNCTION claim_over_authority (claim, end_date timestamp without time zone DEFAULT 'infinity'::timestamp without time zone)
RETURNS BOOLEAN AS $$
 SELECT CASE WHEN (authority IS NOT NULL 
                   AND incurred > authority)
             THEN true
             ELSE false
        END
 FROM (SELECT claim_incurred($1.claim_id, $2) AS incurred, 
              claim_authority($1) AS authority) AS c
$$ LANGUAGE SQL;

CREATE OR REPLACE 
  FUNCTION claim_over_authority (integer, end_date timestamp without time zone DEFAULT 'infinity'::timestamp without time zone)
RETURNS BOOLEAN AS $$
 SELECT claim_over_authority(claim) FROM claim WHERE claim_id = $1
$$ LANGUAGE SQL;                

CREATE OR REPLACE 
  FUNCTION claim_over_authority_date (claim)
RETURNS timestamp without time zone AS $$
WITH RECURSIVE datething(timet) AS (
    SELECT $1.open_date::timestamp without time zone
            AS timet
     UNION ALL
     SELECT timet + INTERVAL '1 day' AS timet
     FROM datething
      WHERE timet < now()
  )

SELECT COALESCE ($1.over_authority,
          (SELECT * from datething WHERE claim_over_authority($1, timet) LIMIT 1))
$$ LANGUAGE SQL;


  

CREATE OR REPLACE FUNCTION contract_authority (claim)
RETURNS numeric(20,2) LANGUAGE SQL AS $$
 SELECT contract_authority(contract)
   FROM risk LEFT JOIN contract USING (contract_id) 
 WHERE risk.risk_id = $1.risk_id
$$ ;             

CREATE OR REPLACE FUNCTION trigger_claim_open_for_recovery_date ()
RETURNS TRIGGER AS $$
 BEGIN
 IF (SELECT true 
     FROM claim 
     WHERE NEW.claim_id = claim_id
     AND recovery_subrogation_date IS NULL
     AND NEW.open_for_recovery) THEN 
 UPDATE claim SET recovery_subrogation_date = now()
  WHERE claim_id = NEW.claim_id;
 END IF;
 RETURN NEW;
 END;
$$ language plpgsql;

DROP TRIGGER IF EXISTS zclaim_open_for_recovery_date ON claim;

CREATE TRIGGER zclaim_open_for_recovery_date
 AFTER INSERT OR UPDATE ON claim
 FOR EACH ROW
 WHEN (NEW.open_for_recovery)
 EXECUTE PROCEDURE trigger_claim_open_for_recovery_date();
