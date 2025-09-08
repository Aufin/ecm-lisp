CREATE OR REPLACE FUNCTION current_app_user ()
 RETURNS app_user AS $$
 WITH app_user_id AS (
  SELECT CASE WHEN (CURRENT_USER = 'maxclaims')
	      THEN 1
	      WHEN ((SELECT SUBSTRING(CURRENT_USER FROM 1 FOR 3)) 
                     = 'mu_')
	      THEN trim(leading 'mu_' FROM CURRENT_USER)::integer
	      ELSE NULL
         END AS app_user_id
)
 SELECT app_user.* 
 FROM app_user INNER JOIN app_user_id USING (app_user_id);

$$ LANGUAGE SQL;




CREATE OR REPLACE FUNCTION app_user_can_read
  (claim, app_user DEFAULT current_app_user())
RETURNS BOOLEAN AS $$
  WITH access_claim AS (   
    SELECT claim_id, access FROM app_user_claim 
    WHERE claim_id = $1.claim_id
    AND app_user_claim.app_user_id = $2.app_user_id
  ), access_contract AS (
    SELECT true 
    FROM risk
    INNER JOIN contract USING (contract_id)
    INNER JOIN app_user_contract 
      ON (app_user_contract.contract_id = contract.contract_id)
    WHERE risk.risk_id = $1.risk_id
    AND app_user_contract.app_user_id = $2.app_user_id
  ), access_agency AS (
   SELECT true
   FROM risk
    INNER JOIN contract USING (contract_id)
    INNER JOIN app_user_agency 
      ON (app_user_agency.agency_id = contract.agency_id)
   WHERE risk.risk_id = $1.risk_id
    AND app_user_agency.app_user_id = $2.app_user_id
  ), all_claims AS (
   (SELECT true FROM app_user_claim
      WHERE app_user_claim.app_user_id = $2.app_user_id
      AND access
      LIMIT 1)
   ), all_contracts AS (
    (SELECT * FROM app_user_contract
           WHERE app_user_contract.app_user_id = $2.app_user_id
           LIMIT 1)
   ), all_agencies AS (
    (SELECT * FROM app_user_agency
      WHERE app_user_agency.app_user_id = $2.app_user_id
      LIMIT 1)
   )
  SELECT CASE WHEN 

 (SELECT true
  WHERE
    -- IF they are all NULL, then we have
    -- access.
 (((SELECT true FROM all_claims) IS NULL)
   AND ((SELECT true FROM all_contracts) IS NULL)
   AND ((SELECT true FROM all_agencies) IS NULL)
   -- If there is an access_claim, and all claims are NULL, then we
   -- are denied.
   AND ((SELECT true FROM access_claim) IS NULL))

   OR -- check that we if we have access
   (SELECT access FROM access_claim)
   OR -- See if we have access via the contract or the agency,
      ((((SELECT true from access_contract) IS NOT NULL)
        OR ((SELECT true from access_agency) IS NOT NULL))
	 -- If there is an access_claim, then we are denied, as we
         -- would have already gone through if it was true
	    AND ((SELECT true FROM access_claim) IS NULL)))

 THEN true ELSE false END;

$$ LANGUAGE SQL;

  CREATE OR REPLACE FUNCTION app_user_is_read_only
    (app_user DEFAULT current_app_user())
  RETURNS BOOLEAN LANGUAGE SQL AS $$
  
   SELECT CASE WHEN (COALESCE(
       (SELECT access FROM app_user_claim 
        WHERE app_user_id = $1.app_user_id
        AND access LIMIT 1),
       (SELECT true 
        FROM risk
        INNER JOIN contract USING (contract_id)
        INNER JOIN app_user_contract 
	ON (app_user_contract.contract_id = contract.contract_id)
        WHERE  app_user_id = $1.app_user_id LIMIT 1),
       (SELECT true
        FROM risk
        INNER JOIN contract USING (contract_id)
        INNER JOIN app_user_agency 
	ON (app_user_agency.agency_id = contract.agency_id)
        WHERE app_user_id = $1.app_user_id
        LIMIT 1)
       ) IS NOT NULL)
       THEN true ELSE false END
$$;

  CREATE OR REPLACE FUNCTION app_user_is_administrator
    (app_user DEFAULT current_app_user())
  RETURNS BOOLEAN LANGUAGE SQL AS $$
   SELECT $1.admin
$$
