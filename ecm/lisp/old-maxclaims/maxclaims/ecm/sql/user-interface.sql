CREATE SCHEMA IF NOT EXISTS ui AUTHORIZATION maxclaims;

  CREATE OR REPLACE FUNCTION ui.claim(ui.claim)
  RETURNS json AS $$
      SELECT json_build_object(
     '_type', 'ui.claim',
     '_id', (json_build_array($1.claim_id, $1.app_user_id)),
     'claim_id', $1.claim_id,
     'app_user_id', $1.app_user_id,
     'info', $1.info,
     'balance', $1.balance,
     'tabs', $1.tabs,
     'history', $1.history,
     'active_tab', $1.active_tab,
     'data', $1.data
    );
  $$ LANGUAGE SQL;    

CREATE OR REPLACE FUNCTION ui.new_claim(integer, integer)
  RETURNS json AS $$
  INSERT INTO ui.claim (claim_id, app_user_id) 
    VALUES ($1, $2) RETURNING ui.claim(claim.*)
$$ LANGUAGE SQL;    
  
  CREATE OR REPLACE FUNCTION ui.claim(integer, integer)
  RETURNS json AS $$

  SELECT COALESCE( 
    (SELECT ui.claim(claim) 
     FROM ui.claim WHERE claim_id = $1 AND app_user_id = $2),

    (SELECT ui.new_claim($1, $2)))
  $$ LANGUAGE SQL;
