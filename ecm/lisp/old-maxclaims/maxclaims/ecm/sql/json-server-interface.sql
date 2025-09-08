
-- JSON server interface 
CREATE SCHEMA IF NOT EXISTS jsi AUTHORIZATION maxclaims;




CREATE OR REPLACE FUNCTION jsi.province(person) 
RETURNS json AS $$
 SELECT pongo.json_strip_nulls(json_build_object(
   '_type', 'province', 
   '_id', $1.province_state_id,
   'short_name', short_name, 
   'long_name', long_name
   ))
  FROM province_state WHERE $1.province_state_id = province_state_id
$$ LANGUAGE sql;


CREATE OR REPLACE FUNCTION jsi.corpus_summary(person) 
RETURNS json AS $$
 SELECT pongo.json_strip_nulls(json_build_object(
   '_type', 'corpus_summary', 
   '_id', $1.person_id, 
   'full_name', person_name($1),
   'first_name', CASE WHEN (trim(both from $1.first_name) != '')
                 THEN $1.first_name END,
   'last_name', CASE WHEN (trim(both FROM $1.last_name) != '')
                  THEN $1.last_name END,
   'company_name', CASE WHEN (trim(both FROM $1.company_name) != '')
                 THEN $1.company_name END, 
   'email_address', CASE WHEN (trim(both FROM $1.email_address) != '')
                 THEN $1.email_address END, 
   'province', jsi.province($1)
   ))
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION jsi.corpus_summary(person_id INTEGER) 
RETURNS json AS $$
 SELECT jsi.corpus_summary(person) FROM person WHERE person_id = $1
$$ LANGUAGE SQL;
CREATE OR REPLACE FUNCTION jsi.corpus_summary(person) 
RETURNS json AS $$
 SELECT pongo.json_strip_nulls(json_build_object(
   '_type', 'corpus_summary', 
   '_id', $1.person_id, 
   'first_name', CASE WHEN (trim(both from $1.first_name) != '')
                 THEN $1.first_name END,
   'last_name', CASE WHEN (trim(both FROM $1.last_name) != '')
                  THEN $1.last_name END,
   'company_name', CASE WHEN (trim(both FROM $1.company_name) != '')
                 THEN $1.company_name END, 
   'email_address', CASE WHEN (trim(both FROM $1.email_address) != '')
                 THEN $1.email_address END, 
   'province', jsi.province($1)
   ))
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION jsi.corpus_summary(person_id INTEGER) 
RETURNS json AS $$
 SELECT jsi.corpus_summary(person) FROM person WHERE person_id = $1
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION jsi.claim_status_detail(claim)
RETURNS json AS $$
SELECT pongo.json_strip_nulls(
  json_build_object(
    '_type', 'claim_status_detail', 
    '_id', $1.claim_id,
    'status', $1.status,
    'update', (select update FROM claim_update WHERE claim_id = $1.claim_id), 
    'open_date', $1.open_date::date,
    'close_date', $1.close_date::date,
    'reopen_date', $1.rev_date::date,
    'claim_received_time', $1.claim_received_time,
    'claim_acknowledged_time', $1.claim_acknowledged_time,
    'denial', $1.denial, 
    'refer_to_underwriters', $1.refer_to_underwriters, 
    'open_for_recovery', CASE WHEN ($1.open_for_recovery)
                                 THEN true
                                 ELSE null
                            END,
    'recovery_subrogation_date', $1.recovery_subrogation_date::date,
    'peer_reviewed', CASE WHEN ($1.peer_reviewed_date IS NOT NULL)
                                 THEN true
                                 ELSE null
                            END,     
    'peer_reviewed_date', $1.peer_reviewed_date::date

  ));
$$ LANGUAGE SQL;
    


CREATE OR REPLACE FUNCTION jsi.claim_headings_balance(claim) 
RETURNS json AS $$
SELECT json_agg(json_build_object(
 'heading', heading, 
 'outstanding_reserve', jsi.price(outstanding_reserve), 
 'total_paid', jsi.price(total_paid)
 ))
 FROM claim_info_by_heading($1.claim_id) as info 
 WHERE total_paid > 0 
 OR outstanding_reserve > 0
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION jsi.claim_balance (claim) 
RETURNS json AS $$
SELECT json_build_object(
  '_type', 'claim_balance', 
  '_id', $1.claim_id,
  'deductible', jsi.price($1.deductible),
  'recovered_deductible', jsi.price(claim_recovered_deductible($1.claim_id)),
  'outstanding_deductible', jsi.price(claim_deductible($1.claim_id)), 
  'outstanding_reserve', jsi.price(claim_outstanding_reserve($1.claim_id)),
  'total_paid', jsi.price(claim_paid($1.claim_id)), 
  'incurred', jsi.price(claim_incurred($1.claim_id)), 
  'headings_balance', jsi.claim_headings_balance($1)
)
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION jsi.policy_summary (policy)
 RETURNS json LANGUAGE SQL AS $$
 
   SELECT json_build_object(
     '_type', 'policy_summary', 
     '_id', $1.policy_id,
     'policy_number', $1.policy_number, 
     'insured', jsi.corpus_summary($1.insured_id),
     'effective_date', $1.effective_date::date,
     'expiry_date', $1.expiry_date::date, 
     'agent', jsi.corpus_summary($1.agent_id),
     'insurance_company', jsi.corpus_summary($1.company_id),
     'underwriter', jsi.corpus_summary($1.underwriter_id),
     'branch', jsi.corpus_summary($1.branch_id),
     'agency_office', jsi.corpus_summary($1.agency_office_id), 
     'deductible', jsi.price($1.deductible)
    );
$$ ;

CREATE OR REPLACE FUNCTION jsi.policy_summary (policy_id INTEGER)
 RETURNS json LANGUAGE SQL AS $$
   SELECT jsi.policy_summary(policy) FROM policy WHERE policy_id = $1 ;
$$ ;



CREATE OR REPLACE FUNCTION jsi.contract_summary (contract)
 RETURNS json LANGUAGE SQL AS $$
 
   SELECT pongo.json_strip_nulls(json_build_object(
     '_type', 'contract_summary',
     '_id', $1.contract_id,
     'contract_number', $1.contract_number, 
     'effective_date', $1.effective_date::date,
     'expiry_date', $1.expiry_date::date, 
     'agency', jsi.corpus_summary($1.agency_id),
     'insurance_company', jsi.corpus_summary($1.insurance_company_id),
     'syndicate', jsi.corpus_summary($1.syndicate_id),
     'london_broker', jsi.corpus_summary($1.london_broker_id),
     'authority', (SELECT jsi.price(ca.authority) FROM contract_authority AS ca
                    WHERE contract_id = $1.contract_id)
    ));
$$ ;

CREATE OR REPLACE FUNCTION jsi.contract_summary (contract_id INTEGER)
 RETURNS json LANGUAGE SQL AS $$
   SELECT jsi.contract_summary(contract) FROM contract WHERE contract_id = $1 ;
$$ ;


CREATE OR REPLACE FUNCTION jsi.risk_summary (risk)
 RETURNS json LANGUAGE SQL AS $$
   SELECT pongo.json_strip_nulls(json_build_object(
      '_type', 'risk-summary',
      '_id', $1.risk_id,
      'risk_type', $1.risk_type_name,
      'risk_code', $1.risk_code,
      'policy', jsi.policy_summary($1.policy_id), 
      'contract', jsi.contract_summary($1.contract_id), 
      'claims', (SELECT json_agg(claim_id) 
                  FROM (SELECT claim_id FROM claim WHERE risk_id = $1.risk_id
                        ORDER BY claim_id) AS c)
               
     )) ;
  $$;

CREATE OR REPLACE FUNCTION jsi.risk_summary (risk_id INTEGER)
 RETURNS json LANGUAGE SQL AS $$
   SELECT jsi.risk_summary(risk) FROM risk WHERE risk_id = $1 ;
$$ ;

CREATE OR REPLACE FUNCTION jsi.claim_cause(claim)
 RETURNS json AS $$ 
  SELECT json_build_object(
   'description', claim_cause_type,
   'code', cause_code)
  FROM claim_cause WHERE claim_cause_type = $1.cause
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION jsi.claim_cause(claim_id INTEGER)
 RETURNS json AS $$ 
  SELECT jsi.claim_cause(claim) FROM claim WHERE claim_id = $1
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION jsi.loss_code(loss)
 RETURNS json AS $$ 
  SELECT to_json(loss_code.*) FROM loss_code WHERE code = $1.code;
$$ LANGUAGE sql;

DROP FUNCTION jsi.loss_code(integer);

CREATE OR REPLACE FUNCTION jsi.loss_code(loss_id INTEGER)
 RETURNS json AS $$ 
  SELECT jsi.loss_code(loss) FROM loss WHERE id = $1;
$$ LANGUAGE SQL;


CREATE OR REPLACE FUNCTION jsi.loss_location(loss)
 RETURNS json AS $$ 
  SELECT pongo.json_strip_nulls(json_build_object(
   'address_line_1', address.line1,
   'address_line_2', address.line2, 
   'city', address.city,
   'province', address.province,
   'country', address.country,
   'postal_code', address.postal_code))
  FROM address WHERE id = $1.location_id;

$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION jsi.loss_location(claim)
 RETURNS json AS $$ 
   SELECT jsi.loss_location(loss) FROM loss WHERE id = $1.loss_id;
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION jsi.loss_location(claim_id INTEGER)
 RETURNS json AS $$ 
  SELECT jsi.loss_location(claim) FROM claim WHERE claim_id = $1
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION jsi.loss_catastrophe(loss)
 RETURNS json AS $$  
  SELECT pongo.json_strip_nulls(json_build_object(
   'name', cat.description,
   'code', cat.code)) FROM loss_catastrophe AS cat 
  WHERE cat.code = $1.catastrophe_code;
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION jsi.loss_catastrophe(claim)
 RETURNS json AS $$  
  SELECT jsi.loss_catastrophe(loss) FROM loss WHERE id = $1.loss_id;
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION jsi.loss_catastrophe(claim_id INTEGER)
 RETURNS json AS $$ 
  SELECT jsi.loss_catastrophe(claim) FROM claim WHERE claim_id = $1
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION jsi.loss(loss)
 RETURNS json AS $$ 
  SELECT pongo.json_strip_nulls(json_build_object(
   'date', $1.date, 
   'description',$1.description,
   'loss_code', jsi.loss_code($1),
   'location', jsi.loss_location($1),
   'catastrophe', jsi.loss_catastrophe($1)));
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION jsi.loss(claim)
 RETURNS json AS $$ 
  SELECT jsi.loss(loss) FROM loss WHERE $1.loss_id = loss.id;
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION jsi.loss(claim_id INTEGER)
 RETURNS json AS $$ 
  SELECT jsi.loss(claim) FROM claim WHERE claim_id = $1
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION jsi.claim_authority(claim)
 RETURNS json AS $$ 
  SELECT pongo.json_strip_nulls(json_build_object(
   '_type', 'claim_authority',
   '_id', $1.claim_id,
   'over_authority', claim_over_authority($1),
   'over_authority_date', $1.over_authority::date, 
   'contract_authority', jsi.price(contract_authority($1)),
   'claim_authority', jsi.price(claim_authority($1))
    ))
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION jsi.claim_authority(claim_id INTEGER)
 RETURNS json AS $$ 
  SELECT jsi.claim_authority(claim) FROM claim WHERE claim_id = $1
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION jsi.user_summary(app_user) RETURNS json AS $$
 SELECT json_build_object(
    '_type', 'user-summary',
    '_id', $1.app_user_id,
    'username', $1.username, 
    'corpus', jsi.corpus_summary($1.person_id),
    'read_only', app_user_is_read_only($1), 
    'admin', $1.admin
   ) ;
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION jsi.user_summary(INTEGER) RETURNS json AS $$
  SELECT jsi.user_summary(app_user) 
   FROM app_user WHERE app_user_id = $1
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION jsi.diary_users()
RETURNS JSON LANGUAGE SQL AS $$
 SELECT json_agg(foo) FROM 
  (SELECT foo 
   FROM (SELECT jsi.user_summary(app_user_id) AS foo
          FROM (SELECT DISTINCT app_user_id FROM diary_entry) AS d) 
          AS s
   ORDER BY (foo->'username')::text) AS o
$$;

CREATE OR REPLACE FUNCTION jsi.diary_entry(diary_entry)
 RETURNS json LANGUAGE SQL AS $$

 SELECT json_build_object(
  '_type', 'diary_entry',
  '_id', $1.diary_entry_id,
  'claim_id', $1.claim_id,
  'processed', $1.processed,
  'outstanding', (CASE WHEN (diary_entry_deadline($1) < now() 
                             AND NOT $1.processed)
                  THEN true ELSE false
                  END),
  'schedule', $1.action_date,
  'deadline', diary_entry_deadline($1),
  'deferred', COALESCE (
              (SELECT json_agg(defer_date) FROM
                (SELECT defer_date
                 FROM defer_diary_entry
                 WHERE diary_entry_id = $1.diary_entry_id
                 ORDER BY defer_date) AS dd),

              array_to_json(ARRAY[]::integer[])),
  'user', jsi.user_summary($1.app_user_id),
  'note', $1.note);

$$;


CREATE OR REPLACE FUNCTION jsi.diary_schedule_numbers(app_user DEFAULT current_app_user())
 RETURNS json LANGUAGE SQL AS $$
  WITH deadline AS (
    SELECT diary_entry_id, 
     GREATEST(action_date, MAX(defer_date)) AS deadline
    FROM diary_entry
    LEFT JOIN defer_diary_entry
    USING (diary_entry_id)
    WHERE app_user_id = $1.app_user_id
    AND NOT processed GROUP BY diary_entry_id)

 SELECT json_build_object (
   '_type', 'diary-schedule-numbers', 
   '_id', $1.app_user_id,
   'user', jsi.user_summary($1),
   'total', (SELECT count(*) FROM deadline),
   'overdue', (SELECT count(*) FROM deadline WHERE deadline < now()::date),
   'today', (SELECT count(*) FROM deadline WHERE deadline = now()::date),
   'tomorrow', (SELECT count(*) FROM deadline WHERE deadline = now()::date + INTERVAL '1 day'), 
   'one_week', (SELECT count(*) FROM deadline WHERE 
               deadline > now()::date + INTERVAL '1 day' 
               AND deadline <= now()::date + INTERVAL '1 day' + INTERVAL '1 week'), 
   'future', (SELECT count(*) FROM deadline WHERE 
               deadline >  now()::date + INTERVAL '1 day' + INTERVAL '1 week')
)
$$;

CREATE OR REPLACE FUNCTION jsi.claim_crux(claim)
 RETURNS json LANGUAGE SQL AS $$
  SELECT
    pongo.json_strip_nulls(
          json_build_object(
   '_type', 'claim_crux', 
   '_id', $1.claim_id,
   'status', $1.status,
   'status_detail', jsi.claim_status_detail($1),
   'examiner', jsi.corpus_summary($1.adjuster_id),
   'external_adjuster', jsi.corpus_summary($1.external_adjuster_id),
   'claimant', jsi.corpus_summary($1.plaintiff_id),
   'coverage_counsel', jsi.corpus_summary($1.coverage_counsel_id),
   'defense_counsel', jsi.corpus_summary($1.defense_counsel_id),
   'line_of_business', $1.line_of_business,
   'industry', (SELECT to_json(ibc_code.*) 
                FROM claim_ibc_code AS cic RIGHT JOIN ibc_code USING (industry) 
                WHERE  cic.claim_id = $1.claim_id),
   'coverage', $1.coverage,
   'date_of_loss', $1.date_of_loss::date,      
   'balance', jsi.claim_balance($1), 
 
   'risk', jsi.risk_crux($1.risk_id),
   'cause', jsi.claim_cause($1), 
   'loss', jsi.loss($1), 
   'authority', jsi.claim_authority($1), 
   'subscription_percent', $1.subscription_percent, 
   'outstanding_diary',  (SELECT json_agg(jsi.diary_entry(diary_entry)) FROM diary_entry
                           WHERE claim_id = $1.claim_id 
                           AND diary_entry_is_outstanding(diary_entry)),
   'transactions', (SELECT json_agg(t.t)
                    FROM 
                     (SELECT jsi.transaction(t) AS t
                      FROM claim_transaction AS t
                      WHERE t.claim_id = $1.claim_id
                      ORDER BY t.transaction_date, t.transaction_id) AS t),
   'attachments', (SELECT json_agg(a.a)
                    FROM 
                     (SELECT jsi.claim_attachment(a) AS a
                      FROM attachment AS a
                      WHERE a.claim_id = $1.claim_id
                      ORDER BY a.date, a.attachment_id) AS a), 
   'diary', (SELECT jsi.claim_diary($1.claim_id)), 
   'timelog', (SELECT jsi.claim_timelog($1.claim_id))

         
 
 
 
  ))

$$ ;

CREATE OR REPLACE FUNCTION jsi.claim_crux(claim_id integer)
 RETURNS json LANGUAGE SQL AS $$
 -- uses claim without schema
  SELECT jsi.claim_crux(claim) from claim WHERE claim_id = $1;
 $$ ;

CREATE OR REPLACE FUNCTION jsi.claim_summary(claim)
 RETURNS json LANGUAGE SQL AS $$
  SELECT
    pongo.json_strip_nulls(
          json_build_object(
   '_type', 'claim-summary', 
   '_id', $1.claim_id,
   'status', $1.status,
   'date_of_loss', $1.date_of_loss::date,      
   'examiner', jsi.corpus_summary($1.adjuster_id),     
   'risk', jsi.risk_summary($1.risk_id) 
  ))

$$ ;

CREATE OR REPLACE FUNCTION jsi.claim_summary(claim_id integer)
 RETURNS json LANGUAGE SQL AS $$
 -- uses claim without schema
  SELECT jsi.claim_summary(claim) from claim WHERE claim_id = $1;
 $$ ;

CREATE OR REPLACE FUNCTION jsi.claim_attachment(attachment) 
RETURNS json AS $$  
SELECT json_build_object(
'_type', 'claim-attachment', '_id', $1.attachment_id,
'claim_id', $1.claim_id,
'user', jsi.user_summary($1.app_user_id), 
'date', $1.date::date,
'time', $1.date,
'description', $1.file_description,
'file_name', $1.file_name, 
'content_type', $1.file_type,
'sha1_digest', $1.sha1_digest, 
'archived', $1.archived)
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION jsi.claim_attachment(integer) 
RETURNS json AS $$
 SELECT jsi.claim_attachment(attachment) FROM attachment WHERE attachment_id = $1 ;
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION jsi.claim_attachments(INTEGER) 
RETURNS json AS $$  
SELECT json_build_object(
'_type', 'claim-attachments', '_id', $1,
'attachments', (SELECT json_agg(a) FROM 
                 (SELECT jsi.claim_attachment(attachment) AS a
                   FROM attachment WHERE claim_id = $1 
                   ORDER BY date) AS foo)
)
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION jsi.timecard(timecard)
RETURNS json AS $$
    SELECT pongo.json_strip_nulls(json_build_object(
   '_type', 'claim-timecard',
   '_id', $1.timecard_id,
   'claim_id', $1.claim_id,
   'date', $1.date,
   'user', jsi.user_summary($1.app_user_id), 
   'notes', $1.notes, 
   'billable_hours', $1.minutes,
   'unbillable_hours', $1.unbillable_hours,
   'mileage', $1.mileage_km, 
   'disbursement', $1.disbursements, 
   'attachment', jsi.claim_attachment($1.attachment_id),
   'interim_id', (SELECT timecard_interim_id FROM timecard_interim
                   WHERE date > $1.date AND claim_id = $1.claim_id ORDER BY date ASC LIMIT 1)   
  ));
$$ LANGUAGE SQL;    

CREATE OR REPLACE FUNCTION jsi.timecard(integer)
RETURNS json AS $$
 SELECT jsi.timecard(timecard) FROM timecard WHERE timecard_id = $1;
$$ LANGUAGE SQL;    

CREATE OR REPLACE FUNCTION jsi.claim_diary(integer)
 RETURNS json LANGUAGE SQL AS $$

 WITH des AS (
   SELECT de FROM 
    (SELECT jsi.diary_entry(diary_entry) AS de
     FROM diary_entry
     WHERE claim_id = $1) AS des 
   ORDER BY 
             CASE WHEN ((de->'outstanding')::text = 'true'::text)
                THEN 0 ELSE 1 END,
             CASE WHEN ((de->'processed')::text = 'false'::text)
                THEN 0 ELSE 1 END,
             (de->'deadline')::text::date,
              (de->'_id')::text::integer

 ), outstanding AS (
  SELECT json_agg(de->'_id') AS o
   from des WHERE (de->'outstanding')::text = 'true'
)

 SELECT json_build_object(
  '_type', 'claim-diary',
  '_id', $1,
  'outstanding',COALESCE((SELECT o from outstanding),
                     array_to_json(ARRAY[]::integer[])), 
  'entries', COALESCE((select json_agg(de) FROM des),
                     array_to_json(ARRAY[]::json[]))
 );
$$;


CREATE OR REPLACE FUNCTION jsi.cheque (claim_transaction) 
RETURNS json AS $$
 WITH json AS (  
 SELECT pongo.json_strip_nulls(json_build_object(
  '_type', 'cheque', 
  '_id', $1.transaction_id,
  'payee', jsi.corpus_summary($1.payee_id),
  'cheque_number', CASE WHEN (($1.cheque_number != '') 
                              AND ($1.cheque_number != 'None'))
                        THEN $1.cheque_number END,
  'schemes_advance_number',CASE WHEN ($1.schemes_advance_number != '')
                                THEN $1.schemes_advance_number END,
  'reference_number', CASE WHEN ($1.reference_number != '')
                           THEN $1.reference_number END
  )) AS cheque)

  SELECT cheque FROM json 
   WHERE (cheque->'payee') IS NOT NULL 
   OR (cheque->'cheque_number') IS NOT NULL
   OR (cheque->'schemes_advance_number') IS NOT NULL
   OR (cheque->'reference_number') IS NOT NULL;
$$ LANGUAGE sql;

 CREATE OR REPLACE FUNCTION jsi.transaction (claim_transaction) 
 RETURNS json AS $$
 SELECT pongo.json_strip_nulls(json_build_object(
   '_type', 'transaction', 
   '_id', $1.transaction_id,
   'claim_id', $1.claim_id, 
   'date', $1.transaction_date::date,
   'transaction_date', $1.transaction_date::timestamp with time zone,
   'type', (SELECT description 
            FROM claim_transaction_type 
            WHERE claim_transaction_type_id = $1.transaction_type_id),
   'heading', $1.transaction_heading,
   'amount', $1.amount::text,
   'expense_type', $1.expense_type,
   'cheque', jsi.cheque($1),
   'approved', $1.approved
))
 $$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION jsi.timecard_interim
  (timecard_interim, 
   type text DEFAULT 'timecard-interim', 
   start_date TIMESTAMP WITH TIME ZONE DEFAULT null::timestamp with time zone,
   end_date TIMESTAMP WITH TIME ZONE DEFAULT null::timestamp with time zone,
   claim_id INTEGER DEFAULT NULL::INTEGER)
 RETURNS json LANGUAGE SQL AS $$ 
 WITH start_time AS (
  SELECT COALESCE($3, (SELECT date FROM timecard_interim
                    WHERE claim_id = $1.claim_id
                    AND timecard_interim.date < $1.date
                    ORDER BY date DESC LIMIT 1),
                  (SELECT min(date) FROM timecard WHERE claim_id = $1.claim_id))
         AS time
  ), timecards AS (
    SELECT * FROM timecard, start_time
      WHERE claim_id = (coalesce($5, $1.claim_id))
      AND timecard.date >= start_time.time
      AND timecard.date < (coalesce($4,$1.date)) ORDER BY timecard.date)
    
 SELECT json_build_object(
  '_type', $2,
  '_id', $1.timecard_interim_id,
  'claim_id', (coalesce($5, $1.claim_id)),
  'effective_time',(SELECT time FROM start_time), 
  'expiry_time', (coalesce($4,$1.date)),
  'billable_hours', (SELECT SUM(minutes) FROM timecards),
  'unbillable_hours', (SELECT SUM(unbillable_hours) FROM timecards),
  'mileage', (SELECT SUM(mileage_km) FROM timecards),
  'disbursement', (SELECT SUM(disbursements)::TEXT FROM timecards),
  'timecard_ids', (SELECT json_agg(timecard_id) FROM timecards)
 );
$$;

CREATE OR REPLACE FUNCTION jsi.timecard_interim
  (interim_id INTEGER)
 RETURNS json LANGUAGE SQL AS $$ 
  SELECT jsi.timecard_interim(timecard_interim) 
  FROM timecard_interim WHERE timecard_interim_id = $1 ;
$$;

CREATE OR REPLACE FUNCTION jsi.claim_continual_timecard
  (claim_id integer)
 RETURNS json LANGUAGE SQL AS $$ 
 SELECT jsi.timecard_interim(null::timecard_interim, 
  'timecard-continual', 
  COALESCE((SELECT MAX(date) FROM timecard_interim WHERE claim_id = $1),
  (SELECT MIN(date) FROM timecard WHERE claim_id = $1)),
  (SELECT MAX(date) + INTERVAL '1 second' FROM timecard WHERE claim_id = $1))
   AS json;
  
$$;


  CREATE OR REPLACE FUNCTION jsi.claim_timelog(integer)
   RETURNS json LANGUAGE SQL AS $$

   WITH RECURSIVE total AS (
     -- The Total
    SELECT now() AS date, jsi.timecard_interim(null::timecard_interim, 
             'timecard-total', 
             (SELECT MIN(date) FROM timecard WHERE claim_id = $1),
             now(), $1) AS jso
   -- The Continual 
   UNION ALL
    SELECT (SELECT MAX(date) + INTERVAL '1 second' FROM timecard WHERE claim_id = $1)
             AS date,
      jsi.timecard_interim(null::timecard_interim, 
      'timecard-continual', 
      COALESCE((SELECT MAX(date) FROM timecard_interim WHERE claim_id = $1),
               (SELECT MIN(date) FROM timecard WHERE claim_id = $1)),
      (SELECT MAX(date) + INTERVAL '1 second' FROM timecard WHERE claim_id = $1),
       $1) AS jso
   UNION ALL
     SELECT date, jsi.timecard_interim(timecard_interim) AS jso
     FROM timecard_interim
     WHERE claim_id = $1
   UNION ALL 
     SELECT date, jsi.timecard(timecard) AS jso
     FROM timecard
     WHERE claim_id = $1
  )

  SELECT json_agg(jso) FROM (SELECT jso FROM total ORDER BY date) AS log
 

$$;
 

  CREATE OR REPLACE FUNCTION jsi.timecard_interim_crux(integer)
   RETURNS json LANGUAGE SQL AS $$

   SELECT json_object_agg(
   (CASE key
     WHEN '_type' THEN 'timecard-interim-crux' 
     WHEN 'timecard_ids' THEN 'timecards'
     WHEN 'claim_id' THEN 'claim'
     ELSE key 
   END), 
   (CASE key
     WHEN 'claim_id' THEN (jsi.claim_summary(value::text::integer))
     WHEN 'timecard_ids' THEN (SELECT json_agg(tc) FROM
               (SELECT jsi.timecard(timecard) AS tc
               FROM timecard 
               WHERE timecard_id IN (SELECT json_array_elements(value)::text::integer)
               ORDER BY timecard.date)
AS t)
    ELSE value 
    END))

  FROM (SELECT (json_each(interim)).* 
        FROM (SELECT jsi.timecard_interim(t) AS interim 
              FROM timecard_interim AS t 
              WHERE timecard_interim_id = $1) AS t) AS ti;
$$; 

CREATE OR REPLACE FUNCTION jsi.risk_code_summary (risk_code)
 RETURNS json LANGUAGE SQL AS $$
   SELECT pongo.json_strip_nulls(json_build_object(
      '_type', 'risk-code-summary',
     '_id', $1.risk_code,
      'code', $1.risk_code,
      'description', $1.description,
      'first_year_of_account', $1.first_year_of_account, 
      'last_year_of_account', $1.last_year_of_account, 
      'terrorism_code', $1.terrorism_code
     )) ;
$$;

CREATE OR REPLACE FUNCTION jsi.risk_code_summary(TEXT)
 RETURNS json LANGUAGE SQL AS $$
   SELECT jsi.risk_code_summary(risk_code.*) FROM risk_code WHERE risk_code = $1 ;
$$ ;

CREATE OR REPLACE FUNCTION jsi.risk_code_summary (risk_code)
 RETURNS json LANGUAGE SQL AS $$
   SELECT pongo.json_strip_nulls(json_build_object(
      '_type', 'risk-code-summary',
      '_id', $1.risk_code,
      'code', $1.risk_code,
      'description', $1.description,
      'first_year_of_account', $1.first_year_of_account, 
      'last_year_of_account', $1.last_year_of_account, 
      'terrorism_code', jsi.risk_code_summary($1.terrorism_code)
     )) ;
$$;



CREATE OR REPLACE FUNCTION jsi.risk_claim_summary(claim)
 RETURNS json LANGUAGE SQL AS $$
  SELECT
    pongo.json_strip_nulls(
          json_build_object(
   '_type', 'claim-summary', 
   '_id', $1.claim_id,
   'status', $1.status,
   'date_of_loss', $1.date_of_loss::date,      
   'examiner', jsi.corpus_summary($1.adjuster_id)
  ))

$$ ;

CREATE OR REPLACE FUNCTION jsi.risk_claim_summary(claim_id integer)
 RETURNS json LANGUAGE SQL AS $$
 -- uses claim without schema
  SELECT jsi.claim_summary(claim) from claim WHERE claim_id = $1;
 $$ ;

CREATE OR REPLACE FUNCTION jsi.active_risk_codes ()
 RETURNS json LANGUAGE SQL AS $$
    SELECT json_agg(jsi.risk_code_summary(risk_code.*)) 
     FROM (SELECT * FROM risk_code 
           WHERE last_year_of_account = 9999 
           ORDER BY risk_code.risk_code) AS risk_code
$$;


CREATE OR REPLACE FUNCTION jsi.risk_summary (risk)
 RETURNS json LANGUAGE SQL AS $$
   SELECT pongo.json_strip_nulls(json_build_object(
      '_type', 'risk-summary',
      '_id', $1.risk_id,
      'risk_type', $1.risk_type_name,
      'risk_code', $1.risk_code,
      'policy', jsi.policy_summary($1.policy_id), 
      'contract', jsi.contract_summary($1.contract_id), 
      'claims', (SELECT json_agg(claim_id) 
                  FROM (SELECT claim_id FROM claim WHERE risk_id = $1.risk_id
                        ORDER BY claim_id) AS c)
               
     )) ;
  $$;

CREATE OR REPLACE FUNCTION jsi.risk_summary (risk_id INTEGER)
 RETURNS json LANGUAGE SQL AS $$
   SELECT jsi.risk_summary(risk) FROM risk WHERE risk_id = $1 ;
$$ ;

CREATE OR REPLACE FUNCTION jsi.risk_crux (risk)
 RETURNS json LANGUAGE SQL AS $$
   SELECT pongo.json_strip_nulls(json_build_object(
      '_type', 'risk-crux',
      '_id', $1.risk_id,
      'risk_type', $1.risk_type_name,
      'risk_code', jsi.risk_code_summary($1.risk_code),
      'policy', jsi.policy_summary($1.policy_id), 
      'contract', jsi.contract_summary($1.contract_id),
      'claims', (SELECT json_agg(jsi.risk_claim_summary(claim))
                 FROM (SELECT * FROM claim WHERE risk_id = $1.risk_id ORDER BY claim_id) AS claim)
     )) ;
  $$;

CREATE OR REPLACE FUNCTION jsi.risk_crux (risk_id INTEGER)
 RETURNS json LANGUAGE SQL AS $$
   SELECT jsi.risk_crux(risk) FROM risk WHERE risk_id = $1 ;
$$ ;
