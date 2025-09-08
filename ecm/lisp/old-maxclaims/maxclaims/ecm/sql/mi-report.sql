
DROP VIEW IF EXISTS mi_report CASCADE;

CREATE OR REPLACE VIEW mi_report AS 
 (
  WITH _first_mi AS (
  SELECT *, 
         daterange(co.effective_date, co.expiry_date) AS "Reporting Period"
   FROM claim AS c 
   JOIN risk AS r USING (risk_id)
   JOIN contract AS co USING (contract_id)
),
  first_mi AS (
   SELECT *, 
      CASE WHEN (claim_status(claim_id, upper("Reporting Period")- INTERVAL '1 Day') = 'Open')
          THEN true ELSE false END AS "Is Claim Open?", 
      ( SELECT count(*) 
       FROM 
        (SELECT GREATEST (d.action_date, MAX(defer_date)) AS due_date
        FROM diary_entry AS d
        JOIN defer_diary_entry AS dde
        USING (diary_entry_id)
        WHERE d.claim_id = mi.claim_id
        AND d.processed = false 
        GROUP BY action_date) AS d
       WHERE d.due_date::date <@ "Reporting Period")
        AS "Volume of Overdue Diary Items",
      
      (WITH foo AS (
        SELECT (mi.claim_received_time + INTERVAL '6 months') AS timeline
        WHERE mi.status = 'Open'
       ), bar AS (
        SELECT *, claim_reserve(mi.claim_id) AS reserve        
         FROM foo 
         WHERE foo.timeline::date <@ "Reporting Period"
       ), baz AS (
         SELECT 1 FROM bar WHERE reserve = 0 OR reserve IS NULL
      ) 
        SELECT * from baz) AS 
       "Volume of nil reserve claims older than 6 months (as %)"
      , 
      (WITH res_paid AS (
       SELECT claim_id, 
              claim_indemnity_reserve(mi.claim_id) AS ores, 
              claim_indemnity_paid(mi.claim_id) AS paid
        -- make sure that the claim is closed in this period
        WHERE mi.status = 'Closed' AND mi.close_date::date <@ "Reporting Period"
      ), final_res AS (
       SELECT claim_id, ores as first, ores - paid AS final, paid AS paid
       FROM res_paid
       -- only for reserves that are > 0
       WHERE ores > 0
      ), thing AS (
        SELECT json_build_object(
                'claim_id', claim_id,
                'indemnity_open_reserve', first,
                'indemnity_final_reserve', final,
                'variance_percent', (SELECT round(((first - final) / first) * 100, 2)),
                'indemnity_paid', paid
               ) AS jso
      FROM final_res
        WHERE first > 0 AND mi.status = 'Closed'
      ) 
      
       SELECT jso FROM thing)
      AS "Variance of final to initial reserve (as %)"
      ,
      (WITH days AS (
       SELECT extract(days from mi.claim_acknowledged_time - 
                      mi.claim_received_time) AS days
         WHERE mi.claim_acknowledged_time IS NOT NULL 
         AND   mi.claim_received_time IS NOT NULL
         AND  mi.claim_acknowledged_time::date  <@ "Reporting Period"
       ) SELECT days FROM days) 
      AS "Response time for acknowledging new claims"
      
 --      AS "Initial Reserve"
 
    FROM _first_mi AS mi
  ), 
 
  second_mi AS (
 
   SELECT  
 
   -- array_to_json(array_remove(array_agg("Variance of final to initial reserve (as %)"::text), NULL))
  --  AS variance, 
 
  -- lower("Reporting Period") AS "Start Day", COND(upper("Reporting
  -- Period") + '1 Day'::INTERVAL)::date, upper_inc("Reporting Period"),
  person_name(syndicate_id) AS "Syndicate Name", 
  syndicate_id AS "syndicate_id", 
  concat(lower("Reporting Period")::text, ' to ',  upper("Reporting Period")::text) AS  "Reporting Period",
 
  -- * Claims Counts
  -- ---------------
 
  -- Total Volume of Claims
  count(claim_id) AS "Volume of Claims",
 
  -- Volume of Open Claims
  SUM(CASE "Is Claim Open?" WHEN true THEN 1 ELSE 0 END)
   AS "Volume of Open Claims",
 
  -- Volume of Closed Claims
  SUM(CASE WHEN (close_date::date <@ "Reporting Period")
           THEN 1 ELSE 0 END)
  AS "Volume of Closed Claims",
 
  -- Volume of Reopened Claims 
  SUM((SELECT count(*) WHERE rev_date::date <@ "Reporting Period")) 
   AS "Volume of Re-Opened Claims",
 
  -- Volume of New Claims
  SUM((SELECT count(*) 
       WHERE open_date::date <@ "Reporting Period")) AS "Volume of New Claims",
 
  -- Value of open claims
  SUM( CASE "Is Claim Open?" 
        WHEN true THEN claim_incurred(claim_id, (upper("Reporting Period") 
                                              - INTERVAL '1 Day'))::money
       END
      )
    AS "Value of open claims",
 
  --  % of work referred to London? 
  (WITH refer_uw AS (
   SELECT SUM(
     (SELECT count(*) WHERE refer_to_underwriters)) AS refer, 
          SUM( 
      (SELECT count(*)
       WHERE NOT refer_to_underwriters
       OR refer_to_underwriters IS NULL)) AS claims)       
  
   SELECT CASE WHEN ((refer > 0) AND (claims > 0)) THEN 
                round (100.00 * (refer / (claims + refer)), 2)
                ELSE 0
                END                 
   FROM refer_uw
  ) AS "% of work referred to London?",
 
  -- Volume of files held open for recovery/subrogation 
  SUM((SELECT count(*) 
       WHERE open_for_recovery AND status = 'Open'))
  AS "Volume of files held open for recovery/subrogation", 
 
  -- * Performance
  -- --------------
 
  --% Open claims referred to UW's outside of Market 5 day SLA 
  (WITH refer_uw AS (
   SELECT SUM(
     (SELECT count(*) 
       WHERE over_authority::date <@ "Reporting Period"
       AND ((over_authority - COALESCE (claim_received_time, open_date))
            > INTERVAL '5 days'))) AS gt,
    SUM(CASE "Is Claim Open?"
              WHEN true THEN 1 ELSE 0 END) AS openclaims
  )        
  
   SELECT CASE WHEN ((gt > 0) AND (openclaims > 0)) THEN 
                round (100.00 * (gt / openclaims), 2)
                ELSE 0
               END
   FROM refer_uw
  ) AS "% Open claims referred to UW's outside of Market 5 day SLA",
 
  -- Volume of Claims not acknowledged within Market 48 hours SLA 
  SUM((SELECT count(*) 
       WHERE claim_acknowledged_time::date <@ "Reporting Period"
       AND (COALESCE(claim_acknowledged_time, upper("Reporting Period")) - claim_received_time) > INTERVAL '48 hours'))
    AS "Volume of Claims not acknowledged within Market 48 hours SLA",
 
  -- Volume of overdue diary items in the last reporting period?
  SUM("Volume of Overdue Diary Items") AS "Volume of Overdue Diary Items", 
 
  -- % of Open Claims peer reviewed    
  (WITH peer_rev AS (
   SELECT SUM(
     (SELECT count(*) 
       WHERE peer_reviewed_date::date <@ "Reporting Period")) AS gt,
    SUM(CASE "Is Claim Open?"
              WHEN true THEN 1 ELSE 0 END) AS openclaims
  )        
  
   SELECT CASE WHEN ((gt > 0) AND (openclaims > 0)) THEN 
                round (100.00 * (gt / openclaims), 2)
                ELSE 0
               END
   FROM peer_rev
  ) AS "% of Open Claims Peer Reviewed", 
 
  -- Average days taken from 1st notification to 1st indemnity payment 
  SUM((WITH notif_to_indem AS (
   SELECT 
          MIN(transaction_date) AS payment
    FROM claim_transaction AS ct
    WHERE ct.claim_id = mi.claim_id
    AND claim_transaction_is_paid(ct)
    AND claim_transaction_is_indemnity(ct)
  ), ninterval AS (
   SELECT (payment - COALESCE(mi.claim_received_time, mi.open_date)) AS n_to_i
    FROM notif_to_indem 
    WHERE payment::date <@ "Reporting Period"
  )
          
   SELECT EXTRACT(day from n_to_i) 
   FROM (
  
   SELECT SUM(n_to_i) / (SELECT count(*) FROM notif_to_indem) AS n_to_i
    FROM ninterval
    WHERE n_to_i > INTERVAL '0 Days'
  ) AS foo ))
   AS "Average days from 1st notification to 1st indemnity payment",
 
  -- What is the average days taken from 1st notification to close
  round (
  SUM((SELECT round((EXTRACT(day from (close_date - COALESCE(claim_received_time, open_date)))))
       WHERE close_date::date <@ "Reporting Period")) 
   / SUM((SELECT count(*)
          WHERE close_date::date <@ "Reporting Period"))
  ) AS "Average days from 1st notification to closed",
 
  -- Average number of days to establish initial reserve from notice of claim
    (WITH outs AS ( 
      SELECT SUM(
          (SELECT count(*) 
           FROM 
            (SELECT mi.claim_id,
                   (claim_outstanding_reserve(mi.claim_id, upper("Reporting Period")) > 0) AS has_reserve,
                   mi.claim_received_time AS noti)
            AS n_to_r
   WHERE has_reserve AND noti IS NOT NULL)
              ) AS outs
  
    ), sums AS (
      SELECT MAX(
          (SELECT (SELECT ct.transaction_date
                    FROM claim_transaction AS ct 
                    WHERE ct.claim_id = n_to_r.claim_id 
                    AND ct.transaction_type_id = 1
                    ORDER BY transaction_date LIMIT 1) 
                  - noti 
  FROM 
            (SELECT mi.claim_id, (claim_outstanding_reserve(mi.claim_id, upper("Reporting Period"))
               > 0) AS has_reserve, mi.claim_received_time AS noti)
            AS n_to_r
        WHERE has_reserve AND noti IS NOT NULL)
        ) AS sums
  
    )
    SELECT round(extract(day from (sums / outs))) from sums, outs)
  
    AS "Average days to establish initial reserve from notice of claim"
  ,
 
  -- Variance of final to initial reserve (as %) 
  round 
   (sum(
    (SELECT * 
      FROM json_to_record("Variance of final to initial reserve (as %)")
            AS x(variance_percent numeric)))
      / count("Variance of final to initial reserve (as %)"), 2)
  AS "Variance of final to initial reserve (as %)",
 
  --  Volume of nil reserve claims older than 6 months (as percentage)
  (COUNT("Volume of nil reserve claims older than 6 months (as %)")
  / COUNT("Is Claim Open?")) * 100 AS "Volume of nil reserve claims older than 6 months (as %)",
 
  -- Response time for acknowledging new claims
  ( sum("Response time for acknowledging new claims") 
   / count("Response time for acknowledging new claims"))
  AS "Response time for acknowledging new claims"
  ,
 
 -- * Volume of Claims Examiners
 -- ----------------------------
 
  -- How many full time Claims Examiners do you have?
  (SELECT count(*) FROM app_adjuster WHERE full_time) 
   AS "How many full time Claims Examiners do you have?"
 
  -- json_agg(json_build_object(
  --  'claim_id', mi.claim_id)) AS claims
 
    FROM first_mi AS mi
    GROUP BY  "Reporting Period", syndicate_id
  ) 
 
 , 
  mi AS (
  SELECT * FROM second_mi
  )

 SELECT * from mi
);

-- DROP FUNCTION mi_report(date,date,person) ;
CREATE OR REPLACE FUNCTION mi_report ("start" date, "end" date, syndicate_id integer)
RETURNS mi_report AS $$

 WITH _first_mi AS (
  SELECT *, 
         daterange($1, $2) AS "Reporting Period"
   FROM claim AS c 
   JOIN risk AS r USING (risk_id)
   JOIN contract AS co USING (contract_id)
   WHERE co.syndicate_id = $3
),
  first_mi AS (
   SELECT *, 
      CASE WHEN (claim_status(claim_id, upper("Reporting Period")- INTERVAL '1 Day') = 'Open')
          THEN true ELSE false END AS "Is Claim Open?", 
      ( SELECT count(*) 
       FROM 
        (SELECT GREATEST (d.action_date, MAX(defer_date)) AS due_date
        FROM diary_entry AS d
        JOIN defer_diary_entry AS dde
        USING (diary_entry_id)
        WHERE d.claim_id = mi.claim_id
        AND d.processed = false 
        GROUP BY action_date) AS d
       WHERE d.due_date::date <@ "Reporting Period")
        AS "Volume of Overdue Diary Items",
      
      (WITH foo AS (
        SELECT (mi.claim_received_time + INTERVAL '6 months') AS timeline
        WHERE mi.status = 'Open'
       ), bar AS (
        SELECT *, claim_reserve(mi.claim_id) AS reserve        
         FROM foo 
         WHERE foo.timeline::date <@ "Reporting Period"
       ), baz AS (
         SELECT 1 FROM bar WHERE reserve = 0 OR reserve IS NULL
      ) 
        SELECT * from baz) AS 
       "Volume of nil reserve claims older than 6 months (as %)"
      , 
      (WITH res_paid AS (
       SELECT claim_id, 
              claim_indemnity_reserve(mi.claim_id) AS ores, 
              claim_indemnity_paid(mi.claim_id) AS paid
        -- make sure that the claim is closed in this period
        WHERE mi.status = 'Closed' AND mi.close_date::date <@ "Reporting Period"
      ), final_res AS (
       SELECT claim_id, ores as first, ores - paid AS final, paid AS paid
       FROM res_paid
       -- only for reserves that are > 0
       WHERE ores > 0
      ), thing AS (
        SELECT json_build_object(
                'claim_id', claim_id,
                'indemnity_open_reserve', first,
                'indemnity_final_reserve', final,
                'variance_percent', (SELECT round(((first - final) / first) * 100, 2)),
                'indemnity_paid', paid
               ) AS jso
      FROM final_res
        WHERE first > 0 AND mi.status = 'Closed'
      ) 
      
       SELECT jso FROM thing)
      AS "Variance of final to initial reserve (as %)"
      ,
      (WITH days AS (
       SELECT extract(days from mi.claim_acknowledged_time - 
                      mi.claim_received_time) AS days
         WHERE mi.claim_acknowledged_time IS NOT NULL 
         AND   mi.claim_received_time IS NOT NULL
         AND  mi.claim_acknowledged_time::date  <@ "Reporting Period"
       ) SELECT days FROM days) 
      AS "Response time for acknowledging new claims"
      
 --      AS "Initial Reserve"
 
    FROM _first_mi AS mi
  ), 
 
  second_mi AS (
 
   SELECT  
 
   -- array_to_json(array_remove(array_agg("Variance of final to initial reserve (as %)"::text), NULL))
  --  AS variance, 
 
  -- lower("Reporting Period") AS "Start Day", COND(upper("Reporting
  -- Period") + '1 Day'::INTERVAL)::date, upper_inc("Reporting Period"),
  person_name(syndicate_id) AS "Syndicate Name", 
  syndicate_id AS "syndicate_id", 
  concat(lower("Reporting Period")::text, ' to ',  upper("Reporting Period")::text) AS  "Reporting Period",
 
  -- * Claims Counts
  -- ---------------
 
  -- Total Volume of Claims
  count(claim_id) AS "Volume of Claims",
 
  -- Volume of Open Claims
  SUM(CASE "Is Claim Open?" WHEN true THEN 1 ELSE 0 END)
   AS "Volume of Open Claims",
 
  -- Volume of Closed Claims
  SUM(CASE WHEN (close_date::date <@ "Reporting Period")
           THEN 1 ELSE 0 END)
  AS "Volume of Closed Claims",
 
  -- Volume of Reopened Claims 
  SUM((SELECT count(*) WHERE rev_date::date <@ "Reporting Period")) 
   AS "Volume of Re-Opened Claims",
 
  -- Volume of New Claims
  SUM((SELECT count(*) 
       WHERE open_date::date <@ "Reporting Period")) AS "Volume of New Claims",
 
  -- Value of open claims
  SUM( CASE "Is Claim Open?" 
        WHEN true THEN claim_incurred(claim_id, (upper("Reporting Period") 
                                              - INTERVAL '1 Day'))::money
       END
      )
    AS "Value of open claims",
 
  --  % of work referred to London? 
  (WITH refer_uw AS (
   SELECT SUM(
     (SELECT count(*) WHERE refer_to_underwriters)) AS refer, 
          SUM( 
      (SELECT count(*)
       WHERE NOT refer_to_underwriters
       OR refer_to_underwriters IS NULL)) AS claims)       
  
   SELECT CASE WHEN ((refer > 0) AND (claims > 0)) THEN 
                round (100.00 * (refer / (claims + refer)), 2)
                ELSE 0
                END                 
   FROM refer_uw
  ) AS "% of work referred to London?",
 
  -- Volume of files held open for recovery/subrogation 
  SUM((SELECT count(*) 
       WHERE open_for_recovery AND status = 'Open'))
  AS "Volume of files held open for recovery/subrogation", 
 
  -- * Performance
  -- --------------
 
  --% Open claims referred to UW's outside of Market 5 day SLA 
  (WITH refer_uw AS (
   SELECT SUM(
     (SELECT count(*) 
       WHERE over_authority::date <@ "Reporting Period"
       AND ((over_authority - COALESCE (claim_received_time, open_date))
            > INTERVAL '5 days'))) AS gt,
    SUM(CASE "Is Claim Open?"
              WHEN true THEN 1 ELSE 0 END) AS openclaims
  )        
  
   SELECT CASE WHEN ((gt > 0) AND (openclaims > 0)) THEN 
                round (100.00 * (gt / openclaims), 2)
                ELSE 0
               END
   FROM refer_uw
  ) AS "% Open claims referred to UW's outside of Market 5 day SLA",
 
  -- Volume of Claims not acknowledged within Market 48 hours SLA 
  SUM((SELECT count(*) 
       WHERE claim_acknowledged_time::date <@ "Reporting Period"
       AND (COALESCE(claim_acknowledged_time, upper("Reporting Period")) - claim_received_time) > INTERVAL '48 hours'))
    AS "Volume of Claims not acknowledged within Market 48 hours SLA",
 
  -- Volume of overdue diary items in the last reporting period?
  SUM("Volume of Overdue Diary Items") AS "Volume of Overdue Diary Items", 
 
  -- % of Open Claims peer reviewed    
  (WITH peer_rev AS (
   SELECT SUM(
     (SELECT count(*) 
       WHERE peer_reviewed_date::date <@ "Reporting Period")) AS gt,
    SUM(CASE "Is Claim Open?"
              WHEN true THEN 1 ELSE 0 END) AS openclaims
  )        
  
   SELECT CASE WHEN ((gt > 0) AND (openclaims > 0)) THEN 
                round (100.00 * (gt / openclaims), 2)
                ELSE 0
               END
   FROM peer_rev
  ) AS "% of Open Claims Peer Reviewed", 
 
  -- Average days taken from 1st notification to 1st indemnity payment 
  SUM((WITH notif_to_indem AS (
   SELECT 
          MIN(transaction_date) AS payment
    FROM claim_transaction AS ct
    WHERE ct.claim_id = mi.claim_id
    AND claim_transaction_is_paid(ct)
    AND claim_transaction_is_indemnity(ct)
  ), ninterval AS (
   SELECT (payment - COALESCE(mi.claim_received_time, mi.open_date)) AS n_to_i
    FROM notif_to_indem 
    WHERE payment::date <@ "Reporting Period"
  )
          
   SELECT EXTRACT(day from n_to_i) 
   FROM (
  
   SELECT SUM(n_to_i) / (SELECT count(*) FROM notif_to_indem) AS n_to_i
    FROM ninterval
    WHERE n_to_i > INTERVAL '0 Days'
  ) AS foo ))
   AS "Average days from 1st notification to 1st indemnity payment",
 
  -- What is the average days taken from 1st notification to close
  round (
  SUM((SELECT round((EXTRACT(day from (close_date - COALESCE(claim_received_time, open_date)))))
       WHERE close_date::date <@ "Reporting Period")) 
   / SUM((SELECT count(*)
          WHERE close_date::date <@ "Reporting Period"))
  ) AS "Average days from 1st notification to closed",
 
  -- Average number of days to establish initial reserve from notice of claim
    (WITH outs AS ( 
      SELECT SUM(
          (SELECT count(*) 
           FROM 
            (SELECT mi.claim_id,
                   (claim_outstanding_reserve(mi.claim_id, upper("Reporting Period")) > 0) AS has_reserve,
                   mi.claim_received_time AS noti)
            AS n_to_r
   WHERE has_reserve AND noti IS NOT NULL)
              ) AS outs
  
    ), sums AS (
      SELECT MAX(
          (SELECT (SELECT ct.transaction_date
                    FROM claim_transaction AS ct 
                    WHERE ct.claim_id = n_to_r.claim_id 
                    AND ct.transaction_type_id = 1
                    ORDER BY transaction_date LIMIT 1) 
                  - noti 
  FROM 
            (SELECT mi.claim_id, (claim_outstanding_reserve(mi.claim_id, upper("Reporting Period"))
               > 0) AS has_reserve, mi.claim_received_time AS noti)
            AS n_to_r
        WHERE has_reserve AND noti IS NOT NULL)
        ) AS sums
  
    )
    SELECT round(extract(day from (sums / outs))) from sums, outs)
  
    AS "Average days to establish initial reserve from notice of claim"
  ,
 
  -- Variance of final to initial reserve (as %) 
  round 
   (sum(
    (SELECT * 
      FROM json_to_record("Variance of final to initial reserve (as %)")
            AS x(variance_percent numeric)))
      / count("Variance of final to initial reserve (as %)"), 2)
  AS "Variance of final to initial reserve (as %)",
 
  --  Volume of nil reserve claims older than 6 months (as percentage)
  (COUNT("Volume of nil reserve claims older than 6 months (as %)")
  / COUNT("Is Claim Open?")) * 100 AS "Volume of nil reserve claims older than 6 months (as %)",
 
  -- Response time for acknowledging new claims
  ( sum("Response time for acknowledging new claims") 
   / count("Response time for acknowledging new claims"))
  AS "Response time for acknowledging new claims"
  ,
 
 -- * Volume of Claims Examiners
 -- ----------------------------
 
  -- How many full time Claims Examiners do you have?
  (SELECT count(*) FROM app_adjuster WHERE full_time) 
   AS "How many full time Claims Examiners do you have?"
 
  -- json_agg(json_build_object(
  --  'claim_id', mi.claim_id)) AS claims
 
    FROM first_mi AS mi
    GROUP BY  "Reporting Period", syndicate_id
  ) 
 
 

 SELECT * FROM second_mi;

$$ LANGUAGE SQL;

-- DROP FUNCTION mi_report(date,date,person) ;
CREATE OR REPLACE FUNCTION mi_report ("start" date, "end" date, contract)
RETURNS mi_report AS $$

 WITH _first_mi AS (
  SELECT *, 
         daterange($1, $2) AS "Reporting Period"
   FROM claim AS c 
   JOIN risk AS r USING (risk_id)
   JOIN contract AS co USING (contract_id)
   WHERE co.contract_id = $3.contract_id
),
  first_mi AS (
   SELECT *, 
      CASE WHEN (claim_status(claim_id, upper("Reporting Period")- INTERVAL '1 Day') = 'Open')
          THEN true ELSE false END AS "Is Claim Open?", 
      ( SELECT count(*) 
       FROM 
        (SELECT GREATEST (d.action_date, MAX(defer_date)) AS due_date
        FROM diary_entry AS d
        JOIN defer_diary_entry AS dde
        USING (diary_entry_id)
        WHERE d.claim_id = mi.claim_id
        AND d.processed = false 
        GROUP BY action_date) AS d
       WHERE d.due_date::date <@ "Reporting Period")
        AS "Volume of Overdue Diary Items",
      
      (WITH foo AS (
        SELECT (mi.claim_received_time + INTERVAL '6 months') AS timeline
        WHERE mi.status = 'Open'
       ), bar AS (
        SELECT *, claim_reserve(mi.claim_id) AS reserve        
         FROM foo 
         WHERE foo.timeline::date <@ "Reporting Period"
       ), baz AS (
         SELECT 1 FROM bar WHERE reserve = 0 OR reserve IS NULL
      ) 
        SELECT * from baz) AS 
       "Volume of nil reserve claims older than 6 months (as %)"
      , 
      (WITH res_paid AS (
       SELECT claim_id, 
              claim_indemnity_reserve(mi.claim_id) AS ores, 
              claim_indemnity_paid(mi.claim_id) AS paid
        -- make sure that the claim is closed in this period
        WHERE mi.status = 'Closed' AND mi.close_date::date <@ "Reporting Period"
      ), final_res AS (
       SELECT claim_id, ores as first, ores - paid AS final, paid AS paid
       FROM res_paid
       -- only for reserves that are > 0
       WHERE ores > 0
      ), thing AS (
        SELECT json_build_object(
                'claim_id', claim_id,
                'indemnity_open_reserve', first,
                'indemnity_final_reserve', final,
                'variance_percent', (SELECT round(((first - final) / first) * 100, 2)),
                'indemnity_paid', paid
               ) AS jso
      FROM final_res
        WHERE first > 0 AND mi.status = 'Closed'
      ) 
      
       SELECT jso FROM thing)
      AS "Variance of final to initial reserve (as %)"
      ,
      (WITH days AS (
       SELECT extract(days from mi.claim_acknowledged_time - 
                      mi.claim_received_time) AS days
         WHERE mi.claim_acknowledged_time IS NOT NULL 
         AND   mi.claim_received_time IS NOT NULL
         AND  mi.claim_acknowledged_time::date  <@ "Reporting Period"
       ) SELECT days FROM days) 
      AS "Response time for acknowledging new claims"
      
 --      AS "Initial Reserve"
 
    FROM _first_mi AS mi
  ), 
 
  second_mi AS (
 
   SELECT  
 
   -- array_to_json(array_remove(array_agg("Variance of final to initial reserve (as %)"::text), NULL))
  --  AS variance, 
 
  -- lower("Reporting Period") AS "Start Day", COND(upper("Reporting
  -- Period") + '1 Day'::INTERVAL)::date, upper_inc("Reporting Period"),
  person_name(syndicate_id) AS "Syndicate Name", 
  syndicate_id AS "syndicate_id", 
  concat(lower("Reporting Period")::text, ' to ',  upper("Reporting Period")::text) AS  "Reporting Period",
 
  -- * Claims Counts
  -- ---------------
 
  -- Total Volume of Claims
  count(claim_id) AS "Volume of Claims",
 
  -- Volume of Open Claims
  SUM(CASE "Is Claim Open?" WHEN true THEN 1 ELSE 0 END)
   AS "Volume of Open Claims",
 
  -- Volume of Closed Claims
  SUM(CASE WHEN (close_date::date <@ "Reporting Period")
           THEN 1 ELSE 0 END)
  AS "Volume of Closed Claims",
 
  -- Volume of Reopened Claims 
  SUM((SELECT count(*) WHERE rev_date::date <@ "Reporting Period")) 
   AS "Volume of Re-Opened Claims",
 
  -- Volume of New Claims
  SUM((SELECT count(*) 
       WHERE open_date::date <@ "Reporting Period")) AS "Volume of New Claims",
 
  -- Value of open claims
  SUM( CASE "Is Claim Open?" 
        WHEN true THEN claim_incurred(claim_id, (upper("Reporting Period") 
                                              - INTERVAL '1 Day'))::money
       END
      )
    AS "Value of open claims",
 
  --  % of work referred to London? 
  (WITH refer_uw AS (
   SELECT SUM(
     (SELECT count(*) WHERE refer_to_underwriters)) AS refer, 
          SUM( 
      (SELECT count(*)
       WHERE NOT refer_to_underwriters
       OR refer_to_underwriters IS NULL)) AS claims)       
  
   SELECT CASE WHEN ((refer > 0) AND (claims > 0)) THEN 
                round (100.00 * (refer / (claims + refer)), 2)
                ELSE 0
                END                 
   FROM refer_uw
  ) AS "% of work referred to London?",
 
  -- Volume of files held open for recovery/subrogation 
  SUM((SELECT count(*) 
       WHERE open_for_recovery AND status = 'Open'))
  AS "Volume of files held open for recovery/subrogation", 
 
  -- * Performance
  -- --------------
 
  -- % Open claims referred to UW's outside of Market 5 day SLA 
  (WITH refer_uw AS (
   SELECT SUM(
     (SELECT count(*) 
       WHERE over_authority::date <@ "Reporting Period"
       AND ((over_authority - COALESCE (claim_received_time, open_date))
            > INTERVAL '5 days'))) AS gt,
    SUM(CASE "Is Claim Open?"
              WHEN true THEN 1 ELSE 0 END) AS openclaims
  )        
  
   SELECT CASE WHEN ((gt > 0) AND (openclaims > 0)) THEN 
                round (100.00 * (gt / openclaims), 2)
                ELSE 0
               END
   FROM refer_uw
  ) AS "% Open claims referred to UW's outside of Market 5 day SLA",
 
  -- Volume of Claims not acknowledged within Market 48 hours SLA 
  SUM((SELECT count(*) 
       WHERE claim_acknowledged_time::date <@ "Reporting Period"
       AND (COALESCE(claim_acknowledged_time, upper("Reporting Period")) - claim_received_time) > INTERVAL '48 hours'))
    AS "Volume of Claims not acknowledged within Market 48 hours SLA",
 
  -- Volume of overdue diary items in the last reporting period?
  SUM("Volume of Overdue Diary Items") AS "Volume of Overdue Diary Items", 
 
  -- % of Open Claims peer reviewed    
  (WITH peer_rev AS (
   SELECT SUM(
     (SELECT count(*) 
       WHERE peer_reviewed_date::date <@ "Reporting Period")) AS gt,
    SUM(CASE "Is Claim Open?"
              WHEN true THEN 1 ELSE 0 END) AS openclaims
  )        
  
   SELECT CASE WHEN ((gt > 0) AND (openclaims > 0)) THEN 
                round (100.00 * (gt / openclaims), 2)
                ELSE 0
               END
   FROM peer_rev
  ) AS "% of Open Claims Peer Reviewed", 
 
  -- Average days taken from 1st notification to 1st indemnity payment 
  SUM((WITH notif_to_indem AS (
   SELECT 
          MIN(transaction_date) AS payment
    FROM claim_transaction AS ct
    WHERE ct.claim_id = mi.claim_id
    AND claim_transaction_is_paid(ct)
    AND claim_transaction_is_indemnity(ct)
  ), ninterval AS (
   SELECT (payment - COALESCE(mi.claim_received_time, mi.open_date)) AS n_to_i
    FROM notif_to_indem 
    WHERE payment::date <@ "Reporting Period"
  )
          
   SELECT EXTRACT(day from n_to_i) 
   FROM (
  
   SELECT SUM(n_to_i) / (SELECT count(*) FROM notif_to_indem) AS n_to_i
    FROM ninterval
    WHERE n_to_i > INTERVAL '0 Days'
  ) AS foo ))
   AS "Average days from 1st notification to 1st indemnity payment",
 
  -- What is the average days taken from 1st notification to close
  round (
  SUM((SELECT round((EXTRACT(day from (close_date - COALESCE(claim_received_time, open_date)))))
       WHERE close_date::date <@ "Reporting Period")) 
   / SUM((SELECT count(*)
          WHERE close_date::date <@ "Reporting Period"))
  ) AS "Average days from 1st notification to closed",
 
  -- Average number of days to establish initial reserve from notice of claim
    (WITH outs AS ( 
      SELECT SUM(
          (SELECT count(*) 
           FROM 
            (SELECT mi.claim_id,
                   (claim_outstanding_reserve(mi.claim_id, upper("Reporting Period")) > 0) AS has_reserve,
                   mi.claim_received_time AS noti)
            AS n_to_r
   WHERE has_reserve AND noti IS NOT NULL)
              ) AS outs
  
    ), sums AS (
      SELECT MAX(
          (SELECT (SELECT ct.transaction_date
                    FROM claim_transaction AS ct 
                    WHERE ct.claim_id = n_to_r.claim_id 
                    AND ct.transaction_type_id = 1
                    ORDER BY transaction_date LIMIT 1) 
                  - noti 
  FROM 
            (SELECT mi.claim_id, (claim_outstanding_reserve(mi.claim_id, upper("Reporting Period"))
               > 0) AS has_reserve, mi.claim_received_time AS noti)
            AS n_to_r
        WHERE has_reserve AND noti IS NOT NULL)
        ) AS sums
  
    )
    SELECT round(extract(day from (sums / outs))) from sums, outs)
  
    AS "Average days to establish initial reserve from notice of claim"
  ,
 
  -- Variance of final to initial reserve (as %) 
  round 
   (sum(
    (SELECT * 
      FROM json_to_record("Variance of final to initial reserve (as %)")
            AS x(variance_percent numeric)))
      / count("Variance of final to initial reserve (as %)"), 2)
  AS "Variance of final to initial reserve (as %)",
 
  --  Volume of nil reserve claims older than 6 months (as percentage)
  (COUNT("Volume of nil reserve claims older than 6 months (as %)")
  / COUNT("Is Claim Open?")) * 100 AS "Volume of nil reserve claims older than 6 months (as %)",
 
  -- Response time for acknowledging new claims
  ( sum("Response time for acknowledging new claims") 
   / count("Response time for acknowledging new claims"))
  AS "Response time for acknowledging new claims"
  ,
 
 -- * Volume of Claims Examiners
 -- ----------------------------
 
  -- How many full time Claims Examiners do you have?
  (SELECT count(*) FROM app_adjuster WHERE full_time) 
   AS "How many full time Claims Examiners do you have?"
 
  -- json_agg(json_build_object(
  --  'claim_id', mi.claim_id)) AS claims
 
    FROM first_mi AS mi
    GROUP BY  "Reporting Period", syndicate_id
  ) 
 
 

 SELECT * FROM second_mi;

$$ LANGUAGE SQL;
