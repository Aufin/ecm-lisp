CREATE OR REPLACE VIEW app_user_claim_time_recorded AS (
 WITH report AS (
  SELECT 
   tstzrange(NULL,
             NULL) AS "Report Period",
   person_name(person_id) AS "User Name", 
   app_user_id
  FROM app_user 
 ), time_recorded AS (
 SELECT 
   "Report Period",
   app_user_id,
   "User Name",
   tc.claim_id 
     AS "Claim Number",
   count(*) 
     AS "Number of Timecards", 
   sum(minutes) 
     AS "Total Hours", 
   sum(mileage_km) 
     AS "Total Mileage",
   sum(disbursements) 
     AS "Total Disbursements",
   json_agg(pongo.as_json(tc)) 
     AS "Timecards"

  FROM timecard AS tc 
  LEFT JOIN report USING (app_user_id)
 GROUP BY claim_id, "Report Period", tc.app_user_id, "User Name"
 ORDER BY claim_id DESC
 )  
 SELECT 
   *
  FROM time_recorded AS tc
);

 
CREATE OR REPLACE FUNCTION public.app_user_claim_time_recorded(app_user, "Report Period" tstzrange DEFAULT tstzrange(NULL::timestamp with time zone, NULL::timestamp with time zone))
 RETURNS SETOF app_user_claim_time_recorded
AS $function$
   WITH report AS (
    SELECT 
     $2 AS "Report Period",
     person_name($1.person_id) AS "User Name", 
     $1.app_user_id
   ), time_recorded AS (
   SELECT 
     "Report Period",
     app_user_id,
     "User Name",
     tc.claim_id 
       AS "Claim Number",
     count(*) 
       AS "Number of Timecards", 
     sum(minutes) 
       AS "Total Hours", 
     sum(mileage_km) 
       AS "Total Mileage",
     sum(disbursements) 
       AS "Total Disbursements",
     json_agg(pongo.as_json(tc)) 
       AS "Timecards"

    FROM timecard AS tc 
    LEFT JOIN report USING (app_user_id)
    WHERE tc.app_user_id = report.app_user_id
     AND tc.date <@ "Report Period"
   GROUP BY claim_id, "Report Period", tc.app_user_id, "User Name"
   ORDER BY claim_id DESC
   )  
   SELECT * from time_recorded

$function$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION public.app_user_claim_time_recorded(app_user_id integer, start_time timestamp with time zone, end_time timestamp with time zone)
 RETURNS SETOF app_user_claim_time_recorded
 LANGUAGE sql
AS $function$
SELECT (app_user_claim_time_recorded (app_user, tstzrange($2, $3))).* 
 FROM app_user WHERE app_user.app_user_id = $1
$function$;


CREATE OR REPLACE FUNCTION json_app_user_claim_time_recorded 
 (app_user_id INTEGER,  
  "start_time" timestamp with time zone, 
  "end_time" timestamp with time zone)
RETURNS json AS $$
WITH report AS (
 SELECT report.* 
  FROM app_user_claim_time_recorded($1, $2, $3) AS report
), json AS (
  SELECT pongo.json_remove(to_json(report.*), 
                            'Report Period', 
                            'User Name', 
                            'app_user_id') AS json FROM report
), json_array AS (
 SELECT json_agg(json.json) AS "Claims"FROM json
), cols AS (
  SELECT DISTINCT "Report Period", 
                  "User Name", app_user_id FROM report
)
 SELECT to_json(report.*) 
  FROM (SELECT json_build_object('start_time', LOWER("Report Period"),
                                 'end_time', UPPER("Report Period"),
                                 'interval' ,  UPPER("Report Period")
                                               -LOWER("Report Period"))
               AS "Report Period", 
                "User Name", app_user_id, json_array.*
                 FROM cols, json_array) AS report
$$ LANGUAGE SQL;
