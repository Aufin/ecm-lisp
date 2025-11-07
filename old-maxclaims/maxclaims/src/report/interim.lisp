(defpackage :maxclaims/report/interim
  (:use :cl)
  (:import-from :rofl 
		#:select)
  (:import-from :maxclaims
		  #:with-adb
		  #:risk-type.type-name)
  (:export 
   #:interim-report))

(in-package :maxclaims/report/interim)

(defun interim-report (&key (minimum-minutes 15))
  (let ((report 
	 (postmodern:query 
	  (string '#:| 

WITH interim AS (

    SELECT claim.claim_id, 
           max(COALESCE (ti.date, '1900-01-01')) AS interim_date
          , adjuster_id, syndicate_id, claim.refer_to_underwriters
     FROM claim
     LEFT JOIN risk USING (risk_id)
     LEFT JOIN contract USING (contract_id)
     LEFT JOIN timecard_interim AS ti 
      ON claim.claim_id = ti.claim_id
      WHERE claim.status = 'Open'
       GROUP BY claim.claim_id, syndicate_id
 ), unbilled AS (
       SELECT 'claim'::text AS Heading,
               t.claim_id AS "Claim Number", 
               sum(minutes) AS "Total Minutes",
               person_short_name(adjuster_id) AS "Examiner",
               person_name(syndicate_id) AS "Syndicate",
               CASE WHEN refer_to_underwriters THEN 'Yes' ELSE 'No' END AS "Over Authority? (Yes/No)"

       FROM timecard AS t RIGHT JOIN interim AS ti 
        ON ti.claim_id = t.claim_id
       WHERE t.date > ti.interim_date 
       GROUP BY t.claim_id, adjuster_id, refer_to_underwriters, syndicate_id
 ), unbilled_gte AS (
   SELECT * from unbilled WHERE "Total Minutes" >= $1
 ), report AS (
   SELECT * from unbilled_gte
   UNION
   SELECT 'All Claims'::text,
          count(*),
          sum("Total Minutes"),
          ' ', ' ', NULL
     FROM unbilled_gte

  ORDER by "Claim Number"
 )

 SELECT * FROM report

|) 

	  minimum-minutes
	  :str-alists)))
    (mapcar (lambda (alist) 
	      (loop for (key . val) in alist
		 :collect (cons key (typecase val 
				      (ratio (float val))
				      (t val)))))
	    report)))


  

