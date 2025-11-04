(defpackage :maxclaims/report/time-recorded
  (:use :cl)
  (:import-from :rofl 
		#:select)
  (:import-from :maxclaims
		  #:with-adb
		  #:risk-type.type-name)
  (:export 
   #:time-recorded-report))

(in-package :maxclaims/report/time-recorded)

(defun time-recorded-report (&key (start-date "2015-01-01")
			          (end-date "2016-01-01"))
  (postmodern:query 
   "WITH gross_total AS (
 SELECT t.claim_id AS claim_id, t.app_user_id, a.username,
       sum(t.minutes) AS gross_total, 
       sum(t.unbillable_hours) AS gross_total_unbilled 
FROM timecard AS t
JOIN claim AS c
 ON t.claim_id = c.claim_id
JOIN app_user AS a
 ON a.app_user_id = t.app_user_id
WHERE t.date >= $1 AND date_trunc('day', t.date) <= $2
 GROUP BY t.app_user_id, username, t.claim_id ORDER BY t.claim_id), 

group_report AS (
   SELECT (sum(gross_total) + sum(gross_total_unbilled))::TEXT AS \"Total Time Recorded\",
           sum(gross_total)::TEXT AS \"Billable\", 
           sum(gross_total_unbilled)::TEXT AS \"UnBillable\", 
          username AS \"User\",
  ARRAY_AGG(claim_id) AS \"Claims\" from gross_total
   GROUP BY username)


  SELECT * FROM group_report ;" 

   start-date end-date
   :str-alists))


  

