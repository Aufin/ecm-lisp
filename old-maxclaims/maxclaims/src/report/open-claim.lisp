(defpackage :maxclaims/report/open-claim
  (:use :cl)
  (:import-from :rofl
		#:select)
  (:import-from :maxclaims
		  #:with-adb
		  #:risk-type.type-name)
  (:export
   #:open-claim-report))

(in-package :maxclaims/report/open-claim)

(defparameter *open-claim-report*
  "
SELECT person_name(adjuster_id) \"Examiner\",
       open AS  \"Open Master Claims\",
       count(*)  \"All Open Claims\",
       array_to_string(array_agg(claim ORDER BY claim), ',') AS \"Open Claim Numbers\"
FROM
 (SELECT claim, adjuster_id, open
  FROM
  (SELECT ca.value AS claims, adjuster_id, open
    FROM (
     SELECT json_agg(claims) AS aclaims,
            adjuster_id,
            count(*) AS open
      FROM (SELECT array_agg(claim_id) as claims, adjuster_id
             FROM claim
              RIGHT JOIN risk USING (risk_id)
              RIGHT JOIN policy USING (policy_id)
              WHERE status = 'Open'
              GROUP BY policy_id, date_of_loss, adjuster_id) AS alla
      GROUP BY adjuster_id ORDER BY adjuster_id DESC) AS jso,
  json_array_elements(aclaims) AS ca) AS one,
 json_array_elements_text(claims) AS claim) AS rep
GROUP BY adjuster_id, open ORDER BY person_name(adjuster_id)
")
(defun open-claim-report ()
  (postmodern:query *open-claim-report* :str-alists))
