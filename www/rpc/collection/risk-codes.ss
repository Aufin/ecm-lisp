(import
  :std/sugar :std/db/dbi :std/srfi/13 :std/text/utf8
  :std/text/json :std/srfi/19
  "../../db" "../../req" "../../user")

(export handle-request)


(def (fetch-risk-codes) 
  (def (esql q . args) (apply ecm-sql sql-eval-query q args))

  (def query "
SELECT json_build_object('results', json_agg(r)) FROM
 (SELECT * FROM risk_code
   ORDER BY risk_code = 'TBD', risk_code.risk_code) r
    ")

  (car (esql query)))

(def handle-request
  (user-request-handler
   (lambda (req res)
     (def risk-codes (fetch-risk-codes))

     (http-response-write
      res 200 '(("Content-Type" . "application/json"))
	  risk-codes))))
