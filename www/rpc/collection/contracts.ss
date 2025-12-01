(import
  :std/sugar :std/db/dbi :std/srfi/13 :std/text/utf8
  :std/text/json :std/srfi/19
  "../../db" "../../req" "../../user")

(export handle-request)


(def (fetch-contracts) 
  (def (esql q . args) (apply ecm-sql sql-eval-query q args))


  (def contracts-text "
 SELECT json_agg(row_to_json(cs)) AS contracts
       FROM
      (SELECT *,
           person_name(agency_id) AS agency,
           person_name(syndicate_id) AS syndicate,
           person_name(london_broker_id) AS london_broker,
           person_name(insurance_company_id) AS insurance_company

           FROM contract WHERE contract_number IS NOT NULL
             ORDER BY contract_id DESC) cs
    ")

  (car (esql contracts-text)))

(def handle-request
  (user-request-handler
   (lambda (req res)
     (def contracts (fetch-contracts))

     (http-response-write
      res 200 '(("Content-Type" . "application/json"))
	  contracts))))
