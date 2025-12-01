(import
  :std/sugar :std/db/dbi :std/srfi/13 :std/text/utf8
  :std/text/json :std/srfi/19
  "../../db" "../../req" "../../user")

(export handle-request)


(def (fetch-risk-types) 
  (def (esql q . args) (apply ecm-sql sql-eval-query q args))

  (def query "
SELECT json_build_object('results', json_agg(r)) FROM (SELECT type_name FROM risk_type
ORDER BY
type_name = 'Commercial' DESC,
type_name = 'Habitational' DESC,
type_name) r ;
    ")

  (car (esql query)))

(def handle-request
  (user-request-handler
   (lambda (req res)
     (def risk-types (fetch-risk-types))

     (http-response-write
      res 200 '(("Content-Type" . "application/json"))
	  risk-types))))
