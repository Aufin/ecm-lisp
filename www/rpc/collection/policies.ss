(import
  :std/sugar :std/db/dbi :std/srfi/13 :std/text/utf8
  :std/text/json :std/srfi/19
  "../../db" "../../req" "../../user")

(export handle-request)


(def (fetch-policies term (limit 20) (offset #f)) 
  (def (esql q . args) (apply ecm-sql sql-eval-query q args))
  (car (esql "SELECT search_policy($1::text, $2::int, $3::int)"
			 term limit offset)))
  ;(car (esql "SELECT search_policy($1, $2::int, $3::int)")))


(def handle-request
  (user-request-handler
   (lambda (req res)
	 (def body (http-request-body req))
	 (def jso (and body (bytes->json-object body)))
	 (def term (and jso (hash-get jso "term")))
	 (def limit (and jso (hash-get jso "limit")))
	 (def offset (and jso (hash-get jso "offset")))
     (def policies (fetch-policies
					(or term "MD")
					(if (or (void? limit) (not limit))
					  10
					  limit)

					(if (not (number? offset))
					  0
					  offset)))

     (http-response-write
      res 200 '(("Content-Type" . "application/json"))
	  policies))))
