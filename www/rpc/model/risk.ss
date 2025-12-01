(import
  :std/sugar :std/db/dbi :std/srfi/13 :std/text/utf8
  :std/text/json :std/srfi/19
  "../../db" "../../req" "../../user")


(export handle-request)

(def (create-risk jso)
  (def query "
	INSERT INTO risk(risk_type_name, policy_id, contract_id,
					 risk_code, risk_number)
	VALUES ($1, $2, $3, $4, $5)
    RETURNING risk_to_json(risk);")

  (let-hash jso
	(car (esql query
		  (or .$risk_type .$risk_type_name "Commercial")
		  .$policy_id
		  .$contract_id
		  .$risk_code
		  .$risk_number))))

 ;; The default sync handler maps CRUD to REST like so:
 ;;    create → POST   /collection
 ;;    read → GET   /collection[/id]
 ;;    update → PUT   /collection/id
 ;;    patch → PATCH   /collection/id
 ;;    delete → DELETE   /collection/id


(def handle-request
  (user-request-handler
   (lambda (req res)
	 (def meth (http-request-method req))
	 (def body (http-request-body req))
	 (def jso (and body (bytes->json-object body)))

	 ;; (error "How is caught this?")
	 
	 (def response
	   (cond
		((eq? meth 'POST)
		 (create-risk jso))
		(else "{\"iGotNothing\": \"risk\"}")))
	 
     (http-response-write
      res 200 '(("Content-Type" . "application/json"))
	  response))))
	
  