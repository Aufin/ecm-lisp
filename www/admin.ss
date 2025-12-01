(import
  :std/db/conpool
  :std/net/httpd
  :std/format
  :std/db/dbi
  :std/interactive
  :std/actor
  "db" "req" "user")
(export handle-request)

(def (login-meta token)
  (ecm-sql sql-eval-query "
  SELECT to_json(meta) FROM (
   SELECT id as token, username, login_time, until
   FROM login.active LEFT JOIN login.login USING (id) LEFT JOIN app_user ON (user_id = app_user_id)
   WHERE id = $1::uuid) meta"
   token))

(def handle-request
  (user-request-handler
   (lambda (req res)

										; (set! std/net/httpd/handler#max-token-length 11024)
     (def tok (assget "ecm-login"
					  (http-request-cookies req)))

     ;; timeout comes from where?


     ;; (thread-sleep! 60) 
     (http-response-write
      res 200 '(("Content-Type" . "text/html"))
	  (format "This should also work. why no clear? :<b> ~a </b>"

     (hash-map (lambda (tok pool) (login-meta tok)) token-db-conpool-table)))
	 
     (hash-map (lambda (tok pool) (list tok pool (conpool-close pool))) token-db-conpool-table)
	 (hash-clear! token-db-conpool-table)

	 )))
