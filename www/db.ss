(import
  :std/db/conpool
  :std/db/postgresql
  :std/db/dbi
  :std/sugar
  :std/os/error
  :std/actor
  :std/error
  "conf")

(export #t)

(set-default-reply-timeout! 601)

;; (update-conf)

(def ecm-conpool-start-times-table
  (make-hash-table weak-keys: #t))

(def (make-pgpool user: (role "maxclaims") (max #f))
  (def host (let (host (conf-value ["database" "master" "host"] #f))
	      (or host "db.ecm.lan")))
  (def port (let (port (conf-value ["database" "master" "port"] #f))
	      (or port 5432)))

  ; (error "Host and Port" host port)
  (def pool 
    (make-conpool
     (cut sql-connect postgresql-connect
          host: host ;"maxbakuntu.lan" ;host
	  port: port
	  ssl: #f user: role
	  db: "maxclaims" timeout: 60)
     max))

  ;; (error "Host and Port And pool" host port pool)
  (hash-put! ecm-conpool-start-times-table
	     pool
	     (time->seconds (current-time)))

  pool)

(def ecm-conpool (make-pgpool))

(def current-db-conpool-timeout (make-parameter 600))

(def (ecm-conpool-timeout? pool)
  (def start-secs (hash-ref ecm-conpool-start-times-table
			    pool 0))
  (def now-secs (time->seconds (current-time)))
  (> (- now-secs start-secs) (current-db-conpool-timeout)))

(def current-db-conpool (make-parameter ecm-conpool))
(def current-db-conpool-constructor (make-parameter make-pgpool))


(def (ecm-sql sql-fn . args)
  (def current-pool (current-db-conpool))

  ;; Check for timeout

  (when (ecm-conpool-timeout? current-pool)
    (try
     (conpool-close current-pool)
     (finally
      (set! current-pool ((current-db-conpool-constructor)))
      (current-db-conpool current-pool))))
  
  (def pool current-pool)
  
  (def conn (conpool-get pool 5))

  (let (err #f)
    (try
     (apply sql-fn conn args)
     (catch (e)
       (set! err e)
       (try (conpool-release pool conn)
	    (catch (e) (set! err e))))
     
     (finally
      (if err
	(raise err)
	(conpool-put pool conn))))))

(def (esql q . args) (apply ecm-sql sql-eval-query q args))

(def (json-object<-sql-eval-query db query . args)
  (def stmt (sql-prepare db query))
  (def cols (sql-columns stmt))
  (when (pair? args) (apply sql-bind stmt args))
  (def rows (sql-query stmt))
  (when (and (pair? rows)
             (not (vector? (car rows))))
    (set! rows (map vector rows)))
  (hash (query query)
        (args args)
        (cols cols)
        (rows rows)))

(def (identity-string obj)
  (if (string? obj) obj
      (raise-bad-argument postgresql "string" obj)))

(defcatalog-default
  ;; DATEOID ;; TIMESTAMPOID ;; TIMESTAMPTZOID
  ((1082 1114 1184) identity-string identity))
