(defpackage :ecm/start
  (:use :cl)
  (:import-from #.(progn (ql:quickload '(cl-postgres simple-date simple-date/postgres-glue))
                         :cl-postgres))
  (:import-from #.(progn (ql:quickload '(simple-date simple-date/postgres-glue))
                         :simple-date))
  (:import-from :ecm/configuration
		            #:configuration-value)
  (:import-from :swank)
  (:import-from :ecm/database)
  (:import-from #.(progn (ql:quickload :maxclaims-ecm)

			 (load (merge-pathnames "src/hunchentoot" (asdf:system-source-directory :maxclaims)))
			 :maxclaims)
		#:*attachment-directory*
		#:*db-admin-parameters*
		#:*db-connection-parameters*)
  #+(or)(:import-from #.(progn (ql:quickload :maxclaims)
                         (load (merge-pathnames
                                "src/report/bordereau"
                                (asdf:system-source-directory :maxclaims)))

                         :maxclaims/report/bordereau))
  (:import-from :ecm/hunchentoot)
  (:import-from :ecm/endpoint/claim)
  (:import-from :ecm/ecm-msg)
  (:import-from #.(progn (ql:quickload :ecm/endpoint/corpus) :ecm/endpoint/corpus))
  (:import-from :ecm/endpoint/transaction)
  (:import-from #.(progn (ql:quickload :ecm/endpoint/report) :ecm/endpoint/report))
  (:import-from :ecm/hack/login)
  (:import-from #.(progn (ql:quickload :ECM/UI/REPORT/BORDEREAU-API)  :ecm/ui/report/bordereau-api))
  (:import-from #.(progn (ql:quickload :ecm/endpoint/api) :ecm/endpoint/api))
  (:import-from :maxclaims/hunchentoot
                #:make-maxclaims-acceptor)
  (:export #:start))
(in-package :ecm/start)

(defun assert-attachment-directory-exists ()
  (let* ((dir (ecm/configuration:attachment-directory))
	 (file "00_README")
	 (fe (cl-fad:file-exists-p (merge-pathnames file dir))))
    (prog1 t
					;      (assert fe nil "Cannot locate attachments: ~A" dir)
      (if (not fe)
	  (warn "No Attachments in ~A" dir))
      (setf *attachment-directory* dir)
      (warn "Attachment Directory is ~A" dir))))


(defun start ()
  (warn "Trying to start ECM")
  (let ((conf (ecm/configuration:find-configuration-file)))
    (unless conf (error "No configuration file found in : {~% ~T ~A }"
			(ecm/configuration:configuration-file-possible-directories)))

    (warn "Found conf: ~A" conf)
    (assert-attachment-directory-exists)
    (let* ((db-host (configuration-value "database.master.host"))
		   (db-port (configuration-value "database.master.port"))
		   (db-database (configuration-value "database.master.database"))
		   (db-username (configuration-value "database.master.username"))
		   (db-password nil #+(or)(configuration-value "database.master.password"))
	       (pm-db (list db-database db-username db-password db-host :port db-port :pooled-p t
                        :use-ssl :no)))
      (warn " Setting DB ~A" pm-db)
      (setf *db-admin-parameters* pm-db
	    *db-connection-parameters* pm-db)
      (eval (read-from-string "(maxclaims/hunchentoot::start-server)"))


      (warn "Started on http port ~A" (hunchentoot::acceptor-port maxclaims/hunchentoot::*hunchentoot-acceptor*))
      (labels ((givr (port)
                 (handler-case (swank:create-server :port port :dont-close t )
                   (error (e) (warn "SWANK: port ~A ~A" port e)
                     (givr (+ 1 port))))))
        (givr 4005)))))
