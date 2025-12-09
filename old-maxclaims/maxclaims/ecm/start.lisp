(defpackage :ecm/start
  (:use :cl)
  (:import-from :parenscript)
  (:import-from :maxclaims)
  (:import-from :maxclaims-src/src/hunchentoot
                #:make-maxclaims-acceptor)
  (:import-from :ecm/configuration)
  (:import-from :maxclaims-ecm)
  (:import-from :ecm/database)
  (:import-from :ecm/hunchentoot) 
  (:import-from :ecm/endpoint/claim) 
  (:import-from :ecm/ecm-msg)
  (:import-from :ecm/endpoint/corpus) 
  (:import-from :ecm/endpoint/transaction) 
  (:import-from :ecm/endpoint/report)
  (:import-from :ecm/hack/login)
  (:import-from :ecm/ui/report/bordereau-api)
  (:import-from :ecm/endpoint/api)
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
