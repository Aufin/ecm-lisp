(defpackage :ecm/entity/api/static
  (:use :cl)
  (:import-from :uiop/stream)
  (:export #:make-static-claims-report-instance))

;; https://superuser.com/questions/1135850/how-do-i-run-a-libreoffice-macro-from-the-command-line-without-the-gui

(in-package :ecm/entity/api/static)

(defstruct static-claims-report
  (interval "3 months")
  (%pathname nil)
  (%lines '()))

(defun static-claims-report-pathname (scr)
  (or (static-claims-report-%pathname scr)
      (setf
       (static-claims-report-%pathname scr)
       (format nil "/tablespaces/shared/csv/static-claims-~A-~A.csv" (sb-posix:getpid) (gensym)))))

(defun static-claims-report-lines (scr)
   (or (static-claims-report-%lines scr)
       (if (cl-fad:file-exists-p (static-claims-report-pathname scr))
	    (setf
	     (static-claims-report-%lines scr)
	     (uiop/stream:read-file-lines (static-claims-report-pathname scr)))
	    (error "Static Claims Report: File does not exist ~A" (static-claims-report-pathname scr)))))

(defun run-static-claims-report (&optional (interval "3 months"))
  (let* ((rep (make-static-claims-report :interval interval))
		 (name (static-claims-report-pathname rep)))
    (ensure-directories-exist name)
    (postmodern:with-transaction ()
      (postmodern:execute "CREATE TEMP TABLE _static_rep ON COMMIT DROP AS
       (SELECT * from static_claims_report($1));" interval)
      (postmodern:execute
       (format nil "COPY (SELECT * from _static_rep) TO '~a' DELIMITER ',' CSV HEADER" name)))
    rep))

(defvar *test-static*)

(defun test-static (&key (uid 2) (interval "3 months"))
  (let* ((user (maxclaims::with-adb (ecm/user:find-user-by-id uid)))
         (report (ecm/user:bind-user
                  user
                  (lambda ()
		    (maxclaims::with-udb 
		    (run-static-claims-report interval))))))

    (setf *test-static* report)))
  
  


