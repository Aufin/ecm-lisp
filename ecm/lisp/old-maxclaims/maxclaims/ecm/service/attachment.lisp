(defpackage :ecm/service/attachment
  (:use :cl)
  (:import-from :ecm/configuration
		:configuration-value)
  (:import-from :ecm/entity/attachment
		#:attachment-absolute-pathname
		#:attachment-long-pathname
		#:attachment-short-pathname
		#:attachment-filename)
  (:import-from :cl-fad)
  (:import-from :alexandria)
  (:export #:ensure-attachment-file-exists))
(in-package :ecm/service/attachment)

(defun restore-from-backup (attachment)
  (let* ((short-backup-file
	  (attachment-absolute-pathname
	   attachment
	   :default-pathname-configuration "attachment.backup"))
	 (long-backup-file
	  (attachment-absolute-pathname
	   attachment
	   :pathname-function #'attachment-long-pathname
	   :default-pathname-configuration  "attachment.backup"))
	 (backup-file (or (cl-fad:file-exists-p short-backup-file)
			              (cl-fad:file-exists-p long-backup-file)))
	 (to (attachment-absolute-pathname
	      attachment)))
    (unless backup-file
      (error "There is not a backup file for ~A ~A ~% from Checking ~A and ~A." (attachment-filename attachment) t short-backup-file long-backup-file
             ))
    
    (alexandria:copy-file backup-file to)
    (ensure-short-name-and-symlink attachment)))

(defun ensure-short-name-and-symlink (attachment)
  (let* ((short (attachment-absolute-pathname attachment))
	 (long (attachment-absolute-pathname
		attachment
		:pathname-function #'attachment-long-pathname))
	 (short-sym (make-pathname :name (pathname-name short)
				   :type (pathname-type short)))
	 (long-sym (make-pathname :name (pathname-name long)
				  :type (pathname-type long)))
	 (dir (make-pathname :directory (pathname-directory short))))
    (ensure-directories-exist short)
    (flet ((sym ()
	     (let ((*default-pathname-defaults* dir))
	       (sb-posix:chdir dir)
	       (warn "Linking ~A to ~A in ~A"
		     short-sym long-sym *default-pathname-defaults*)
	       (sb-posix:symlink short-sym long-sym))))
      (cond ((and (cl-fad:file-exists-p short)
		              (cl-fad:file-exists-p long))
	           (cl-fad:file-exists-p short))
	          ((cl-fad:file-exists-p short)
	           (sym)
	           (cl-fad:file-exists-p short))
	          ((cl-fad:file-exists-p long)
	           (rename-file long short)
	           (sym)
	           (cl-fad:file-exists-p short))
	          (t (restore-from-backup attachment))))))

	
(defun ensure-attachment-file-exists (attachment)
  (ensure-short-name-and-symlink attachment))

(defparameter *errors* nil)
    
