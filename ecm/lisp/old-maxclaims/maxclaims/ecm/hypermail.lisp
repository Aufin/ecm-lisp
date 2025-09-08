(defpackage :ecm/hypermail
  (:use :cl)
  (:import-from :uiop/run-program)
  (:import-from :cl-fad)
  (:export #:hypermail))
(in-package :ecm/hypermail)

(defvar *hypermail-conf*
  (merge-pathnames
   "etc/hypermail.conf"
   (asdf:system-source-directory :ecm)))
		   
(defun hypermail (from-mbox to-directory
		              &key (conf-path *hypermail-conf*)
		                (label nil))
  (unless (cl-fad:file-exists-p from-mbox)
    (error "hypermail: ~A does not exist" from-mbox))
  (ensure-directories-exist to-directory)
  
  (let ((command  (format nil "hypermail ~A -c \"~A\" -m ~A -d  \"~A\""
			                    (if label
			                        (format
			                         nil "-l ~A"
			                         (uiop/run-program::escape-shell-token label))
			                        "")
			                    conf-path
			                    (uiop/run-program::escape-shell-token
			                     (namestring from-mbox))
			                    to-directory))
	      (index (merge-pathnames "index.html" to-directory)))
    ;;    (break "~A" command)
    (with-output-to-string (out)
      (with-output-to-string (err)
	      (handler-case
	          (multiple-value-bind (o e i)
		            (uiop/run-program:run-program
		             command
		             :output out :error-output err
		             :ignore-error-status t)
	            (if (cl-fad:file-exists-p index)
		              index
		              (error "hypermail error : ~A ~A ~A"
			                   o e i)))
	        (error (c)
	          (error "~A~%~% command: ~A ~%~%out: ~A~%% condition: ~A"
		               (get-output-stream-string err)
		               command
		               (get-output-stream-string out)
		               c)))))))
