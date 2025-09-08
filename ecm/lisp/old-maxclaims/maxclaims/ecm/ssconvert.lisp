(defpackage :ecm/ssconvert
  (:use :cl)
  (:import-from :uiop/run-program)
  (:import-from :cl-fad)
  (:export #:ssconvert))
(in-package :ecm/ssconvert)

(defun ssconvert (from to &key (export-type nil))
  (unless (cl-fad:file-exists-p from)
    (error "ssconvert: ~A does not exist" from))
  (ensure-directories-exist to)
  (let ((command  (format nil "ssconvert ~A ~A ~A"
			  (if export-type
			      (concatenate
			       'string " -T " export-type)
			      "")
					 
			  (uiop/run-program::escape-shell-token
			   (namestring from))
			  (uiop/run-program::escape-shell-token
			   (namestring to)))))
    (with-output-to-string (out)
      (with-output-to-string (err)
	(handler-case 
	    (uiop/run-program:run-program
	     command
	     :output out :error-output err
	     :shell t)
	  (error (c)
	    (error "~A~%command:~A ~%out: ~A~% condition: ~A"
		   
		   (get-output-stream-string err)
		   command
		   (get-output-stream-string out)
		   c)))))))
