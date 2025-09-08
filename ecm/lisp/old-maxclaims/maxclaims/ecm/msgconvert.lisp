(defpackage :ecm/msgconvert
  (:use :cl)
  (:import-from :uiop/run-program)
  (:import-from :cl-fad)
  (:export #:msgconvert))
(in-package :ecm/msgconvert)

(defun msgconvert (from &optional to-mbox)
  (unless (cl-fad:file-exists-p from)
    (error "msgconvert: ~A does not exist" from))
  (ensure-directories-exist to-mbox)
  
  (let ((command  (format nil "msgconvert --mbox \"~A\" \"~A\""
			  to-mbox from)))
    (flet ((ret ()
	     (let* ((mbox (uiop/run-program::escape-shell-token
			   (namestring to-mbox)))
		    (enc (uiop/run-program:run-program
			  (format nil "file --brief --mime-encoding ~A"
				  mbox)
			  :output '(:STRING :STRIPPED T))))
	       #+(or)(when (string= enc "unknown-8bit")
		       (setf enc "ASCII"))
	       #+(or)(unless (string= enc "utf-8")
		 (uiop/run-program:run-program
		  (format nil "iconv -f ~A -t utf-8 ~A -o ~A"
			  enc mbox mbox)))
	       #+(or)(uiop/run-program:run-program
		      (format nil "sed -i 's/FYDIBOHF23SPDLT/Marion Maxwell/g' ~A"
			      mbox))
	       to-mbox)))
      (with-output-to-string (out)
	(with-output-to-string (err)
	  (handler-case 
	      (multiple-value-bind (o e i)
		  (uiop/run-program:run-program
		   command
		   :output out :error-output err
		   :ignore-error-status t)
		(if (cl-fad:file-exists-p to-mbox)
		    (ret)
		    (error "msgconvert cannot convert : ~A ~A ~A"
			   o e i)))

	    (error (c)
	      (if (cl-fad:file-exists-p to-mbox)
		  (ret)
		  (error "Error from msgconvert: ~A ~%command:~A ~%out: ~A~% condition: ~A"		   
			 (get-output-stream-string err)
			 command
			 (get-output-stream-string out)
			 c)))))))))
