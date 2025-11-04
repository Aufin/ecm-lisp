(defpackage :ecm/ecm-msg
  (:use :cl)
  (:import-from :uiop/run-program)
  (:import-from :cl-fad)
  (:export #:ecm-msg->html-file))
(in-package :ecm/ecm-msg)

(defun ecm-msg->html-file (from to)
  (unless (cl-fad:file-exists-p from)
    (error "ecm-msg: ~A does not exist" from))
  (ensure-directories-exist to)
  
  (let ((command
          (format nil "sh -c 'ecm-msg body-html \"~A\" > \"~A\"'"
			            from to)))
    (with-output-to-string (out)
	    (with-output-to-string (err)
	      (handler-case
	          (multiple-value-bind (o e i)
		            (uiop/run-program:run-program
		             command
		             :output out :error-output err
		             :ignore-error-status t)
              ;;(error "to: " to )
		          (if (cl-fad:file-exists-p to)
		              to
		              (error "ecm-msg: cannot convert : ~A ~A ~a"
                         o e i)))
	        (error (c)
             ;; (error "to: " to )
	          (if (cl-fad:file-exists-p to)
		            to
		            (error "Error from ecm-msg: ~A ~%command:~A ~%out: ~A~% condition: ~A"
			                 (get-output-stream-string err)
			                 command
			                 (get-output-stream-string out)
			                 c))))))))
