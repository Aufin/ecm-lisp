(defpackage :maxclaims/entry-point/download
  (:use :cl)
  (:import-from :maxclaims/entry-point/toplevel
		#:select-objects/key-value)
    (:import-from :maxclaims/web-display/html-page
		  #:call-with-html-page
		  #:with-user-html-page)
  (:import-from :maxclaims/web-display/display
		  #:get-app-user
		  #:display)
  (:import-from :maxclaims/web-display/timecard
		#:timecard-page)
  (:import-from :maxclaims
		#:with-udb
		#:call-with-app-user
		#:claim
		#:attachment
		#:merge-attachment-pathname
		#:attachment.file-name
		#:attachment.file-type
		#:attachment.sha1-digest)
 (:import-from :maxclaims/hunchentoot
		#:define-easy-handler))
  

(in-package :maxclaims/entry-point/download)

(define-easy-handler (|download-timecards| 
		      :uri "/ecm/download-timecards")
    ((claim-id :real-name "timecard[claim]")
     (interim-date :real-name "timecard[interim]"))
  (let ((html-string (yaclml:with-yaclml-output-to-string 
		       (timecard-page 
			:layers :download
			:claim-id claim-id
			:interim-date interim-date))))
    (alexandria:with-output-to-file 
	(stream "/tmp/timecards" 
		:if-exists :supersede)
      (with-input-from-string (string html-string)
	 (alexandria:copy-stream 
	  string stream)))
    (call-with-app-user 
     (get-app-user) 
     (lambda () 
       (let ((name (format nil "Timecards-claim-~A-Interim-~A.html"
			   claim-id interim-date))
	     (type "text/html"))
	 (prog1 t
	   (setf (hunchentoot:content-type*)
		 type)
	   (setf (hunchentoot:header-out 
		  "Content-Disposition")
		 (format nil "attachment; filename=\"~A\""
			 name))
	   (let ((output-stream (hunchentoot:send-headers)))
	     (alexandria:with-input-from-file 
		 (stream "/tmp/timecards"
			 :element-type '(unsigned-byte 8))
	       (alexandria:copy-stream 
		stream output-stream)))))))))

(define-easy-handler (|download| :uri "/ecm/download")
    ((claim-number :real-name "claim")
     (attachment-id :real-name "attachment")
     (filename :real-name "n")
     (type :real-name "t")
     (content-disposition :real-name "content-disposition"
			  :init-form "attachment"))
  (if (not (and claim-number attachment-id))
      (call-with-app-user
       (get-app-user)
       (lambda ()
	 (with-udb
	   (let* ((path (make-pathname :name filename
					       :directory '(:absolute "tmp")
					       :type type))
		        (name (format nil "~A.~A" filename type))
		        (type "application/octet-stream"))
		   (prog1 t
		     (setf (hunchentoot:content-type*)
			         type)
		     (setf (hunchentoot:header-out
			          "Content-Disposition")
			         (format nil "~A; filename=\"~A\""
				               content-disposition
                       (remove-if-not (lambda (char)
                                        (< (char-code char) 128)) name)))
		     (let ((output-stream (hunchentoot:send-headers)))
			     (alexandria:with-input-from-file (stream path
								                                    :element-type '(unsigned-byte 8))
			       (alexandria:copy-stream
			        stream output-stream))))))))
      (hunchentoot:redirect (format nil "/ecm/attachment/~A/download" attachment-id)
                            :protocol ecm/hunchentoot:*protocol*)))

