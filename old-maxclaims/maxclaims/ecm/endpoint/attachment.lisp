(defpackage :ecm/endpoint/attachment
  (:use :cl)
  (:import-from :ecm/user)
  (:import-from :ecm/endpoint
		#:define-endpoint)
  (:import-from :ecm/ui/attachment
		#:<open-attachment-page>
		#:edit-attachment-page)
  (:import-from :ecm/entity/attachment
		#:find-attachment)
  (:import-from :ecm/service/attachment)
  (:import-from :ecm/json #:getjso)
  (:import-from :ecm/request-context))
(in-package :ecm/endpoint/attachment)

(define-endpoint claim-create-attachment
    "ecm/claim/(\\d+)/create-attachment$")

(defun claim-create-attachment/get (claim-id &aux (claim-id
					   (parse-integer claim-id)))
  (ecm/request-context:with-request-context ()
    (edit-attachment-page :claim-id claim-id
			  :title "Create Attachment")))

(defvar *attachment-files* (make-hash-table :test #'equal))

(defun claim-create-attachment/post (claim-id &aux (claim-id
						    (parse-integer claim-id)))
  (ecm/request-context:with-request-context ()
    (handler-case
	(let* ((gensym (ecm/hunchentoot:parameter "gensym"))
	       (files (gethash gensym *attachment-files*))
	       (date (ecm/hunchentoot:parameter-or-nil "date"))
	       (description (ecm/hunchentoot:parameter-or-nil "description"))
	       (attachments (mapcar (lambda (file)
				      (apply
				       #'ecm/entity/attachment:create-attachment
				       :app-user-id (ecm/user:user-id)
				       :claim-id claim-id
				       :date date
				       :description description
				       file))
				    files)))
;	  (break "~A" attachments)
	  (ecm/ui/attachment:open-attachments-page attachments))
      (error (c)	  
	(edit-attachment-page :claim-id claim-id
			      :title "Create Attachment"
			      :error c)))))

(define-endpoint attachment-upload
    "ecm/attachment/upload")

(defun attachment-upload/post ()
  (ecm/request-context:with-request-context ()
    (destructuring-bind (path file-name content-type)
	(ecm/hunchentoot:post-parameter "file")
      (let* ((gensym (ecm/hunchentoot:parameter "gensym"))
	     (sha1-digest 
	      (ironclad:byte-array-to-hex-string
	       (ironclad:digest-file 
		:sha1 path)))
	     (new-path (merge-pathnames
			(make-pathname :name file-name)
			(make-pathname :directory (list :absolute "tmp")))))
	
	(cl-fad:copy-file path new-path :overwrite t)
	     
	(push (list :path new-path :file-name file-name
		    :content-type content-type)
	      (gethash gensym *attachment-files*)))
      (with-output-to-string (ecm/ml:*sexpml-output*)
	(ecm/ml:<> :text path)))))

(define-endpoint attachment
    "ecm/attachment/(\\d+)/$")

(defun attachment/get (attachment-id &aux (attachment-id
					   (parse-integer attachment-id)))
  (ecm/request-context:with-request-context ()
    (let ((attachment (find-attachment attachment-id)))
      (when attachment
	      (ecm/service/attachment:ensure-attachment-file-exists attachment)
	      (<open-attachment-page> attachment)))))

(define-endpoint attachment-list 
    "/ecm/attachment/(\\d+)/(.+)")

(defun attachment-list/get (attachment-id pathname
			    &aux (attachment-id
				  (parse-integer attachment-id))
			      (pathname (and pathname (pathname pathname))))
  (ecm/request-context:with-request-context ()
    (let ((attachment (find-attachment attachment-id))
	  (type (and pathname (pathname-type pathname))))
      (when attachment
	(ecm/service/attachment:ensure-attachment-file-exists attachment)
	(ecm/hunchentoot:handle-static-file
	 (merge-pathnames
	  pathname
	  (ecm/entity/attachment:attachment-mailing-list-archive-absolute-pathname
	   attachment)))))))
(define-endpoint attachment-archive
    "ecm/attachment/(\\d+)/archive/(.*)")

(defun attachment-archive/get (attachment-id pathname
			       &aux (attachment-id
				     (parse-integer attachment-id))
				 (pathname (and pathname (pathname pathname))))
  (ecm/request-context:with-request-context ()
    (let ((attachment (find-attachment attachment-id))
	  (type (and pathname (pathname-type pathname))))
      (when attachment
	(ecm/service/attachment:ensure-attachment-file-exists attachment)
	(if (string-equal type "ods")
	    (ecm/hunchentoot:handle-static-file
	     (ecm/entity/attachment:attachment-ods-archive-absolute-pathname
	      attachment))
	    (ecm/hunchentoot:handle-static-file
	     (ecm/entity/attachment:attachment-absolute-pathname
	      attachment)))))))

(define-endpoint attachment-download "ecm/attachment/(\\d+)/download")

(defun attachment-download/get (attachment-id
			    &aux (attachment-id
				  (parse-integer attachment-id)))
  (ecm/request-context:with-request-context ()
    (let* ((attachment (find-attachment attachment-id))
	         (content-type (ecm/entity/attachment:attachment-content-type
			                    attachment))
	         (pathname (ecm/entity/attachment:attachment-absolute-pathname
		                  attachment))
	         (content-disposition "attachment")
	         (filename (ecm/entity/attachment:attachment-filename attachment)))
	    (ecm/service/attachment:ensure-attachment-file-exists attachment)
      (setf (ecm/hunchentoot:header-out 
	     "Content-Disposition")
	    (format nil "~A; filename=\"~A\""
		          content-disposition
              (remove-if-not (lambda (char)
                               (< (char-code char) 128)) 
		                         filename)))
      (ecm/hunchentoot:handle-static-file
       pathname content-type))))
