(uiop:define-package :ecm/entity/attachment
  (:use :cl)
  (:import-from :ecm/json
		#:getjso)
  (:import-from :ecm/configuration)
  (:import-from :ecm/entity/corpus)
  (:import-from :ecm/mail/rfc2822
		#:make-rfc2822-message)
  (:import-from :ecm/ecm-msg)
  (:export #:find-attachment
	   #:attachment-content-type
	   #:attachment-filename
	   #:create-attachment
	   #:claim-attachments
	   #:attachment-claim-id
	   #:attachment-absolute-pathname
	   #:archive-attachment
	   #:unarchived-claim-attachments
	   #:attachment-ods-archive-absolute-pathname
	   #:attachment-html-archive-absolute-pathname
	   #:attachment-mailing-list-archive-absolute-pathname
	   #:find-or-create-mailing-list-archive))
(in-package :ecm/entity/attachment)

(defun attachment-content-type (attachment)
  (getjso "content_type" attachment))

(defun attachment-filename (attachment)
  (getjso "file_name" attachment))

(defun claim-attachments (claim-id &key (where nil))
  (mapcar
   #'ecm/json:read-json-from-string
   (postmodern:query
    (s-sql:sql-compile
     `(:select
       (:jsi.claim-attachment 'attachment)
       :from 'attachment
       :where (:and (:= 'attachment.claim-id ,claim-id) ,where)))
    :column)))

(defun find-attachment (attachment-id &key claim-id sha1-digest)
  (if (and claim-id sha1-digest (not attachment-id))
      (ecm/json:read-json-from-string
       (postmodern:query
	(:select
	 (:jsi.claim-attachment 'attachment)
	 :from 'attachment
	 :where (:and (:= claim-id 'claim-id)
		      (:= sha1-digest 'sha1-digest)))
	:single))
      (ecm/json:read-json-from-string
       (postmodern:query
	(:select
	 (:jsi.claim-attachment 'attachment)
	 :from 'attachment
	 :where (:= attachment-id 'attachment-id))
	:single))))

(defun insert-attachment
    (&key app-user-id
       claim-id date description file-name content-type
       sha1-digest)
  (ecm/database:query/return-jso
    (:insert-into
     'attachment
     :set
     :date date :claim-id claim-id
     :app-user-id app-user-id
     :file-description description
     :file-name file-name
     :file-type content-type
     :sha1-digest sha1-digest
     :returning (:jsi.claim-attachment 'attachment))))

(defun filename->pathname (filename)
  (pathname (concatenate
   'string
   (loop :for char across filename
		 :when (char= char #\[) :collect #\\  :end
		 :collect char))))

(defun attachment-long-pathname (attachment)
  (pathname (concatenate
	     'string (getjso "sha1_digest" attachment)
	     "_"
	     (map 'string 
		  (lambda (char)
		    (if (or (eql #\[ char)
			    (eql #\] char))
			#\-
			char))
		  (getjso "file_name" attachment)))))

(defun attachment-short-pathname (attachment)
  (let ((type (pathname-type (filename->pathname (getjso "file_name" attachment)))))
    (pathname (concatenate
	       'string (getjso "sha1_digest" attachment)
	       "." type))))

(defun attachment-relative-pathname (attachment
				     &key (pathname-function
						   #'attachment-short-pathname))
  (merge-pathnames
   (funcall pathname-function attachment)
   (make-pathname
    :directory (list :relative
		     (princ-to-string (getjso "claim_id" attachment))))))

(defun attachment-absolute-pathname (attachment
				     &key (default-pathname-configuration
					      "attachment.directory")
				       (pathname-function
					#'attachment-short-pathname))
  (let* ((attachment-directory
	  (ecm/configuration:configuration-value
	   default-pathname-configuration))
	 (path 
    
	  (merge-pathnames (attachment-relative-pathname
			    attachment :pathname-function pathname-function)
			   (cl-fad:pathname-as-directory attachment-directory))))
    path))
		  
(defun write-attachment (file attachment)
 (prog1 attachment
      (ensure-directories-exist (attachment-absolute-pathname attachment))
      (alexandria:copy-file 
       file
       (attachment-absolute-pathname attachment) 
       :if-to-exists :supersede
       :element-type '(unsigned-byte 8))))
  
(defun create-attachment (&key app-user-id claim-id date description file-name path content-type)
  (let* ((sha1-digest 
	  (ironclad:byte-array-to-hex-string
	   (ironclad:digest-file 
	    :sha1 path)))
	 (attachment 
	  (or (ignore-errors (insert-attachment
			      :date date :claim-id claim-id
			      :app-user-id app-user-id
			      :description description
			      :file-name file-name
			      :content-type content-type
			      :sha1-digest sha1-digest))
	      (find-attachment nil :claim-id claim-id :sha1-digest sha1-digest))))
    (prog1 attachment
      (ensure-directories-exist (attachment-absolute-pathname attachment))
      (alexandria:copy-file 
       path
       (attachment-absolute-pathname attachment) 
       :if-to-exists :supersede
       :element-type '(unsigned-byte 8))
      (cl:delete-file path))))


 
(defun unarchived-claim-attachments (claim-id)
  (ecm/entity/attachment:claim-attachments
   claim-id :where '(:= nil 'archived)))

(defun attachment-archived-p (attachment)
  (unless (or (eql :false (getjso "archived" attachment))
	      (eql :null (getjso "archived" attachment))
	      (not (getjso "archived" attachment)))
    attachment))

(defun attachment-claim-id (attachment)
  (getjso "claim_id" attachment))

(defun attachment-user (attachment)
  (getjso "user" attachment))


(defun make-attachment-message (attachment)
  (declare (optimize (debug 3)))
  (let* ((corpus (getjso "corpus" (attachment-user attachment)))
	 (from (apply #'concatenate
		'string
		(getjso "first_name" corpus)
		" " (getjso "last_name" corpus) " "
		(let ((address (ecm/entity/corpus:corpus-email-address
				(getjso "corpus" (attachment-user attachment)))))
		  (when address
		    (list  "<" address ">")))))
	 (from (string-trim " " from))
	 (from (if (equal from "")
		   (getjso "username" (attachment-user attachment))
		   from))
	 (from (string-trim " " from))
	 (from (if (equal from "")
		   (format nil "Unknown : ~A" (getjso "_id" (attachment-user attachment)))
		   from))
	 
	(to (concatenate 'string (princ-to-string (attachment-claim-id attachment))
			 "@ecm.maxwellclaims.net"))
	 (subject (concatenate 'string "[attachment] "
			       (princ-to-string (getjso "description" attachment))))
	 (subject (subseq subject 0 (min 77 (length subject))))
	 (subject (if (>= (length subject) 76)
		      (concatenate 'string subject "...")
		      subject))
	 (body (format nil "~A~%~%Date: ~A~%~%File Name: ~A"
		       (getjso "description" attachment)
		       (getjso "date" attachment)
		       (getjso "file_name" attachment)))
	 (path (attachment-absolute-pathname attachment))
	 (unix-path (uiop/run-program::escape-shell-token (princ-to-string path)))
	 (tmpdir (uiop:run-program "mktemp -d"
				   :OUTPUT '(:STRING :STRIPPED T)))
	 (symlink-path
	  (merge-pathnames (remove-if (lambda (char)
					(> (char-code char) 127))
					(getjso "file_name" attachment))
			   (make-pathname :directory tmpdir)))
	 (unix-symlink-path (uiop/run-program::escape-shell-token
			     (princ-to-string symlink-path))))

    ;;; Does the attachment exist?
    (let* ((wild (concatenate 'string (getjso "sha1_digest" attachment) "*"))
	   (stallion (merge-pathnames wild path))
	   #+(or)(unix-wild  (uiop/run-program::escape-shell-token
			      (princ-to-string stallion))))
;;; This will error if there is no file.
      (if (cl-fad:file-exists-p path)
	  (progn
	    ;(warn "File ~A exists, still trying link" path)
	    (ignore-errors (uiop:run-program (format nil "ln ~A ~A"
						     stallion unix-path))))
	  (progn ; (warn "File ~A does not exist" path)
;;; Is it the wrong encoding for the filename?		 
		 (ignore-errors (uiop:run-program (format nil "ls ~A || exit 42 && ln ~A ~A"
							  stallion stallion unix-path) :ignore-error-status t)))))
		
    (uiop:run-program (format nil "ln -s ~A ~A" unix-path unix-symlink-path) :ignore-error-status t)
    (values
     (ecm/mail/rfc2822:make-rfc2822-message
      :subject subject
      :from from
      :to to
      :date (local-time:timestamp-to-universal
	     (local-time:parse-timestring
	      (format nil "~A-07:00" (getjso "time" attachment))))
      :body body
      :attachments (list symlink-path))
     symlink-path)))
	 
(defun archive-attachment-as-message (attachment)
  ; (warn "Creating email message for ~A" (getjso "_id" attachment))
  (multiple-value-bind (message symlink-path)
      (make-attachment-message attachment)
    (let* ((message-file (uiop:run-program "mktemp"
					   :OUTPUT '(:STRING :STRIPPED T)))
	   (sh (format nil "cat ~A | /home/maxclaims/bin/archive-attachment ~A"
		       message-file (getjso "claim_id" attachment))))
    ;   (warn " trting ~A " sh)
      (alexandria:with-output-to-file (stream message-file :if-exists :supersede)
	(ecm/mail/rfc2822:write-message-to-unix-string-stream message stream))

      (prog1 sh (uiop:run-program sh :output :string :error-output :output :ignore-error-status t)
	(uiop:run-program (format nil "rm ~A" message-file))
;	(break "~A ~A " sh message-file)
	(let ((dir (make-pathname :directory (pathname-directory symlink-path))))
	  (uiop:run-program (format nil "rm ~A" (uiop/run-program::escape-shell-token
						 (princ-to-string symlink-path))))
	  (uiop:run-program (format nil "rmdir ~A" dir)))))))

(defun attachment-html-archive-directory-relative-pathname (attachment)
  (make-pathname
   :directory (list :relative
		    (princ-to-string (getjso "claim_id" attachment))
		    (princ-to-string (getjso "_id" attachment))
		    "html")))

(defun attachment-html-archive-relative-pathname (attachment)
  (let* ((apath (pathname (getjso "file_name" attachment)))
	 (html-path (make-pathname
		     :name (pathname-name apath) :type "html")))
    (if (member (pathname-type apath) '("html" "htm")
		:test #'string-equal)
	(values (attachment-relative-pathname attachment) nil)
	(values (merge-pathnames
		 html-path
		 (attachment-html-archive-directory-relative-pathname
		  attachment))
		t))))

(defun attachment-html-archive-absolute-pathname (attachment)
  (multiple-value-bind (path archived?)
      (attachment-html-archive-relative-pathname
       attachment)
    (merge-pathnames path 
		     (if archived?
			 (concatenate
			  'string
			  (ecm/configuration:configuration-value
			   "archive.directory") "/")
			 (ecm/configuration:configuration-value
			  "attachment.directory")))))

(defun attachment-mailing-list-archive-relative-pathname (attachment)
  (make-pathname
   :directory (list :relative
		    (princ-to-string (getjso "claim_id" attachment))
		    (princ-to-string (getjso "_id" attachment))
		    "mailing-list")))

(defun find-or-create-mailing-list-archive (attachment)
  (let* ((mlist (attachment-mailing-list-archive-absolute-pathname
		             attachment))
	       (path (ecm/entity/attachment:attachment-absolute-pathname
		            attachment))
	       #+(or)(mbox (merge-pathnames
		            (make-pathname :name (pathname-name path)
			                         :type "mbox")
		            mlist))
	       (index (merge-pathnames
		             (make-pathname :name "index"
				                        :type "html")
		             mlist)))
    ;; (break "~A ~A" html path)
    (if (cl-fad:file-exists-p index)
	      index
	      (progn
	        (ensure-directories-exist index)
	        (let ((idx (ecm/ecm-msg:ecm-msg->html-file path index)))
	          (if (cl-fad:file-exists-p index)
		            index
		            (error "Cannot create archive for ~A~%~%~A~%~%"
		                   path
		                   idx)))))))

(defun attachment-mailing-list-archive-absolute-pathname (attachment)
  (merge-pathnames
   (attachment-mailing-list-archive-relative-pathname attachment)
   (cl-fad:pathname-as-directory
    (ecm/configuration:configuration-value "archive.directory"))))

(defun attachment-odf-archive-relative-pathname (attachment)
  (make-pathname
   :directory (list :relative
		    (princ-to-string (getjso "claim_id" attachment))
		    (princ-to-string (getjso "_id" attachment))
		    "odf")))

(defun attachment-ods-archive-relative-pathname (attachment)
  (let* ((apath (pathname (getjso "file_name" attachment)))
	 (ods-path (make-pathname
		    :name (pathname-name apath) :type "ods")))
    (merge-pathnames ods-path (attachment-odf-archive-relative-pathname attachment))))

(defun attachment-ods-archive-absolute-pathname (attachment)
  (merge-pathnames (attachment-ods-archive-relative-pathname attachment)
		   (ecm/configuration:configuration-value
		    "archive.directory")))

(defun archive-attachment
    (attachment
     &key (archive-path "/home/maxclaims/bin/archive-attachment")
       (include-attachment-id nil)
       (force nil))
  #+(or)(if (and (not force) (attachment-archived-p attachment))
      "Attachment Already Archived"
      (let* ((path (attachment-absolute-pathname attachment))
	     (type (pathname-type path)))
				
	(prog1
	    (if (string-equal type "msg")
		(uiop:run-program
		 (concatenate
		  'string (namestring archive-path) " "
		  (princ-to-string (getjso "claim_id" attachment))
		  (if include-attachment-id
		      (format nil " ~A " (getjso "_id" attachment))
		      "")
		  " \"" (princ-to-string path) "\"")
		 :OUTPUT '(:STRING :STRIPPED T)
		 :error-output :output)
		(archive-attachment-as-message attachment))
	  (postmodern:query (format nil "UPDATE attachment SET archived = true 
  WHERE attachment_id = ~A" (getjso "_id" attachment)))))))

    
