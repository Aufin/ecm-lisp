(in-package :maxclaims)

'file ;;; this is needed for something ... do not know what yet.

(defvar *attachment-directory* #P"/home/drewc/src/maxclaims/database/attachments/")
(defvar *attachment-trash-directory* #P"/home/drewc/src/maxclaims/database/attachment-trash/")

(defconstant +linux-path-max+ 255
  "ext3, xfs, btrfs, &c all have a lame 255 char maximum path
   length. Truncate internal file names of attachments to this length.

 WARNING: changing this will make the system unable to find
  some attachments.")

(defclass attachment ()
  ((attachment-id :primary-key t)
   (claim :column claim-id 
	  :references claim)
   (app-user :column app-user-id
	     :references app-user
	     :initform $app-user)
   (date :initform (simple-date:universal-time-to-timestamp (get-universal-time)))
   file-description
   file-name 
   file-type
   sha1-digest
   claim-id
   app-user-id)
  (:metaclass described-db-access-class))

(defun attachment-relative-pathname (attachment)
  (merge-pathnames
   (if (every (curry #'slot-boundp attachment) '(file-name sha1-digest))
       (let ((n (format nil "~A_~A"
			(attachment.sha1-digest attachment)
			(map 'string 
			     (lambda (char)
			       (if (or (eql #\[ char)
				       (eql #\] char))
				   #\-
				   char))
			     (attachment.file-name attachment)))))
	 (if (< (length n) +linux-path-max+)
	     n (subseq n 0 +linux-path-max+)))
       "") 
   (make-pathname 
    :directory  
    `(:relative ,(format nil "~A" 
			 (claim.claim-id 
			  (attachment.claim attachment)))))))

(defun merge-attachment-pathname (attachment)
  (merge-pathnames (attachment-relative-pathname attachment)
		   *attachment-directory*))

(defmethod user-can-delete-p ((object attachment))
  (and (persistentp object) 
       (or (call-next-method) 
	   (db= $app-user (ignore-errors (attachment.app-user object))))))

(defclass trashed-attachment (attachment)
  (deleted-time)
  (:metaclass described-db-access-class))

(defclass attachment-export ()
  ((attachment-id :primary-key t :accessor attachment.attachment-id)
   file-name
   claim-id)
  (:metaclass described-db-access-class)
  (:table-name attachment))
