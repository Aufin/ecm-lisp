(in-package :maxclaims)

(define-description attachment (description-for-attachment)
  ((claim :validate (boundp))
   (app-user :validate (boundp))
   (date :validate (boundp))
   (file-description :input (:type textarea) :active :when)
   (file-name :active :when)
   (file :attribute-class file-attribute :function 'merge-attachment-pathname
	 :validate (attachment-path-valid-p))
   (active-attributes :value '((file-name :active :when)
			       file-type
			       file-description
			       date
			       (app-user :attributes (username))
			       claim
			       file))))

(define-description attachment 
    (description-for-attachment)
  ((active-attributes :value '(file file-description)))
  (:in-description editable))

(define-description attachment 
    (description-for-attachment)
  ((active-attributes 
    :value '(file-name file-type claim date (app-user :attributes (username)))))
  (:in-description log))

(define-description attachment-export (description-for-attachment-export)
  ()
  (:attributes (file-name :active :when)))

(defvar *attachment-directory* #P"/home/drewc/src/maxclaims/database/attachments/")
(defvar *attachment-trash-directory* #P"/home/drewc/src/maxclaims/database/attachment-trash/")

(defconstant +linux-path-max+ 255
  "ext3, xfs, btrfs, &c all have a lame 255 char maximum path
   length. Truncate internal file names of attachments to this length.

 WARNING: changing this will make the system unable to find
  some attachments.")

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

(lol::register-validator 'attachment-path-valid-p
			 (lambda (a v)
			   (or (pathname-name v)
			       (prog1 nil
				 (signal (make-condition
					  'lol::validation-condition
					  :format-string "Please select a file to upload"
					  :format-args nil
					  :attribute a
					  :object (attribute-object a)))))))


(defmethod user-can-delete-p ((object attachment))
  (and (persistentp object) 
       (or (call-next-method) 
	   (db= $app-user (ignore-errors (attachment.app-user object))))))

(defun merge-trashed-attachment-pathname (trashed-attachment)
  ;; trashed attachments are prefixed with the time they were deleted
  ;; to prevent any potential file name or content conflicts (it is
  ;; permissible to delete the same file more than once per claim)
  (let ((initial-name (attachment-relative-pathname trashed-attachment)))
    (merge-pathnames
     (merge-pathnames
      (make-pathname :name (let ((n (concatenate
				     'string
				     (princ-to-string
				      (simple-date:timestamp-to-universal-time
				       (trashed-attachment.deleted-time trashed-attachment)))
				     "_"
				     (pathname-name initial-name))))
			     (if (< (length n) +linux-path-max+)
				 n
				 (subseq n 0 +linux-path-max+))))
		      
      initial-name)
     *attachment-trash-directory*)))

(defmethod delete-object :around ((attachment attachment))
  "Copy deleted attachment to a temporary trash directory"
  (let ((attachment-pathname (merge-attachment-pathname attachment))
	trashed-attachment-pathname)
    (handler-case 
	(ensure-transaction ()
	  (let ((trashed-attachment (insert-object
				     (make-object-from-plist
				      'trashed-attachment
				      (rofl::make-insert-object-plist attachment)))))
	    (reload-object trashed-attachment)
	    ;; bind this outside of here for the error handler
	    (setf trashed-attachment-pathname
		  (merge-trashed-attachment-pathname trashed-attachment))
	    (call-next-method)
	    ;; if any database operations are performed after this
	    ;; point the error handler needs updating
	    (ensure-directories-exist trashed-attachment-pathname)
	    (fad:copy-file attachment-pathname
			   trashed-attachment-pathname)
	    (delete-file attachment-pathname)))
      (file-error (e)
	(when (and trashed-attachment-pathname
		   (fad:file-exists-p trashed-attachment-pathname))
	  ;; this should be fine (if the directories cannot be created
	  ;; it doesn't exist, if the file can't be copied it doesn't
	  ;; exist or may partially exist, if the delete failed the
	  ;; original still exists)
	  (delete-file trashed-attachment-pathname))
	(error e)))))
