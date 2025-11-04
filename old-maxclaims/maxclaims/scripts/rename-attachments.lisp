;;; Script to rename attachments from $claim-id/$file-name ->
;;; $claim-id/$attachment-id_$file-name

(in-package #:maxclaims)

(defvar *dups* (list))
(defvar *bad-attachments* (list))

;;; `rename-attachment-files' must be invoked explicitely (it cannot
;;; be undone easily like the database part)

(defun test-rename ()
  (setf *dups* nil *bad-attachments* nil)
  (touch-dummy-files)

  ;; delete a few random files to make sure the missing-file machinery works
  (with-db
    (with-dummy-context (:render nil :action nil)
      (mapc (lambda (a)
	      (ignore-errors (delete-file (merge-attachment-pathname/old a))))
	    (query-objects 'attachment (lambda (table fields)
					 `(:limit (:order-by (:select ,@fields
								      :from ,table)
							     (:random))
						  25))))))
  
  (prepare-db-for-merge))

(defun prepare-db-for-merge ()
  (fix-trashed-attachments)
  (seed-sha1digest)
  (purge-bad-attachments)
  (merge-duplicate-attachments))

(defun touch-dummy-files ()
  (with-dummy-context (:render nil :action nil)
    (with-db
      (touch-dummy-files/attachment)
      (touch-dummy-files/trash)))
  (values (length *bad-attachments*) (length *dups*)))

(defun seed-sha1digest ()
  (with-dummy-context (:render nil :action nil)
    (with-db
      (seed-sha1digest/attachment)
      (seed-sha1digest/trash))))

(defun rename-attachment-files ()
  (with-dummy-context (:render nil :action nil)
    (with-db
      (rename-files/attachment)
      (rename-files/trash)))
  (values (length *bad-attachments*) (length *dups*)))

;;; Dummy attachments for local testing

(defun touch-dummy-files/attachment ()
  "Create empty files for all attachments to permit local testing of
attachment related code"
  (dolist* (attachment (select-objects 'attachment))
    (let ((path (merge-attachment-pathname/old attachment)))
      (cond ((or (not (slot-boundp attachment 'file-name))
		 (string= "" (attachment.file-name attachment)))
	     (push attachment *bad-attachments*))
	    ((fad:file-exists-p path)
	     (push attachment *dups*))
	    (t (ensure-directories-exist path)
	       (with-open-file (dummy path
				      :direction :output
				      :if-exists :error
				      :if-does-not-exist :create)
		 ;; attempt to avoid duplicate digests
		 (write-string (arnesi:random-string 1024) dummy)
		 (terpri dummy)))))))

(defun touch-dummy-files/trash ()
  "Create empty files for all attachments to permit local testing of
attachment related code"
  (dolist* (attachment (select-objects 'trashed-attachment))
    (let ((path (merge-trashed-attachment-pathname/old attachment)))
      (cond ((fad:file-exists-p path)
	     (push attachment *dups*))
	    (t (ensure-directories-exist path)
	       (with-open-file (dummy path
				      :direction :output
				      :if-exists :error
				      :if-does-not-exist :create)
		 ;; attempt to avoid duplicate digests
		 (write-string (arnesi:random-string 1024) dummy)
		 (terpri dummy)))))))


;;; SHA-1 Digest for Preexisting Attachment

(define-symbol-macro +null-file-sha1sum+ "da39a3ee5e6b4b0d3255bfef95601890afd80709")

(defun seed-sha1digest/attachment ()
  "Store SHA-1 Digest for preexisting attachments"
  (with-transaction* ()
    (without-ro-contracts
	(lambda ()
	  (dolist* (attachment (select-objects 'attachment))
	    (cond ((not (slot-boundp attachment 'file-name))
		   nil)
		  ((string= "" (attachment.file-name attachment))
		   ;; evil workaround: set the sha1-digest to that of
		   ;; an empty file
		   (setf (attachment.sha1-digest attachment)
			 +null-file-sha1sum+))
		  ((fad:file-exists-p (merge-attachment-pathname/old attachment))
		   (setf (attachment.sha1-digest attachment)
			 (ironclad:byte-array-to-hex-string
			  (ironclad:digest-file :sha1 (merge-attachment-pathname/old attachment)))))
		  (t ;; the file doesn't exist but it has a name
		     ;; ... no good. Give it the sha1-digest of an
		     ;; empty file
		   (setf (attachment.sha1-digest attachment)
			 +null-file-sha1sum+)))
	    (update-object attachment))))))

(defun seed-sha1digest/trash ()
  "Store SHA-1 Digest for preexisting trashed attachments"
  (with-transaction* ()
    (without-ro-contracts
	(lambda ()
	  (dolist* (attachment (select-objects 'trashed-attachment))
	    (setf (attachment.sha1-digest attachment)
		  (ironclad:byte-array-to-hex-string
		   (ironclad:digest-file :sha1 (merge-trashed-attachment-pathname/old
						attachment))))
	    (update-object attachment))))))

;;; Rename

(defun rename-files/attachment ()
  (dolist* (attachment (select-objects 'attachment))
    (let ((path (merge-attachment-pathname/old attachment)))
      (cond ((or (not (slot-boundp attachment 'file-name))
		 (string= "" (attachment.file-name attachment)))
	     (push attachment *bad-attachments*))
	    (t (rename-file path (merge-attachment-pathname/new attachment)))))))

(defun rename-files/trash ()
  (dolist* (attachment (select-objects 'trashed-attachment))
    (let ((path (merge-trashed-attachment-pathname/old attachment)))
      (rename-file path (merge-trashed-attachment-pathname/new attachment)))))

;;; Utilities

(defun without-ro-contracts (thunk)
  "Call `thunk' with read only contract checking disabled."
  (ensure-transaction ()
    (query (:raw "SELECT * INTO TEMP ro_contract_stash FROM read_only_contract"))
    (query (:delete-from 'read-only-contract))

    (funcall thunk)

    (query (:insert-into 'read-only-contract (:select '* :from 'ro-contract-stash)))
    (query (:drop-table 'ro-contract-stash))))


;;; Copy&Pasted Code Warning

;;; To make the script independent of the application definition of
;;; `merge-*' wrt attachment file names, the old and new versions of
;;; each are here as /old and /new. pcos is frowning at me for doing
;;; this the ugly way instead of using the magic of contextl to do
;;; versioning... but this is quicker.

(defun attachment-relative-pathname/new (attachment)
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

(defun attachment-relative-pathname/old (attachment)
  (merge-pathnames (map 'string 
			(lambda (char)
			  (if (or (eql #\[ char)
				  (eql #\] char))
			      #\-
			      char))
			(ignore-errors (attachment.file-name attachment))) 
		   (make-pathname 
		    :directory  
		    `(:relative ,(format nil "~A" 
					(claim.claim-id 
					 (attachment.claim attachment)))))))

(defun merge-attachment-pathname/old (attachment)
  (merge-pathnames (attachment-relative-pathname/old attachment)
		   *attachment-directory*))

(defun merge-attachment-pathname/new (attachment)
  (merge-pathnames (attachment-relative-pathname/new attachment)
		   *attachment-directory*))


(defun merge-trashed-attachment-pathname/old (trashed-attachment)
  ;; trashed attachments are prefixed with the time they were deleted
  ;; to prevent any potential file name or content conflicts (it is
  ;; permissible to delete the same file more than once per claim)
  (let ((initial-name (attachment-relative-pathname/old trashed-attachment)))
    (merge-pathnames
     (merge-pathnames
      (make-pathname :name (concatenate
			    'string
			    (princ-to-string
			     (simple-date:timestamp-to-universal-time
			      (trashed-attachment.deleted-time trashed-attachment)))
			    (pathname-name initial-name)))
		      
      initial-name)
     *attachment-trash-directory*)))

(defun merge-trashed-attachment-pathname/new (trashed-attachment)
  ;; trashed attachments are prefixed with the time they were deleted
  ;; to prevent any potential file name or content conflicts (it is
  ;; permissible to delete the same file more than once per claim)
  (let ((initial-name (attachment-relative-pathname/new trashed-attachment)))
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


;;; Bad Attachment Purge

(defclass empty-attachment (attachment)
  ()
  (:metaclass described-db-access-class))

(defclass missing-attachment (attachment)
  ()
  (:metaclass described-db-access-class))

(defun purge-bad-attachments ()
  "Remove useless attachments"
  (with-dummy-context (:render nil :action nil)
    (with-db
      (without-ro-contracts
	  (lambda ()
	    ;; file-name IS NULL attachments have no data and can be dropped
	    (query (:delete-from 'attachment :where (:is-null 'file-name)))
	    ;; file-name = ''
	    (dolist* (attachment (query-objects 'attachment
						(lambda (table fields)
						  `(:select ,@fields
							    :from ,table
							    :where (:= 'file-name "")))))
	      (insert-object (make-object-from-plist 'empty-attachment
						     (rofl::make-insert-object-plist attachment)))
	      (query (:delete-from 'attachment :where (:= 'attachment-id
							  (attachment.attachment-id attachment)))))
	    ;; file isn't there
	    (dolist* (attachment (remove-if (compose #'fad:file-exists-p
						     #'merge-attachment-pathname/old)
					    (query-objects 'attachment
							   (lambda (table fields)
							     `(:select ,@fields
								       :from ,table)))))
	      (insert-object (make-object-from-plist 'missing-attachment
						     (rofl::make-insert-object-plist attachment)))
	      (query (:delete-from 'attachment :where (:= 'attachment-id
							  (attachment.attachment-id attachment))))))))))


(defun fix-trashed-attachments ()
  ;; Some trashed attachments were created by removing attachments
  ;; with no associated file... just touch an empty file for each
  ;; these
  (with-db
    (with-dummy-context (:render nil :action nil)
      (mapc (lambda (p) (close (open p :direction :output :if-exists :error)))
	    (remove-if #'fad:file-exists-p
		       (mapcar #'merge-trashed-attachment-pathname/old
			       (select-objects 'trashed-attachment)))))))

;;; 'Duplicate' Attachment Merge

(defclass duplicate-attachment (attachment)
  ((duplicated-attachment :references attachment
			  :column duplicated-attachment-id)
   duplicated-attachment-id)
  (:metaclass described-db-access-class))

(defun merge-duplicate-attachments ()
  (with-dummy-context (:render nil :action nil)
    (with-db
      (with-transaction* (attachment-transaction)
	(without-ro-contracts
	    (lambda ()
	      (dolist* ((claim-id file-name) (query (:select 'claim-id 'file-name :distinct
							     :from 'attachment
							     :where (:not (:is-null 'file-name)))))
		(destructuring-bind (master . duplicates)
		    (query-objects 'attachment
				   (lambda (table fields)
				     `(:order-by
				       (:select ,@fields :from ,table
						:where (:and (:= 'claim-id ,claim-id)
							     (:= 'file-name ,file-name)))
				       'date)))
		  (dolist* (a duplicates)
		    (insert-object (make-object-from-plist 'duplicate-attachment
							   (list* 'duplicated-attachment-id
								  (attachment.attachment-id master)
								  (rofl::make-insert-object-plist a))))
		    (query (:delete-from 'attachment :where (:= 'attachment-id
								(attachment.attachment-id a)))))))))
	
	
	(break "~A ~A ~A"
	       (query (:select (:count '*) :from 'attachment))
	       (query (:select (:count '*) :from 'duplicate-attachment))
	       (length (query (:select 'claim-id 'file-name :distinct :from 'attachment))))))))


(defun verify-rename ()
  (let ((bad (list)))
    (with-dummy-context (:render nil :action nil)
      (with-db
	(dolist (attachment (select-objects 'attachment))
	  (unless (and (fad:file-exists-p (merge-attachment-pathname/new attachment))
		       (string= (attachment.sha1-digest attachment)
				(ironclad:byte-array-to-hex-string
				 (ironclad:digest-file :sha1 (merge-attachment-pathname/new attachment)))))
	    (push (list (attachment.attachment-id attachment)
			(attachment.claim-id attachment)
			(attachment.sha1-digest attachment))
		  bad)))))
    bad))

#+nil(defun nuke-it-from-orbit (&key dry-run)
  "Create empty files for all attachments to permit local testing of
attachment related code"
  (with-dummy-context (:render nil :action nil)
    (with-db
      (dolist* (attachment (select-objects 'attachment))
	(let ((path (new-attachment-filename attachment)))
	  (ignore-errors (delete-file path))
	  ))))
  (values (length *bad-attachments*) (length *dups*)))