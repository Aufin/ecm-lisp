(in-package :maxclaims)

;;; fixme: looks like export-... and make-a-f-stream can safely be nuked
(defun export-attachments ()
  (dolist (attachment (select-objects 'attachment-export)) 
    (let* ((file-directory (merge-pathnames 
			    (format nil "~A/" (attachment-export.claim-id attachment))
			    *attachment-directory*))
	   (file-name (merge-pathnames (or (ignore-errors (attachment-export.file-name attachment)) 
					   (format nil "NO-NAME-~A"(attachment.attachment-id attachment))) file-directory))
	   (attachment-stream (make-attachment-file-stream attachment))) 
      (ensure-directories-exist file-directory)
      (with-output-to-file (stream (if (cl-fad:directory-pathname-p file-name)
				       (merge-pathnames 						    (format nil "NO-NAME-~A"(attachment.attachment-id attachment)) file-name) file-name):if-exists :overwrite :if-does-not-exist :create :element-type '(unsigned-byte 8))
	(copy-stream attachment-stream stream)))))

(defun make-attachment-file-stream (attachment)
  (make-instance 'rofl::bytea-stream
		 :table 'attachment
		 :column 'file
		 :primary-key-name 'attachment-id
		 :primary-key-value(attachment.attachment-id attachment)))