(defpackage :ecm/endpoint/thread
  (:use :cl)
  (:import-from :ecm/ui/thread)
  (:import-from :ecm/user)
  (:import-from :ecm/configuration)
  (:import-from :trivial-backtrace)
  (:import-from :ecm/entity/attachment
		#:archive-attachment)
  (:import-from :ecm/endpoint
		#:define-endpoint))
(in-package :ecm/endpoint/thread)

(defstruct threadmaker
  claim-id
  (start-time (get-universal-time))
  completed
  errors)

(defvar *threads* (make-hash-table :test 'equal))

(defun threadmaker-completed-p (tm)
  (or (threadmaker-completed tm)
      (not (bt:thread-alive-p (cdr (gethash (threadmaker-claim-id tm) *threads*))))))

(defun make-archive-thread (claim-id &key (error nil))
  (declare (optimize (debug 3)))
  (let (errors)
    (maxclaims::with-adb
      (postmodern:with-transaction ()
	(let* ((unarchived-attachments
		(ecm/entity/attachment:claim-attachments
		 claim-id :where '(:= nil 'archived)))
	       (dir (concatenate 'string "/tmp/threadmaster/"
				 (princ-to-string claim-id)"/"))
	       (error-log (merge-pathnames "error.log" dir)))
	(uiop:run-program
	 (concatenate 'string "mkdir -p " dir))
	(alexandria:with-output-to-file
	    (stdout (merge-pathnames "archive.log" dir)
		    :if-exists :overwrite
		    :if-does-not-exist :create)
	  (alexandria:with-output-to-file (stderr error-log
						  :if-exists :overwrite
						  :if-does-not-exist :create)
	    (loop :for a :in unarchived-attachments
	       :do #+(or) (warn "Archiving attachment : ~A"
			 (ecm/json:getjso "_id" a))
	       (block foo
		     (handler-bind
			 ((error
			   (lambda (c)
			     (push a errors)
			     (format stderr "%~A~%" c)
			     (format stdout "%~A~%" c)
			     (trivial-backtrace:print-backtrace-to-stream stderr)
			     (terpri stderr)
			     (unless error (return-from foo nil)))))
		       (print (archive-attachment a) stdout)
		     )))
	    (unless errors (delete-file error-log))))

	(when errors
	  (with-open-file (s error-log :direction :input)
	    (uiop:run-program
	     (format nil "mail -t me@drewc.ca -s 'Error for Claim #~A : attachment archive' " claim-id)
			      :input s)))
	(apply #'values (not errors) `(,@(when errors (list* error-log errors)))))))))
	    
(defun threadmaker (claim-id &optional (create t))
  (declare (optimize (debug 3)))
  (or (car (gethash claim-id *threads*))
      (when create 
	(let* ((thread (make-threadmaker :claim-id claim-id))
	       (hash (cons thread nil)))
	  (setf (gethash claim-id *threads*) hash)
	  (prog1 thread
	    (setf (cdr hash)
		  (bt:make-thread
		   (lambda ()
		     (declare (optimize (debug 3)))
		     (multiple-value-call
			 (lambda (done &optional error-log &rest errors)
			   (setf (threadmaker-completed thread) t
				 (threadmaker-errors thread)
				 (when (not done)
				   (cons error-log errors))))
		       (make-archive-thread claim-id))))))))))
				
(define-endpoint claim-thread
    "ecm/claim/(\\d+)/thread/(.*)")

(defun claim-thread/get (claim-id path)
  (ecm/user:with-user ()
    (let ((claim-id (parse-integer claim-id))
	  (read?
	   (postmodern:query
	    (:select (:app_user_can_read 'claim)
		     :FROM 'claim
		     :where (:= 'claim-id (:type claim-id integer)))
	    :single)))
      (when (eq read? t)
	(claim-thread-page claim-id path)))))

(defun claim-thread-page (claim-id path)
  (let* ((archive-dir (merge-pathnames (make-pathname 
					:directory `(:relative
						     ,(princ-to-string claim-id)  "list"))
				       (ecm/configuration:archive-directory)))
	 (index? (uiop:file-exists-p
		  (merge-pathnames  "index.html" archive-dir)))
	 (thread (threadmaker claim-id nil))
	 (absolute-path  (merge-pathnames (if (string-equal path "")
					      "index.html"
					      path)
					  archive-dir)))

    #+(or)   (break "~A" 		  (merge-pathnames  "index.html" archive-dir))
    
    (cond ((and index? (or (not thread) (threadmaker-completed-p thread)))
	   (remhash claim-id *threads*)
	   (when (ecm/entity/attachment:unarchived-claim-attachments claim-id)
	     (let* ((*threads* (make-hash-table :test 'equal))
		    (thread (threadmaker claim-id t)))
	       (loop :until (threadmaker-completed-p thread))))
	   (ecm/hunchentoot:handle-static-file absolute-path))
	  ((and (not index?) thread (threadmaker-completed-p thread))
	   (remhash claim-id *threads*)
	   "")
	  (t
	   (let ((thread (or thread (threadmaker claim-id t))))
	     (declare (ignore thread))
	     (ecm/ui/thread:make-new-thread-page claim-id ))))))
  
  




