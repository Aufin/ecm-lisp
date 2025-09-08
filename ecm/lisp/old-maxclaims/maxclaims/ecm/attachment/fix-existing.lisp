(defpackage :ecm/attachment/fix-existing
  (:use :cl)
  (:export))
(in-package :ecm/attachment/fix-existing)

(defun find-attachment-mime-type (filename)
  (let* ((type (pathname-type filename))
	 (mime-type (maxclaims::with-adb
		      (postmodern:query
		       (format nil "SELECT file_type FROM attachment WHERE file_name ILIKE '%~A' LIMIT 1;" type)
		       :single))))
    (or mime-type
	(string-right-trim '(#\Newline)
			   (uiop/run-program:run-program
			    (format nil "file --brief --mime-type ~A"
				    (uiop/run-program:escape-sh-token filename)) :output :string)))))
    
    
(defvar *attachment-default-pathname* "/ecm/mnt/auto/attachment/")

(defvar *attachment-directory-list*
  (remove-if-not (lambda (pn) 
		   (and
		    (equalp #\4 (aref (first (last (pathname-directory pn))) 0))
		    (or (equalp #\1 (aref (first (last (pathname-directory pn))) 1))
			(and (equalp #\0 (aref (first (last (pathname-directory pn))) 1))
			     (<= 5 (parse-integer (string (aref (first (last (pathname-directory pn))) 2)))))
		    )))
		 (cl-fad:list-directory "/ecm/mnt/auto/attachment/")))

(define-condition symlink (error)
  ((pathname :initarg :pathname
	     :reader symlink-pathname))
  (:report (lambda (c s)
	     (format s "Symlink: ~A" (symlink-pathname c)))))
   
	     
  
(defun import-attachment (pathname)
    (let* ((name (pathname-name pathname))
	   (name (or (ignore-errors (subseq name (length "0ca58576ec2726776d2ab1d436e63be11bfc75b6"))) name))
	   (name (if (and (not (equalp name ""))
			  (equalp (aref name 0) #\_))
		     (subseq name 1)
		     (error 'symlink :pathname pathname)))
	   (claim-id (parse-integer (first (last (pathname-directory pathname)))))
	   
           
           (file-type (find-attachment-mime-type pathname))	   
	   (date (local-time:format-rfc1123-timestring
		  nil
		  (local-time:universal-to-timestamp
		   (file-write-date pathname))))  

           (description name)
	   (file-name (make-pathname :name name
				     :type (pathname-type pathname)))
	   (claim-existing?
	    (maxclaims::with-adb
	     (postmodern:query (:select 'claim-id :from 'claim :where (:= 'claim-id
									claim-id))
			       :single)))
	   (new 
            (maxclaims::with-adb
	     (warn "Importing Attachment ~A from ~A"
		   (princ-to-string file-name) pathname)
	     (unless claim-existing?
	       (postmodern:query
		(s-sql:sql-compile
		 (create-import-claim-s-sql 
		  (create-import-risk-s-sql 
		   "Commercial" (create-policy-s-sql  
				 claim-id))
		  claim-id))))
	     (ecm/entity/attachment:create-attachment
	      :app-user-id 2
	      :claim-id claim-id
	      :date date
	      :description description
	      :file-name (princ-to-string file-name)
	      :path pathname
	      :content-type file-type))))
	   (list new
	     claim-id
	    pathname 
	    name file-name
	    file-type
	    date
       )
      ))

(defun import-attachments ()
  (loop for dir in *attachment-directory-list*
     :collect (progn (print dir)
		     (loop for att in (cl-fad:list-directory dir)
			:collect (handler-case
				     (print (import-attachment att))
				   (symlink (c) (format nil "~A" c)))))))
(defun create-insert-contract-s-sql ()
  `(:select (:as (:import_contract_id
		  "Claims from Backup Attachments"
		  (:import-person-id "Maxwell Claims")
		  "2017-08-01" "2018-08-01")
		 id)))

(defun create-insured-s-sql (name)
  `(:import-person-id
    ,name :null :null nil))

(defun create-company-s-sql (name &optional  (must-exist t))
  `(:import-person-id ,name :null :null ,must-exist))

(defun create-insert-maxclaims-s-sql ()
  '(:select (:import-person-id "Maxwell Claims")))



(defun create-policy-s-sql (claim-id)
  (let ((number (princ-to-string claim-id))
	(agent "Maxwell Claims")
	(company "Maxwell Claims")
	(underwriter "Maxwell Claims"))
      `(:select (:import-policy-id
                 ,(create-insured-s-sql "Maxwell Claims")
                 ,number
                 ,(create-company-s-sql agent)
                 ,(create-company-s-sql company)
                 ,(if underwriter (create-company-s-sql underwriter) :null)
		 "2017-01-01" "2018-01-01"))))

(defun create-import-risk-s-sql (type policy)
  `(:select (:import-risk-id
	     ,type ,policy
	     ,(create-insert-contract-s-sql))))

(defun create-import-claim-s-sql (risk claim-id)
    `(:select (:import-claim-id
               ,risk
               ,claim-id
               "Open"
	       :null
	       :null
	       :null
	       "2018-09-01"
	       :null
               :null
	       :null
	       :null
               nil
               0
               :null
               :null
               :null)))
