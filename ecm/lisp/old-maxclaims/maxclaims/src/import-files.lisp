(in-package :maxclaims)

(defvar *import-files-csv*)
(defvar *import-files-alist*)
(defvar *import-files-html-csv*)
(defvar *import-files-html-alist*)
;;(with-db (execute "drop table import_file"))
(defun make-import-table ()
  
  (with-db (execute "
CREATE TABLE import_file (
 import_file_id SERIAL PRIMARY KEY,
 pathname TEXT,
 owner_id INTEGER REFERENCES app_user(app_user_id),
 external_claim_number text,
 open BOOLEAN default true)")))

(defclass import-file ()
  ((import-file-id :primary-key t)
   pathname 
   (owner :references app-user 
	  :on app-user-id
	  :column owner-id)
   external-claim-number 
   open)
  (:metaclass described-db-access-class))

(defun find-claim-files (&key (directory #P"/home/drewc/src/maxclaims.old/database/scm-export/export/"))
  (let ((claim-files (list)))
    (flet ((maybe-add-claim-file 
	       (pathname &aux (name (pathname-name pathname)))
	     (when (and name (eql #\5 (aref name 0)))
	       (push pathname claim-files))))
      (cl-fad:walk-directory directory #'maybe-add-claim-file :directories t))
    claim-files))

(defun find-claim-html-files (&key (directory #P"/home/drewc/src/maxclaims.old/database/scm-export/export/"))
  (let ((claim-files (list)))
    (flet ((maybe-add-claim-file 
	       (pathname &aux (name (pathname-name pathname)))
	     (when (and name (equal "HTMLDOCS" (first (last (pathname-directory pathname)))))
	       (push pathname claim-files))))
      (cl-fad:walk-directory directory #'maybe-add-claim-file :directories t))
    claim-files))

(defun make-file-alist (path &optional (finder #'find-claim-files) (format-string "~A-~A")) 
  (let ((table (make-hash-table :test 'equalp)))
    (dolist (file (funcall finder :directory path))
      (let* ((%id (loop for i in (split-sequence #\_ (pathname-name file)) repeat 2 collect i)) 
	     (id (format nil format-string (first %id) (second %id))))
	(push file (gethash id table))))
    (alexandria:hash-table-alist table)))

(defun import-claim-files (path)
  (let ((claim-files (make-file-alist path)))
    (loop for file in claim-files do 
	 (unless (select-only 1 '* :from 'import-file :where `(:= external-claim-number ,(car file)))
	   (insert-object (make-object 'import-file :external-claim-number (car file) :pathname path))))))
  
(defcomponent claim-file-import-component ()
  ((path :accessor path :initform "")))

(defun pathname-file-name (pathname)
  (format nil "~A.~A" (pathname-name pathname) (pathname-type pathname)))

(defstruct (import-csv (:type list)) 
  BRANCH CLAIM SEQUENCE DOCUMENTDATE EMPLOYEE TITLE PATH CLONEABLE TEMPLATE
  ADDED-BY UPDATED-BY DATE-ADDED DATE-UPDATED MIME-TYPE FILE-TYPE ARCHIVED THUMBNAIL
  DATE-VIEWED VIEWED-BY REFNUMBER PRINT-ORDER INCLUDEINPRINT COPIEDTOCALGARY
  VIEWINCLIENTDATA COPYSTATUS DELETION)

(defstruct (html-csv (:type list))
  Sequence Branch Claim Description Template Content Orientation HeaderSize FooterSize HeaderPage1 FooterPage1 HeaderRemainder FooterRemainder AddedBy DateAdded UpdatedBy DateUpdated Cloneable HeaderSizeR FooterSizeR Margins ReportWriter MetaInfo ReadOnly ClientViewable VersionOf ReleaseStatus ReleaseDate Auditor Synched)

(defmethod render ((component claim-file-import-component))
  (flet ((render ()	   
	   (unless (find :import-file (list-tables))
	     (make-import-table))
	   (let ((files (sort (select-objects 'import-file)
			      #'string< 
		     
			      :key #'import-file.external-claim-number)))
	     (let* ((path (import-file.pathname (car files)))
		    (csv-pathname (or (probe-file (merge-pathnames #P"CHUCLCHUTT-Documents.csv" path))
				      (probe-file  (merge-pathnames #P"CHUCLCHULL-Documents.csv" path))
				      (probe-file (merge-pathnames #P"CHUTTCHUCL-Documents.csv" path))))
		    (html-csv-pathname (or 
			     (probe-file (merge-pathnames #P"CHUCLCHUTT-iDocuments.csv" path))
			     (probe-file  (merge-pathnames #P"CHUCLCHULL-iDocuments.csv" path))
			     (probe-file (merge-pathnames #P"CHUTTCHUCL-iDocuments.csv" path))))
		    )
	       (unless (boundp '*import-files-csv*)
		 (setf *import-files-csv* (read-csv-file csv-pathname)))
	       (unless (boundp '*import-files-alist*)
		 (setf *import-files-alist* (make-file-alist path)))
	       (unless (boundp '*import-files-html-csv*)
		 (setf *import-files-html-csv* (read-csv-file html-csv-pathname)))
	       (unless (boundp '*import-files-html-alist*)
		 (setf *import-files-html-alist* (make-file-alist path #'find-claim-html-files "~A"))))

	     (when (equal  (app-user.username $app-user) "admin")
	       (<:as-html "warning: admin use only")
	       (<ucw:form  
		:action (import-claim-files (path component))
		(<:as-html "enter the path to the import files")
		(<ucw:input :accessor (path component))

		(<:submit)))

	     (progn 
	       (<:H1 (display component $app-user :activate '(inline) :attributes '(username))
		     (<:as-html " : Import SCM Files"))
	       (<:P "This is a list of all the claims in the current SCM export. If you click on the link 'view and download', you will see a list of all the files that were attached to the claim in the other system.")
	       (<:P "These import claims can have one of two status markers : OPEN, which means it has yet to be entered into the system, and 'CLOSED', which means it has. Claims that are closed will be displayed "
		    (<:span :style "background :grey" "On a grey background like this. ")
		    "You can OPEN and CLOSE claims by clicking on them and selecting the OPEN/CLOSE link")
	       (<:as-html (print (length files)))
	       
	       (arnesi:dolist* (file files)
		 (<:li 

		  (<ucw:a 
		   :action 	  
		   (let* ((file-alist *import-files-alist*)
			  (file-csv *import-files-csv*))
			  
		     (view-import-files 
		      file 
		      (assoc (import-file.external-claim-number file) 
			     file-alist :test 'equalp)
		      file-csv))
		   :style (if (import-file.open file)
			      ""
			      "background : grey")
		   (display component file 
			    :attributes '(external-claim-number)
			    :activate '(inline)))))))))
    (if (user-read-only-p $app-user)
	(<:p "Error: You are not authorized to use this functionality.")
	(render))))


(defcomponent view-import-files ()
  ((file :accessor file :initarg :file)
   (file-alist :accessor file-alist :initarg :file-alist)
   (file-csv :accessor file-csv :initarg :file-csv)))

(defmethod render ((component view-import-files))
  (with-slots (file file-alist file-csv) component
    (let (files)
            (let ((claim-number ""))
	      (<:h3 "Import attachments to claim #"
		    (<ucw:form 
		     :action (copy-attachments-to-claim claim-number files)
		     (<ucw:input :accessor claim-number )
		     (<:submit)))) 
      (<:h1 "Import Claim : "
	    (display component file 
		     :attributes '(external-claim-number)
		     :activate '(inline)))
      (<ucw:a :action (answer t) "<----- Go Back")
      (if (import-file.open file)
	  (<:p "This claim is marked 'OPEN', which means it has not been entered into the system yet. If you have finished entering the data, " 
	       (<ucw:a :action (progn (setf (import-file.open file) nil)
				      (update-object file))
		       "Click here to mark this claim 'CLOSED'"))
	  (<:p :style "background : grey"
	       "This claim is marked 'CLOSED', which means it has already been entered. If it was closed by mistake, "
	       (<ucw:a :action (progn (setf (import-file.open file) t)
				      (update-object file))
		       "Click here to return this claim to 'OPEN'")))
    
      (dolist* (path (cdr file-alist))
	(let ((csv-line (find (third (split-sequence #\_ (pathname-name path))) file-csv :key #'third :test #'string=)))
	  (push (list path (format nil "~A-~A-~A.~A" 
					       (import-csv-branch csv-line)
					       (import-csv-claim csv-line)
					       (import-csv-title csv-line)
					       (pathname-type path))
		      (import-csv-mime-type csv-line)) 
		files)
	  (<:li (<ucw:a :action (download-file 
				 :file path 
				 :name (format nil "~A-~A-~A.~A" 
					       (import-csv-branch csv-line)
					       (import-csv-claim csv-line)
					       (import-csv-title csv-line)
					       (pathname-type path))
				 :content-type (import-csv-mime-type csv-line))
			(<:as-html (import-csv-title csv-line) " " path)))))
    
      (<:div 
       (<:h3 "HTML documents")
       (let* ((file-alist *import-files-html-alist*)
	      
	      (file-csv *import-files-html-csv*)
	      (csv-lines (remove-if-not
			  (lambda (x)
			    (equal (second (split-sequence #\- (import-file.external-claim-number file)))
				   (html-csv-claim x))) file-csv)))
	 (dolist* (line csv-lines)
	   (push (list (first  (cdr (assoc (html-csv-sequence line)
							      file-alist :test #'equalp)))
		       (format nil "~A-~A.html" 
						  (html-csv-description line)
						  (html-csv-sequence line))
		       "text/html")
		 files)
	   (let ()
	     (<:li (<ucw:a :action (download-file 
				    :file (first  (cdr (assoc (html-csv-sequence line)
							      file-alist :test #'equalp))) 
				    :name (format nil "~A-~A.html" 
						  (html-csv-description line)
						  (pathname-type path))
				    :content-type "text/html")
			   (<:as-html (html-csv-description line) " ")))))))

)))

(defun move-attachment-to-claim (claim file)

  (let ((attachment 
	 (insert-object (make-object 'attachment :claim claim :file-name (substitute #\- #\/  (second file)) :file-type (third file)))))
    (ensure-directories-exist (merge-attachment-pathname attachment))

    (when (probe-file (merge-attachment-pathname attachment))
      (setf (attachment.file-name attachment)
	    (concatenate 'string  (arnesi:random-string) "-"(attachment.file-name attachment) 
			 )))
	 
	 (with-input-from-file (in (first file) :element-type '(unsigned-byte 8)) 
	   (with-output-to-file (out (merge-attachment-pathname attachment) 
				     :if-does-not-exist :create 
				     :element-type '(unsigned-byte 8))
	     (copy-stream in out)))))

(defaction copy-attachments-to-claim (claim-number files)
  (let ((claim (select-only-n-objects 1 'claim :where `(:= claim-id ,claim-number))))
    (loop 

       for file in files 
       do (move-attachment-to-claim claim file)
       )

    (view-object claim)
    (answer nil)))

(defaction view-import-files (file file-alist file-csv)
  (call 'view-import-files :file file :file-alist file-alist :file-csv file-csv))
