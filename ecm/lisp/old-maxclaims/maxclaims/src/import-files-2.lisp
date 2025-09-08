(in-package :maxclaims)

;;(with-db (execute "drop table import_file"))
(defun make-import-table ()
  
  (with-db (execute "
CREATE TABLE import_file_2 (
 import_file_id SERIAL PRIMARY KEY,
 pathname TEXT,
 owner_id INTEGER REFERENCES app_user(app_user_id),
 external_claim_number text,
 open BOOLEAN default true)")))

(defclass import-file-2 (import-file)
  ()
  (:metaclass described-db-access-class))

(defun import-claim-files-2 (path)
  (let ((claim-files (make-file-alist path)))
    (loop for file in claim-files do 
	 (unless (select-only 1 '* :from 'import-file-2 :where `(:= external-claim-number ,(car file)))
	   (insert-object (make-object 'import-file-2 :external-claim-number (car file) :pathname path))))))
  
(defcomponent claim-file-import-component-2 ()
  ((path :accessor path :initform "")))


(defmethod render ((component claim-file-import-component-2))
  (unless (find :import-file-2 (list-tables))
    (make-import-table))
  (let ((files (sort (select-objects 'import-file-2)
		     #'string< 
		     
		     :key #'import-file.external-claim-number)))
    (when (equal  (app-user.username $app-user) "admin")
      (<:as-html "warning: admin use only")
      (<ucw:form  
       :action (import-claim-files-2 (path component))
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
			(let* ((path (import-file.pathname file))
			       (file-alist (make-file-alist path))
			       (csv-pathname (if (probe-file (merge-pathnames #P"CHUCLCHUTT-Documents.csv" path))
						 (merge-pathnames #P"CHUCLCHUTT-Documents.csv" path)
						 (merge-pathnames #P"CHUTTCHUCL-Documents.csv" path)))
			       (file-csv (read-csv-file csv-pathname)))
			  
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
				 :activate '(inline))))))))


(defcomponent view-import-files ()
  ((file :accessor file :initarg :file)
   (file-alist :accessor file-alist :initarg :file-alist)
   (file-csv :accessor file-csv :initarg :file-csv)))



(defaction view-import-files (file file-alist file-csv)
  (call 'view-import-files :file file :file-alist file-alist :file-csv file-csv))
