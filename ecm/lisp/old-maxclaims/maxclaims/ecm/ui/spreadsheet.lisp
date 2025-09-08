(defpackage :ecm/ui/spreadsheet
  (:use :cl)
  (:import-from :ecm/ml #:<>)
  (:import-from #:ecm/ui/utility)
  (:import-from :ecm/spreadsheet
                #:ssconvert-export-types
                #:ssconvert-type-name
                #:ssconvert-type-value
		#:ssconvert)
  (:export #:<spreadsheet-type-select>
	   #:<download-spreadsheet>))
(in-package :ecm/ui/spreadsheet)

(defun <spreadsheet-type-select>
    (&key (name "spreadsheet-type")
       (in-browser "in-browser")
       (class "ecm/spreadsheet-type-select")
       (type nil))
  (<> (select :class (ecm/ui/utility:cat "form-control " class) :name name)
    (when in-browser
      (<> `(option :value ,in-browser)
        (<> "View in browser")))
    (let ((options (ssconvert-export-types)))
      (dolist (option options)
        (<> (option :value (ssconvert-type-value option)
		    (when (equalp type (ssconvert-type-value option))
		      "selected"))
          (<> :text (first 
                     (split-sequence:split-sequence
                      #\; 
                      (ssconvert-type-name option)))))))))

(defun <download-spreadsheet> (spreadsheet
                               &key (type "in-browser")
				 (filename (if (pathnamep spreadsheet)
					       (pathname-name spreadsheet)
					       "spreadsheet")))
  (let ((file
	 (typecase
	     spreadsheet
	   (pathname
	    (ssconvert
	     spreadsheet
	     (if (string= type "in-browser")
		 (find "Gnumeric_html:html40"
		       (MAX-ECM/GNUMERIC/SSCONVERT::ssconvert-export-types)
		       :key 'max-ecm/gnumeric/ssconvert:ssconvert-type-value
		       :test #'equalp)
		 (find type
		       (MAX-ECM/GNUMERIC/SSCONVERT::ssconvert-export-types)
		       :key 'max-ecm/gnumeric/ssconvert:ssconvert-type-value
		       :test #'equalp))
             "-I Gnumeric_stf:stf_csvtab"))
	   (t
	    (ecm/spreadsheet:make-spreadsheet-file
	     spreadsheet :name filename
	     :type (if (string= type "in-browser")
		       "html"
		       type))))))
    	
    (if (string= type "in-browser")
	(<> :unescaped
	    (alexandria:read-file-into-string file))
	(progn
	  (setf (hunchentoot:header-out
		 "Content-Disposition")
		(format nil "attachment; filename=\"~A.~A\""
			(pathname-name file)
			(pathname-type file)))
	  (hunchentoot:handle-static-file file)))))
