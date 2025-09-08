(defpackage :ecm/report/agency-bordereau
  (:use :cl)
  (:import-from #:ecm/spreadsheet
		#:create-spreadsheet)
  #+(or)(:import-from :ecm/spreadsheet
		#:cell-value)
  (:export #:agency-bordereau-csv-file))
(in-package :ecm/report/agency-bordereau)

(defun escape-comma (string)
  (with-output-to-string (es)
    (map nil (lambda (c)
               (when (equalp c #\,) (princ #\\ es))
               (princ c es))
         string)))
(defun agency-bordereau-csv-file
    (agency-id
     start-date
     end-date
     &key
       (universal-time (get-universal-time))
       (filename
	(format nil "bordereau-~A-~A-~A-~A"
		agency-id start-date end-date
		universal-time)))
  (let* ((fieldnames (mapcar (lambda (n) (intern (first n) :ecm/report/agency-bordereau))
     (maxclaims/report/bordereau:agency-bordereau-fields)))
	(form
	 `(postmodern:doquery 
	      ,(maxclaims/report/bordereau:find-bordereau
		 agency-id
		 :fields 
		 (maxclaims/report/bordereau:agency-bordereau-fields)
		 :where `(= CONTRACT.AGENCY-ID ,agency-id)
		 :start-date start-date
		 :end-date end-date
		 :risk-type nil
		 :query-fn #'list)
	      ,fieldnames
	    (format stream "~{~A~^	 ~}~%"
		          (mapcar (lambda (thing)
                        (typecase thing
				                  (symbol
				                   (if (eql :null thing)
				                       ""
				                       (string thing)))
				                  (simple-date:timestamp
				                   (multiple-value-bind (y m d)
				                       (simple-date:decode-timestamp
				                        thing)
				                     (format nil "'~A-~A-~A" y m d)))
				                  (t thing)))
			                (list ,@fieldnames)))))
	 (pathname (make-pathname
		    :name filename
		    :directory "tmp"
		    :type "csvtab")))
					   
    
	 (prog1 pathname
	   (eval `(alexandria:with-output-to-file (stream
					     (make-pathname
					      :name ,filename
					      :directory "tmp"
					      :type "csvtab")
					     :if-does-not-exist :create)
		    (format stream "~{~a~^	 ~}~%" (mapcar #'symbol-name ',fieldnames))
	      ,form)))))
     



(defun agency-bordereau-spreadsheet (agency-bordereau-pathname)
  )
