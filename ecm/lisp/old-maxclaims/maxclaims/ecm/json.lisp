(uiop:define-package :ecm/json
  (:use :cl :st-json)
  (:import-from :st-json
                #:read-json-from-string
                #:jso-alist
		#:write-json-element)
  (:shadow #:getjso* )
  (:import-from :alexandria)
  (:import-from :hunchentoot)
  (:import-from :flexi-streams)
  (:reexport :st-json)
  (:export #:read-json-from-string
           #:get-post-body-as-json
           #:jso-alist
	   #:null->nil))
(in-package :ecm/json)

(defmethod read-json ((in pathname) &optional junk-allowed-p)
  (alexandria:with-input-from-file (s in)
    (read-json s junk-allowed-p)))

(defun from-json-bool (value)
  "Convert :true or :false to its boolean equivalent."
  (case value (:true t) (:false nil) (null nil)))

(defun getjso* (keys map)
  (let ((last (position #\. keys :from-end t)))
    (when map
      (if last
	  (let ((next (getjso* (subseq keys 0 last) map)))
	    (when next (getjso (subseq keys (1+ last)) next)))
	  (getjso keys map)))))

(defun get-post-body-as-json ()
  (let* ((raw-json (hunchentoot:raw-post-data))
	 (stringized-json
	  (if (stringp raw-json)
	      raw-json
	      (flexi-streams:octets-to-string raw-json :external-format :utf-8))))
    (let ((*read-default-float-format* 'double-float))
      (read-json-from-string stringized-json))))

(defun null->nil (jso)
  (unless (eq :null jso) jso))

(defmethod write-json-element ((element real) stream)
    (format stream "~A" element))


