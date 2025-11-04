(defpackage :ecm/endpoint/corpus
  (:use :cl)
  (:import-from :ecm/user)
  (:import-from :ecm/entity/corpus)
  (:import-from :ecm/ui/corpus)
  (:import-from :ecm/request-context
		#:with-request-context)
  (:import-from :ecm/endpoint
		#:define-endpoint))
(in-package :ecm/endpoint/corpus)

(define-endpoint corpus-search "/ecm/corpus/search")

(defun corpus-search/get ()
  (with-request-context ()
    (let* ((term (ecm/hunchentoot:parameter "term"))
	   (corpus (ecm/entity/corpus:search-for-corpus term :limit 25)))
    (ecm/request-context:send-json-response
     corpus))))

(define-endpoint corpus-inline "/ecm/corpus/(\\d+)/inline$")

(defun corpus-inline/get (corpus-id &aux (corpus-id (parse-integer corpus-id)))
  (with-request-context ()
    (let ((corpus (ecm/entity/corpus:find-corpus corpus-id)))
    (with-output-to-string (ecm/ml:*sexpml-output*)
      (ecm/ui/corpus:<corpus> corpus)))))

(define-endpoint corpus-create-inline "/ecm/corpus/create/inline$")

(defun corpus-create-inline/get ()
  (with-request-context ()
    (with-output-to-string (ecm/ml:*sexpml-output*)
      (ecm/ui/corpus:<corpus-create>
       :name-prefix (or (ecm/hunchentoot:parameter "prefix")
			"")))))


  
			    
