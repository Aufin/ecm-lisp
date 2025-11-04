(defpackage :maxclaims/data-entity/ibc-code
  (:use :cl)
  (:import-from :maxclaims)
  (:import-from :postmodern 
		#:execute
		#:query)
  (:import-from :rofl
		#:standard-db-access-class)
  (:export #:ibc-code))

(in-package :maxclaims/data-entity/ibc-code)

(defclass ibc-code ()
  ((industry :primary-key t)
   (industry-classification)
   (description))
  (:metaclass rofl:standard-db-access-class))

(defclass maxclaims::claim-ibc-code ()
  ((claim-id :primary-key t)
   (industry :references ibc-code))
  (:metaclass rofl:standard-db-access-class))

