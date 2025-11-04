(defpackage :maxclaims/ecm-description/ibc-code
  (:use ))

(in-package :maxclaims/ecm-description)

(define-descriptions maxclaims/data-entity/ibc-code:ibc-code
  (:default identity)
  (:inline 
   maxclaims/data-entity/ibc-code::industry
   maxclaims/data-entity/ibc-code::industry-classification
   maxclaims/data-entity/ibc-code::description))


