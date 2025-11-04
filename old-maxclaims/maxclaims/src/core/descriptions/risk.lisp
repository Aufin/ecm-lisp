(in-package :maxclaims)

;;; fixme: need better descriptions than the default for
;;; risk-type/category/detail/etc.

(define-description risk-risk-detail (description-for-risk-risk-detail)
  ((risk :validate (boundp))
   (risk-detail :validate (boundp))))

(define-description risk-type (description-for-risk-type)
  ((type-name :label nil)
   (active-attributes :value '(type-name)))
  (:in-description inline))

(define-description risk-detail (description-for-risk-detail)
  ((active-attributes :value '((code :active :when)
			       description)))
  (:in-description inline))

(define-description risk-risk-detail (description-for-risk-risk-detail)
  ((active-attributes :value '((risk-detail :active :when)
			       (detail
				:active :when
				:label nil))))
  
  (:in-description inline))

(define-display :in-description inline ((desc risk-risk-detail))
  (lol::generic-format *display* "~A: ~A"
		  (risk-detail.description
		   (attribute-value (find-attribute desc 'risk-detail)))
		  (let ((d (attribute-value (find-attribute desc 'detail))))
		    (unless (or (lol::unbound-slot-value-p d) 
				(not d))
		      (risk-risk-detail-detail.detail
		       (car d))))))

(define-description risk-risk-detail-detail (description-for-risk-risk-detail-detail)
  ((active-attributes :value '(detail)))
  (:in-description inline))


;;; based upon the common bits of the old risk and
;;; commercial/marine/etc. descriptions
(define-description risk (description-for-risk)
  ((type :input (:type select-db-object :db-type risk-type
		       :attributes (type-name)))
   (policy :input (:type select-db-object :db-type policy)
	   :validate (boundp))
   (contract :input (:type select-db-object
			   :db-type contract
			   :attributes (contract-number))
	     :validate (boundp))
   (details :attribute-class risk-details-attribute
	    :active t)
   (lol::label :function (lambda (self)
			   (risk-type.type-name (risk.type self))))
   (claim-number :function (lambda (self)
			     (length (risk.claims self))))
   (claims :active :when :attributes ((list :item (:activate (link-to-viewer)))))
   (active-attributes :value '((type)
			       (policy :activate (link-to-viewer))
			       (contract :activate (link-to-viewer))
			       claims
			       (details :active :when)))))

(define-description risk (description-for-risk)
  ((active-attributes :value '(contract
			       policy
			       (claims :attributes ((list :item (:activate (link-to-viewer))))))))
  (:in-description inline))

(define-description risk (description-for-risk)
  ((active-attributes :value '((policy :attributes (policy-number))
			       (contract :validate (boundp))
			       details)))
  (:in-description editable))
