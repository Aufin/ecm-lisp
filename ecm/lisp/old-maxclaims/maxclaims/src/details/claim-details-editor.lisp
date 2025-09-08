(in-package #:maxclaims)

;;; Details Editors

;;; Claims details

(defmethod attribute-editor ((attribute claim-details-attribute))
  (make-instance 'claim-details-attribute-editor))

(define-layered-method lol::display-html-attribute-value (object
							  (attribute claim-details-attribute-editor))
  ;; not sure...
  (call-next-method))

(defmethod attribute-object-risk-type ((attribute claim-details-attribute)
				       (object claim))
  (risk.type (claim.risk object)))

(defmethod attribute-value-detail-type ((attribute claim-details-attribute)
					(detail-detail claim-claim-detail))
  (claim-claim-detail.claim-detail-type detail-detail))

(defmethod attribute-value-detail ((attribute claim-details-attribute)
				   (detail-detail claim-claim-detail))
  (claim-claim-detail.claim-detail detail-detail))


(define-description claim-claim-detail (foo-foo-detail)
  ((active-attributes :value '((claim-detail :input (:type select-relation-detail)
				:label nil))))
  (:in-description editing-details))