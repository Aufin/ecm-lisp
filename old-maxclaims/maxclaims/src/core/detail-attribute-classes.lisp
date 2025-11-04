(in-package #:maxclaims)

;;; I do not like having to put these here, but compilation ordering
;;; being what it is forces this, bah. Oh well. --clinton

;;; Not sure if the following is the cleanest, but it works. --clinton
(defclass relation-relation-detail ()
  ()
  (:metaclass described-db-access-class)
  (:documentation "Protocol class for detail<->object relation of risk/claims"))

(defclass relation-detail ()
  ()
  (:metaclass described-db-access-class)
  (:documentation "Protocol class for detail relations"))

(defclass relation-with-details-mixin ()
  ()
  (:documentation "Interface for relations which have details (see
  NOTIFY-DETAIL-CHANGED)")
  (:metaclass described-db-access-class))

(defgeneric relation.details (relation))
(defgeneric (setf relation.details) (new-value relation))



(defgeneric notify-detail-changed (object relation-relation-detail old-value new-value)
  (:method (object relation-relation-detail old-value new-value)
    #+nil(break "no method: ~A/~A/~A/~A" object relation-relation-detail old-value new-value)))

;;; todo: Might want to replace these with a common details getting
;;; protocol on the detail/claim/risk classes themselves
(defgeneric attribute-object-risk-type (attribute object)
  (:documentation "Return RISK-TYPE of attribute's object"))

(defgeneric attribute-value-detail-type (attribute detail-detail)
  (:documentation "Return RISK-TYPE of member DETAIL-DETAIL in the details
  list for ATTRIBUTE"))

(defgeneric attribute-value-detail (attribute detail-detail)
  (:documentation "Return DETAIL of member DETAIL-DETAIL in the
  details list for ATTRIBUTE"))

(defgeneric make-object-detail (object)
  (:documentation "Return empty FOO-FOO-detail associated with OBJECT"))

(defgeneric detail.detail-detail-type-p (detail)
  (:method ((detail t))
    nil))

(defun find-object-detail (foo-foo-detail object accessor)
  (find foo-foo-detail (funcall accessor object)
	:test (lambda (dao1 dao2)
		(if (and (persistentp dao1) (persistentp dao2))
		    (db= dao1 dao2)
		    (eql dao1 dao2)))))

(deftype claim-details ()
  '(or (cons claim-claim-detail *) null))


(deftype risk-details ()
  `(or (cons risk-risk-detail *) null))
