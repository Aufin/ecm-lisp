(in-package :maxclaims)

(define-description defer-diary-entry (description-for-defer-diary-entry)
  ((defer-date :input (:type simple-date)
	       :validate (boundp)
	       :label "Defer Until Date")
   (diary-entry )
   
   (active-attributes :value '(defer-date diary-entry))))

(define-description defer-diary-entry (description-for-defer-diary-entry)
  (
   
   (active-attributes :value '(defer-date
			       (diary-entry 
				:editp nil
				:deactivate (editable)
				:attributes (action-date claim note)))))
  (:in-description editable))
