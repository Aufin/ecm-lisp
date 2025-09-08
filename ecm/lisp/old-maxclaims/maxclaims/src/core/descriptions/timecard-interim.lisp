(in-package :maxclaims)

(define-description timecard-interim (description-for-timecard-interim)
  ((claim :validate (boundp))
   (date :input (:type simple-date))
   (active-attributes
    :value '((claim :active :when
	      :attributes (claim-id insured)
	      :deactivate (editable))
	     (date)))))