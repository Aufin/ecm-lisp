(in-package #:maxclaims)

(define-description app-adjuster (description-for-app-adjuster)
  ((person :input (:type select-db-object
			 :db-type person)
	   :label "Person"
	   :attributes (first-name last-name company-name)
	   :validate (boundp)
	   :active :when)
   (active-attributes :value '(person))))

(define-description app-adjuster (description-for-app-adjuster)
  ((person :attributes ((first-name :label nil)
			(last-name :label nil)
			(company-name :label nil)))
   (active-attributes :value '(person)))
  (:in-description inline))

(define-description app-adjuster (description-for-app-adjuster)
  ((person :active t))
  (:in-description editable))
