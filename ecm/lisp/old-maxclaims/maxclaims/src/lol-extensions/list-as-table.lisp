(in-package #:maxclaims)

(define-description description-as-table-row ()
 ())

(define-layered-method display-using-description 
    :in-layer #.(lol::defining-description 'description-as-table-row)
    :around (desc disp obj &rest args)
    (<:tr 
     (with-inactive-descriptions (description-as-table-row)
       (with-active-descriptions (attribute-as-column)
	 (call-next-layered-method)))))

(define-description attribute-as-column ()
  ())

(define-layered-method lol::display-html-attribute
    :in-layer #.(lol::defining-description 'attribute-as-column)
    :around (object attribute)
    (<:td 
     (with-inactive-descriptions (attribute-as-column)
       #+nil(call-next-layered-method)
       #+nil(lol::display-html-attribute-value object attribute)
       (display-attribute-value attribute))))

(define-description description-as-table-header-2 ()
  ())

(define-layered-method display-using-description 
    :in-layer #.(lol::defining-description 'description-as-table-header-2)
    :around (desc disp obj &rest args)
    (<:tr
     (with-inactive-descriptions (description-as-table-header-2)
       (with-active-descriptions (attribute-as-column-header)
	 (call-next-layered-method)))))

(define-description attribute-as-column-header ()
  ())

(define-layered-method lol::display-html-attribute
    :in-layer #.(lol::defining-description 'attribute-as-column-header)
    :around (object attribute)
    (<:th 
     (with-inactive-descriptions (attribute-as-column-header)
       (with-inactive-descriptions (inline)
	 (<:as-html (lol::attribute-label attribute))))))

#+nil(define-layered-method lol::display-html-attribute-value
    :in-layer #.(lol::defining-description 'attribute-as-column-header)
    :around (object attribute)
    (<:th 
     (with-inactive-descriptions (attribute-as-column-header)
       (with-inactive-descriptions (inline)
	 (<:as-html (lol::attribute-label attribute))))))

(define-description list-as-table ()
  ())

(define-layered-method display-using-description 
    :in-layer #.(lol::defining-description 'list-as-table)
    (desc disp obj &rest args)
    (<:table
     (with-active-descriptions (description-as-table-header-2)
       (apply #'display desc disp (first obj) args))
     (dolist* (item obj)
       (with-active-descriptions (description-as-table-row)
	 (apply #'display desc disp item args)))))

#+nil(define-layered-method lol::display-html-attribute-value 
    :in-layer #.(lol::defining-description 'list-as-table)
    (object (attribute lol::list-attribute))
    (call-next-layered-method))

(define-description t ()
  ((attribute-delimiter :value ""))
  (:in-description list-as-table))