(in-package :maxclaims)

(define-layered-class text-slot-attribute (slot-definition-attribute)
 ())

(define-layered-method attribute-active-p :around ((attribute text-slot-attribute))		       
 (let ((active? (slot-value attribute 'lol::activep)))
   (if (and (eq :when active?)
	    (let ((v (attribute-value attribute)))
	      (and (typep v 'sequence)
		   (and (= 0 (length v))))))
       NIL       
       (call-next-method))))

(define-layered-method attribute-active-p 
 :in-layer #.(defining-description 'editable) 
 :around ((attribute text-slot-attribute))		       
 (let ((active? (slot-value attribute 'lol::activep)))
   (if (and (eq :when active?)
	    (let ((v (attribute-value attribute)))
	      (and (typep v 'sequence)
		   (and (= 0 (length v))))))
       t      
       (call-next-method))))