(in-package :maxclaims)

(defclass maxclaims-static-application (maxclaims-application)
  ()
  (:default-initargs 
   :url-prefix "/maxwell/"
   :static-roots (list (cons "" (project-relative-pathname #P"wwwroot/maxwell/")))))

(defparameter *maxclaims-static-application* (make-instance 'maxclaims-static-application))


