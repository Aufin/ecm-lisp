(defpackage :ecm/api/find
  (:use :cl)
  (:import-from :hunchentoot)
  (:import-from :postmodern)
  (:import-from :ecm/user)
  (:export ))
(in-package :ecm/api/find)

(defun call-with-user (functiom)
  (let ((user (hunchentoot:session-value :app-user)))
    (if user
        (maxclaims::call-with-app-user
         user         
         (lambda ()
           (maxclaims::with-udb
             (funcall functiom user))))
        (error "No user available"))))

(hunchentoot:define-easy-handler (pongo-find-handler :uri "/ecm/api/find")
    (find)
      (let* ((q (s-sql:sql-compile `(:select (pongo.find ,find))))
             (result (ecm/user:call-with-user 
                      (lambda (user) (declare (ignorable user))
                              (postmodern:query q :single)))))
        ;; (warn " Found : ~A ~A" find result)
        (if (eql :null result)
            "[]"
            result)))


        
    
       
       
        
                   
           
                                       
    
    
    

