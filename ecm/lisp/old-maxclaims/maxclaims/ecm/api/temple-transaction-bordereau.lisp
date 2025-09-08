(defpackage :ecm/api/temple-transaction-bordereau
  (:use :cl)
  (:import-from :hunchentoot)
  (:import-from :postmodern)
  (:export ))
(in-package :ecm/api/temple-transaction-bordereau)

(hunchentoot:define-easy-handler (pongo-find-handler :uri "/ecm/api/temple-transaction-bordereau")
    (find)
      (let* ((q (s-sql:sql-compile `(:select (pongo.find ,find))))
             (result (call-with-user (lambda (user) (declare (ignorable user))
                                             (postmodern:query q :single)))))
        (warn " : ~A ~A" find result)
        (if (eql :null result)
            "[]"
            result)))





        
    
       
       
        
                   
           
                                       
    
    
    

