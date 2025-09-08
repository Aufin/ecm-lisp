(cl:defpackage :relational-objects-for-lisp
  (:use 
   #:closer-common-lisp
   #:arnesi
   #:postmodern)
  (:nicknames #:rofl)
  (:export

   ;; ROFL
   #:standard-db-access-class
   #:standard-db-access-object
   #:make-object-from-plist
   #:make-object
   #:described-db-access-class
   #:make-object
   #:select-only
   #:select-only*
   #:select
   #:insert-into   
   #:select-objects
   #:select-only-n-objects
   #:select-only-n-objects*
   #:select-using-object
   #:query-objects
   #:insert-object
   #:update-object
   #:delete-object
   #:insert-foreign-object
   #:update-foreign-object
   #:delete-foreign-object
   #:modify-foreign-object
   #:reload-object
   #:primary-key-boundp
   #:persistentp
   #:modifiedp
   #:slot-modified-p
   :db-access-object-=
   :db=
   #:find-object
   #:object-id
  
   #:define-versioned-database

   ;; extensions to postmodern
   #:with-transaction*
   #:ensure-transaction))
