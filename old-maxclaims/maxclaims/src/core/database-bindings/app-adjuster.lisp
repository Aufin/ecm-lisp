(in-package #:maxclaims)

(defclass app-adjuster ()
  ((app-adjuster-id :primary-key t)
   (person :references person :column person-id)
   full-time
   active
   person-id)
  (:metaclass described-db-access-class))
