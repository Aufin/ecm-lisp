(in-package :maxclaims)

(defgeneric make-deleted-record (record))
(defgeneric undelete-record (deleted-record))
(defgeneric deleted-record.class (deleted-record)
  (:documentation "name of class that should be created when undeleting"))

(defmethod undelete-record ((r deleted-record))
  (prog1 (insert-object (apply #'make-object (deleted-record.class r)
			       (rofl::make-insert-object-plist r)))
    (delete-object r)))