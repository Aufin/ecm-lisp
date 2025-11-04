(cl:in-package #:maxclaims.test)

(defun in-transaction (closure)
  (with-transaction (test-transaction)
    (prog1 (funcall closure)
      (abort-transaction test-transaction))))

(defun select-one (query)
  (query (sql-compile `(:limit ,query 1))
	 :single))

(defun select-one-randomly (query)
  (select-one `(:order-by ,query (:random))))