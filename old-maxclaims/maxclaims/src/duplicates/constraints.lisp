(in-package :maxclaims)

(defun table.name (table)
  (first table))

(defun table.key (table)
  (second table))

(defun table.foreign-keys (table)
  (third table))

(defun merge.table.key (merge foreign-table-name)
  (table.key (assoc foreign-table-name (merge.constraints merge))))

(defun merge.table.foreign-keys (merge foreign-table-name)
  (table.foreign-keys (assoc foreign-table-name (merge.constraints merge))))
