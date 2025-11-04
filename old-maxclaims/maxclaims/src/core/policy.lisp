(in-package :maxclaims)

(defun policyp (policy)
  (typep policy 'policy))

(defun policy-exists-p (policy)
  (and (policyp policy)
       (primary-key-boundp policy)))