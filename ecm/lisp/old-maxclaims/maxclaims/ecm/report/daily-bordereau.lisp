(defpackage :ecm/report/daily-bordereau
  (:use :cl))
(in-package :ecm/report/daily-bordereau)
(in-package :ecm/report/bordereau)
(defun daily-bordereau (&optional (syndicate-id 59756) (day nil))
  (maxclaims::with-adb
   (let* ((end (or day (postmodern:query "select date_trunc('day', now())::date::text" :single)))
          (start (postmodern:query "SELECT ($1::date - interval '1 day')::date::text" end :single))
          (name (postmodern:query "SELECT '4747-Carbon_' || regexp_replace((date_trunc('second', now()) AT TIME ZONE 'UTC')::text, ' ', '_')" :single))
          (bdx (lloyds-v5-bordereau-spreadsheet
                nil
                syndicate-id start end nil
                :bordereau (lloyds-v5-bordereau nil syndicate-id start end nil)))
          (file (ecm/spreadsheet:make-spreadsheet-file bdx :name name :type "csv"))
          (lines (with-open-file (s file :direction :input)
                    (loop for line = (read-line s nil nil)
                          while line :collect line)))
          (lines (cdddr (nbutlast lines 2)))
          (lines (list* (car lines) (cddr lines))))
     (with-open-file (s file :direction :output :if-exists :supersede)
                     (loop for line in lines :do (format s "~A~%" line)))
     (cl-fad:copy-file file (merge-pathnames (make-pathname :name name :type "csv") "/home/carbon/"))
     
    file)))
