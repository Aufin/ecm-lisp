(in-package :maxclaims)

(defun $-formatter (value)
  (if (not value)
      "$0"
      (format nil "$~$" value)))

(defun format-simple-date (timestamp &rest keys &key &allow-other-keys)
  (apply #'local-time:format-timestring
	 nil
	 (local-time:universal-to-timestamp (simple-date:timestamp-to-universal-time
					     timestamp))
	keys))
