(defpackage ecm/mail/rfc2822
  (:use :cl)
  (:import-from :mel)
  (:import-from :flexi-streams)
  (:import-from :alexandria)
  (:export #:make-rfc2822-message
	   #:open-stream
	   #:write-message-to-unix-string-stream
	   #:princ-rfc2822-message-to-string))

(in-package :ecm/mail/rfc2822)

(defun make-rfc2822-message
    (&rest headers
     &key
       (subject "")
       from to cc attachments date (body "")
       &allow-other-keys)
  (declare (ignore headers))
  (mel:make-message :subject subject
		    :to to
		    :from from
		    :cc cc
		    :attached-files attachments
		    :date date
		    :body body))

(defun open-stream (message &optional (start 0))
  (mel:open-message-input-stream message start))


(defun open-string-stream (message
			   &key (start 0)
			     (stream-format :utf8))
  (let* (
	(string-stream
	 (flexi-streams:make-flexi-stream
	  message-stream
	  :external-format '(:us-ascii :eol-style :crlf))))
    string-stream))
										
(defun write-message-to-unix-string-stream (message string-stream)
  (loop :with message-stream := (open-stream message)
     :with count := 0
     :for char := (read-char message-stream nil nil)
;     :do   (warn " message string stream ~A ~A" count char) 
     :if (and char (not (char= char #\return)))
     :do (write-char char string-stream) (incf count)
     :else if (not char) :do  (return count)))

(defun read-stream-content-into-string (stream &key (buffer-size 4096))
  "Return the \"content\" of STREAM as a fresh string."
  (let ((*print-pretty* nil))
    (with-output-to-string (datum)
      (let ((buffer (make-array buffer-size :element-type 'character)))
        (loop
          :for bytes-read = (read-sequence buffer stream)
          :do (write-sequence buffer datum :start 0 :end bytes-read)
	   :while (= bytes-read buffer-size))))))

(defun princ-rfc2822-message-to-string (message)
  (remove #\Return
	  (read-stream-content-into-string
	   (open-stream message))))
