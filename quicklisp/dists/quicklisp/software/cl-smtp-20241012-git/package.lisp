;;; -*- mode: Lisp -*-

;;; This file is part of CL-SMTP, the Lisp SMTP Client

;;; Copyright (C) 2004/2005/2006/2007 Jan Idzikowski

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser General Public License
;;; (http://opensource.franz.com/preamble.html), known as the LLGPL.

;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; Lisp Lesser GNU General Public License for more details.

;;; File: package.lisp
;;; Description: cl-smtp package definition file

(in-package :cl-user)

(defpackage :cl-smtp
  (:use #:cl)
  (:local-nicknames (:a :alexandria))
  (:import-from #:usocket
		#:with-client-socket
		#:get-host-name)
  (:import-from #:flexi-streams
		#:string-to-octets
		#:make-external-format
		#:flexi-stream-stream
		#:make-flexi-stream
		#:flexi-stream-external-format)
  #-allegro
  (:import-from #:cl-base64
		#:usb8-array-to-base64-string
		#:usb8-array-to-base64-stream)
  #-allegro
  (:import-from #:cl+ssl
		#:make-ssl-client-stream
		#:stream-fd)
  (:export #:send-email
           #:with-smtp-mail
	   #:*x-mailer*
	   #:smtp-error
	   #:smtp-protocol-error
           #:no-supported-authentication-method
           #:rcpt-failed
           #:ignore-recipient
	   #:attachment
	   #:make-attachment
	   #:attachment-name
	   #:attachment-data-pathname
	   #:attachment-mime-type
           #:rfc2045-q-encode-string
           #:rfc2231-encode-string
	   #:write-rfc5322-message
           #:write-rfc8822-message ;; deprecated
           #:*debug*
           ;; restart-case case-names
           #:send-without-message-id
           #:use-local-hostname
           #:use-custom-domain-part
           #:use-custom-message-id))

(in-package :cl-smtp)

(defparameter *debug* nil)

(defmacro print-debug (str)
  `(when *debug*
      (print ,str)))
