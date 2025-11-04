(defpackage :ecm/ui/thread
  (:use :cl)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{} #:|.|)
  (:import-from :ecm/ui/page)
  (:import-from :ecm/user)
  (:import-from :ecm/configuration)

  (:export #:make-new-thread-page))

(in-package :ecm/ui/thread)

(defun make-new-thread-page (claim-id)
  (<> '(ecm/ui/page:page
	:title "Make Thread"
	:refresh 10)
    (<> :text "Creating archive for " claim-id ". Will refresh shortly, and archive will appear when it is finished.")
    (<> 'hr)
    (<> :text "If this is seen for 20 minutes or more, contact the administrator.")))


 
	      

  
