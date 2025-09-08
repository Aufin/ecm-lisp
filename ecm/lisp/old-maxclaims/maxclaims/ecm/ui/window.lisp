(defpackage :ecm/ui/window
  (:use :cl)
  (:import-from :ecm/ml #:<>)
  (:import-from :ecm/ps #:{} #:|.|)
  (:export #:<window>)
(in-package  :ecm/ui/window)

(defmacro <window> (heading &body body)
  `(progn
       (<> '(html5:div :class "container-fluid")
	 (<> '(html5:div :class "row collapse-hover")	  
	   (<> '(div :class "bg-primary"
		 :style "padding:5px")
	     (<> '(html5:h6 :style "display:inline")
	       (<> 'html5:strong
		 ,heading)))
	   (<> '(html5:div :class "ecm-window-body collapse in")
	     ,@body)))))
