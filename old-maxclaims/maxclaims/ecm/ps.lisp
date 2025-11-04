(uiop:define-package :ecm/ps
    (:use :cl :parenscript)
  (:shadow #:{} #:[])
  (:reexport :parenscript)
  (:export  #:|.|
	    #:$.))
(in-package :ecm/ps)

(ps:defmacro+ps {} (&body plist)
  `(ps:create ,@plist))

(ps:defpsmacro [] (&body array-elements)
  `(ps:array ,@array-elements))

(ps:defpsmacro |.| (&body array-elements)
  `(ps:chain ,@array-elements))

(ps:defpsmacro $. (selector &body jquery)
  `(ps:chain ($ ,selector) ,@jquery))


