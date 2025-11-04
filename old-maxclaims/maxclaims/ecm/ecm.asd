#-asdf3 (error "ECM requires ASDF 3 or later. Please upgrade your ASDF.")

(asdf:defsystem :ecm
  :description "ECM: Electronic Claims Manager"
  :author "Drew Crampsie <me@drewc.ca>"
  :licence "MIT"
  :class :package-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on ())

(asdf:register-system-packages :mel-base
			                         :mel)
(register-system-packages
 "cxml-stp"
 '(:stp
   :cxml-stp))
