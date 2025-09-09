#-asdf3 (error "SEXPML requires ASDF 3 or later with \"asdf-package-system\"")
(asdf:defsystem :sexpml
  :description "SEXPML: Symbolic Expression eXtensible Programmable Markup Language"
  :class :package-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:sexpml/sexpml))



			       
