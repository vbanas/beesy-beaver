(asdf:defsystem :beesy-beaver
  :depends-on (:fset)
  :components ((:file "src/packages")
               (:file "src/field" :depends-on ("src/packages"))
               (:file "src/game-state" :depends-on ("src/packages" "src/field"))
	       (:file "src/lcgen" :depends-on ("src/packages"))))
