(asdf:defsystem :beesy-beaver
  :depends-on (:fset :yason)
  :components ((:file "src/packages")
	       (:file "src/utils" :depends-on ("src/packages" "src/task" "src/field"))
               (:file "src/field" :depends-on ("src/packages"))
               (:file "src/search" :depends-on ("src/packages"))
               (:file "src/game-state" :depends-on ("src/packages" "src/field" "src/task" "src/utils"))
               (:file "src/lcgen" :depends-on ("src/packages"))
               (:file "src/task" :depends-on ("src/packages" "src/field"))
               ))

