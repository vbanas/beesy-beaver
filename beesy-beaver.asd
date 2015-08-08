(asdf:defsystem :beesy-beaver
  :depends-on (:fset :yason)
  :components ((:file "src/packages")
	       (:file "src/main" :depends-on ("src/packages" "src/task" "src/other-searches"))
	       (:file "src/utils" :depends-on ("src/packages" "src/task" "src/field"))
               (:file "src/field" :depends-on ("src/packages"))
               (:file "src/search" :depends-on ("src/packages"
                                                "src/game-state"))
               (:file "src/game-state" :depends-on ("src/packages" "src/field" "src/task" "src/utils" "src/lcgen"))
               (:file "src/lcgen" :depends-on ("src/packages"))
               (:file "src/task" :depends-on ("src/packages" "src/field"))
               (:file "src/other-searches" :depends-on ("src/packages" "src/game-state" "src/task"))
               ))

