(asdf:defsystem :beesy-beaver
  :depends-on ()
  :components ((:file "src/packages")
               (:file "src/field" :depends-on ("src/packages"))))
