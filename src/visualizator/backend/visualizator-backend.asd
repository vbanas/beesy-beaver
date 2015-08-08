
(asdf:defsystem :visualizator-backend
  :depends-on (:hunchentoot :beesy-beaver :yason)
  :components ((:file "package")
               (:file "get-map" :depends-on ("package"))
               (:file "control" :depends-on ("package"))))
