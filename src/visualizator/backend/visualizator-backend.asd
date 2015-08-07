
(asdf:defsystem :visualizator-backend
  :depends-on (:hunchentoot :beesy-beaver :yason)
  :components ((:file "get-map")))
