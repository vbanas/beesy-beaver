(load "init.lisp")

(quicklisp:quickload "hunchentoot")
(load "src/visualizator/backend/visualizator-backend.asd")
(asdf:load-system :visualizator-backend)
