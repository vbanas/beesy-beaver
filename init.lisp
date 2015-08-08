
;;; load quicklisp
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)

    ))

;; quickload dependencies
(quicklisp:quickload "asdf")
(quicklisp:quickload "alexandria")
(quicklisp:quickload "fset")
(quicklisp:quickload "yason")
;;; optional for vizualizator
(quicklisp:quickload "hunchentoot")

;;; optimization options
(proclaim '(optimize (debug 3) (safety 3)))

;;; load system
(load "beesy-beaver.asd")
;;; optional
(load "src/visualizator/backend/visualizator-backend.asd")
(asdf:load-system :visualizator-backend)
(asdf:load-system :beesy-beaver)

(print "Welcome to the beesy-beaver")

