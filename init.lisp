
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

;;; optimization options
(proclaim '(optimize (debug 3) (safety 3)))

;;; load system
(load "beesy-beaver.asd")
(asdf:load-system :beesy-beaver)

(print "Welcome to the beesy-beaver")

