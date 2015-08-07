
;;; load quicklisp
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)

    ;; quickload dependencies
    ))

;;; optimization options
(proclaim '(optimize (debug 3) (safety 3)))

;;; load system

(print "Welcome to the beesy-beaver")

