(in-package :beesy-beaver)

(defun get-one-solution-for-file (file seed)
  (when (probe-file file)
    (let ((task (decode-task (alexandria:read-file-into-string file))))
      (values task (with-output-to-string (str)
                     (yason:encode (simple-wave-from-task-one-seed
                                    task seed)
                                   str))))))

(defun main ()
  (when sb-ext:*posix-argv*
    (let* ((parsed-args (apply-argv:parse-argv* ;;'("./test" "-f" "problems/problem_24.json")))
			 sb-ext:*posix-argv*))
	   (files) (phrases) (time) (memory) (proc-count))
      ;;(format t "~A~%~A~%" parsed-args (alexandria:plist-alist (cdr parsed-args)))
      (mapcar (lambda (p) 
		(let ((o (string (car p)))
		      (v (cdr p)))
		  (cond
		    ((string= "-f" o) (push v files))
		    ((string= "-p" o) (push v phrases))
		    ((string= "-c" o) (setq proc-count v))
		    ((string= "-m" o) (setq memory v))
		    ((string= "-t" o) (setq time v)))))
	      (alexandria:plist-alist (cdr parsed-args)))
      ;;(format t "~A~%" files)
      (let ((result-list nil))
	(dolist (f (reverse files))			
	  (when (probe-file f)
	    ;;(format t "~A~%~%" (alexandria:read-file-into-string f))
	    (setf result-list 
		  (append result-list (simple-wave-from-task 
				       (decode-task (alexandria:read-file-into-string f))))))) 
	(yason:encode result-list)))))
