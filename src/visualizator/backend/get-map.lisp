
(in-package :visualizator-backend)

(declaim (optimize (debug 3)))

(define-easy-handler (get-map :uri "/get-map") (pos)
  (setf (content-type*) "application/json")
  (let ((result (make-string-output-stream)))
    (yason:encode-alist
     (case pos
       (0 (get-current-map))
       (1 (get-next-map))
       (-1 (get-prev-map)))
     result)
    (get-output-stream-string result)))


(defun get-current-map ()
  )


(defun get-next-map ())


(defun get-prev-map ())
