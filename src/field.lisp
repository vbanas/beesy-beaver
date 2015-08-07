(in-package :beesy-beaver)

(defun generate-seq (size func)
  (let ((seq (fset:empty-seq)))
    (loop for ind below size
       do (setf seq (fset:with-last seq (funcall func ind))))
    seq))

(defun generate-row (width)
  (generate-seq width (lambda (ind)
                        (declare (ignore ind))
                        0)))

(defun make-field (width height)
  (generate-seq
   height
   (lambda (ind)
     (declare (ignore ind))
     (generate-row width))))

(defun get-cell (field row col)
  (fset:@ (fset:@ field row) col))

(defun put-cell (field row col val)
  (let ((old-row (fset:@ field row)))
    (fset:with field row
               (fset:with old-row col val))))

(defun remove-row (field row width)
  (fset:with-first (fset:less field row)
    (generate-row width)))

;; And now for hexagonal operations



;; Little benchmarking

(defun benchmark-field (width height)
  (let* ((field (make-field width height))
         (num-reads 100000)
         (num-writes 100000)
         (num-removes 10000)
         (reads (loop for i below num-reads
                     collect (cons (random height) (random width))))
         (writes (loop for i below num-writes
                     collect (cons (random height) (random width))))
         (removes (loop for i below num-removes
                     collect (random height))))
    (time (loop for (row . col) in reads
               do (get-cell field row col)))
    (time (loop for (row . col) in writes
             do (setf field (put-cell field row col 1))))
    (time (loop for row in removes
             do (setf field (remove-row field row width))))
    (time (loop for (row . col) in reads
               do (get-cell field row col)))))
