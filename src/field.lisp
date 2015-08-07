(in-package :beesy-beaver)

(defun generate-seq (size func)
  (let ((seq (fset:empty-seq)))
    (loop for ind below size
       do (setf seq (fset:with-last seq (funcall func ind))))
    seq))

(defun make-field (width height)
  (generate-seq
   height
   (lambda (ind)
     (declare (ignore ind))
     (generate-seq width (lambda (ind)
                           (declare (ignore ind))
                           0)))))

(defun get-cell (field row col)
  (fset:@ (fset:@ field row) col))

(defun put-cell (field row col val)
  (let ((old-row (fset:@ field row)))
    (fset:with field row
               (fset:with old-row col val))))

