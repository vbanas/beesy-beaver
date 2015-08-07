(in-package :beesy-beaver)

(defvar *width*)
(defvar *height*)

;; Pos

(defstruct (pos (:constructor mk-pos))
  row
  col)

(defun make-pos (row col)
  (mk-pos :row row :col col))

(defun generate-seq (size func)
  (let ((seq (fset:empty-seq)))
    (loop for ind below size
       do (setf seq (fset:with-last seq (funcall func ind))))
    seq))

(defun generate-row ()
  (generate-seq *width*
                (lambda (ind)
                  (declare (ignore ind))
                  0)))

(defun make-field ()
  (generate-seq
   *height*
   (lambda (ind)
     (declare (ignore ind))
     (generate-row))))

(defun get-cell (field pos)
  (fset:@ (fset:@ field (pos-row pos)) (pos-col pos)))

(defun put-cell (field pos val)
  (let ((old-row (fset:@ field (pos-row pos))))
    (fset:with field (pos-row pos)
               (fset:with old-row (pos-col pos) val))))

(defun remove-row (field row)
  (fset:with-first (fset:less field row)
    (generate-row)))

;; And now for hexagonal operations



;; Little benchmarking

(defun benchmark-field (width height)
  (let* ((*width* width)
         (*height* height)
         (field (make-field))
         (num-reads 100000)
         (num-writes 100000)
         (num-removes 10000)
         (reads (loop for i below num-reads
                     collect (make-pos (random height) (random width))))
         (writes (loop for i below num-writes
                     collect (make-pos (random height) (random width))))
         (removes (loop for i below num-removes
                     collect (random height))))
    (time (loop for pos in reads
               do (get-cell field pos)))
    (time (loop for pos in writes
             do (setf field (put-cell field pos 1))))
    (time (loop for row in removes
             do (setf field (remove-row field row))))
    (time (loop for pos in reads
               do (get-cell field pos)))))
