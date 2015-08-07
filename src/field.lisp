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

(defun move (pos dir)
  (with-slots (row col) pos
    (let ((new-row row)
          (new-col col))
      (case dir
        (:west (decf new-col))
        (:east (incf new-col))
        (:south-west (incf new-row)
                     (when (evenp row)
                       (decf new-col)))
        (:south-east (incf new-row)
                     (when (oddp row)
                       (incf new-col)))
        (otherwise (return-from move :invalid)))
      (when (or (< new-row 0)
                (>= new-row *height*))
        (return-from move :invalid))
      (when (or (< new-col 0)
                (>= new-col *width*))
        (return-from move :invalid))
      (make-pos new-row new-col))))


 
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

(defun test-move ()
  (let* ((*width* 10)
         (*height* 20)
         (failed 0))
    (labels ((%test (row col dir ref-res)
               (let ((res (move (make-pos row col) dir)))
                 (unless (equalp res ref-res)
                   (format t "Wrong move ~A: from ~A, res = ~A, reference = ~A~%"
                           dir
                           (make-pos row col)
                           res
                           ref-res)
                   (incf failed)))))

      (%test 0 2 :east (make-pos 0 3))
      (%test 0 8 :east (make-pos 0 9))
      (%test 5 8 :east (make-pos 5 9))
      (%test 19 8 :east (make-pos 19 9))
      (%test 5 9 :east :invalid)
      (%test 3 9 :east :invalid)
      (%test 19 9 :east :invalid)

      (%test 0 1 :west (make-pos 0 0))
      (%test 0 9 :west (make-pos 0 8))
      (%test 5 1 :west (make-pos 5 0))
      (%test 19 1 :west (make-pos 19 0))
      (%test 5 0 :west :invalid)
      (%test 3 0 :west :invalid)
      (%test 19 0 :west :invalid)

      (%test 0 2 :south-east (make-pos 1 2))
      (%test 0 2 :south-west (make-pos 1 1))

      (%test 0 0 :south-east (make-pos 1 0))
      (%test 0 0 :south-west :invalid)

      (%test 0 9 :south-east (make-pos 1 9))
      (%test 0 9 :south-west (make-pos 1 8))

      (%test 1 2 :south-east (make-pos 2 3))
      (%test 1 2 :south-west (make-pos 2 2))

      (%test 1 0 :south-east (make-pos 2 1))
      (%test 1 0 :south-west (make-pos 2 0))

      (%test 1 9 :south-east :invalid)
      (%test 1 9 :south-west (make-pos 2 9))

      (%test 19 0 :south-east :invalid)
      (%test 19 0 :south-west :invalid)
      (%test 19 5 :south-east :invalid)
      (%test 19 5 :south-west :invalid)
      (%test 19 9 :south-east :invalid)
      (%test 19 9 :south-west :invalid)

      (format t "Tests failed: ~A~%" failed))))
