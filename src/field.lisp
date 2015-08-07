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
  (cons 0
        (generate-seq *width*
                      (lambda (ind)
                        (declare (ignore ind))
                        0))))

(defun make-field ()
  (generate-seq
   *height*
   (lambda (ind)
     (declare (ignore ind))
     (generate-row))))

(defun get-cell (field pos)
  (fset:@ (cdr (fset:@ field (pos-row pos))) (pos-col pos)))

(defun put-cell (field pos val)
  (let* ((old-row (fset:@ field (pos-row pos)))
         (old-val (fset:@ (cdr old-row) (pos-col pos)))
         (delta (- (logand val 1)
                   (logand old-val 1)))
         (new-fill (+ (car old-row)
                        delta)))
    ;; TODO: do something when new-fill = *width*
    (fset:with field (pos-row pos)
               (cons new-fill
                     (fset:with (cdr old-row) (pos-col pos) val)))))

(defun remove-row (field row)
  (fset:with-first (fset:less field row)
    (generate-row)))

(defstruct cube-pos
  x
  y
  z)

(defun cube-pos-add (pos1 pos2)
  (make-cube-pos
   :x (+ (cube-pos-x pos1) (cube-pos-x pos2))
   :y (+ (cube-pos-y pos1) (cube-pos-y pos2))
   :z (+ (cube-pos-z pos1) (cube-pos-z pos2))))

(defun cube-pos-sub (pos1 pos2)
  (make-cube-pos
   :x (- (cube-pos-x pos1) (cube-pos-x pos2))
   :y (- (cube-pos-y pos1) (cube-pos-y pos2))
   :z (- (cube-pos-z pos1) (cube-pos-z pos2))))

(defun cube-to-pos (cube-pos)
  (with-slots (x y z) cube-pos
    (make-pos z (+ x (truncate (- z (logand z 1)) 2)))))

(defun pos-to-cube (pos)
  (with-slots (row col) pos
    (let* ((xx (- col (truncate (- row (logand row 1)) 2)))
           (zz row)
           (yy (- (+ xx zz))))
      (make-cube-pos :x xx
                     :y yy
                     :z zz))))

(defun cube-rotate (cube-pos clockwise)
  (with-slots (x y z) cube-pos
    (if clockwise
        (make-cube-pos :x (- z) :y (- x) :z (- y))
        (make-cube-pos :x (- y) :y (- z) :z (- x)))))

;; And now for hexagonal operations

(defun validate-pos (pos)
  (with-slots (row col) pos
    (when (or (< row 0)
              (>= row *height*))
      (return-from validate-pos :invalid))
    (when (or (< col 0)
              (>= col *width*))
      (return-from validate-pos :invalid))
    pos))

(defun make-and-check-pos (row col)
  (validate-pos (make-pos row col)))

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
      (make-and-check-pos new-row new-col))))

(defun rotate (pivot cell clockwise)
  (let* ((cpivot (pos-to-cube pivot))
         (ccell (pos-to-cube cell))
         (rotated (cube-rotate (cube-pos-sub ccell cpivot)
                               clockwise)))
    (validate-pos (cube-to-pos (cube-pos-add cpivot rotated)))))
 
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

(defun test-rotate ()
  (let* ((*width* 10)
         (*height* 20)
         (failed 0))
    (labels ((%test (row1 col1 row2 col2 clockwise ref-res)
               (let ((res (rotate (make-pos row1 col1) (make-pos row2 col2) clockwise)))
                 (unless (equalp res ref-res)
                   (format t "Wrong rotation (clockwise: ~A): pivot ~A pos ~A, res = ~A, reference = ~A~%"
                           clockwise
                           (make-pos row1 col1)
                           (make-pos row2 col2)
                           res
                           ref-res)
                   (incf failed)))))

      ;; Odd row
      
      (%test 5 2 4 3 t (make-pos 5 3))
      (%test 5 2 4 3 nil (make-pos 4 2))

      (%test 5 2 4 2 t (make-pos 4 3))
      (%test 5 2 4 2 nil (make-pos 5 1))

      (%test 5 2 5 1 t (make-pos 4 2))
      (%test 5 2 5 1 nil (make-pos 6 2))

      (%test 5 2 6 2 t (make-pos 5 1))
      (%test 5 2 6 2 nil (make-pos 6 3))

      (%test 5 2 6 3 t (make-pos 6 2))
      (%test 5 2 6 3 nil (make-pos 5 3))

      (%test 5 2 5 3 t (make-pos 6 3))
      (%test 5 2 5 3 nil (make-pos 4 3))

      ;; Even row

      (%test 4 2 4 3 t (make-pos 5 2))
      (%test 4 2 4 3 nil (make-pos 3 2))

      (%test 4 2 5 2 t (make-pos 5 1))
      (%test 4 2 5 2 nil (make-pos 4 3))

      (%test 4 2 5 1 t (make-pos 4 1))
      (%test 4 2 5 1 nil (make-pos 5 2))

      (%test 4 2 4 1 t (make-pos 3 1))
      (%test 4 2 4 1 nil (make-pos 5 1))

      (%test 4 2 3 1 t (make-pos 3 2))
      (%test 4 2 3 1 nil (make-pos 4 1))

      (%test 4 2 3 2 t (make-pos 4 3))
      (%test 4 2 3 2 nil (make-pos 3 1))

      ;; Odd row dist 2
      
      (%test 5 2 5 4 t (make-pos 7 3))
      (%test 5 2 5 4 nil (make-pos 3 3))      

      (%test 5 2 7 3 t (make-pos 7 1))
      (%test 5 2 7 3 nil (make-pos 5 4))      

      (%test 5 2 7 1 t (make-pos 5 0))
      (%test 5 2 7 1 nil (make-pos 7 3))      

      (%test 5 2 5 0 t (make-pos 3 1))
      (%test 5 2 5 0 nil (make-pos 7 1))      

      (%test 5 2 3 1 t (make-pos 3 3))
      (%test 5 2 3 1 nil (make-pos 5 0))      

      (%test 5 2 3 3 t (make-pos 5 4))
      (%test 5 2 3 3 nil (make-pos 3 1))
      
      ;; Even row dist 2

      (%test 4 2 5 3 t (make-pos 6 2))
      (%test 4 2 5 3 nil (make-pos 3 3))      

      (%test 4 2 6 2 t (make-pos 5 0))
      (%test 4 2 6 2 nil (make-pos 5 3))      

      (%test 4 2 5 0 t (make-pos 3 0))
      (%test 4 2 5 0 nil (make-pos 6 2))      

      (%test 4 2 3 0 t (make-pos 2 2))
      (%test 4 2 3 0 nil (make-pos 5 0))      

      (%test 4 2 2 2 t (make-pos 3 3))
      (%test 4 2 2 2 nil (make-pos 3 0))      

      (%test 4 2 3 3 t (make-pos 5 3))
      (%test 4 2 3 3 nil (make-pos 2 2))      

      ;; (%test 4 2 5 3)
      ;; (%test 4 2 6 2)
      ;; (%test 4 2 5 0)
      ;; (%test 4 2 3 0)
      ;; (%test 4 2 2 2)
      ;; (%test 4 2 3 3)
      
      (format t "Tests failed: ~A~%" failed))))
