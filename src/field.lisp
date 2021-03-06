(in-package :beesy-beaver)

(defvar *width*)
(defvar *height*)

;; Pos

(defstruct (pos (:constructor mk-pos))
  (row nil :type fixnum)
  (col nil :type fixnum))

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

(defun try-get-cell (field pos &optional default)
  (let ((p1 (validate-pos pos)))
    (if (eq p1 :invalid)
        default
        (get-cell field pos))))

(defun row-filled (field row)
  (car (fset:@ field row)))

(defvar *filled-rows* nil)

(defun put-cell (field pos val)
  (let* ((old-row (fset:@ field (pos-row pos)))
         (old-val (fset:@ (cdr old-row) (pos-col pos)))
         (delta (- (logand val 1)
                   (logand old-val 1)))
         (new-fill (+ (car old-row)
                        delta)))
    (when (= new-fill *width*)
      (pushnew (pos-row pos) *filled-rows*))
    (fset:with field (pos-row pos)
               (cons new-fill
                     (fset:with (cdr old-row) (pos-col pos) val)))))

(defun remove-row (field row)
  (fset:with-first (fset:less field row)
    (generate-row)))

(defstruct cube-pos
  (x nil :type fixnum)
  (y nil :type fixnum)
  (z nil :type fixnum))

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
        (:north-west (decf new-row)
                     (when (evenp row)
                       (decf new-col)))
        (:north-east (decf new-row)
                     (when (oddp row)
                       (incf new-col)))
        (otherwise (return-from move :invalid)))
      (make-pos new-row new-col))))

(defun rotate (pivot cell clockwise)
  (let* ((cpivot (pos-to-cube pivot))
         (ccell (pos-to-cube cell))
         (rotated (cube-rotate (cube-pos-sub ccell cpivot)
                               clockwise)))
    (cube-to-pos (cube-pos-add cpivot rotated))))
 
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
               (let ((res (validate-pos (move (make-pos row col) dir))))
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


(defun field-to-cl-graph (field)
  (let ((graph (cl-graph:make-graph 'cl-graph:graph-container :vertex-test #'equalp)))
    (loop for row below *height*
       do (loop for col below *width*
             do (let* ((val (get-cell field (make-pos row col))))
                  (when (zerop val)
                    (cl-graph:add-vertex graph (make-pos row col))
                    (loop for cmd in '(:east :west :south-east :south-west :north-east :north-west)
                       do (let* ((new-pos (move (make-pos row col) cmd))
                                 (val2 (try-get-cell field new-pos 1)))
                            (when (and (zerop val2))
                              (cl-graph:add-edge-between-vertexes
                               graph
                               (make-pos row col)
                               new-pos
                               :edge-type :undirected
                               :if-duplicate-do :ignore))))))))
    graph))

(defun print-field (field)
  (let ((row-ind 0))
    (fset:do-seq (row field)
      (format t "~A~A~%"
              (if (oddp row-ind)
                  " "
                  "")
              (cdr row))
      (incf row-ind))))

(defun generate-seq-mutations (seq)
  (declare (optimize (debug 3)))
  (let* ((max-len (length seq))
         (start-pos (make-pos 0 (1+ max-len)))
         (finish-pos (reduce (lambda (pos move)
                               (if (or (eq move :clockwise)
                                       (eq move :counter-clockwise))
                                   pos
                                   (let ((new-pos (move pos move)))
                                     (assert (not (eq new-pos :invalid)))
                                     new-pos)))
                             seq
                             :initial-value start-pos))
         (queue (list (list start-pos 0 nil)))
         (result nil))
    (loop while queue do
         (let ((state (pop queue)))
           (destructuring-bind (pos steps path) state
             (dolist (move '(:east :west :south-east :south-west))
               (let ((new-pos (move pos move)))
                 (cond
                   ((equalp new-pos finish-pos)
                    (push (reverse (cons move path)) result))
                   ((and (< steps max-len)
                         (not (eq new-pos :invalid)))
                    (push (list new-pos (1+ steps) (cons move path))
                          queue))))))))
    result))

(defun generate-seq-automata (seqs)
  (let ((mutations (mapcan (lambda (seq)
                             (mapcar (lambda (mutation)
                                       (cons mutation seq))
                                     (generate-seq-mutations seq)))
                           seqs)))
    (make-command-seq-matching-tree-1 mutations)))

(defun detect-and-replace-power-seqs (cst initial-state commands)
  (let ((matchers nil)
        (result nil)
        (state initial-state)
        (commands-so-far 0))
    (dolist (cmd commands)
      (setf matchers (mapcar (lambda (matcher)
                               (cst-next-state cst matcher cmd))
                             (cons +cst-initial+ matchers)))
      (incf commands-so-far)
      (push cmd result)
      (setf state (next-state state cmd))
      (let ((spell (block find-spell
                     (loop for matcher in matchers do
                          (let* ((spells (cst-words cst matcher))
                                 (start-state
                                  (when spells
                                    (reduce #'next-state (subseq commands 0 (- commands-so-far
                                                                               (length (car spells))))
                                            :initial-value initial-state))))
                            (dolist (spell spells)
                              (let* ((simulated-state
                                      (reduce #'next-state spells
                                              :initial-value start-state)))
                                (when (equalp (gs-unit-cells state)
                                              (gs-unit-cells simulated-state))
                                  (return-from find-spell spell))))))
                     nil)))
        (when spell
          (setf matchers nil)
          (setf result (nthcdr (length spell) result))
          (dolist (cmd (reverse spell))
            (push cmd result)))))
    (reverse result)))
