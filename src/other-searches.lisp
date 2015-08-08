(in-package :beesy-beaver)

(defun wave-state-id (state)
  (with-slots (pivot unit-cells unit-generator) state
    (let* ((cpivot (pos-to-cube pivot))
           (cunit (pos-to-cube (car unit-cells)))
           (crot (cube-pos-sub cunit cpivot)))
      (list (pos-row pivot) (pos-col pivot)
            (cube-pos-x crot) (cube-pos-y crot) (cube-pos-z crot)
            (unit-generator-units-left unit-generator)))))

(defun estimate-field (field)
  (let ((vert-compactness 0)
        (min-row (1- *height*))
        (horiz-nums (make-array *width* :initial-element 0))
        (num-holes 0)
        (sum-heights 0)
        (avg-height)
        (horiz-planarity 0))
    (loop for row below *height*
       do (loop for col below *width*
             do (let* ((val (get-cell field (make-pos row col)))
                       ;; TODO: correct up detection
                       (up (validate-pos (make-pos (1- row) col)))
                       (up-val (if (eq up :invalid)
                                   nil
                                   (get-cell field up))))
                  (when (and up-val
                             (zerop val)
                             (not (zerop up-val)))
                    (incf num-holes))
                  (when (not (zerop val))
                    (incf vert-compactness row)
                    (when (< row min-row)
                      (setf min-row row))
                    (incf (aref horiz-nums col))))))
    (loop for val across horiz-nums
       do (incf sum-heights val))
    (setf avg-height (floor sum-heights *width*))
    ;; TODO: Squares here
    (loop for val across horiz-nums
       do (incf horiz-planarity (abs (- val avg-height))))
    ;; (format t "Estimated ~A ~A ~A~%"
    ;;         min-row horiz-planarity num-holes)
    ;; TODO: Coefs here
    (+ (floor vert-compactness *height*)
       (- (* *width* *height*) horiz-planarity)
       (- (* 10 *width* *height*) (* 10 num-holes)))))

(defun one-unit-wave (state)
  (let ((visited (make-hash-table :test #'equal))
        (front (list (list state nil 0 nil)))
        (best-state state)
        (best-state-est -99999999)
        (best-path nil))
    (labels ((%estimate (state old-pivot)
               ;;(declare (ignore old-pivot))
               (+ (1+ (pos-row old-pivot)) 
                ;;(estimate-field (gs-field state))
                (gs-score state)))
             (%add-and-visit (state-data)
               (destructuring-bind (state locked-cnt locked-list path pivot-before-move) state-data
                 ;; (format t "Considering ~A (est ~A) (key ~A) (visited ~A)~%" (reverse path) (%estimate state pivot-before-move)
                 ;;         (wave-state-id state) (gethash (wave-state-id state) visited))
                 (let* ((finished (or (>= locked-cnt 1)
                                      (gs-terminal? state)))
                        (id (cons locked-list (wave-state-id state))))
                   (if (and (not finished)
                            (gethash id visited))
                       nil
                       (progn
                         (unless finished
                           (setf (gethash id visited)
                                 t))
                         (if finished
                             (let ((est (%estimate state pivot-before-move)))
                               ;; (format t "Found finished ~A (~A > ~A)~%" path est best-state-est)
                               (when (> est
                                        best-state-est)
                                 (setf best-state state)
                                 (setf best-path path)
                                 (setf best-state-est est))
                               nil)
                             (list (list state path locked-cnt locked-list))))))))
             (%one-cell (state path locked-cnt locked-list)
               (let* ((moves (allowed-commands state))
                      (new-states (mapcar (lambda (m)
                                            (let ((*was-locked* nil))
                                              (list (next-state state m)
                                                    (if *was-locked*
                                                        (1+ locked-cnt)
                                                        locked-cnt)
                                                    (if *was-locked*
                                                        (cons (wave-state-id state)
                                                              locked-list)
                                                        locked-list)
                                                    (cons m path)
                                                    (gs-pivot state))))
                                          moves)))
                 (mapcan #'%add-and-visit new-states)))
             (%run-wave ()
               (setf front
                     (mapcan (lambda (el)
                               (%one-cell (first el) (second el) (third el) (fourth el)))
                             front))
               (when front
                 (%run-wave))))
      ;;(setf best-state-est (%estimate state (gs-pivot state)))
      (%run-wave)
      (values best-state (reverse best-path)))))

(defparameter *wave-limit* 1)

(defun wave-one-by-one (state)
  (let ((path nil))
    (loop while (not (gs-terminal? state))
       do
       ;; (format t "New wave~%")
         (multiple-value-bind (new-state new-path) (one-unit-wave state)
           (if (and (null new-path)
                    (not (gs-terminal? state)))
               (progn
                 (format t "Ooops in wave, no moves found~%")
                 (setf state (terminate-state new-state)))
               (progn
                 (setf state new-state)
                 (setf path (append (reverse new-path) path))))))
    (values state (reverse path))))

(defun simple-encode-solution (path)
  (coerce (mapcar #'simple-encode-command path)
          'string))

(defun simple-wave-from-task-one-seed (task seed-id)
  (multiple-value-bind (state path) (wave-one-by-one (initial-state task seed-id))
    ;;(declare (ignore state))
    (let ((res (make-instance 'play-result
                              :seed (nth seed-id (task-source-seeds task))
                              :problemId (task-id task)
			      :tag (format nil "~A_SCORE_~A" (task-id task) (gs-score state))
                              :solution (simple-encode-solution path))))
      res)))

(defun simple-wave-from-task (task)
  (loop for seed-id below (length (task-source-seeds task))
     collect (simple-wave-from-task-one-seed task seed-id)))

(defun simple-wave-from-task-json (task-file seed-id &optional tag)
  (let ((task (decode-task (alexandria:read-file-into-string task-file))))
    (multiple-value-bind (state path) (wave-one-by-one (initial-state task seed-id))
      (let ((res (make-instance 'play-result
                                :tag tag
                                :seed (nth seed-id (task-source-seeds task))
                                :problemId (task-id task)
                                :solution (simple-encode-solution path))))
        (values (with-output-to-string (str)
                  (yason:encode (list res) str))
                state
                (estimate-field (gs-field state))
                path)))))
