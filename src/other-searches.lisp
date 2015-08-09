(in-package :beesy-beaver)

(defun wave-state-id (state)
  (with-slots (pivot unit-cells unit-generator) state
    (let* ((crot (mapcar (lambda (cell)
                           (list (pos-row cell)
                                 (pos-col cell)))
                         unit-cells)))
      (list (pos-row pivot) (pos-col pivot)
            crot
            (unit-generator-units-left unit-generator)))))

(defparameter *vert-coef* 70)
(defparameter *horiz-coef* 4)
(defparameter *hole-coef* 5)

(defun raw-field-estimates (field lines-removed)
  (let ((vert-compactness 0)
        (min-row (1- *height*))
        (horiz-nums (make-array *width* :initial-element 0))
        (num-holes 0)
        (sum-heights 0)
        (filled 0)
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
                    (incf filled)
                    (when (< row min-row)
                      (setf min-row row))
                    (incf (aref horiz-nums col))))))
    (loop for val across horiz-nums
       do (incf sum-heights val))
    (setf avg-height (floor sum-heights *width*))
    ;; TODO: Squares here
    (loop for val across horiz-nums
       do (incf horiz-planarity (abs (- val avg-height))))
    (list min-row lines-removed vert-compactness avg-height horiz-nums filled num-holes horiz-planarity)))

(defun compute-estimate (raw-ests)
  (destructuring-bind (min-row lines-removed vert-compactness avg-height horiz-nums filled num-holes horiz-planarity)
      raw-ests
    (let* ((vc-est (if (zerop filled)
                       1
                       (/ vert-compactness filled)))
           (hp-est (- (* *width* *height*) horiz-planarity))
           (nh-est (- (* *width* *height*) num-holes))
           (total-est (* (+ (* *vert-coef* vc-est)
                            (* *horiz-coef* hp-est)
                            (* *hole-coef* nh-est))
                         (+ lines-removed 1)
                         min-row)))
      ;; (format t "~A~%" field)
      ;; (format t "Estimated: vert comp: ~A, horiz-planarity: ~A, num-holes: ~A~%"
      ;;         vc-est hp-est nh-est)
      ;; (format t "Total: ~A~%"
      ;;         total-est)
      ;; (read-line)
      ;; TODO: Coefs here
      total-est)))

(defun estimate-field (field lines-removed)
  (compute-estimate (raw-field-estimates field lines-removed)))

(defun update-estimate (raw-est lock-delta field lines-removed)
  (destructuring-bind (min-row lines-removed-1 vert-compactness avg-height horiz-nums filled num-holes horiz-planarity)
      raw-est
    (destructuring-bind (filled-rows cells) lock-delta
      (setf lines-removed-1 (* lines-removed-1 lines-removed))
      (loop for cell in cells do
           (when (< (pos-row cell)
                    min-row)
             (setf min-row (pos-row cell)))
           (incf filled)
           (incf vert-compactness (pos-row cell))
           (let ((sw (validate-pos (move cell :south-west)))
                 (se (validate-pos (move cell :south-west))))
             (when (or (and (not (eq sw :invalid))
                            (zerop (get-cell field sw)))
                       (and (not (eq se :invalid))
                            (zerop (get-cell field se))))
               (incf num-holes)))
           (if (>= (aref horiz-nums (pos-col cell))
                   avg-height)
               (decf horiz-planarity)
               (incf horiz-planarity)))
      (list min-row lines-removed-1 vert-compactness avg-height horiz-nums filled num-holes horiz-planarity))))

(defun one-unit-wave (state)
  (let ((visited (make-hash-table :test #'equal))
        (front (list (list state nil 0 nil)))
        (best-state state)
        (best-state-est -99999999)
        (best-path nil)
        (base-ests (raw-field-estimates (gs-field state) (gs-cleared-prev state))))
    (labels ((%estimate (lock-delta state old-pivot)
               (declare (ignore old-pivot))
               (* ;;(1+ (pos-row old-pivot)) 
                (compute-estimate (update-estimate
                                   base-ests lock-delta
                                   (gs-field state) (gs-cleared-prev state)))
                ;; (if (gs-cleared-prev state)
                ;;     5000
                ;;     0)
                (gs-score state)))
             (%add-and-visit (state-data)
               (destructuring-bind (state locked-cnt locked-list path pivot-before-move) state-data
                 ;; (format t "Considering ~A (est A) (key ~A) (visited ~A)~%" (reverse path) #|(%estimate state pivot-before-move)|#
                 ;;         (wave-state-id state) (gethash (cons locked-list (wave-state-id state)) visited))
                 (let* ((finished (or (>= locked-cnt 1)
                                      (gs-terminal? state)))
                        (id (wave-state-id state)))
                   (if (and (not finished)
                            (gethash id visited))
                       nil
                       (progn
                         (unless finished
                           (setf (gethash id visited)
                                 t))
                         (if finished
                             (let ((est (%estimate *lock-delta* state pivot-before-move)))
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
         for num-unit from 0
       do
       ;; (format t "New wave~%")
         (multiple-value-bind (new-state new-path) (one-unit-wave state)
           (if (and (null new-path)
                    (not (gs-terminal? state)))
               (progn
                 (format t "Ooops in wave, no moves found~%")
                 (setf state (terminate-state new-state)))
               (progn
                 ;; (format t "Chose path ~A, unit ~A~%" new-path num-unit)
                 (setf state new-state)
                 (setf path (append (reverse new-path) path))))))
    (values state (reverse path))))

(defun simple-encode-solution (path)
  (encode-commands-with-magic-words *magic-words* path))

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
                (estimate-field (gs-field state) (gs-cleared-prev state))
                path)))))

(defun try-coefs (task-file seed-id)
  (let ((all nil))
    (loop for vertc = 70 then (+ vertc 10) while (< vertc 100) do
         (loop for horizc = 4 then (+ horizc 10) while (< horizc 50) do
              (loop for holesc = 5 then (+ holesc 10) while (< holesc 50) do
                   (format t "Running with ~A, ~A, ~A~%"
                           vertc horizc holesc)
                   (let ((*vert-coef* vertc)
                         (*horiz-coef* horizc)
                         (*hole-coef* holesc))
                     (multiple-value-bind (json state)
                         (simple-wave-from-task-json task-file seed-id)
                         (declare (ignore json))
                       (let ((score (gs-score state)))
                         (format t "Score = ~A~%" score)
                         (push (list score vertc horizc holesc)
                               all)))))))
    (format t "Result: ~%")
    (loop for (score vertc horizc holesc) in (sort all #'> :key #'car)
         do (format t "~A : ~A, ~A, ~A~%"
                    score vertc horizc holesc))))
