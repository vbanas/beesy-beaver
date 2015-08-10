(in-package :beesy-beaver)

(declaim (optimize (debug 3)))

(defparameter *magic-words-cst* nil)


(defun wave-state-id (state)
  (with-slots (pivot unit-cells unit-generator) state
    (let* ((crot (mapcar (lambda (cell)
                           (list (pos-row cell)
                                 (pos-col cell)))
                         unit-cells)))
      (list (pos-row pivot) (pos-col pivot)
            crot
            (unit-generator-units-left unit-generator)))))

(defparameter *vert-coef* 40)
(defparameter *horiz-coef* 10)
(defparameter *hole-coef* 10)
(defparameter *lines-coef* 15)
(defparameter *min-row-coef* 1)

(defun raw-field-estimates (field lines-removed)
  (let ((vert-compactness 0)
        (min-row (1- *height*))
        (horiz-nums (make-array *width* :initial-element 0))
        (num-holes 0)
        (sum-heights 0)
        (filled 0)
        (avg-height)
        (horiz-planarity 0)
        (almost-filled-rows 0))
    (loop for row below *height*
       do (loop for col below *width*
             do
               (when (= (1+ (row-filled field row))
                        *width*)
                 (incf almost-filled-rows))
               (let* ((val (get-cell field (make-pos row col)))
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
    (list min-row almost-filled-rows almost-filled-rows lines-removed lines-removed vert-compactness avg-height horiz-nums filled num-holes num-holes horiz-planarity)))

(defun compute-estimate (raw-ests)
  (destructuring-bind (min-row almost-filled-rows old-almost-filled-rows lines-removed old-lines-removed
                               vert-compactness avg-height horiz-nums filled num-holes old-num-holes horiz-planarity)
      raw-ests
    (let* ((vc-est (if (zerop filled)
                       1
                       (/ vert-compactness filled)))
           (hp-est (/ (- (* *width* *height*) horiz-planarity)
                      (* *width* *height*)))
           (nh-est (expt 2.0 (- old-num-holes num-holes)))
           (ln-est (* (+ old-lines-removed 1)
                      ;; (+ (- almost-filled-rows old-almost-filled-rows) 1)
                      lines-removed))
           (min-row-est (/ min-row (1- *height*)))
           (total-est (+ (* *vert-coef* vc-est)
                         (* *horiz-coef* hp-est)
                         (* *hole-coef* nh-est)
                         (* *lines-coef* ln-est)
                         (* *min-row-coef* min-row-est))))
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
  (destructuring-bind (min-row almost-filled-rows old-almost-filled-rows lines-removed-1 old-lines-removed vert-compactness avg-height horiz-nums filled num-holes old-num-holes horiz-planarity)
      raw-est
    (destructuring-bind (filled-rows cells) lock-delta
      (let ((cur-filled filled-rows)
            (delta-row (length filled-rows)))
        (loop for cell in (sort (copy-list cells) #'< :key #'pos-row) do
             (let ((cell (copy-pos cell)))
               (loop while (and cur-filled
                                (> (pos-row cell)
                                   (car cur-filled)))
                  do
                    (decf delta-row)
                    (pop cur-filled))
               (unless (and cur-filled
                            (= (pos-row cell)
                               (car cur-filled)))
                 (incf (pos-row cell)
                       delta-row)
                 (when (= (1+ (row-filled field (pos-row cell)))
                          *width*)
                   (incf almost-filled-rows))
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
                     (incf horiz-planarity))))))
      ;; (when (/= num-holes old-num-holes)
      ;;   (let ((graph (field-to-cl-graph field)))
      ;;     (setf num-holes (1- (cl-graph:connected-component-count graph)))
      ;;     ;; (when (> num-holes 1)
      ;;     ;;   (print-field field)
      ;;     ;;   (cl-graph:graph->dot graph #P"graph.dot"
      ;;     ;;                        :edge-labeler (lambda (e s) (declare (ignore e s)))
      ;;     ;;                        :vertex-labeler (lambda (v s) (format s "(~A, ~A)"
      ;;     ;;                                                              (pos-row (cl-graph:element v))
      ;;     ;;                                                              (pos-col (cl-graph:element v)))))
      ;;     ;;   (format t "Holes: ~A~%"
      ;;     ;;           num-holes)
      ;;     ;;   (read-line))
      ;;     ))
      (list min-row almost-filled-rows old-almost-filled-rows lines-removed old-lines-removed vert-compactness avg-height horiz-nums filled num-holes old-num-holes horiz-planarity))))

(defvar *current-solutions*)
(defvar *solutions-limit*)

(defun make-solutions-box ()
  (make-instance 'cl-heap:fibonacci-heap :key #'car)
  ;;(list 0 nil nil)
  )

(defvar *solutions-by-pos*)

(defun add-solution-by-pos (pos state estimate path)
  (let ((old-res (gethash pos *solutions-by-pos*)))
    (when (or (null old-res)
              (> estimate (first old-res)))
      (setf (gethash pos *solutions-by-pos*)
            (list estimate state path)))))

(defun flush-solutions ()
  (maphash (lambda (pos val)
             (declare (ignore pos))
             (destructuring-bind (estimate state path) val
               (add-solution state estimate path)))
           *solutions-by-pos*))

(defun add-solution (state estimate path)
  (cl-heap:add-to-heap *current-solutions* (list estimate state path))
  (when (> (cl-heap:heap-size *current-solutions*) *solutions-limit*)
    (cl-heap:pop-heap *current-solutions*))
  ;; (when (> estimate (first *current-solutions*))
  ;;   (setf *current-solutions* (list estimate state path)))
  )

(defun found-solutions ()
  (let ((lst nil))
    (loop while (not (cl-heap:is-empty-heap-p *current-solutions*))
       do (push (cl-heap:pop-heap *current-solutions*)
                lst))
    lst)
  ;; (list *current-solutions*)
  )

(defun one-unit-wave (state base-path)
  ;; front is list of wave-states
  ;; wave-state is list of state path locked-cnt locked-list matching-words
  ;; matching-words is list of states to match magic words (see magic-words.lisp)
  (let ((visited (make-hash-table :test #'equalp))
        (front (list (list state base-path)))
        (magic-words-front nil)
        (*solutions-by-pos* (make-hash-table :test #'equalp))
        (base-ests (raw-field-estimates (gs-field state) (gs-cleared-prev state))))
    (labels ((%estimate (lock-delta state old-pivot)
               (declare (ignore old-pivot))
               (* ;;(1+ (pos-row old-pivot))
                (compute-estimate (update-estimate
                                   base-ests lock-delta
                                   (gs-field state) (gs-cleared-prev state)))
                (+ (gs-score state)
                   (compute-magic-words-bonus state))
                ))
             (%add-and-visit (state-data)
               (destructuring-bind (state path was-locked pivot-before-move) state-data
                 ;; (format t "Considering ~A (est A) (key ~A) (visited ~A)~%" (reverse path) #|(%estimate state pivot-before-move)|#
                 ;;         (wave-state-id state) (gethash (cons locked-list (wave-state-id state)) visited))
                 (let* ((finished (or was-locked
                                      (gs-terminal? state)))
                        (id (wave-state-id state)
                          ;; (cons (gs-matchers state)
                          ;;       (wave-state-id state))
                          ))
                   (if (and (not finished)
                            (gethash id visited))
                       nil
                       (progn
                         (unless finished
                           (setf (gethash id visited)
                                 t))
                         (if finished
                             (let ((est (%estimate *lock-delta* state pivot-before-move)))
                               (add-solution-by-pos pivot-before-move state est path)
                               nil)
                             (list (list state path))))))))
             (%one-cell (state path)
               (let* ((moves (allowed-commands state))
                      (new-states (mapcar (lambda (m)
                                            (let ((*was-locked* nil))
                                              (list (next-state state m)
                                                    (cons m path)
                                                    *was-locked*
                                                    (gs-pivot state))))
                                          moves)))
                 (mapcan #'%add-and-visit new-states)))
             (%run-wave ()
               (let ((front-to-process (or magic-words-front front)))
                 (if magic-words-front
                     (setf magic-words-front nil)
                     (setf front nil))
                 (dolist (front-state front-to-process)
                   (destructuring-bind (state path) front-state
                     (let ((new-front-states (%one-cell state path)))
                       (dolist (new-front-state new-front-states)
                         ;; check matching-words
                         (if nil ;;(gs-matchers (first new-front-state))
                             (push new-front-state magic-words-front)
                             (push new-front-state front)))))))
               (when (or front magic-words-front)
                 (%run-wave))))
      ;;(setf best-state-est (%estimate state (gs-pivot state)))
      (%run-wave)
      (flush-solutions)
      ;;(values best-state (reverse best-path))
      )))

(defun wave-one-by-one (state)
  (let ((best-score 0)
        (best-path nil)
        (best-state state)
        (states-to-try (list (list 0 state nil))))
    (labels ((%best (new-state new-path)
               (when (>= (gs-score new-state)
                         best-score)
                 (setf best-score (gs-score new-state))
                 (setf best-path (reverse new-path))
                 (setf best-state new-state))))
      (loop while states-to-try
         for unit-num from 0
         do (let ((new-states nil)
                  (*current-solutions* (make-solutions-box))
                  (*solutions-limit* 3))
              ;; (format *error-output* "Processing unit #~A~%" unit-num)
              (start-time-delta-measurement)
              (loop for (est state path) in states-to-try
                 do
                 ;; (format t "Est = ~A, Path = ~A~%" est path)
                   (one-unit-wave state path))
              (stop-time-delta-measurement)
              (loop for (new-est new-state new-path) in (found-solutions)
                 do (if (gs-terminal? new-state)
                        (%best new-state new-path)
                        (push (list new-est new-state new-path)
                              new-states)))
              (setf states-to-try new-states)
              (when (time-expired?)
                (loop for (est state path) in states-to-try
                   do
                     (%best state path))
                (setf states-to-try nil)))))
    (values best-state best-path)))

(defparameter *magic-words* nil)

(defun simple-encode-solution (path)
  (encode-commands-with-magic-words *magic-words* path))

(defun simple-wave-from-task-one-seed (task seed-id)
  (let ((init-state (initial-state task seed-id)))
    (multiple-value-bind (state path) (wave-one-by-one init-state)
      ;;(declare (ignore state))
      ;; Add power phrases to the path
      (time
       (setf path (detect-and-replace-power-seqs init-state path)))
      (setf state (run-state-path state path))
      (let ((res (make-instance 'play-result
                                :seed (nth seed-id (task-source-seeds task))
                                :problemId (task-id task)
                                :tag (format nil "~A_SCORE_~A" (task-id task) (+ (compute-magic-words-bonus state)
                                                                                 (gs-score state)))
                                :solution (simple-encode-solution path))))
        res))))

(defun simple-wave-from-task (task)
  (loop for seed-id below (length (task-source-seeds task))
     collect
       (progn (format *error-output* "Seed #~A~%" seed-id)
              (prog1
                  (simple-wave-from-task-one-seed task seed-id)
                (when (time-expired?)
                  (setf seed-id (1+ (length (task-source-seeds task)))))))))

(defun run-state-path (state path)
  (reduce #'next-state path
          :initial-value state))

(defun simple-wave-from-task-json (task-file seed-id &optional tag)
  (let* ((*magic-words-cst* (make-command-seq-matching-tree '("r'lyeh" "ei!" "ia! ia!" "yuggoth")))
         (*magic-words* '("r'lyeh" "ei!" "ia! ia!" "yuggoth"))
         (task (decode-task (alexandria:read-file-into-string task-file)))
         (init-state (initial-state task seed-id)))
    (multiple-value-bind (state path) (wave-one-by-one init-state)
      (setf path (detect-and-replace-power-seqs
                  (generate-seq-automata
                   (mapcar #'map-word-to-commands *magic-words*))
                  init-state path))
      (setf state (run-state-path state path))
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

