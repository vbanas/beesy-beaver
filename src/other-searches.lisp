(in-package :beesy-beaver)

(defun wave-state-id (state)
  (with-slots (pivot unit-cells unit-generator) state
    (let* ((cpivot (pos-to-cube pivot))
           (cunit (pos-to-cube (car unit-cells)))
           (crot (cube-pos-sub cunit cpivot)))
      (list (pos-row pivot) (pos-col pivot)
            (cube-pos-x crot) (cube-pos-y crot) (cube-pos-z crot)
            (unit-generator-units-left unit-generator)))))

(defun one-unit-wave (state)
  (let ((visited (make-hash-table :test #'equal))
        (front (list (cons state nil)))
        (best-state state)
        (best-state-est)
        (best-path nil))
    (labels ((%estimate (state old-pivot)
               (* (1+ (pos-row old-pivot)) (gs-score state)))
             (%add-and-visit (state-data)
               (destructuring-bind (state locked path pivot-before-move) state-data
                 ;; (format t "Considering ~A (est ~A) (key ~A) (visited ~A)~%" (reverse path) (%estimate state pivot-before-move)
                 ;;         (wave-state-id state) (gethash (wave-state-id state) visited))
                 (let ((id (wave-state-id state)))
                   (if (and (not locked)
                            (gethash id visited))
                       nil
                       (progn
                         (unless locked
                           (setf (gethash id visited)
                                 t))
                         (if (or locked
                                 (gs-terminal? state))
                             (let ((est (%estimate state pivot-before-move)))
                               (when (> est
                                        best-state-est)
                                 (setf best-state state)
                                 (setf best-path path)
                                 (setf best-state-est est))
                               nil)
                             (list (cons state path))))))))
             (%one-cell (state path)
               (let* ((moves (allowed-commands state))
                      (new-states (mapcar (lambda (m)
                                            (let ((*was-locked* nil))
                                              (list (next-state state m)
                                                    *was-locked*
                                                    (cons m path)
                                                    (gs-pivot state))))
                                          moves)))
                 (mapcan #'%add-and-visit new-states)))
             (%run-wave ()
               (setf front
                     (mapcan (lambda (el)
                               (%one-cell (car el) (cdr el)))
                             front))
               (when front
                 (%run-wave))))
      (setf best-state-est (%estimate state (gs-pivot state)))
      (%run-wave)
      (values best-state (reverse best-path)))))

(defun wave-one-by-one (state)
  (let ((path nil))
    (loop while (not (gs-terminal? state))
       do (multiple-value-bind (new-state new-path) (one-unit-wave state)
            (setf state new-state)
            (setf path (append (reverse new-path) path))))
    (values state (reverse path))))

(defun simple-encode-command (command)
  (case command
    (:west (elt "p'!.03" 0))
    (:east (elt "bcefy2" 0))
    (:south-west (elt "aghij4" 0))
    (:south-east (elt "lmno 5" 0))
    (:clockwise (elt "dqrvz1" 0))
    (:counter-clockwise (elt "kstuwx" 0))))

(defun simple-encode-solution (path)
  (coerce (mapcar #'simple-encode-command path)
          'string))

(defun simple-wave-from-task-one-seed (task seed-id)
  (multiple-value-bind (state path) (wave-one-by-one (initial-state task seed-id))
    (declare (ignore state))
    (let ((res (make-instance 'play-result
                              :seed (nth seed-id (task-source-seeds task))
                              :problemId (task-id task)
                              :solution (simple-encode-solution path))))
      res)))

(defun simple-wave-from-task-one (task)
  (loop for seed-id below (task-source-seeds task)
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
                path)))))
