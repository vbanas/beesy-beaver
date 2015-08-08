(in-package :beesy-beaver)

(defstruct (game-state (:conc-name gs-))
  field
  score
  pivot
  unit-cells
  unit-generator
  cleared-prev
  terminal?
  last-horiz-move
  last-rotations
  max-rotations)

(defun update-cells (field func cells new-pos-l)
  (if (null cells)
      (nreverse new-pos-l)
      (let* ((cell (car cells))
             (rest (cdr cells))
             (new-pos (funcall func cell)))
        (if (or (eq new-pos :invalid)
                (not (zerop (get-cell field new-pos))))
            :locked
            (update-cells field func rest (cons new-pos new-pos-l))))))

(defun update-move-cells (field cells move)
  (update-cells field
                (lambda (cell)
                  (validate-pos (move cell move)))
                cells
                nil))

(defun update-rotate-cells (field cells pivot clockwise)
  (update-cells field
                (lambda (cell)
                  (validate-pos (rotate pivot cell clockwise)))
                cells
                nil))

(defun update-command-cells (field pivot cells command)
  (case command
    ((:west :east :south-west :south-east)
     (let ((new-cells (update-move-cells field cells command)))
       (if (eq new-cells :locked)
           :locked
           (values
            (move pivot command)
            new-cells))))
    ((:clockwise :counter-clockwise)
     (let ((new-cells (update-rotate-cells field cells pivot (eq command :clockwise))))
       (if (eq new-cells :locked)
           :locked
           (values
            pivot
            new-cells))))))

(defun updated-last-horiz-move (command last-move)
  (case command
    (:west :west)
    (:east :east)
    ((:south-west :south-east) nil)
    (otherwise last-move)))

(defun updated-last-rotations (command last-rotations)
  (case command
    (:clockwise (1+ last-rotations))
    (:counter-clockwise (1- last-rotations))
    (otherwise 0)))

(defun lock-cells (field cells)
  (let ((*filled-rows* nil)
        (num-removed-rows 0))
    (loop for cell in cells
       do (setf field (put-cell field cell 1)))
    (setf num-removed-rows (length *filled-rows*))
    (loop for row in (sort *filled-rows* #'<)
       do (setf field (remove-row field row)))
    (values field num-removed-rows)))

(defun check-cells (field cells)
  (loop for cell in cells always
       (zerop (get-cell field cell))))

(defun compute-score (cleared cleared-prev unit-size)
  (let* ((points (+ unit-size (* (truncate 100 2) (1+ cleared) cleared)))
         (line-bonus (if (> cleared-prev 1)
                         (floor (* (1- cleared-prev) points)
                                10)
                         0)))
    (+ points line-bonus)))

(defun equal-figures (cells1 cells2)
  (labels ((%less (pos1 pos2)
             (if (< (pos-row pos1)
                    (pos-row pos2))
                 t
                 (if (= (pos-row pos1)
                        (pos-row pos2))
                     (< (pos-col pos1)
                        (pos-col pos2))
                     nil))))
    (equalp (sort (copy-list cells1) #'%less)
            (sort (copy-list cells2) #'%less))))

(defun compute-max-rotations (pivot cells)
  (let ((new-cells cells))
    (loop for rot below 5
         do (setf new-cells
                  (mapcar (lambda (cell)
                            (rotate pivot cell t))
                          new-cells))
         (when (equal-figures cells new-cells)
           (return-from compute-max-rotations rot)))
    5))

(defstruct (unit-generator (:constructor mk-ug))
  rnumbers
  units
  max-rots)

(defun make-unit-generator (task seed-index)
  (let* ((units (make-array (length (task-units task))
                            :initial-contents (task-units task)))
         (unit-max-rots (make-array (length (task-units task))
                                    :initial-contents (mapcar (lambda (unit)
                                                                (compute-max-rotations
                                                                 (unit-pivot unit)
                                                                 (unit-members unit)))
                                                              (task-units task))))
         (rnumbers (lcgen (nth seed-index (task-source-seeds task))
                          (task-source-length task))))
    (mk-ug :rnumbers rnumbers
           :units units
           :max-rots unit-max-rots)))

(defun generate-new-unit (gen)
  (with-slots (rnumbers units max-rots) gen
    (let* ((num (car rnumbers))
           (unit (and num (aref units (mod num (array-dimension units 0)))))
           (max-rot (and num (aref max-rots (mod num (array-dimension units 0))))))
      (when unit
        (multiple-value-bind (pivot members)
            (unit-start-position (unit-pivot unit) (unit-members unit))
          (values pivot members max-rot
                  (mk-ug :rnumbers (cdr rnumbers)
                         :units units
                         :max-rots max-rots)))))))

(defun allowed-commands (state)
  (with-slots (last-horiz-move last-rotations max-rotations) state
    (append
     (case last-horiz-move
       (:west (list :west :south-east :south-west))
       (:east (list :east :south-east :south-west))
       (otherwise (list :east :west :south-east :south-west)))
     (cond ((= max-rotations 0)
            nil)
           ((= last-rotations 0)
            (list :clockwise :counter-clockwise))
           ((> last-rotations 0)
            (if (< last-rotations max-rotations)
                (list :clockwise)
                nil))
           (t
            (if (> last-rotations (- max-rotations))
                (list :counter-clockwise)
                nil))))))

(defun next-state (cur-state command)
  (with-slots (field score pivot unit-cells unit-generator cleared-prev last-rotations last-horiz-move max-rotations)
      cur-state
    (multiple-value-bind (new-pivot new-cells)
        (update-command-cells field pivot unit-cells command)
      (let ((new-state
             (if (eq new-pivot :locked)
                 (multiple-value-bind (new-field removed-rows)
                     (lock-cells field unit-cells)
                   (multiple-value-bind (pivot units new-max-rot new-gen) (generate-new-unit unit-generator)
                     (if (and pivot
                              (check-cells new-field units))
                         (make-game-state :field new-field
                                          :score (+ score (compute-score removed-rows cleared-prev (length unit-cells)))
                                          :pivot pivot
                                          :unit-cells units
                                          :unit-generator new-gen
                                          :cleared-prev removed-rows
                                          :terminal? nil
                                          :last-horiz-move nil
                                          :last-rotations 0
                                          :max-rotations new-max-rot)
                         (make-game-state :field new-field
                                          :score (+ score (compute-score removed-rows cleared-prev (length unit-cells)))
                                          :pivot nil
                                          :unit-cells nil
                                          :unit-generator unit-generator
                                          :cleared-prev removed-rows
                                          :last-horiz-move nil
                                          :last-rotations 0
                                          :max-rotations 0
                                          :terminal? t))))
                 (make-game-state :field field
                                  :score score
                                  :pivot new-pivot
                                  :unit-cells new-cells
                                  :unit-generator unit-generator
                                  :cleared-prev cleared-prev
                                  :last-horiz-move (updated-last-horiz-move command last-horiz-move)
                                  :last-rotations (updated-last-rotations command last-rotations)
                                  :max-rotations max-rotations
                                  :terminal? nil))))
        (values new-state (gs-terminal? new-state))))))

(defun initial-state (task seed-index)
  (let ((gen (make-unit-generator task seed-index)))
    (multiple-value-bind (pivot units max-rot new-gen) (generate-new-unit gen)
      (let ((state (make-game-state :field (task-field task)
                                    :score 0
                                    :pivot pivot
                                    :unit-cells units
                                    :unit-generator new-gen
                                    :cleared-prev 0
                                    :terminal? nil
                                    :last-horiz-move nil
                                    :last-rotations 0
                                    :max-rotations max-rot)))
        (if (and pivot
                 (check-cells (task-field task) units))
            (values
             state
             nil)
            (values
             state
             t))))))

(defun state-identifier (state)
  (with-slots (field pivot unit-cells) state
    (sxhash
     (list
      (fset:reduce #'+ (fset:image #'car field))
      (pos-row pivot)
      (pos-col pivot)
      (mapcar #'pos-row unit-cells)
      (mapcar #'pos-col unit-cells)))))
