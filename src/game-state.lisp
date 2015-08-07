(in-package :beesy-beaver)

(defstruct (game-state (:conc-name gs-))
  field
  score
  pivot
  unit-cells
  unit-generator
  cleared-prev
  terminal?)

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
                  (rotate pivot cell clockwise))
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

(defun make-unit-generator (task seed-index)
  (let ((units (make-array (length (task-units task))
                           :initial-contents (task-units task)))
        (rnumbers (lcgen (nth seed-index (task-source-seeds task))
                         (task-source-length task))))
    (lambda ()
      (let* ((num (pop rnumbers))
            (unit (and num (aref units (mod num (array-dimension units 0))))))
        (when unit
          (unit-start-position (unit-pivot unit) (unit-members unit)))))))

(defun next-state (cur-state command)
  (with-slots (field score pivot unit-cells unit-generator cleared-prev)
      cur-state
    (multiple-value-bind (new-pivot new-cells)
        (update-command-cells field pivot unit-cells command)
      (let ((new-state
             (if (eq new-pivot :locked)
                 (multiple-value-bind (new-field removed-rows)
                     (lock-cells field unit-cells)
                   (multiple-value-bind (pivot units) (funcall unit-generator)
                     (if (and pivot
                              (check-cells new-field units))
                         (make-game-state :field new-field
                                          :score (+ score (compute-score removed-rows cleared-prev (length unit-cells)))
                                          :pivot pivot
                                          :unit-cells units
                                          :unit-generator unit-generator
                                          :cleared-prev removed-rows
                                          :terminal? nil)
                       (make-game-state :field new-field
                                        :score (+ score (compute-score removed-rows cleared-prev (length unit-cells)))
                                        :pivot nil
                                        :unit-cells nil
                                        :unit-generator unit-generator
                                        :cleared-prev removed-rows
                                        :terminal? t))))
               (make-game-state :field field
                                :score score
                                :pivot new-pivot
                                :unit-cells new-cells
                                :unit-generator unit-generator
                                :cleared-prev cleared-prev
                                :terminal? nil))))
        (values new-state (gs-terminal? new-state))))))

(defun initial-state (task seed-index)
  (let ((gen (make-unit-generator task seed-index)))
    (multiple-value-bind (pivot units) (funcall gen)
      (let ((state (make-game-state :field (task-field task)
                                    :score 0
                                    :pivot pivot
                                    :unit-cells units
                                    :unit-generator gen
                                    :cleared-prev 0
                                    :terminal? nil
                                    )))
        (if (and pivot
                 (check-cells (task-field task) units))
            (values
             state
             nil)
            (values
             state
             t))))))
