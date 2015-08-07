(in-package :beesy-beaver)

(defstruct (game-state (:conc-name gs-))
  field
  score
  pivot
  unit-cells
  unit-generator
  cleared-prev)

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

(defun compute-score (cleared cleared-prev unit-size)
  (let* ((points (+ unit-size (* (truncate 100 2) (1+ cleared) cleared)))
         (line-bonus (if (> cleared-prev 1)
                         (floor (* (1- cleared-prev) points)
                                10)
                         0)))
    (+ points line-bonus)))

(defun next-state (cur-state command)
  (with-slots (field score pivot unit-cells unit-generator cleared-prev)
      cur-state
    (multiple-value-bind (new-cells new-pivot)
        (update-command-cells field pivot unit-cells command)
      (if (eq new-cells :locked)
          (multiple-value-bind (new-field removed-rows)
              (lock-cells field unit-cells)
            (make-game-state :field new-field
                             :score (+ score (compute-score removed-rows cleared-prev (length unit-cells)))
                             ;; TODO: generate new unit
                             :pivot new-pivot
                             :unit-cells new-cells
                             :unit-generator unit-generator
                             :cleared-prev removed-rows))
          (make-game-state :field field
                           :score score
                           :pivot new-pivot
                           :unit-cells new-cells
                           :unit-generator unit-generator
                           :cleared-prev cleared-prev)))))

(defun initial-state (task seed-index)
  (make-game-state :field (task-field task)
                   :score 0
                   ;; TODO: start generating units
                   :pivot (make-pos 0 0)
                   :unit-cells (list (make-pos 0 0) (make-pos 0 1))
                   :unit-generator nil
                   :cleared-prev 0
                   ))
