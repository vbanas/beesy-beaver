(in-package :beesy-beaver)

(defstruct (game-state (:conc-name gs-))
  field
  score
  pivot
  unit-cells
  unit-generator)

(defun update-move-cells (field cells move new-pos-l)
  (if (null cells)
      new-pos-l
      (let* ((cell (car cells))
             (rest (cdr cells))
             (new-pos (move cell move)))
        )))

(defun next-state (cur-state command)
  (with-slots (field score pivot unit-cells unit-generator)
      cur-state
    ))
