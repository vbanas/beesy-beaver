
(in-package :visualizator-backend)

(declaim (optimize (debug 3)))

(defvar *current-game-state*)

(push (create-static-file-dispatcher-and-handler
       "/index.html" "src/visualizator/frontend/index.html")
      *dispatch-table*)

(push (create-static-file-dispatcher-and-handler
       "/honeycomb.js" "src/visualizator/frontend/honeycomb.js")
      *dispatch-table*)


(define-easy-handler (get-map :uri "/get-map") (pos)
  (declare (ignore pos))
  (setf (content-type*) "application/json")
  (let ((result (make-string-output-stream)))
    (yason:encode-alist
     (get-current-map)
     ;; (case pos
     ;;   (0 (get-current-map))
     ;;   (1 (get-next-map))
     ;;   (-1 (get-prev-map)))
     result)
    (get-output-stream-string result)))

(define-easy-handler (send-command :uri "/send-command") (command)
  (setf (content-type*) "application/json")
  (setf *current-game-state*
        (beesy-beaver::next-state *current-game-state* (intern (string-upcase command) :keyword)))
  (let ((result (make-string-output-stream)))
    (yason:encode-alist
     (get-current-map)
     ;; (case pos
     ;;   (0 (get-current-map))
     ;;   (1 (get-next-map))
     ;;   (-1 (get-prev-map)))
     result)
    (get-output-stream-string result)))

(defun get-current-map ()
  (let ((units (beesy-beaver::gs-unit-cells *current-game-state*))
        (field (beesy-beaver::gs-field *current-game-state*))
        (pivot (beesy-beaver::gs-pivot *current-game-state*)))
    (loop for cell in units do
         (setf field (beesy-beaver::put-cell field cell 1)))
    (list (cons :columns beesy-beaver::*width*)
          (cons :rows beesy-beaver::*height*)
          (cons :points
                (loop for row below beesy-beaver::*width* append
                     (loop for column below beesy-beaver::*height* collect
                          (beesy-beaver::get-cell field (beesy-beaver::make-pos row column)))))
          (cons :pivot (list (cons :row (beesy-beaver::pos-row pivot))
                             (cons :col (beesy-beaver::pos-col pivot))))
          (cons :score (beesy-beaver::gs-score *current-game-state*)))))


(defun get-next-map ())


(defun get-prev-map ())
