(in-package :visualizator-backend)

(declaim (optimize (debug 3)))

(defvar *current-game-state*)


(define-easy-handler (get-map :uri "/get-map") ()
  (setf (content-type*) "application/json")
  (let ((result (make-string-output-stream)))
    (yason:encode-alist
     (get-current-map)
     result)
    (get-output-stream-string result)))


(define-easy-handler (send-command :uri "/send-command") (command)
  (setf (content-type*) "application/json")
  (setf *current-game-state*
        (bb::next-state *current-game-state* (intern (string-upcase command) :keyword)))
  (let ((result (make-string-output-stream)))
    (yason:encode-alist
     (get-current-map)
     result)
    (get-output-stream-string result)))


(define-easy-handler (load-level :uri "/load-level") (file seed words)
  (setf (content-type*) "application/json")
  (let ((seed-int (parse-integer seed)))
    (setf bb::*magic-words* (parse-magic-words words))
    (setf bb::*magic-words-cst*
          (bb::make-command-seq-matching-tree bb::*magic-words* ))
    (setf *current-game-state*
          (bb::initial-state (bb::decode-task
                              (alexandria:read-file-into-string
                               (pathname file)))
                             seed-int))
    (with-output-to-string (str)
      (yason:encode-alist
       (get-current-map)
       str))))


(define-easy-handler (simulate :uri "/simulate") ()
  (setf (content-type*) "application/json")
  (multiple-value-bind (state path) (bb::wave-one-by-one *current-game-state*)
    (setf *current-game-state* state)
    (bb::simple-encode-solution path)))


(defun parse-magic-words (words-str)
  (cl-ppcre:split ";" words-str))


(defun get-current-map ()
  (let ((units (bb::gs-unit-cells *current-game-state*))
        (field (bb::gs-field *current-game-state*))
        (pivot (bb::gs-pivot *current-game-state*)))
    (loop for cell in units do
         (setf field (bb::put-cell field cell 1)))
    (list (cons :columns bb::*width*)
          (cons :rows bb::*height*)
          (cons :points
                (loop for row below bb::*height* append
                     (loop for column below bb::*width* collect
                          (bb::get-cell field (bb::make-pos row column)))))
          (cons :pivot (list (cons :row (bb::pos-row pivot))
                             (cons :col (bb::pos-col pivot))))
          (cons :score (bb::gs-score *current-game-state*))
          (cons :is_terminated (if (bb::gs-terminal? *current-game-state*)
                                   1 0))
          (cons :units_left (bb::unit-generator-units-left
                             (bb::gs-unit-generator *current-game-state*))))))
