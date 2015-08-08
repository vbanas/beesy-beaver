(in-package :bb)

(declaim (optimize (debug 3)))

(defparameter *commands-encoding*
  '((:west . "p'!.03")
    (:east . "bcefy2")
    (:south-west . "aghij4")
    (:south-east . "lmno 5")
    (:clockwise . "dqrvz1")
    (:counter-clockwise . "kstuwx")))

(defun simple-encode-command (command)
  (elt
   (cdr (assoc command *commands-encoding* :test #'eq))
   0))

(defun encode-commands-with-magic-words (words commands)
  (mapcar (lambda (cmd)
            (if (symbolp cmd)
                (simple-encode-command cmd)
                cmd))
          (insert-magic-words
           (make-command-seq-matching-tree words)
           commands)))

(defparameter *letters-to-commands-map*
  (let ((map (make-hash-table)))
    (loop for (command . letters) in *commands-encoding* do
         (loop for ch across letters do
              (setf (gethash ch map) command)))
    map))

(defun map-word-to-commands (word)
  (let ((seq (loop for ch across word collect
                  (gethash ch *letters-to-commands-map*))))
    (unless (some #'null seq)
      seq)))

(defun map-word-to-commands.test.1 ()
  (assert
   (equalp
    (map-word-to-commands
     "pbaldqt")
    '(:west :east :south-west :south-east :clockwise :clockwise :counter-clockwise))))

(defun map-word-to-commands.test.2 ()
  (loop for (command . letters) in *commands-encoding* do
       (assert
        (equalp
         (map-word-to-commands letters)
         (make-list (length letters) :initial-element command)))))

(defparameter +cst-initial+ 0)

(defstruct (command-seq-tree (:conc-name cst-))
  (transitions (make-hash-table :test #'equalp)
               :type hash-table)
  (state/seq (make-hash-table) :type hash-table))

(defun make-command-seq-matching-tree (words)
  (let ((tree (make-command-seq-tree))
        (states-counter 0))
    (with-slots (transitions state/seq) tree
      (dolist (word words)
        (let ((seq (map-word-to-commands word))
              (state +cst-initial+))
          (dolist (cmd seq)
            (let ((key (cons state cmd)))
              (setf state (or (gethash key transitions)
                              (setf (gethash key transitions)
                                    (incf states-counter))))))
          (push word (gethash state state/seq)))))
    ;; all words in the same state are of the same length
    ;; (maphash
    ;;  (lambda (state words)
    ;;    (setf (gethash state (cst-state/seq tree))
    ;;          (sort words #'> :key #'length)))
    ;;  (cst-state/seq tree))
    tree))

(defun cst-next-state (cst state command)
  (gethash (cons state command)
           (cst-transitions cst)))

(defun cst-words (cst state)
  (gethash state (cst-state/seq cst)))

(defun make-command-seq-matching-tree.test.1 ()
  (let* ((seqs '((:west :east :south-west :south-east :clockwise :counter-clockwise)
                 (:west :east :south-west :south-east :clockwise :east)
                 (:west :east :west :south-east)
                 (:east :west :clockwise :counter-clockwise)))
         (words (mapcar (alexandria:compose
                         (alexandria:rcurry #'coerce 'string)
                         (alexandria:curry #'mapcar #'simple-encode-command))
                        seqs))
         (tree (make-command-seq-matching-tree words)))
    (labels ((%run (seq)
               (let ((state +cst-initial+))
                 (dolist (cmd seq)
                   (setf state (cst-next-state tree state cmd)))
                 state)))
      (loop
         for seq in seqs
         for word in words
         do
           (let ((state (%run seq)))
             (assert (equalp (cst-words tree state) (list word)))))
      )))

(defun cst-show (tree)
  (format t "Transitions:~%")
  (maphash (lambda (pair state)
             (format t "  ~A -> ~A~%" pair state))
           (cst-transitions tree))
  (format t "Words:~%")
  (maphash (lambda (state words)
             (format t "  ~A -> ~A~%" state words))
           (cst-state/seq tree)))

(defun insert-magic-words (cst-tree seq)
  (let ((arr (coerce seq 'vector))
        (matchers nil))
    ;; (cst-show cst-tree)
    (loop for i from 0 to (1- (length arr)) do
         ;; (format t ">> looking at ~A~%" (elt arr i))
         ;; (format t ">> matchers are ~A~%" matchers)
         (setf matchers
               (remove
                nil
                (mapcar (lambda (matcher)
                          (cst-next-state cst-tree matcher (elt arr i)))
                        (cons +cst-initial+ matchers))))
         (let ((magic-word
                (car
                 (some (alexandria:curry #'cst-words cst-tree)
                       matchers))))
           (when magic-word
             ;; (format t ">> replacing at pos ~A, word is '~A' of len ~A, start1 is ~A~%"
             ;;         i magic-word (length magic-word) (1+ (- i (length magic-word))))
             (replace arr (coerce magic-word 'vector)
                      :start1 (1+ (- i (length magic-word))))
             (setf matchers nil))))
    (coerce arr 'list)))

(defun insert-magic-words.test.1 ()
  (assert
   (equalp
    (insert-magic-words
     (make-command-seq-matching-tree '("32" "34"))
     '(:west :east :SOUTH-WEST :west :east :west :SOUTH-WEST))
    '(#\3 #\2 :SOUTH-WEST #\3 #\2 #\3 #\4)))
  (assert
   (equalp
    (insert-magic-words
     (make-command-seq-matching-tree '("32"))
     '(:west :east :SOUTH-WEST :west :east))
    '(#\3 #\2 :SOUTH-WEST #\3 #\2))))

(defun encode-commands-with-magic-words.test.1 ()
  (assert
   (equalp
    (encode-commands-with-magic-words
     '("32" "34")
     '(:west :east :SOUTH-WEST :west :east :west :SOUTH-WEST))
    '(#\3 #\2 #\a #\3 #\2 #\3 #\4)))
  (assert
   (equalp
    (encode-commands-with-magic-words
     '("32")
     '(:west :east :SOUTH-WEST :west :east))
    '(#\3 #\2 #\a #\3 #\2))))
