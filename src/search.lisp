(in-package :search)

(declaim (optimize (debug 3) (safety 3)))

(defgeneric get-moves (state))
(defgeneric apply-move (state move))
(defgeneric estimate-reward (state))

(defparameter *node-id* 0)

(defclass node ()
  ((id :reader id
       :initform (incf *node-id*))
   (visits :accessor visits
           :initform 1
           :type integer)
   (reward :accessor reward
           :initform 0
           :initarg :reward
           :type integer)
   (state :accessor state
          :initarg :state)
   (move :accessor move
         :initarg :move
         :initform nil)
   (children :accessor children
             :initarg :children
             :initform nil
             :type list)
   (moves-to-explore :accessor moves-to-explore
                     :initarg :moves-to-explore
                     :initform nil
                     :type list)))

(defparameter *debug* nil)

(defmacro dprint (fmt &rest args)
  `(when *debug*
     (format t ,fmt ,@args)))

(defgeneric show-state (state))
(defgeneric show-move (state move))

(defun explore-state (initial-state iterations)
  (let ((state/node-map (make-hash-table :test #'equalp)))
    (labels
        ((%new-node (new-state move)
           (make-instance
            'node
            :move move
            :state new-state
            :reward (estimate-reward new-state)
            :moves-to-explore (shuffle (get-moves new-state))))
         (%expand (node)
           (let* ((move (pop (moves-to-explore node)))
                  (new-state (apply-move (state node) move))
                  (hash (bb::state-big-hash new-state))
                  (redirect (gethash hash state/node-map)))
             (if redirect
                 (%iterate redirect)
                 (let ((new-node (%new-node new-state move)))
                   (setf (gethash hash state/node-map) new-node)
                   (push new-node (children node))
                   new-node))))
         (%iterate (node)
           (dprint "iterate: node ~A, state:~%~A~%"
                   (id node) (show-state (state node)))
           (when (and *debug* (moves-to-explore node))
             (dprint "iterate: moves to explore ~A~%"
                     (mapcar (curry #'show-move (state node))
                             (moves-to-explore node))))
           (incf (visits node))
           (if (moves-to-explore node)
               (%expand node)
               (when (children node)
                 (%iterate (best-child node))))
           (when (children node)
             (setf
              (reward node)
              (reduce #'max
                      (mapcar #'reward (children node)))))
           (dprint "iterate: node ~A, reward ~A~%"
                   (id node) (reward node))))
      (let ((root-node (make-instance
                        'node
                        :move nil
                        :state initial-state
                        :reward (estimate-reward initial-state)
                        :moves-to-explore (get-moves initial-state))))
        (loop for i from 1 to iterations do
             (dprint "~%Iteration #~A~%~%" i)
             (%iterate root-node))
        (prog1
            root-node
          (dprint "~%Exploration done~%~%"))))))

(defun search-tree-to-dot (node file)
  (let ((queue (list node)))
    (with-open-file (stream
                     file
                     :if-exists :supersede
                     :if-does-not-exist :create
                     :direction :output)
      (format stream "digraph G {~%")
      (loop while queue do
           (let* ((node (pop queue))
                  (children (children node)))
             (setf queue (append children queue))
             (format stream "  ~A [label=\"~A ~A\"]~%" (id node) (reward node)
                     (beesy-beaver::state-identifier (state node)))
             (dolist (ch children)
               (format stream "  ~A -> ~A;~%" (id node) (id ch)))
             ))
      (format stream "}~%"))
    (sb-ext:run-program
     "/usr/bin/dot" (list "-Tsvg" "-O" file)
     :error *error-output*
     :wait t)
    (sb-ext:run-program
     "/usr/bin/google-chrome" (list (format nil "~A.svg" file))
     :wait nil)))

(defun draw-search-tree-for-file (json-file dot-file iterations)
  (search-tree-to-dot
   (explore-state
    (bb::initial-state
     (bb::decode-task
      (alexandria:read-file-into-string json-file)) 0)
    iterations)
   dot-file))

(defparameter *magical-c* 1)

(defun best-child (node &key (reward nil))
  (labels ((%scan (fn key)
             (cdr
              (reduce
               (lambda (rank/node child)
                 (let ((rank (funcall key child)))
                   (dprint "best-child: node ~A, state ~A, rank ~A ~A~%"
                           (id child) (show-state (state child))
                           key rank)
                   (if (or (null rank/node)
                           (funcall fn rank (car rank/node)))
                       (cons rank child)
                       rank/node)))
               (children node)
               :initial-value nil))))
    (if (and (= 0 (random 2)) (null reward))
        (%scan #'< #'visits)
        (%scan #'> #'reward))))

;; (defun rank-node (parent node)
;;   (+ (reward node)
;;      (* *magical-c*
;;         (sqrt
;;          (/ (log (visits parent))
;;             (visits node))))))

;; ----------------------------------------

(defmethod get-moves ((state beesy-beaver::game-state))
  (unless (beesy-beaver::gs-terminal? state)
    (beesy-beaver::allowed-commands
     state)))

(defmethod apply-move ((state beesy-beaver::game-state) move)
  (beesy-beaver::next-state state move))

(defmethod estimate-reward ((state beesy-beaver::game-state))
  (dprint "estimate-reward:~%~A~%" state)
  (let ((st (beesy-beaver::copy-game-state state)))
    (loop
       (when (beesy-beaver::gs-terminal? st)
         (return))
       (let ((move (random-elt (get-moves st))))
         (dprint "estimate-reward: move ~A~%" move)
         (setf st (apply-move st move))))
    (dprint "estimate-reward final:~%~A~%" st)
    (beesy-beaver::gs-score st)))

(defmethod show-state ((state beesy-beaver::game-state))
  ;;state
  nil)

(defmethod show-move ((state beesy-beaver::game-state) move)
  move)

(defun play-tetris (initial-state iterations)
  (mapcar
   (lambda (node)
     (cons (move node) (reward node)))
   (cdr ;; first elt is a move of root state (nil)
    (collect-best-children
     (explore-state
      initial-state iterations)))))


;; ----------------------------------------

(defstruct test-state
  (score 0)
  current-node
  graph)

(defmethod get-moves ((s test-state))
  (with-slots (current-node graph) s
    (loop for i from 0 to (1- (length (gethash current-node graph)))
       collect i)))

(defmethod apply-move ((s test-state) move)
  (with-slots (current-node graph score) s
    (let ((node/cost (nth move (gethash current-node graph))))
      (make-test-state
       :score (+ score (cdr node/cost))
       :current-node (car node/cost)
       :graph graph))))

(defmethod estimate-reward ((s test-state))
  (with-slots (graph score current-node) s
    (let ((score1 score))
      (labels ((%step (node)
                 (let ((links (gethash node graph)))
                   (when links
                     (let ((link (nth (random (length links))
                                      links)))
                       (incf score1 (cdr link))
                       (%step link))))))
        (%step current-node)
        score1))))

(defmethod show-state ((s test-state))
  (test-state-current-node s))

(defmethod show-move ((s test-state) move)
  (nth move (gethash (test-state-current-node s)
                     (test-state-graph s))))

(defun collect-best-children (node)
  (loop while node
     collect node
     do (setf node (best-child node :reward t))))

(defun test-explore-state (graph-spec)
  (let ((graph (make-hash-table)))
    (loop for (name . links) in graph-spec do
         (loop for (to cost) in (reverse links) do
              (push (cons to cost)
                    (gethash name graph))))
    (let ((node (explore-state 
                 (make-test-state :graph graph
                                  :current-node 'start)
                 100)))
      (mapcar (compose #'test-state-current-node #'state)
              (collect-best-children node)))))

(defun test-1 ()
  (assert
   (equalp
    (test-explore-state
     '((start
        (a 10)
        (b 20))
       (b (c 1))
       (a
        (finish 30)
        (c 2))
       (c (finish 1))))
    '(start a finish)))
  t)
