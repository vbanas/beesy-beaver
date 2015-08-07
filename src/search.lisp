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

(defvar *debug* t)

(defmacro dprint (fmt &rest args)
  `(when *debug*
     (format t ,fmt ,@args)))

(defgeneric show-state (state))
(defgeneric show-move (state move))

(defun explore-state (initial-state iterations)
  (labels
      ((%link (from to)
         (push to (children from)))
       (%new-node (parent state move)
         (let* ((new-state (apply-move state move))
                (new-node
                 (make-instance
                  'node
                  :move move
                  :state new-state
                  :reward (estimate-reward new-state)
                  :moves-to-explore (get-moves new-state))))
           (%link parent new-node)
           new-node))
       (%expand (node)
         (dprint "expand: node ~A move ~A~%"
                 (id node)
                 (show-move
                  (state node) (car (moves-to-explore node))))
         (let* ((move (pop (moves-to-explore node)))
                (new-node (%new-node node (state node) move)))
           (push new-node (children node))
           new-node))
       (%iterate (node)
         (dprint "iterate: node ~A, state ~A~%"
                 (id node) (show-state (state node)))
         (when (and *debug* (moves-to-explore node))
           (dprint "iterate: moves to explore ~A~%"
                   (mapcar (curry #'show-move (state node))
                           (moves-to-explore node))))
         (incf (visits node))
         (prog1
             (if (moves-to-explore node)
                 (reward (%expand node))
                 (if (children node)
                     (progn
                       (%iterate (best-child node))
                       (setf
                        (reward node)
                        (reduce #'max
                                (mapcar #'reward (children node)))))
                     (reward node)))
           (dprint "iterate: node ~A, state ~A, reward ~A~%"
                   (id node) (show-state (state node))
                   (reward node)))))
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
        (dprint "~%Exploration done~%~%")))))

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
    (if (or (= 0 (random 2)) reward)
        (%scan #'> #'reward)
        (%scan #'< #'visits))))

;; (defun rank-node (parent node)
;;   (+ (reward node)
;;      (* *magical-c*
;;         (sqrt
;;          (/ (log (visits parent))
;;             (visits node))))))

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
      (loop while node
         collect (test-state-current-node
                  (state node))
         do (setf node (best-child node :reward t)))
      )))

(defun test-1 ()
  (test-explore-state
   '((start
      (a 10)
      (b 20))
     (b (c 1))
     (a
      (finish 30)
      (c 2))
     (c (finish 1)))))
