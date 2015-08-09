(in-package :beesy-beaver)

(defparameter *magic-words* nil)

;; { "id": number              /* A unique number identifying the problem */

;; , "units": [ Unit ]
;;   /* The various unit configurations that may appear in this game.
;;      There might be multiple entries for the same unit.
;;      When a unit is spawned, it will start off in the orientation
;;      specified in this field. */

;; , "width":  number          /* The number of cells in a row */

;; , "height": number          /* The number of rows on the board */

;; , "filled": [ Cell ]        /* Which cells start filled */

;; , "sourceLength": number    /* How many units in the source */

;; , "sourceSeeds": [ number ] /* How to generate the source and
;;                                how many games to play */
;; }

(defvar *test-input* "{\"height\":10,\"width\":10,\"sourceSeeds\":[0],\"units\":[{\"members\":[{\"x\":0,\"y\":0}],\"pivot\":{\"x\":0,\"y\":0}},{\"members\":[{\"x\":0,\"y\":0},{\"x\":2,\"y\":0}],\"pivot\":{\"x\":1,\"y\":0}}],\"id\":0,\"filled\":[{\"x\":2,\"y\":5},{\"x\":3,\"y\":5}],\"sourceLength\":100}")

(defclass task ()
  ((id :accessor task-id
       :initarg :id)
   (units :accessor task-units
	  :initform nil
	  :initarg :units
	  :type list)
   (field :accessor task-field
	  :initarg :field)
   (source-length :accessor task-source-length
		 :initarg :source-length)
   (source-seeds :accessor task-source-seeds
		 :initarg :source-seeds)))

(defclass unit ()
  ((members :accessor unit-members
	    :initarg :members)
   (pivot :accessor unit-pivot
	  :initarg :pivot)))

;;{ "x": number, "y": number } /* x: column, y: row */
;;make-pos (row col)
(defun decode-cell (point)
  (make-pos (gethash "y" point) (gethash "x" point)))

;; { "members":  [ Cell ]  /* The unit members. */
;; , "pivot":   Cell       /* The rotation point of the unit. */
;; }
(defun decode-units (units)
  (mapcar (lambda (u) (make-instance 'unit
				     :members (mapcar (lambda (p) (decode-cell p)) (gethash "members" u))
				     :pivot (decode-cell (gethash "pivot" u)))) 
	  units))

;;
(defun decode-field (width height filled)
  (setf *width* width
        *height* height)
  (let ((field (make-field)))
    (mapcar (lambda (p) (setf field (put-cell field (decode-cell p) 1)))  filled)
    field))

(defun decode-task (json)
  (let ((parsed (yason:parse json)))
    (make-instance 'task
		   :id (gethash "id" parsed)
		   :units (decode-units (gethash "units" parsed))
		   :field (decode-field 
			   (gethash "width" parsed)
			   (gethash "height" parsed)
			   (gethash "filled" parsed))
		   :source-length (gethash "sourceLength" parsed)
		   :source-seeds (gethash "sourceSeeds" parsed))))


;; result
;; [ { "problemId": number   /* The `id` of the game configuration */
;;   , "seed":      number   /* The seed for the particular game */
;;   , "tag":       string   /* A tag for this solution. */
;;   , "solution":  Commands
;;   }
;; ]
(defclass play-result ()
  ((problemId :accessor play-result-id
	      :initarg :problemId)
   (seed :accessor play-result-seed
	 :initarg :seed)
   (tag :accessor play-result-tag
	:initarg :tag
        :initform nil)
   (solution :accessor play-result-solution
	     :initarg :solution)))

(defmethod yason:encode ((play-result play-result) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "problemId" (play-result-id play-result))
      (yason:encode-object-element "seed" (play-result-seed play-result))
      (when (play-result-tag play-result)
        (yason:encode-object-element "tag" (play-result-tag play-result)))
      (yason:encode-object-element "solution" (play-result-solution play-result)))))

(defun test-result-encode ()
  (let ((res (list (make-instance 'play-result 
				  :problemId 1 :seed 0 
				  :tag "test1" :solution "some command1")
		   (make-instance 'play-result 
				  :problemId 2 :seed 1 
				  :tag "test2" :solution "some command2"))))
    (yason:encode res)))
