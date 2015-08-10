(in-package :beesy-beaver)

(defvar *time-to-halt* nil)

(defun set-timeout (seconds)
  (setf *time-to-halt*
        (+ (get-internal-real-time)
           ;; 10 should be enough
           (* seconds
              internal-time-units-per-second))))

(defvar *time-delta* (* 5 internal-time-units-per-second))
(defvar *time-delta-start* nil)

(defun start-time-delta-measurement ()
  (setf *time-delta-start* (get-internal-real-time)))

(defun stop-time-delta-measurement ()
  (let ((new-delta (- (get-internal-real-time)
                      *time-delta-start*)))
    (when (> new-delta)
      (setf *time-delta* new-delta))))

(defun time-expired? ()
  (and *time-to-halt*
       (>= (+ (get-internal-real-time)
              *time-delta*)
           *time-to-halt*)))

(defun min-max-list (min-fn key-fn list)
  (let ((keys (mapcar key-fn list)))
    (reduce min-fn (cdr keys) :initial-value (car keys))))

(defun unit-start-position (pivot members)
  (let* ((left (min-max-list #'min #'pos-col members))
	 (right (min-max-list #'max #'pos-col members))
	 (rowOffset (min-max-list #'min #'pos-row members))
	 (colOffset (truncate (- *width* (+ (- right left) 1)) 2))
	 (toAdd (pos-to-cube (make-pos 0 colOffset)))
	 (toSub (pos-to-cube (make-pos rowOffset left))))
    (values 
     (cube-to-pos (cube-pos-sub (cube-pos-add (pos-to-cube pivot) toAdd) toSub))
     (mapcar (lambda (m) (cube-to-pos (cube-pos-sub 
				       (cube-pos-add (pos-to-cube m) toAdd) 
				       toSub))) 
	     members))))

;;{ "x": number, "y": number } /* x: column, y: row */
(defun test-unit-start-position ()
  (let ((test (car (decode-units (yason:parse "[{\"members\":[{\"x\":0,\"y\":1},{\"x\":1,\"y\":1}],\"pivot\":{\"x\":1,\"y\":0}}]"))))
	(*width* 5))
    (unit-start-position (unit-pivot test) (unit-members test))))


(defun combine-solutions (input-filenames output-filename)
  (with-open-file (output output-filename :direction :output)
    (yason:encode
     (mapcan (lambda (file)
               (let ((file-entry (alexandria:read-file-into-string file)))
                 (when file-entry file-entry)))
             input-filenames)
     output)))

(defun get-file-names (string)
  "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
  (mapcar #'pathname
          (loop for i = 0 then (1+ j)
             as j = (position #\Space string :start i)
             collect (subseq string i j)
             while j)))
