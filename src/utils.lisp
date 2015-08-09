(in-package :beesy-beaver)


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
               (yason:parse (alexandria:read-file-into-string file)))
             input-filenames)
     output)))
