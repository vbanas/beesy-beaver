(in-package :beesy-beaver)

(defun unit-start-position (pivot members)
  (let* ((left (apply (lambda (p1 p2) (min (pos-col p1) (pos-col p2))) members))
	 (right (apply (lambda (p1 p2) (max (pos-col p1) (pos-col p2))) members))
	 (rowOffset (apply (lambda (p1 p2) (min (pos-row p1) (pos-row p2))) members))
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
