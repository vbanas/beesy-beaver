(defconstant +modulus+ (expt 2 32))
(defconstant +multiplier+ 1103515245)
(defconstant +increment+ 12345)

(defun lcgen (seed len)
  (if (= len 0) nil
      (let ((next (mod (+ (* +multiplier+ seed) +increment+) +modulus+))) 
	(cons seed (lcgen next (1- len))))))

(defun get-seeds (seed len)
  (mapcar (lambda (x) (ldb (byte 15 16) x)) (lcgen seed len)))
