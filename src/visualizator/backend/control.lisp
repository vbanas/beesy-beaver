
(in-package :visualizator-backend)

(defparameter *server* nil)

(defun start-server (&key (port 4343))
  (unless (and *server* (started-p *server*))
    (setf *server* (make-instance 'easy-acceptor :port port))
    (start *server*)))


(defun stop-server ()
  (when (and *server* (started-p *server*))
    (stop *server*)))