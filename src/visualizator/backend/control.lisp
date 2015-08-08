
(in-package :visualizator-backend)

(defparameter *server* nil)

;;; some kind of dispatch table for static files
(push (create-static-file-dispatcher-and-handler
       "/index.html" "src/visualizator/frontend/index.html")
      *dispatch-table*)

(push (create-static-file-dispatcher-and-handler
       "/honeycomb.js" "src/visualizator/frontend/honeycomb.js")
      *dispatch-table*)

(push (create-static-file-dispatcher-and-handler
       "/events.js" "src/visualizator/frontend/events.js")
      *dispatch-table*)


(defun start-server (&key (port 4343))
  (unless (and *server* (started-p *server*))
    (setf *server* (make-instance 'easy-acceptor :port port))
    (start *server*)))


(defun stop-server ()
  (when (and *server* (started-p *server*))
    (stop *server*)))
