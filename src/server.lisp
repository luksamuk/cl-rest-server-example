(in-package #:rest-server)

(defparameter *server-handler* nil)

(defun start-server ()
  (unless *server-handler*
    (db:db-connect)
    (setf *server-handler*
          (clack:clackup (snooze:make-clack-app)
                         :port 9003))
    t))

(defun stop-server ()
  (when *server-handler*
    (clack:stop *server-handler*)
    (setf *server-handler* nil)
    (db:db-disconnect)
    t))
