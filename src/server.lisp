(in-package #:rest-server)

(defparameter *server-handler* nil
  "Default handler for the server. Non-nil when the server
is running.")

(defparameter *server-port* 9003
  "Default port for the server.")

(defparameter *app* (make-instance 'ningle:<app>))

(defun start-server ()
  "Initializes the server if it wasn't initialized yet.

Returns T if it succeeded in starting the server."
  (unless *server-handler*
    (db:db-connect)
    (setf *server-handler*
          (clack:clackup *app*
                         :port *server-port*))
    t))

(defun stop-server ()
  "Stops the REST server if it is running.

Returns T if it succeeded in stopping the server."
  (when *server-handler*
    (clack:stop *server-handler*)
    (setf *server-handler* nil)
    (db:db-disconnect)
    t))
