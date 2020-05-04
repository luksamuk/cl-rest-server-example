(in-package #:rest-server)

(setf (route *app* "/users" :method :GET)
      (lambda (params)
        (declare (ignore params))
        (db:control-index :user *request* *response*)))

(setf (route *app* "/users/:id" :method :GET)
      (lambda (params)
        (db:control-show :user params *response*)))

(setf (route *app* "/users" :method :POST)
      (lambda (params)
        (declare (ignore params))
        (db:control-store :user *request* *response*)))

(setf (route *app* "/login" :method :POST)
      (lambda (params)
        (declare (ignore params))
        (db:control-store :session *request* *response*)))
