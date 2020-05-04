(in-package #:rest-server)

(setf (route *app* "/users" :method :GET)
      (lambda (params)
        (declare (ignore params))
        (db:control-index :user)))

(setf (route *app* "/users/:id" :method :GET)
      (lambda (params)
        (db:control-show :user params)))

(setf (route *app* "/users" :method :POST)
      (lambda (params)
        (declare (ignore params))
        (db:control-store :user)))

(setf (route *app* "/login" :method :POST)
      (lambda (params)
        (declare (ignore params))
        (db:control-store :session)))
