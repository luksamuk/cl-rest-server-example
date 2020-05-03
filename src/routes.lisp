(in-package #:rest-server)

(setf (route *app* "/users" :method :GET)
      (lambda (params)
        (declare (ignore params))
        (->> (mito:select-dao 'db:user)
             (mapcar #'util:dao->filtered-alist)
             json:encode-json-to-string)))

(setf (route *app* "/users/:id" :method :GET)
      (lambda (params)
        (let ((the-user
               (mito:find-dao 'db:user
                              :id (util:agetf :id params))))
          (if (null the-user)
              (util:http-response (404)
                "Unknown user ID ~a"
                (util:agetf :id params))
              (util:dao->json the-user)))))

(setf (route *app* "/users" :method :POST)
      (lambda (params)
        (declare (ignore params))
        (db:control-store :user *request* *response*)))

(setf (route *app* "/login" :method :POST)
      (lambda (params)
        (declare (ignore params))
        (db:control-store :session *request* *response*)))
