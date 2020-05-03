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
              (util:http-response *response* (404)
                                  "Unknown user ID ~a"
                                  (util:agetf :id params))
              (util:dao->json the-user)))))

(setf (route *app* "/users" :method :POST)
      (lambda (params)
        (declare (ignore params))
        (let ((object (util:get-payload *request*)))
          (if (not (util:post-valid-data-p 'db:user object))
              (util:http-response *response* (400)
                                  "Malformed user data")
              (handler-case
                  (let ((user (db:from-alist :user object)))
                    (mito:insert-dao user)
                    (util:http-response *response* ())) ; OK
                (dbi.error:dbi-database-error (e)
                  (progn
                    (princ "Error: User already exists.")
                    (terpri)
                    (format t "Condition: ~a~%Payload: ~a" e object))
                  (util:http-response
                   *response* (400)
                   "User already exists")))))))
