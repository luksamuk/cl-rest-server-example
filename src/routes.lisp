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
        (let ((object (util:get-payload *request*)))
          (if (not (util:post-valid-data-p 'db:user object
                                           :has-password t))
              (util:http-response (400)
                "Malformed user data")
              (handler-case
                  (progn
                    (db:control-store :user object)
                    ;;(db:control-store :user *request* *response*)
                    (util:http-response ())) ; OK
                (dbi.error:dbi-database-error (e)
                  (progn
                    (princ "Error: User already exists.")
                    (terpri)
                    (format t "Condition: ~a~%Payload: ~a" e object))
                  (util:http-response (400)
                    "User already exists")))))))

(setf (route *app* "/login" :method :POST)
      (lambda (params)
        (declare (ignore params))
        (let ((object (util:get-payload *request*)))
          (if (or (null (util:agetf :mail object))
                  (null (util:agetf :password object)))
              (util:http-response (400)
                "Malformed login data")
              (let ((dao
                     (mito:find-dao
                      'db:user
                      :mail (util:agetf :mail object))))
                (cond ((null dao)
                       (util:http-response (404)
                         "Unknown user"))
                      ((mito-auth:auth
                        dao
                        (util:agetf :password object))
                       (util:http-response ()
                         `((:mail  . ,(util:agetf :mail object))
                           (:token . "")))) ; TODO: JWT token
                      (t (util:http-response (403)
                          "Wrong password"))))))))
