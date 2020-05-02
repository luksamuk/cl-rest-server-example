(in-package #:rest-server)

(defroute users (:get "application/json" &optional (id 'all))
  (if (eq id 'all)
      (->> (mito:select-dao 'db:user)
           (mapcar #'util:dao->filtered-alist)
           json:encode-json-to-string)
      (let ((the-user (mito:find-dao 'db:user :id id)))
        (if (null the-user)
            (http-condition 404 "Unknown user ID")
            (util:dao->json the-user)))))

(defroute users (:post "application/json" &optional id)
  (if id
      (http-condition 403 "Route does not accept POST.")
      (let ((object (util:route-validate-json (payload-as-string))))
        (if (not (util:post-valid-data-p 'db:user object))
            (http-condition 400 "Malformed user data")
            (handler-case (let ((user (db:from-alist :user object)))
                            (mito:insert-dao user) ; todo: handle
                            (json:encode-json-to-string
                             '((message . "Ok"))))
              (error (e)
                (declare (ignore e))
                (http-condition 400 "Malformed user data")))))))
