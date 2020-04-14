(in-package #:rest-server)

(defroute users (:get "application/json" &optional (id 'all))
  (if (eq id 'all)
      (json:encode-json-to-string
       (mapcar #'db:into-alist (mito:select-dao 'db:user)))
      (let ((the-user (mito:find-dao 'db:user :id id)))
        (if (null the-user)
            (http-condition 404 "Unknown user ID")
            (db:into-json the-user)))))

(defmacro validate-json (payload)
  (let ((payload-sym (gensym)))
    `(let ((,payload-sym ,payload))
       (handler-case (json:decode-json-from-string
                      ,payload-sym)
         (error (e)
           (declare (ignore e))
           (http-condition 400 "Malformed JSON: ~a" ,payload-sym))))))

(defun valid-user-p (user-data)
  (let ((fields '(:name :address :mail :pass)))
    (loop for field in user-data
       always (and (consp field)
                   (stringp (cdr field))
                   (and (member (car field) fields))))))

(defroute users (:post "application/json" &optional id)
  (declare (ignore id))
  (let ((object (validate-json (payload-as-string))))
    (if (not (valid-user-p object))
        (http-condition 400 "Malformed user data")
        (handler-case (let ((user (db:from-alist :user object)))
                        (mito:insert-dao user)
                        (json:encode-json-to-string
                         '((message . "Ok"))))
          (error (e)
            (declare (ignore e))
            (http-condition 400 "Malformed user data"))))))
