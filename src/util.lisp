(in-package #:rest-server.util)

(defmacro agetf (key alist)
  `(cdr (assoc ,key ,alist)))

(defmacro route-validate-json (payload)
  (let ((payload-sym (gensym)))
    `(let ((,payload-sym ,payload))
       (handler-case (json:decode-json-from-string
                      ,payload-sym)
         (error (e)
           (declare (ignore e))
           (http-condition 400 "Malformed JSON: ~a" ,payload-sym))))))

(defun post-valid-user-p (user-data)
  (let ((fields '(:name :birthdate :address :mail :pass)))
    (loop for field in user-data
       always (and (consp field)
                   (stringp (cdr field))
                   (and (member (car field) fields))))))
