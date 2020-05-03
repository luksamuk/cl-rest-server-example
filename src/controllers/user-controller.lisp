(in-package #:rest-server.db)

(defmethod control-store ((type (eql :user)) req res)
  (let ((payload (util:get-payload req)))
    (if (not (util:post-valid-data-p 'db:user payload
                                     :has-password t))
        (util:http-response (400)
          "Malformed user data")
        (handler-case
            (macrolet ((get-field (field)
                         `(util:agetf ,field payload)))
              (mito:create-dao
               'user
               :name (get-field :name)
               :birthdate (get-field :birthdate)
               :address (get-field :address)
               :mail (get-field :mail)
               :password (get-field :password))
              (util:http-response ())) ; 200 OK
          (dbi.error:dbi-database-error (e)
            (declare (ignore e))
            (util:http-response (400)
              "User already exists"))))))
