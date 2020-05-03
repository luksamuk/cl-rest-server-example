(in-package #:rest-server.db)

(defmethod control-store ((type (eql :user)) alist)
  "Specializes CONTROL-STORE for an entity,
inserting it on table `user`."
  (macrolet ((get-field (field)
               `(util:agetf ,field alist)))
    (mito:create-dao
     'user
     :name (get-field :name)
     :birthdate (get-field :birthdate)
     :address (get-field :address)
     :mail (get-field :mail)
     :password (get-field :password))))
