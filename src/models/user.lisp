(in-package #:rest-server.db)

(deftable user (has-secure-password)
  ((name :col-type (:varchar 80)
         :initarg :name
         :accessor user-name)
   (birthdate :col-type :timestamptz
              :initarg :birthdate
              :accessor user-birthdate)
   (address :col-type (:varchar 255)
            :initarg :address
            :accessor user-address)
   (mail :col-type (:varchar 64)
         :initarg :mail
         :accessor user-mail))
  (:unique-keys mail)
  (:documentation
   "Represents the `user` table on database."))

(defmethod create-from-alist ((type (eql :user)) alist)
  "Specializes CREATE-FROM-ALIST for an entity,
inserting it on table `user`."
  (format t "From alist: ~a~%" alist)
  (macrolet ((get-field (field)
               `(util:agetf ,field alist)))
    (mito:create-dao
     'user
     :name (get-field :name)
     :birthdate (get-field :birthdate)
     :address (get-field :address)
     :mail (get-field :mail)
     :password (get-field :password))))
