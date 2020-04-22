(in-package #:rest-server.db)

(deftable user ()
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
         :accessor user-mail)
   (pass :col-type (:varchar 64)
         :initarg :pass
         :accessor user-pass))
  (:unique-keys mail)
  (:documentation
   "Represents the `user` table on database."))

(defmethod from-alist ((type (eql :user)) alist)
  "Specializes FROM-ALIST for an entity which can
be inserted on table `user`."
  (macrolet ((get-field (field)
               `(util:agetf ,field alist)))
    (mito:make-dao-instance
     'user
     :name (get-field :name)
     :birthdate (get-field :birthdate)
     :address (get-field :address)
     :mail (get-field :mail)
     :pass (get-field :pass))))
