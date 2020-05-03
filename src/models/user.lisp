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
