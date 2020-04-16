(in-package #:rest-server.db)

(defclass user ()
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
  (:metaclass mito:dao-table-class)
  (:unique-keys mail))

(defmethod into-alist ((user user))
  `((:id      . ,(mito:object-id user))
    (:name    . ,(user-name user))
    (:birthdate . ,(user-birthdate user))
    (:address . ,(user-address user))
    (:mail    . ,(user-mail user))
    (:pass    . ,(user-pass user))))

(defmethod into-json ((user user))
  (json:encode-json-to-string (into-alist user)))

(defmethod from-alist ((type (eql :user)) alist)
  (macrolet ((get-field (field)
               `(util:agetf ,field alist)))
    (make-instance 'user
                   :name (get-field :name)
                   :birthdate (get-field :birthdate)
                   :address (get-field :address)
                   :mail (get-field :mail)
                   :pass (get-field :pass))))
