(in-package #:rest-server.db)

(defun db-connect ()
  (mito:connect-toplevel :postgres
                         :username "postgres"
                         :database-name "cl-rest"
                         :password "docker"))

(defun db-disconnect ()
  (mito:disconnect-toplevel))

(defparameter *orm-tables*
  '(user))

(defun db-migrate-tables ()
  (mapcar #'mito:ensure-table-exists *orm-tables*))

(defun db-seed ()
  (labels ((seed-users (seed)
             (loop for user in seed
                do (mito:insert-dao
                    (make-instance
                     'user
                     :name (util:agetf :name user)
                     :address (util:agetf :address user)
                     :mail (util:agetf :mail user)
                     :pass (util:agetf :pass user))))))
    (seed-users '(((:name    . "Fulano da Silva")
                   (:address . "Rua dos Bobos, 0")
                   (:mail    . "fulano@exemplo.com")
                   (:pass    . "123456"))
                  ((:name    . "Ciclano da Silva")
                   (:address . "Rua dos Bobos, 1")
                   (:mail    . "ciclano@exemplo.com")
                   (:pass    . "123456"))))))
