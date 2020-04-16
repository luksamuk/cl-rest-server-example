(in-package #:rest-server.db)

(defparameter *db-username* "postgres")
(defparameter *db-dbname*   "cl-rest")
(defparameter *db-pass*     "docker")

(defun db-connect ()
  (mito:connect-toplevel
   :postgres
   :username      *db-username*
   :database-name *db-dbname*
   :password      *db-pass*))

(defun db-disconnect ()
  (mito:disconnect-toplevel))

(defparameter *db-tables* '(user))

(defun db-ensure-tables ()
  (mapcar #'mito:ensure-table-exists *db-tables*))

(defun db-migrate-tables ()
  (mapcar #'mito:migrate-table *db-tables*))

(defun db-migration-expressions ()
  (loop for table in *db-tables*
     for expr = (mito:migration-expressions table)
     when expr
     collect (list table expr)))

(defun db-table-definitions ()
  (loop for table in *db-tables*
     for expr = (mito:table-definition table)
     collect (list table expr)))

(defun db-gen-tables ()
  (db-ensure-tables)
  (db-migrate-tables))

(defun db-seed ()
  (labels ((seed-users (seed)
             (loop for user in seed
                do (mito:insert-dao
                    (make-instance
                     'user
                     :name (util:agetf :name user)
                     :birthdate (util:agetf :birthdate user)
                     :address (util:agetf :address user)
                     :mail (util:agetf :mail user)
                     :pass (util:agetf :pass user))))))
    (seed-users '(((:name      . "Fulano da Silva")
                   (:birthdate . "1990-01-01 12:00:00-03")
                   (:address   . "Rua dos Bobos, 0")
                   (:mail      . "fulano@exemplo.com")
                   (:pass      . "123456"))
                  ((:name      . "Ciclano da Silva")
                   (:birthdate . "1990-01-01 12:00:00-03")
                   (:address   . "Rua dos Bobos, 1")
                   (:mail      . "ciclano@exemplo.com")
                   (:pass      . "123456"))))))
