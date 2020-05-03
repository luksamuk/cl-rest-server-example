(in-package #:rest-server.db)

(defparameter *db-username* "postgres"
  "Username for accessing the database.")
(defparameter *db-dbname*   "cl-rest"
  "Name of the database in the RDBMS.")
(defparameter *db-pass*     "docker"
  "Password of the database in the RDBMS.
Consider replacing this by an environment variable.")

(defun db-connect ()
  "Starts a connection with the database."
  (mito:connect-toplevel
   :postgres
   :username      *db-username*
   :database-name *db-dbname*
   :password      *db-pass*))

(defun db-disconnect ()
  "Disconnects from the database."
  (mito:disconnect-toplevel))

(defparameter *db-tables* '(user)
  "List of tables which should be checked on migration.")

(defun db-ensure-tables ()
  "Ensures that the tables exist."
  (mapcar #'mito:ensure-table-exists *db-tables*))

(defun db-migrate-tables ()
  "Performs migrations on existing tables, adjusting
them if their definitions were changed."
  (mapcar #'mito:migrate-table *db-tables*))

(defun db-migration-expressions ()
  "Retrieves migration expressions for the tables
which should be migrated.

Returns an alist containing the migration expressions
for the tables which demand migration. If no table
demands any migration, returns NIL."
  (loop for table in *db-tables*
     for expr = (mito:migration-expressions table)
     when expr
     collect (list table expr)))

(defun db-table-definitions ()
  "Retrieves the table definition expressions for
all tables."
  (loop for table in *db-tables*
     for expr = (mito:table-definition table)
     collect (list table expr)))

(defun db-gen-tables ()
  "Generates the application's tables for the first
time. This ensures that they exist and also migrates
them if necessary."
  (db-ensure-tables)
  (db-migrate-tables))

(defun db-seed ()
  "Populates the database with test information."
  (labels ((seed-users (seed)
             (loop for user in seed
                do (mito:insert-dao
                    (make-instance
                     'user
                     :name (util:agetf :name user)
                     :birthdate (util:agetf :birthdate user)
                     :address (util:agetf :address user)
                     :mail (util:agetf :mail user)
                     :password (util:agetf :password user))))))
    (seed-users '(((:name      . "Fulano da Silva")
                   (:birthdate . "1990-01-01 12:00:00-03")
                   (:address   . "Rua dos Bobos, 0")
                   (:mail      . "fulano@exemplo.com")
                   (:password  . "123456"))
                  ((:name      . "Ciclano da Silva")
                   (:birthdate . "1990-01-01 12:00:00-03")
                   (:address   . "Rua dos Bobos, 1")
                   (:mail      . "ciclano@exemplo.com")
                   (:password  . "123456"))))))

(defgeneric control-index (type-key request response))

(defgeneric control-show (type-key request response))

(defgeneric control-store (type-key request response))

(defgeneric control-update (type-key request response))

(defgeneric control-delete (type-key request response))
