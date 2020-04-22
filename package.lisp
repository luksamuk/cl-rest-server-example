(defpackage #:rest-server.util
  (:nicknames #:util)
  (:use #:cl)
  (:export #:agetf
           #:route-validate-json
           #:symbol->keyword
           #:class-table-p
           #:table-get-lispy-columns
           #:table-get-lispy-register-columns
           #:table-get-string-columns
           #:table-get-string-register-columns
           #:post-valid-data-p
           #:dao->alist
           #:filter-alist
           #:dao->filtered-alist
           #:dao->json)
  (:documentation
   "Utilities and miscellaneous structures for
all other project modules."))

(defpackage #:rest-server.db
  (:nicknames #:db)
  (:use #:cl #:mito)
  (:export #:db-connect
           #:db-disconnect
           #:into-json
           #:into-alist
           #:from-alist
           #:user)
  (:documentation
   "Utilities related to dealing with the database,
including connection, migrations, models and
controllers."))

(defpackage #:rest-server
  (:nicknames #:restmain)
  (:use #:cl #:snooze)
  (:export #:start-server
           #:stop-server)
  (:documentation
   "Default package for the application, containing
routes and routines for starting/stopping the web
server."))
