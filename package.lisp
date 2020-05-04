(defpackage #:rest-server.util
  (:nicknames #:util)
  (:use #:cl #:cl-arrows)
  (:export #:agetf
           #:route-prepare-response
           #:http-response
           #:symbol->keyword
           #:class-table-p
           #:table-get-lispy-columns
           #:table-get-lispy-register-columns
           #:table-get-string-columns
           #:table-get-string-register-columns
           #:get-payload
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
  (:use #:cl #:mito #:mito-auth #:cl-arrows)
  (:export #:db-connect
           #:db-disconnect
           #:into-json
           #:into-alist
           #:control-index
           #:control-show
           #:control-store
           #:control-update
           #:control-delete
           #:user)
  (:documentation
   "Utilities related to dealing with the database,
including connection, migrations, models and
controllers."))

(defpackage #:rest-server
  (:nicknames #:restmain)
  (:use #:cl #:cl-arrows #:ningle)
  (:export #:start-server
           #:stop-server)
  (:documentation
   "Default package for the application, containing
routes and routines for starting/stopping the web
server."))
