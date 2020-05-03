(asdf:defsystem #:rest-server-example
    :description "Exemplo de um servidor REST."
    :author "Lucas S. Vieira <lucasvieira@protonmail.com>"
    :license "MIT"
    :version "0.0.5"
    :serial t
    :depends-on (#:cl-json
                 #:ningle
                 #:clack
                 #:mito
                 #:mito-auth
                 #:ironclad
                 #:closer-mop
                 #:alexandria
                 #:cl-arrows)
    :components
    ((:file "package")
     (:module "src"
       :components ((:file "util")
                    (:file "server")
                    (:file "routes")
                    (:file "db")
                    (:module "models"
                      :components ((:file "user")))
                    (:module "controllers"
                      :components ((:file "user-controller")
                                   (:file "session-controller")))))))
