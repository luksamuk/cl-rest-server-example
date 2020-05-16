(asdf:defsystem #:rest-server-example
    :description "Exemplo de um servidor REST."
    :author "Lucas S. Vieira <lucasvieira@protonmail.com>"
    :license "MIT"
    :version "0.1.0"
    :serial t
    :depends-on (#:cl-json
                 #:ningle
                 #:clack
                 #:mito
                 #:mito-auth
                 #:ironclad
                 #:jose
                 #:closer-mop
                 #:alexandria
                 #:cl-arrows
                 #:split-sequence)
    :components
    ((:file "package")
     (:module "src"
       :components ((:file "util")
                    (:file "server")
                    (:file "routes")
                    (:file "db")
                    (:file "jwt")
                    (:module "models"
                      :components ((:file "user")))
                    (:module "controllers"
                      :components ((:file "user-controller")
                                   (:file "session-controller")))))))
