(asdf:defsystem #:rest-server-example
    :description "Exemplo de um servidor REST"
    :author "Lucas S. Vieira <lucasvieira@protonmail.com>"
    :license "MIT"
    :version "0.0.1"
    :serial t
    :depends-on (#:uiop #:cl-json #:snooze #:clack #:mito)
    :components
    ((:file "package")
     (:module "src"
       :components ((:file "util")
                    (:file "routes")
                    (:file "server")
                    (:file "db")
                    (:module "models"
                      :components ((:file "user")))))))
