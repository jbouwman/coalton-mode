(in-package :asdf-user)

(defsystem #:coalton-mode
  :depends-on (#:usocket
               #:bordeaux-threads
               #:cl-json
               #:coalton)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               ;; everything in 'lib' is general purpose code
               (:module "lib"
                :serial t
                :components ((:file "log")
                             (:file "list")
                             (:file "json")
                             (:file "rpc")
                             (:file "process")
                             (:file "message")))
               (:file "session")
               (:file "protocol")
               (:file "server")))

(defsystem #:coalton-mode/examples
  :pathname "resources/"
  :serial t
  :components ((:file "messages")))

(defsystem #:coalton-mode/tests
  :depends-on (#:coalton-mode
               #:coalton-mode/examples
               #:fiasco)
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "rpc-tests")
               (:file "lsp-tests")
               (:file "session-tests")))
