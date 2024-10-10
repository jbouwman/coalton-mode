(in-package :asdf-user)

(defsystem #:coalton-mode
  :depends-on (#:alexandria
               #:usocket
               #:bordeaux-threads
               #:com.inuoe.jzon
               #:coalton)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               ;; 'lib' contains general purpose code
               (:module "lib"
                :serial t
                :components ((:file "log")
                             (:file "list")
                             (:file "name")
                             (:file "rpc")
                             (:file "process")
                             (:file "message")
                             (:file "json")
                             (:file "uri")))
               (:file "session")
               (:file "protocol")
               (:file "server")))

(defsystem #:coalton-mode/tests
  :depends-on (#:coalton-mode
               #:fiasco)
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "mock")
               (:file "lsp-tests")
               (:file "message-tests")
               (:file "protocol-tests")
               (:file "rpc-tests")
               (:file "session-tests")))
