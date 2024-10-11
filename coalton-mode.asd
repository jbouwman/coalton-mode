(in-package :asdf-user)

(defsystem #:coalton-mode
  :depends-on (#:usocket
               #:bordeaux-threads
               #:cl-json
               #:coalton)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "rpc")
               (:file "process")
               (:file "message")
               (:file "lsp")
               (:file "session")
               (:file "server")
               (:file "main")))

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
               (:file "lsp-tests")))
