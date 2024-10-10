(in-package #:coalton-mode/tests)

(deftest lsp-tests/initialize-result ()
  (let ((x (cm::make-message 'cm::initialize-result)))
    (cm::set-field x (list :capabilities :position-encoding)
                   :utf32)
    (cm::set-field x (list :server-info :name)
                   "Emacs")
    x))
