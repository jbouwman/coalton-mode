(in-package #:coalton-mode/tests)

(deftest initialize-message ()
  (let ((x (cm::new-message 'cm::initialize-result)))
    (cm::set-field x (list :capabilities :position-encoding)
                   :utf32)
    (cm::set-field x (list :server-info :name)
                   "Emacs")
    x))
