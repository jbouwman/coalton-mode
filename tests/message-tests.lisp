(in-package :coalton-mode/tests)

(cm::define-message test-message ()
  (:enable boolean)
  (:flags (string :vector t))
  (:debug (boolean :optional t)))

(deftest message-test ()
  (let ((message (cm::new-message 'test-message)))
    (cm::set-field message :debug t)
    (cm::set-field message :flags (list "a" "b" "g"))
    (is (equal (cm::message-value message)
               '((:FLAGS "a" "b" "g")
                 (:DEBUG . T))))))
