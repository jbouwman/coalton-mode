(in-package #:coalton-mode/tests)

(deftest initialize-session ()
  (let ((session (make-instance 'cm::session)))
    (is (equal (slot-value
                (cm::process-request
                 session
                 (cm::new-message 'cm::request-message coalton-mode/examples:initialize))
                'cm::value)
               '((:RESULT
                  (:CAPABILITIES
                   (:POSITION-ENCODING . "utf-16")
                   (:TEXT-DOCUMENT-SYNC (:CHANGE . 1) (:OPEN-CLOSE . T)))
                  (:SERVER-INFO
                   (:NAME . "Coalton")))
                 (:ID . 1)
                 (:JSONRPC . "2.0"))))))
