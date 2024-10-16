(in-package #:coalton-mode/tests)

(defun send-message (session message)
  (cm::message-value
   (cm::process-request session (cm::new-message 'cm::request-message message))))


(deftest initialize-session ()
  (let ((session (make-instance 'cm::session)))
    (is (equal (send-message session coalton-mode/examples:initialize)
               '((:RESULT
                  (:CAPABILITIES
                   (:POSITION-ENCODING . "utf-16")
                   (:TEXT-DOCUMENT-SYNC (:CHANGE . 1) (:OPEN-CLOSE . T)))
                  (:SERVER-INFO
                   (:NAME . "Coalton")))
                 (:ID . 1)
                 (:JSONRPC . "2.0"))))))


(send-message (make-instance 'cm::session) coalton-mode/examples:initialize)
