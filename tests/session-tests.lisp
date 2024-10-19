(in-package #:coalton-mode/tests)

(deftest session-tests/initialize ()
  (let ((session (make-instance 'cm::session)))
    (is (equalp (cm::message-value
                 (cm::process-request
                  session (cm::make-request (rpc-example "initialize.json"))))
                '(("result"
                   ("capabilities" ("positionEncoding" . "utf-16")
                    ("documentFormattingProvider" ("workDoneProgress" . T))
                    ("definitionProvider" ("workDoneProgress" . T))
                    ("textDocumentSync" ("change" . 1) ("openClose" . T)))
                   ("serverInfo" ("name" . "Coalton")))
                  ("id" . 1) ("jsonrpc" . "2.0"))))))

(deftest session-tests/get-field ()
  (let ((init (cm::make-request (rpc-example "initialize.json"))))
    (is (eq 1 (cm::get-field init :id)))

    (let ((params (cm::request-params init)))
      (is (eq t
              (cm:get-field params '(:capabilities :workspace
                                     :did-change-watched-files :dynamic-registration)))))))

(deftest session-tests/set-field ()
  (let ((params (cm::make-message 'cm::initialize-params)))
    (cm::message-value (cm::set-field-1 params :capabilities 'x)))
  (let ((params (cm::make-message 'cm::initialize-params)))
    (cm:set-field params '(:capabilities :workspace) 'x)))

