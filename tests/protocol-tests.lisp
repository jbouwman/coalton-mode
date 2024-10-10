(in-package #:coalton-mode/tests)

(deftest protocol-tests/initialize ()
  (let ((params (cm::request-params (cm::make-request (rpc-example "initialize.json")))))
    (cm:get-field params :root-uri)
    (cm:get-field params :workspace-folders)))
