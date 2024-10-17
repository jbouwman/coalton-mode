(in-package #:coalton-mode/tests)

(deftest initialize-params ()
  (let ((params (cm::request-params
                 (new-request coalton-mode/examples:initialize))))
    (cm:get-field params :root-uri)
    (cm:get-field params :workspace-folders)))
