(in-package #:coalton-mode/tests)

(deftest json-tests/decode ()

  (is (equalp (cm::decode-json
               "{\"key\": \"value\"}")
              '(("key" . "value")))))
