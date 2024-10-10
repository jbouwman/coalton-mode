(in-package #:coalton-mode/tests)

(deftest json-tests/decode ()

  (is (equalp (cm::decode-json
               "{\"key\": \"value\"}")
              '(("key" . "value"))))
  (is (equalp (cm::decode-json
               "{\"key\": [\"value\"]}")
              '(("key" . #("value")))))
  (is (equalp (cm::decode-json
               "{\"key\": [{\"key\": \"value\"}]}")
              '(("key" . ((("key" . "value")))))))

  (is (equalp (cm::decode-json
               (cm::to-json
                (cm::make-diagnostic 5 4 5 8
                                     "'x' is undefined"
                                     "undefined-export")))
              '(("range"
                 ("start"
                  ("line" . 5)
                  ("character" . 4))
                 ("end"
                  ("line" . 5)
                  ("character" . 8)))
                ("severity" . 2)
                ("code" . "undefined-export")
                ("source" . "coalton")
                ("message" . "'x' is undefined"))))

  (is (equalp (cm::decode-json
               (cm::to-json
                (mock-publish-diagnostics)))
              '(("jsonrpc" . "2.0")
                ("method" . "textDocument/publishDiagnostics")
                ("params"
                 ("uri" . "file:///Users/jbouwman/git/coalton-mode/resources/fib.coal")
                 ("diagnostics"
                  (("range"
                    ("start"
                     ("line" . 5)
                     ("character" . 4))
                    ("end"
                     ("line" . 5)
                     ("character" . 8)))
                   ("severity" . 2)
                   ("code" . "undefined-export")
                   ("source" . "coalton")
                   ("message" . "export: 'fiib' is undefined"))))))))
