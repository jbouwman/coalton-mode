(in-package :coalton-mode/tests)

(defun mock-publish-diagnostics ()
  (cm::make-notification
   "textDocument/publishDiagnostics"
   (let ((message (cm::make-message 'cm::text-document-publish-diagnostics-params)))
     (cm:set-field message :uri "file:///Users/jlbouwman/git/coalton-mode/resources/fib.coal")
     (cm:set-field message :diagnostics
                   (list (cm::message-value
                          (cm::make-diagnostic 4 4 4 7
                                               "export: 'fob' is undefined"
                                               "undefined-export"))))
     message)))
