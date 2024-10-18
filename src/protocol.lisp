;;;; LSP messages and protocol

(in-package #:coalton-mode)

;;; Atom types

(define-atom t)

(define-atom string)

(deftype uri () 'string)

(define-atom uri)

(define-atom boolean)

(define-atom integer)

(deftype uinteger () '(integer 0 #.(1- (expt 2 31))))

(define-atom uinteger)

;;; Common structural types

(define-message position ()
  ("line" uinteger)
  ("character" uinteger))

(define-message range ()
  ("start" position)
  ("end" position))

(define-message location ()
  ("uri" uri)
  ("range" range))

(define-union progress-token (integer string))

(define-message work-done-progress-params ()
  ("workDoneToken" progress-token))

;;; Errors

(define-enum error-code ()
  (:unknown-error-code -32001)
  (:server-not-initialized -32002)
  (:invalid-request -32600)
  (:method-not-found -32601)
  (:invalid-params -32602)
  (:internal-error -32603)
  (:parse-error -32700)
  (:request-cancelled -32800)
  (:content-modified -32801)
  (:server-cancelled -32802)
  (:request-failed -32803))

(define-message response-error ()
  ("code" error-code)
  ("message" string)
  ("data" t))

;;; Messages

(define-message message ()
  ("jsonrpc" string))

(define-message notification-message (message)
  ("method" string)
  ("params" t))

(define-message message ()
  ("jsonrpc" string))

(define-message request-message (message)
  ("id" integer)
  ("method" string)
  ("params" t))

(define-message response-message (message)
  ("id" integer)
  ("result" t)
  ("error" response-error))

;;; Session initialization
;;;
;;; The first message sent by the client is 'initialize', containing
;;; 'initialize-params' in the :params field, describing client
;;; capabilities. The server replies with 'initialized', and describes
;;; its capabilities.

(define-enum position-encoding-kind ()
  (:utf8 "utf-8")
  (:utf16 "utf-16")
  (:utf32 "utf-32"))

(define-enum text-document-sync-kind ()
  (:none 0)
  (:full 1)
  (:incremental 2))

(define-message save-options ()
  ("includeText" (boolean :optional t)))

(define-union text-document-sync-options-save (boolean save-options))

(define-message text-document-sync-options ()
  ("openClose" boolean)
  ("change" (text-document-sync-kind :optional t))
  ("willSave" (boolean :optional t))
  ("willSaveWaitUntil" (boolean :optional t))
  ("save" (text-document-sync-options-save :optional t)))

(define-message server-info ()
  ("name" string)
  ("version" (string :optional t)))

(define-enum resource-operation-kind ()
  (:create "create")
  (:rename "rename")
  (:delete "delete"))

(define-enum failure-handling-kind ()
  (:abort "abort")
  (:transactional "transactional")
  (:undo "undo")
  (:text-only-transactional "textOnlyTransactional"))

(define-message workspace-edit-client-capabilities ()
  ("documentChanges" boolean)
  ("resourceOperations" (resource-operation-kind :vector t))
  ("failureHandling" failure-handling-kind)
  ("normalizesLineEndings" boolean)
  ("changeAnnotationSupport" boolean))

(define-message did-change-configuration-client-capabilities ()
  ("dynamicRegistration" (boolean :optional t)))

(define-message did-change-watched-files-client-capabilities ()
  ("dynamicRegistration" (boolean :optional t))
  ("relativePatternSupport" (boolean :optional t)))

(define-enum symbol-kind ()
  (:file 1)
  (:module 2)
  (:namespace 3)
  (:package 4)
  (:class 5)
  (:method 6)
  (:property 7)
  (:field 8)
  (:constructor 9)
  (:enum 10)
  (:interface 11)
  (:function 12)
  (:variable 13)
  (:constant 14)
  (:string 15)
  (:number 16)
  (:boolean 17)
  (:array 18)
  (:object 19)
  (:key 20)
  (:null 21)
  (:enum-member 22)
  (:struct 23)
  (:event 24)
  (:operator 25)
  (:type-parameter 26))

(define-message symbol-kind-value-set ()
  ("valueSet" (symbol-kind :vector t)))

(define-enum symbol-tag ()
  (:deprecated 1))

(define-message symbol-tag-value-set ()
  ("valueSet" (symbol-tag :vector t)))

(define-message resolve-support-properties ()
  ("properties" (string :vector t)))

(define-message workspace-symbol-client-capabilities ()
  ("dynamicRegistration" (boolean :optional t))
  ("symbolKind" (symbol-kind-value-set :optional t))
  ("tagSupport" (symbol-tag-value-set :optional t))
  ("resolveSupport" (resolve-support-properties :optional t)))

(define-message execute-command-client-capabilities ()
  ("dynamicRegistration" (boolean :optional t)))

(define-message semantic-tokens-workspace-client-capabilities ()
  ("refreshSupport" (boolean :optional t)))

(define-message code-lens-workspace-client-capabilities ()
  ("refreshSupport" (boolean :optional t)))

(define-message workspace-client-capabilities ()
  ("applyEdit" (boolean :optional t))
  ("workspaceEdit" (workspace-edit-client-capabilities :optional t))
  ("didChangeConfiguration" (did-change-configuration-client-capabilities :optional t))
  ("didChangeWatchedFiles" (did-change-watched-files-client-capabilities :optional t))
  ("symbol" (workspace-symbol-client-capabilities :optional t))
  ("executeCommand" (execute-command-client-capabilities :optional t))
  ("workspaceFolders" (boolean :optional t))
  ("configuration" (boolean :optional t))
  ("semanticTokens" (semantic-tokens-workspace-client-capabilities :optional t))
  ("codeLens" (code-lens-workspace-client-capabilities :optional t))
  #++ ("fileOperations" (file-operations-client-capabilities :optional t))
  #++ ("inlineValue" (inline-value-workspace-client-capabilities :optional t))
  #++ ("inlayHint" (inlay-hint-workspace-client-capabilities :optional t))
  #++ ("diagnostics" (diagnostic-workspace-client-capabilities :optional t)))

(define-message text-document-sync-client-capabilities ()
  ("dynamicRegistration" (boolean :optional t))
  ("willSave" (boolean :optional t))
  ("willSaveWaitUntil" (boolean :optional t))
  ("didSave" (boolean :optional t)))

(define-message declaration-client-capabilities ()
  ("dynamicRegistration" (boolean :optional t))
  ("linkSupport" (boolean :optional t)))

(define-message definition-client-capabilities ()
  ("dynamicRegistration" (boolean :optional t))
  ("linkSupport" (boolean :optional t)))

(define-enum insert-text-mode ()
  (:as-is 1)
  (:adjust-indentation 2))

(define-enum completion-item-tag ()
  (:deprecated t))

(define-message tag-support-value-set ()
  ("valueSet" (completion-item-tag :vector t)))

(define-message insert-text-mode-value-set ()
  ("valueSet" (insert-text-mode :vector t)))

(define-message resolve-support-properties ()
  ("properties" (string :vector t)))

(define-enum markup-kind ()
  (:plaintext "plaintext")
  (:markdown "markdown"))

(define-message completion-item-capabilities ()
  ("snippetSupport" (boolean :optional t))
  ("commitCharactersSupport" (boolean :optional t))
  ("documentationFormat" (markup-kind :optional t :vector t))
  ("deprecatedSupport" (boolean :optional t))
  ("preselectSupport" (boolean :optional t))
  ("tagSupport" (tag-support-value-set :optional t))
  ("insertReplaceSupport" (boolean :optional t))
  ("resolveSupport" (resolve-support-properties :optional t))
  ("insertTextModeSupport" (insert-text-mode-value-set :optional t))
  ("labelDetailsSupport" (boolean :optional t)))

(define-message completion-list-capabilities ()
  ("itemDefaults" (string :optional t :vector t)))

(define-message completion-client-capabilities ()
  ("dynamicRegistration" (boolean :optional t))
  ("completionItem" completion-item-capabilities)
  #++ ("completionItemKind" completion-item-kind-capabilities)
  ("contextSupport" (boolean :optional t))
  ("insertTextMode" insert-text-mode)
  ("completionList" completion-list-capabilities))

(define-message hover-client-capabilities ()
  ("dynamicRegistration" (boolean :optional t))
  ("contentFormat" (markup-kind :vector t :optional t)))

(define-message signature-help-client-capabilities ()
  ("dynamicRegistration" (boolean :optional t))
  #++ ("signatureInformation" (signature-information-options :optional t))
  ("contextSupport" (boolean :optional t)))

(define-message type-definition-client-capabilities ()
  ("dynamicRegistration" (boolean :optional t))
  ("linkSupport" (boolean :optional t)))

(define-message implementation-client-capabilities ()
  ("dynamicRegistration" (boolean :optional t))
  ("linkSupport" (boolean :optional t)))

(define-message reference-client-capabilities ()
  ("dynamicRegistration" (boolean :optional t)))

(define-message document-highlight-client-capabilities ()
  ("dynamicRegistration" (boolean :optional t)))

(define-message document-symbol-client-capabilities ()
  ("dynamicRegistration" (boolean :optional t))
  ("symbolKind" (symbol-kind-value-set :optional t))
  ("hierarchicalDocumentSymbolSupport" (boolean :optional t))
  ("tagSupport" (symbol-tag-value-set :optional t))
  ("labelSupport" (boolean :optional t)))

(define-message text-document-client-capabilities ()
  ("synchronization" (text-document-sync-client-capabilities :optional t))
  ("completion" (completion-client-capabilities :optional t))
  ("hover" (hover-client-capabilities :optional t))
  ("signatureHelp" (signature-help-client-capabilities :optional t))
  ("declaration" (declaration-client-capabilities :optional t))
  ("definition" (definition-client-capabilities :optional t))
  ("typeDefinition" (type-definition-client-capabilities :optional t))
  ("implementation" (implementation-client-capabilities :optional t))
  ("references" (reference-client-capabilities :optional t))
  ("documentHighlight" (document-highlight-client-capabilities :optional t))
  ("documentSymbol" (document-symbol-client-capabilities :optional t))
  #++ ("code-action" (code-action-client-capabilities :optional t))
  #++ ("code-lens" (code-lens-client-capabilities :optional t))
  #++ ("document-link" (document-link-client-capabilities :optional t))
  #++ ("color-provider" (document-color-client-capabilities :optional t))
  #++ ("formatting" (document-formatting-client-capabilities :optional t))
  #++ ("range-formatting" (document-range-formatting-client-capabilities :optional t))
  #++ ("on-type-formatting" (document-on-type-formatting-client-capabilities :optional t))
  #++ ("rename" (rename-client-capabilities :optional t))
  #++ ("publish-diagnostics" (publish-diagnostics-client-capabilities :optional t))
  #++ ("folding-range" (folding-range-client-capabilities :optional t))
  #++ ("selection-range" (selection-range-client-capabilities :optional t))
  #++ ("linked-editing-range" (linked-editing-range-client-capabilities :optional t))
  #++ ("call-hierarchy" (call-hierarchy-client-capabilities :optional t))
  #++ ("semantic-tokens" (semantic-tokens-client-capabilities :optional t))
  #++ ("moniker" (moniker-client-capabilities :optional t))
  #++ ("type-hierarchy" (type-hierarchy-client-capabilities :optional t))
  #++ ("inline-value" (inline-value-client-capabilities :optional t))
  #++ ("inlay-hint" (inlay-hint-client-capabilities :optional t))
  #++ ("diagnostic" (diagnostic-client-capabilities :optional t)))

(define-message client-capabilities ()
  ("workspace" (workspace-client-capabilities :optional t))
  ("text-document" (text-document-client-capabilities :optional t))
  #++ ("notebook-document" (notebook-document-client-capabilities :optional t))
  #++ ("window" (window-client-capabilities :optional t))
  #++ ("general" (general-client-capabilities :optional t))
  #++ ("experimental" lsp-any))

(define-message client-info ()
  ("name" string)
  ("version" (string :optional t)))

(define-message workspace-folder ()
  ("uri" uri)
  ("name" string))

(define-enum trace-value ()
  (:off "off")
  (:messages "messages")
  (:verbose "verbose"))

(define-message initialize-params (work-done-progress-params)
  ("processId" (integer :optional t))
  ("clientInfo" (client-info :optional t))
  ("locale" (string :optional t))
  ("rootPath" (string :optional t))
  ("rootUri" (uri :optional t))
  ("initialization-options" t)
  ("capabilities" client-capabilities)
  ("trace" (trace-value :optional t))
  ("workspaceFolders" (workspace-folder :vector t :optional t)))

(define-message completion-item-options ()
  ("labelDetailsSupport" (boolean :optional t)))

(define-message work-done-progress-options ()
  ("workDoneProgress" (boolean :optional t)))

(define-message declaration-options (work-done-progress-options))

(define-message definition-options (work-done-progress-options))

(define-message document-link-options (work-done-progress-options)
  ("resolveProvider" (boolean :optional t)))

(define-message document-formatting-options (work-done-progress-options))

(define-message document-symbol-options (work-done-progress-options)
  ("label" (string :optional t)))

(define-message completion-options (work-done-progress-options)
  ("triggerCharacters" (string :optional t :vector t))
  ("allCommitCharacters" (string :optional t :vector t))
  ("resolveProvider" (boolean :optional t))
  ("completionItem" (completion-item-options :optional t)))

(define-message text-document-registration-options ()
  )

(define-message static-registration-options ()
  )

(define-message diagnostic-options ()
  )

(define-message diagnostic-registration-options (text-document-registration-options diagnostic-options static-registration-options))

(define-message declaration-options (work-done-progress-options))

(define-message declaration-registration-options (text-document-registration-options declaration-options static-registration-options))

(define-message server-capabilities ()
  ("position-encoding" (position-encoding-kind :optional t))
  ("text-document-sync" (text-document-sync-options :optional t))
  #++ ("notebook-document-sync" (or notebook-document-sync-options notebook-document-sync-registration-options))
  ("completion-provider" (completion-options :optional t))
  #++ ("hover-provider" (or boolean hover-options))
  #++ ("signature-help-provider" (signature-help-options :optional t))
  #++ ("declaration-provider" (declaration-registration-options :optional t))
  #++ ("definition-provider" (definition-options :optional t))
  #++ ("document-symbol-provider" (document-symbol-options :optional t))
  #++ ("document-formatting-provider" (document-formatting-options :optional t))
  ("declarationProvider" (boolean :optional t))
  ("definitionProvider" (boolean :optional t))
  ("documentSymbolProvider" (boolean :optional t))
  ("documentFormattingProvider" (boolean :optional t))
  #++ ("type-definitionProvider" (or boolean type-definition-options type-definition-registration-options))
  #++ ("implementationProvider" (or boolean implementation-options implementation-registration-options))
  #++ ("referencesProvider" (or boolean reference-options))
  #++ ("documentHighlight-provider" (or boolean document-highlight-options))
  #++ ("codeAction-provider" (or boolean code-action-options))
  #++ ("codeLens-provider" (code-lens-options :optional t))
  ("documentLink-provider" (document-link-options :optional t))
  #++ ("colorProvider" (or boolean document-color-options document-color-registration-options))
  #++ ("documentRangeFormatting-provider" (or boolean document-range-formatting-options))
  #++ ("documentOnTypeFormatting-provider" (document-on-type-formatting-options :optional t))
  #++ ("renameProvider" (or boolean rename-options))
  #++ ("foldingRangeProvider" (or boolean folding-range-options folding-range-registration-options))
  #++ ("executeCommandProvider" (execute-command-options :optional t))
  #++ ("selectionRangeProvider" (or boolean selection-range-options selection-range-registration-options))
  #++ ("linkedEditingRangeProvider" (or boolean linked-editing-range-options linked-editing-range-registration-options))
  #++ ("callHierarchyProvider" (or boolean call-hierarchy-options call-hierarchy-registration-options))
  #++ ("semanticTokensProvider" (or semantic-tokens-options semantic-tokens-registration-options))
  #++ ("monikerProvider" (or boolean moniker-options moniker-registration-options))
  #++ ("typeHierarchyProvider" (or boolean type-hierarchy-options type-hierarchy-registration-options))
  #++ ("inlineValueProvider" (or boolean inline-value-options inline-value-registration-options))
  #++ ("inlayHintProvider" (or boolean inlay-hint-options inlay-hint-registration-options))
  ("diagnosticProvider" (diagnostic-registration-options :optional t))
  #++ ("workspaceSymbolProvider" (boolean workspace-symbol-options))
  #++ ("workspace" (workspace-server-capabilities :optional t))
  ("experimental" t))

(define-message initialize-result ()
  ("capabilities" server-capabilities)
  ("server-info" server-info))

(define-handler ("initialize" initialize-params)
    (session request)
  (setf (session-state session) 'initialized)
  (let ((params (request-params request))
        (result (make-message 'initialize-result)))
    (setf (root-uri session) (get-field params :root-uri))
    (set-field result (list :server-info :name) "Coalton")
    (set-field result (list :capabilities :text-document-sync :open-close) t)
    (set-field result (list :capabilities :text-document-sync :change) :full)
    ;;(set-field result (list :capabilities :text-document-sync :save :include-text) t)
    (set-field result (list :capabilities :declaration-provider) t)
    (set-field result (list :capabilities :definition-provider) t)
    (set-field result (list :capabilities :document-symbol-provider) t)
    (set-field result (list :capabilities :document-formatting-provider) t)
    ;; TODO positionEncoding to be selected from list offered by client
    (set-field result (list :capabilities :position-encoding) :utf16)
    (let ((response (make-response request)))
      (set-field response :result (slot-value result 'value))
      response)))

(define-handler ("initialized" initialized-params)
  (session request)
  (declare (ignore session request))
  (/info "Received 'initialized' notification")
  nil)

(define-message did-change-configuration-params ()
  ("settings" t))

(define-handler ("workspace/didChangeConfiguration" did-change-configuration-params)
  (session request)
  (let ((params (request-params request)))
    (/info "workspace/didChangeConfiguration settings = ~a" (get-field params :settings))
    ;; queue fake diagnostic information
    (write-message session (demo-diagnostic)) ; FIXME do this from a locked session thread
    nil))

(define-enum diagnostic-severity ()
  (:error 1)
  (:warning 2)
  (:information 3)
  (:hint 4))

(define-enum diagnostic-tag ()
  (:unnecessary 1)
  (:deprecated 2))

(define-message diagnostic-related-information ()
  ("location" location)
  ("message" string))

(define-message diagnostic ()
  ("range" range)
  ("severity" diagnostic-severity)
  ("code" string)
  ("source" string)
  ("message" string)
  ("tags" (diagnostic-tag :vector t))
  ("relatedInformation" (diagnostic-related-information :vector t))
  ("data" t))

(define-message publish-diagnostics-params ()
  ("uri" uri)
  ("version" (integer :optional t))
  ("diagnostics" (diagnostic :vector t)))

;;; Server publishes diagnostics to client.

(define-handler ("textDocument/publishDiagnostics" publish-diagnostics-params)
  (session request)
  (declare (ignorable session request))
  nil)

(defun demo-diagnostic ()
  (make-notification
   "textDocument/publishDiagnostics"
   (message-value
    (make-message 'publish-diagnostics-params
                  '((:uri . "file:///Users/jbouwman/git/coalton-mode/resources/fibonacci.coal")
                    (:diagnostics . (((:range . ((:start . ((:line . 5)
                                                            (:character . 4)))
                                                 (:end . ((:line . 5)
                                                          (:character . 8)))))
                                      (:tags)
                                      (:message . "export: 'fiib' is undefined")
                                      (:code . "undefined-export")
                                      (:langs)
                                      (:severity . 2)
                                      (:source . "coalton")))))))))
