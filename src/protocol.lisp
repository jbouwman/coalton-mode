;;;; LSP messages and protocol

(in-package #:coalton-mode)

;;; Atom types

(define-atom t)

(define-atom string)

(define-atom boolean)

(define-atom integer)

(deftype uinteger () '(integer 0 #.(1- (expt 2 31))))

(define-atom uinteger)

;;; Common structural types

(define-message position ()
  (:line uinteger)
  (:character uinteger))

(define-message range ()
  (:start position)
  (:end position))

(define-union progress-token (integer string))

(define-message work-done-progress-params ()
  (:work-done-token progress-token))

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
  (:code error-code)
  (:message string)
  (:data t))

;;; Messages

(define-message message ()
  (:jsonrpc string))

(define-message request-message (message)
  (:id integer)
  (:method string)
  (:params t))

(define-message response-message (message)
  (:id integer)
  (:result t)
  (:error response-error))

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

(define-message text-document-sync-options ()
  (:open-close boolean)
  (:change (text-document-sync-kind :optional t)))

(define-message server-capabilities ()
  (:position-encoding (position-encoding-kind :optional t))
  (:text-document-sync (text-document-sync-options :optional t)))

(define-message server-info ()
  (:name string)
  (:version (string :optional t)))

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
  (:document-changes boolean)
  (:resource-operations (resource-operation-kind :vector t))
  (:failure-handling failure-handling-kind)
  (:normalizes-line-endings boolean)
  (:change-annotation-support boolean))

(define-message did-change-configuration-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

(define-message did-change-watched-files-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:relative-pattern-support (boolean :optional t)))

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
  (:value-set (symbol-kind :vector t)))

(define-enum symbol-tag ()
  (:deprecated 1))

(define-message symbol-tag-value-set ()
  (:value-set (symbol-tag :vector t)))

(define-message resolve-support-properties ()
  (:properties (string :vector t)))

(define-message workspace-symbol-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:symbol-kind (symbol-kind-value-set :optional t))
  (:tag-support (symbol-tag-value-set :optional t))
  (:resolve-support (resolve-support-properties :optional t)))

(define-message execute-command-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

(define-message semantic-tokens-workspace-client-capabilities ()
  (:refresh-support (boolean :optional t)))

(define-message code-lens-workspace-client-capabilities ()
  (:refresh-support (boolean :optional t)))

(define-message workspace-client-capabilities ()
  (:apply-edit (boolean :optional t))
  (:workspace-edit (workspace-edit-client-capabilities :optional t))
  (:did-change-configuration (did-change-configuration-client-capabilities :optional t))
  (:did-change-watched-files (did-change-watched-files-client-capabilities :optional t))
  (:symbol (workspace-symbol-client-capabilities :optional t))
  (:execute-command (execute-command-client-capabilities :optional t))
  (:workspace-folders (boolean :optional t))
  (:configuration (boolean :optional t))
  (:semantic-tokens (semantic-tokens-workspace-client-capabilities :optional t))
  (:code-lens (code-lens-workspace-client-capabilities :optional t))
  #++ (:file-operations (file-operations-client-capabilities :optional t))
  #++ (:inline-value (inline-value-workspace-client-capabilities :optional t))
  #++ (:inlay-hint (inlay-hint-workspace-client-capabilities :optional t))
  #++ (:diagnostics (diagnostic-workspace-client-capabilities :optional t)))

(define-message text-document-sync-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:will-save (boolean :optional t))
  (:will-save-wait-until (boolean :optional t))
  (:did-save (boolean :optional t)))

(define-message declaration-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:link-support (boolean :optional t)))

(define-message definition-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:link-support (boolean :optional t)))


(define-enum insert-text-mode ()
  (:as-is 1)
  (:adjust-indentation 2))

(define-enum completion-item-tag ()
  (:deprecated t))

(define-message tag-support-value-set ()
  (:value-set (completion-item-tag :vector t)))

(define-message insert-text-mode-value-set ()
  (:value-set (insert-text-mode :vector t)))

(define-message resolve-support-properties ()
  (:properties (string :vector t)))

(define-enum markup-kind ()
  (:plaintext "plaintext")
  (:markdown "markdown"))

(define-message completion-item-capabilities ()
  (:snippet-support (boolean :optional t))
  (:commit-characters-support (boolean :optional t))
  (:documentation-format (markup-kind :optional t :vector t))
  (:deprecated-support (boolean :optional t))
  (:preselect-support (boolean :optional t))
  (:tag-support (tag-support-value-set :optional t))
  (:insert-replace-support (boolean :optional t))
  (:resolve-support (resolve-support-properties :optional t))
  (:insert-text-mode-support (insert-text-mode-value-set :optional t))
  (:label-details-support (boolean :optional t)))

(define-message completion-list-capabilities ()
  (:item-defaults (string :optional t :vector t)))

(define-message completion-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:completion-item completion-item-capabilities)
  #++ (:completion-item-kind completion-item-kind-capabilities)
  (:context-support (boolean :optional t))
  (:insert-text-mode insert-text-mode)
  (:completion-list completion-list-capabilities))

(define-message hover-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:content-format (markup-kind :vector t :optional t)))

(define-message signature-help-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  #++ (:signature-information (signature-information-options :optional t))
  (:context-support (boolean :optional t)))

(define-message type-definition-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:link-support (boolean :optional t)))

(define-message implementation-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:link-support (boolean :optional t)))

(define-message reference-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

(define-message document-highlight-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

(define-message document-symbol-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:symbol-kind (symbol-kind-value-set :optional t))
  (:hierarchical-document-symbol-support (boolean :optional t))
  (:tag-support (symbol-tag-value-set :optional t))
  (:label-support (boolean :optional t)))

(define-message text-document-client-capabilities ()
  (:synchronization (text-document-sync-client-capabilities :optional t))
  (:completion (completion-client-capabilities :optional t))
  (:hover (hover-client-capabilities :optional t))
  (:signature-help (signature-help-client-capabilities :optional t))
  (:declaration (declaration-client-capabilities :optional t))
  (:definition (definition-client-capabilities :optional t))
  (:type-definition (type-definition-client-capabilities :optional t))
  (:implementation (implementation-client-capabilities :optional t))
  (:references (reference-client-capabilities :optional t))
  (:document-highlight (document-highlight-client-capabilities :optional t))
  (:document-symbol (document-symbol-client-capabilities :optional t))
  #++ (:code-action (code-action-client-capabilities :optional t))
  #++ (:code-lens (code-lens-client-capabilities :optional t))
  #++ (:document-link (document-link-client-capabilities :optional t))
  #++ (:color-provider (document-color-client-capabilities :optional t))
  #++ (:formatting (document-formatting-client-capabilities :optional t))
  #++ (:range-formatting (document-range-formatting-client-capabilities :optional t))
  #++ (:on-type-formatting (document-on-type-formatting-client-capabilities :optional t))
  #++ (:rename (rename-client-capabilities :optional t))
  #++ (:publish-diagnostics (publish-diagnostics-client-capabilities :optional t))
  #++ (:folding-range (folding-range-client-capabilities :optional t))
  #++ (:selection-range (selection-range-client-capabilities :optional t))
  #++ (:linked-editing-range (linked-editing-range-client-capabilities :optional t))
  #++ (:call-hierarchy (call-hierarchy-client-capabilities :optional t))
  #++ (:semantic-tokens (semantic-tokens-client-capabilities :optional t))
  #++ (:moniker (moniker-client-capabilities :optional t))
  #++ (:type-hierarchy (type-hierarchy-client-capabilities :optional t))
  #++ (:inline-value (inline-value-client-capabilities :optional t))
  #++ (:inlay-hint (inlay-hint-client-capabilities :optional t))
  #++ (:diagnostic (diagnostic-client-capabilities :optional t)))

(define-message client-capabilities ()
  (:workspace (workspace-client-capabilities :optional t))
  (:text-document (text-document-client-capabilities :optional t))
  #++ (:notebook-document (notebook-document-client-capabilities :optional t))
  #++ (:window (window-client-capabilities :optional t))
  #++ (:general (general-client-capabilities :optional t))
  #++ (:experimental lsp-any))

(define-message initialize-params (work-done-progress-params)
  (:process-id (integer :optional t))
  #++ (:client-info (client-info :optional t))
  (:locale (string :optional t))
  (:root-path (string :optional t))
  #++ (:root-uri (document-uri :optional t))
  (:initialization-options t)
  (:capabilities client-capabilities)
  #++ (:trace (trace-value :optional t))
  #++ (:workspace-folders (workspace-folder :vector t :optional t)))

(define-message initialize-result ()
  (:capabilities server-capabilities)
  (:server-info server-info))

(define-handler ("initialize" initialize-params)
    (session request)
  (setf (session-state session) 'initialized)
  (let ((result (new-message 'initialize-result)))
    (set-field result (list :server-info :name) "Coalton")
    (set-field result (list :capabilities :text-document-sync :open-close) t)
    (set-field result (list :capabilities :text-document-sync :change) :full)
    ;; TODO positionEncoding to be selected from list offered by client
    (set-field result (list :capabilities :position-encoding) :utf16)
    (let ((response (make-response request)))
      (set-field response :result (slot-value result 'value))
      response)))
