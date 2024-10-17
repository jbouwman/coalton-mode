(defpackage lsp
  (:use :cl))

(in-package lsp)

(define-message message ()
  (:jsonrpc string))

(define-message request-message (message)
  (:id (or integer string))
  (:method string)
  (:params (or array object)))

(define-message response-message (message)
  (:id (or integer string null))
  (:result lsp-any)
  (:error response-error))

(define-message response-error ()
  (:code integer)
  (:message string)
  (:data lsp-any))

(defconstant +parse-error+ -32700)
(defconstant +invalid-request+ -32600)
(defconstant +method-not-found+ -32601)
(defconstant +invalid-params+ -32602)
(defconstant +internal-error+ -32603)

(defconstant +jsonrpc-reserved-error-range-start+ -32099)
(defconstant +server-error-start+ +jsonrpc-reserved-error-range-start+)
(defconstant +server-not-initialized+ -32002)
(defconstant +unknown-error-code+ -32001)

(defconstant +jsonrpc-reserved-error-range-end+ -32000)
(defconstant +server-error-end+ +jsonrpc-reserved-error-range-end+)

(defconstant +lsp-reserved-error-range-start+ -32899)
(defconstant +request-failed+ -32803)
(defconstant +server-cancelled+ -32802)
(defconstant +content-modified+ -32801)
(defconstant +request-cancelled+ -32800)

(defconstant +lsp-reserved-error-range-end+ -32800)

(define-message notification-message (message)
  (:method string)
  (:params (or array object)))

(define-message cancel-params ()
  (:id (or integer string)))

(deftype progress-token () '(or integer string))

(define-message progress-params ()
  (:token progress-token)
  (:value t))

(define-message hover-params ()
  (:text-document string)
  (:position (cons uinteger uinteger)))

(define-message hover-result ()
  (:value string))

(deftype document-uri () 'string)

(deftype uri () 'string)

(define-message regular-expressions-client-capabilities ()
  (:engine string)
  (:version string))

(define-message position ()
  (:line uinteger)
  (:character uinteger))

(deftype position-encoding-kind () 'string)

(define-message range ()
  (:start position)
  (:end position))

(define-message text-document-item ()
  (:uri document-uri)
  (:language-id string)
  (:version integer)
  (:text string))

(define-message text-document-identifier ()
  (:uri document-uri))

(define-message versioned-text-document-identifier (text-document-identifier)
  (:version integer))

(define-message optional-versioned-text-document-identifier (text-document-identifier)
  (:version (integer :optional t)))

(define-message text-document-position-params ()
  (:text-document text-document-identifier)
  (:position position))

(define-message document-filter ()
  (:language string)
  (:scheme string)
  (:pattern string))

(deftype document-selector () '(vector document-filter))

(define-message text-edit ()
  (:range range)
  (:new-text string))

(define-message change-annotation ()
  (:label string)
  (:needs-confirmation boolean)
  (:description string))

(deftype change-annotation-identifier () 'string)

(define-message annotated-text-edit (text-edit)
  (:annotation-id change-annotation-identifier))

(define-message text-document-edit ()
  (:text-document optional-versioned-text-document-identifier)
  (:edits (vector (or text-edit annotated-text-edit))))

(define-message location ()
  (:uri document-uri)
  (:range range))

(define-message location-link ()
  (:origin-selection-range range)
  (:target-uri document-uri)
  (:target-range range)
  (:target-selection-range range))

(define-message diagnostic ()
  (:range range)
  (:severity diagnostic-severity)
  (:code (or integer string))
  (:source string)
  (:message string)
  (:tags (vector diagnostic-tag))
  (:related-information (vector diagnostic-related-information))
  (:data lsp-any))

(deftype diagnostic-severity () '(integer 1 4))

(defconstant +error+ 1)
(defconstant +warning+ 2)
(defconstant +information+ 3)
(defconstant +hint+ 4)

(deftype diagnostic-tag () '(integer 1 2))

(defconstant +unnecessary+ 1)
(defconstant +deprecated+ 2)

(define-message diagnostic-related-information ()
  (:location location)
  (:message string))

(define-message code-description ()
  (:href uri))

(define-message command ()
  (:title string)
  (:command string)
  (:arguments (vector lsp-any)))

(deftype markup-kind () '(member "plaintext" "markdown"))

(define-message markup-content ()
  (:kind markup-kind)
  (:value string))

(define-message markdown-client-capabilities ()
  (:parser string)
  (:version string)
  (:allowed-tags (vector string)))

(define-message create-file-options ()
  (:overwrite boolean)
  (:ignore-if-exists boolean))

(define-message create-file ()
  (:kind (eql "create"))
  (:uri document-uri)
  (:options create-file-options)
  (:annotation-id change-annotation-identifier))

(define-message rename-file-options ()
  (:overwrite boolean)
  (:ignore-if-exists boolean))

(define-message rename-file ()
  (:kind (eql "rename"))
  (:old-uri document-uri)
  (:new-uri document-uri)
  (:options rename-file-options)
  (:annotation-id change-annotation-identifier))

(define-message delete-file-options ()
  (:recursive boolean)
  (:ignore-if-not-exists boolean))

(define-message delete-file ()
  (:kind (eql "delete"))
  (:uri document-uri)
  (:options delete-file-options)
  (:annotation-id change-annotation-identifier))

(define-message workspace-edit ()
  (:changes (hash-table document-uri (vector text-edit)))
  (:document-changes (vector (or text-document-edit create-file rename-file delete-file)))
  (:change-annotations (hash-table change-annotation-identifier change-annotation)))

(define-message workspace-edit-client-capabilities ()
  (:document-changes boolean)
  (:resource-operations (vector resource-operation-kind))
  (:failure-handling failure-handling-kind)
  (:normalizes-line-endings boolean)
  (:change-annotation-support change-annotation-support))

(deftype resource-operation-kind () '(member "create" "rename" "delete"))

(deftype failure-handling-kind () '(member "abort" "transactional" "undo" "textOnlyTransactional"))

(define-message work-done-progress-begin ()
  (:kind (eql "begin"))
  (:title string)
  (:cancellable boolean)
  (:message string)
  (:percentage uinteger))

(define-message work-done-progress-report ()
  (:kind (eql "report"))
  (:cancellable boolean)
  (:message string)
  (:percentage uinteger))

(define-message work-done-progress-end ()
  (:kind (eql "end"))
  (:message string))

(define-message work-done-progress-params ()
  (:work-done-token progress-token))

(define-message work-done-progress-options ()
  (:work-done-progress boolean))

(define-message partial-result-params ()
  (:partial-result-token progress-token))

(deftype trace-value () '(member "off" "messages" "verbose"))

(define-message initialize-params (work-done-progress-params)
  (:process-id (integer :optional t))
  (:client-info (or null client-info))
  (:locale (or string null))
  (:root-path (or string null))
  (:root-uri (or document-uri null))
  (:initialization-options lsp-any)
  (:capabilities client-capabilities)
  (:trace (or trace-value null))
  (:workspace-folders (or (vector workspace-folder) null)))

(define-message client-info ()
  (:name string)
  (:version (or string null)))

(define-message text-document-client-capabilities ()
  (:synchronization (or text-document-sync-client-capabilities null))
  (:completion (or completion-client-capabilities null))
  (:hover (or hover-client-capabilities null))
  (:signature-help (or signature-help-client-capabilities null))
  (:declaration (or declaration-client-capabilities null))
  (:definition (or definition-client-capabilities null))
  (:type-definition (or type-definition-client-capabilities null))
  (:implementation (or implementation-client-capabilities null))
  (:references (or reference-client-capabilities null))
  (:document-highlight (or document-highlight-client-capabilities null))
  (:document-symbol (or document-symbol-client-capabilities null))
  (:code-action (or code-action-client-capabilities null))
  (:code-lens (or code-lens-client-capabilities null))
  (:document-link (or document-link-client-capabilities null))
  (:color-provider (or document-color-client-capabilities null))
  (:formatting (or document-formatting-client-capabilities null))
  (:range-formatting (or document-range-formatting-client-capabilities null))
  (:on-type-formatting (or document-on-type-formatting-client-capabilities null))
  (:rename (or rename-client-capabilities null))
  (:publish-diagnostics (or publish-diagnostics-client-capabilities null))
  (:folding-range (or folding-range-client-capabilities null))
  (:selection-range (or selection-range-client-capabilities null))
  (:linked-editing-range (or linked-editing-range-client-capabilities null))
  (:call-hierarchy (or call-hierarchy-client-capabilities null))
  (:semantic-tokens (or semantic-tokens-client-capabilities null))
  (:moniker (or moniker-client-capabilities null))
  (:type-hierarchy (or type-hierarchy-client-capabilities null))
  (:inline-value (or inline-value-client-capabilities null))
  (:inlay-hint (or inlay-hint-client-capabilities null))
  (:diagnostic (or diagnostic-client-capabilities null)))

(define-message notebook-document-client-capabilities ()
  (:synchronization notebook-document-sync-client-capabilities))

(define-message client-capabilities ()
  (:workspace (or workspace-client-capabilities null))
  (:text-document (or text-document-client-capabilities null))
  (:notebook-document (or notebook-document-client-capabilities null))
  (:window (or window-client-capabilities null))
  (:general (or general-client-capabilities null))
  (:experimental lsp-any))

(define-message workspace-client-capabilities ()
  (:apply-edit (boolean :optional t))
  (:workspace-edit (or workspace-edit-client-capabilities null))
  (:did-change-configuration (or did-change-configuration-client-capabilities null))
  (:did-change-watched-files (or did-change-watched-files-client-capabilities null))
  (:symbol (or workspace-symbol-client-capabilities null))
  (:execute-command (or execute-command-client-capabilities null))
  (:workspace-folders (boolean :optional t))
  (:configuration (boolean :optional t))
  (:semantic-tokens (or semantic-tokens-workspace-client-capabilities null))
  (:code-lens (or code-lens-workspace-client-capabilities null))
  (:file-operations (or file-operations-client-capabilities null))
  (:inline-value (or inline-value-workspace-client-capabilities null))
  (:inlay-hint (or inlay-hint-workspace-client-capabilities null))
  (:diagnostics (or diagnostic-workspace-client-capabilities null)))

(define-message window-client-capabilities ()
  (:work-done-progress (boolean :optional t))
  (:show-message (or show-message-request-client-capabilities null))
  (:show-document (or show-document-client-capabilities null)))

(define-message general-client-capabilities ()
  (:stale-request-support (or stale-request-support-client-capabilities null))
  (:regular-expressions (or regular-expressions-client-capabilities null))
  (:markdown (or markdown-client-capabilities null))
  (:position-encodings (or (vector position-encoding-kind) null)))

(define-message stale-request-support-client-capabilities ()
  (:cancel boolean)
  (:retry-on-content-modified (vector string)))

(define-message client-capabilities ()
  (:general (or general-client-capabilities null))
  (:experimental lsp-any))

(define-message general-client-capabilities ()
  (:stale-request-support (or stale-request-support-client-capabilities null))
  (:regular-expressions (or regular-expressions-client-capabilities null))
  (:markdown (or markdown-client-capabilities null))
  (:position-encodings (or (vector position-encoding-kind) null)))

(define-message stale-request-support-client-capabilities ()
  (:cancel boolean)
  (:retry-on-content-modified (vector string)))

(define-message initialize-result ()
  (:capabilities server-capabilities)
  (:server-info (or server-info null)))

(define-message server-info ()
  (:name string)
  (:version (or string null)))

(define-message initialize-error ()
  (:retry boolean))

(define-message server-capabilities ()
  (:position-encoding (or position-encoding-kind null))
  (:text-document-sync (or text-document-sync-options text-document-sync-kind))
  (:notebook-document-sync (or notebook-document-sync-options notebook-document-sync-registration-options))
  (:completion-provider (or completion-options null))
  (:hover-provider (or boolean hover-options))
  (:signature-help-provider (or signature-help-options null))
  (:declaration-provider (or boolean declaration-options declaration-registration-options))
  (:definition-provider (or boolean definition-options))
  (:type-definition-provider (or boolean type-definition-options type-definition-registration-options))
  (:implementation-provider (or boolean implementation-options implementation-registration-options))
  (:references-provider (or boolean reference-options))
  (:document-highlight-provider (or boolean document-highlight-options))
  (:document-symbol-provider (or boolean document-symbol-options))
  (:code-action-provider (or boolean code-action-options))
  (:code-lens-provider (or code-lens-options null))
  (:document-link-provider (or document-link-options null))
  (:color-provider (or boolean document-color-options document-color-registration-options))
  (:document-formatting-provider (or boolean document-formatting-options))
  (:document-range-formatting-provider (or boolean document-range-formatting-options))
  (:document-on-type-formatting-provider (or document-on-type-formatting-options null))
  (:rename-provider (or boolean rename-options))
  (:folding-range-provider (or boolean folding-range-options folding-range-registration-options))
  (:execute-command-provider (or execute-command-options null))
  (:selection-range-provider (or boolean selection-range-options selection-range-registration-options))
  (:linked-editing-range-provider (or boolean linked-editing-range-options linked-editing-range-registration-options))
  (:call-hierarchy-provider (or boolean call-hierarchy-options call-hierarchy-registration-options))
  (:semantic-tokens-provider (or semantic-tokens-options semantic-tokens-registration-options))
  (:moniker-provider (or boolean moniker-options moniker-registration-options))
  (:type-hierarchy-provider (or boolean type-hierarchy-options type-hierarchy-registration-options))
  (:inline-value-provider (or boolean inline-value-options inline-value-registration-options))
  (:inlay-hint-provider (or boolean inlay-hint-options inlay-hint-registration-options))
  (:diagnostic-provider (or diagnostic-options diagnostic-registration-options))
  (:workspace-symbol-provider (or boolean workspace-symbol-options))
  (:workspace (or workspace-server-capabilities null))
  (:experimental lsp-any))

(define-message workspace-server-capabilities ()
  (:workspace-folders (or workspace-folders-server-capabilities null))
  (:file-operations (or file-operations-server-capabilities null)))

(define-message workspace-folders-server-capabilities ()
  (:supported boolean)
  (:change-notifications (or string null)))

(define-message file-operations-server-capabilities ()
  (:did-create (or file-operation-registration-options null))
  (:will-create (or file-operation-registration-options null))
  (:did-rename (or file-operation-registration-options null))
  (:will-rename (or file-operation-registration-options null))
  (:did-delete (or file-operation-registration-options null))
  (:will-delete (or file-operation-registration-options null)))

(define-message initialized-params ()
  )

(define-message registration ()
  (:id string)
  (:method string)
  (:register-options lsp-any))

(define-message registration-params ()
  (:registrations (vector registration)))

(define-message static-registration-options ()
  (:id (or string null)))

(define-message text-document-registration-options ()
  (:document-selector (or document-selector null)))

(define-message unregistration ()
  (:id string)
  (:method string))

(define-message unregistration-params ()
  (:unregisterations (vector unregistration)))

(define-message set-trace-params ()
  (:value trace-value))

(define-message log-trace-params ()
  (:message string)
  (:verbose (or string null)))

(define-message text-document-sync-kind ()
  (:none (eql 0))
  (:full (eql 1))
  (:incremental (eql 2)))

(defconstant +text-document-sync-kind-none+ 0)
(defconstant +text-document-sync-kind-full+ 1)
(defconstant +text-document-sync-kind-incremental+ 2)

(define-message text-document-sync-options ()
  (:open-close (boolean :optional t))
  (:change (or text-document-sync-kind null)))

(define-message did-open-text-document-params ()
  (:text-document text-document-item))

(define-message text-document-change-registration-options (text-document-registration-options)
  (:sync-kind text-document-sync-kind))

(define-message did-change-text-document-params ()
  (:text-document versioned-text-document-identifier)
  (:content-changes (vector text-document-content-change-event)))

(define-message text-document-content-change-event ()
  (:range (or range null))
  (:range-length (or uinteger null))
  (:text string))

(define-message will-save-text-document-params ()
  (:text-document text-document-identifier)
  (:reason text-document-save-reason))

(define-message text-document-save-reason ()
  (:manual (eql 1))
  (:after-delay (eql 2))
  (:focus-out (eql 3)))

(defconstant +text-document-save-reason-manual+ 1)
(defconstant +text-document-save-reason-after-delay+ 2)
(defconstant +text-document-save-reason-focus-out+ 3)

(define-message save-options ()
  (:include-text (boolean :optional t)))

(define-message text-document-save-registration-options (text-document-registration-options)
  (:include-text (boolean :optional t)))

(define-message did-save-text-document-params ()
  (:text-document text-document-identifier)
  (:text (or string null)))

(define-message did-close-text-document-params ()
  (:text-document text-document-identifier))

(define-message text-document-sync-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:will-save (boolean :optional t))
  (:will-save-wait-until (boolean :optional t))
  (:did-save (boolean :optional t)))

(define-message notebook-document ()
  (:uri uri)
  (:notebook-type string)
  (:version integer)
  (:metadata (or lsp-object null))
  (:cells (vector notebook-cell)))

(define-message notebook-cell ()
  (:kind notebook-cell-kind)
  (:document document-uri)
  (:metadata (or lsp-object null))
  (:execution-summary (or execution-summary null)))

(define-message notebook-cell-kind ()
  (:markup (eql 1))
  (:code (eql 2)))

(defconstant +notebook-cell-kind-markup+ 1)
(defconstant +notebook-cell-kind-code+ 2)

(define-message execution-summary ()
  (:execution-order uinteger)
  (:success (boolean :optional t)))

(define-message notebook-cell-text-document-filter ()
  (:notebook (or string notebook-document-filter))
  (:language (or string null)))

(define-message notebook-document-filter ()
  (:notebook-type string)
  (:scheme (or string null))
  (:pattern (or string null)))

(define-message notebook-document-sync-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:execution-summary-support (boolean :optional t)))

(define-message notebook-document-sync-options ()
  (:notebook-selector (vector notebook-selector-item))
  (:save (boolean :optional t)))

(define-message notebook-selector-item ()
  (:notebook (or string notebook-document-filter))
  (:cells (or (vector (list :language string)) null)))

(define-message notebook-document-sync-registration-options (notebook-document-sync-options static-registration-options)
  )

(define-message did-open-notebook-document-params ()
  (:notebook-document notebook-document)
  (:cell-text-documents (vector text-document-item)))

(define-message did-change-notebook-document-params ()
  (:notebook-document versioned-notebook-document-identifier)
  (:change notebook-document-change-event))

(define-message versioned-notebook-document-identifier ()
  (:version integer)
  (:uri uri))

(define-message notebook-document-change-event ()
  (:metadata (or lsp-object null))
  (:cells (or notebook-cells-change null)))

(define-message notebook-cells-change ()
  (:structure (or notebook-cell-array-change null))
  (:data (or (vector notebook-cell) null))
  (:text-content (or (vector text-content-change) null)))

(define-message notebook-cell-array-change ()
  (:start uinteger)
  (:delete-count uinteger)
  (:cells (or (vector notebook-cell) null)))

(define-message text-content-change ()
  (:document versioned-text-document-identifier)
  (:changes (vector text-document-content-change-event)))

(define-message did-save-notebook-document-params ()
  (:notebook-document notebook-document-identifier))

(define-message did-close-notebook-document-params ()
  (:notebook-document notebook-document-identifier)
  (:cell-text-documents (vector text-document-identifier)))

(define-message notebook-document-identifier ()
  (:uri uri))

(define-message declaration-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:link-support (boolean :optional t)))

(define-message declaration-options (work-done-progress-options)
  )

(define-message declaration-registration-options (declaration-options text-document-registration-options static-registration-options)
  )

(define-message declaration-params (text-document-position-params work-done-progress-params partial-result-params)
  )

(define-message definition-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:link-support (boolean :optional t)))

(define-message definition-options (work-done-progress-options)
  )

(define-message definition-registration-options (text-document-registration-options definition-options)
  )

(define-message definition-params (text-document-position-params work-done-progress-params partial-result-params)
  )

(define-message type-definition-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:link-support (boolean :optional t)))

(define-message type-definition-options (work-done-progress-options))

(define-message type-definition-registration-options
    (text-document-registration-options type-definition-options static-registration-options))

(define-message type-definition-params (text-document-position-params work-done-progress-params partial-result-params)
  )

(define-message implementation-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:link-support (boolean :optional t)))

(define-message implementation-options (work-done-progress-options)
  )

(define-message implementation-registration-options (text-document-registration-options implementation-options static-registration-options)
  )

(define-message implementation-params (text-document-position-params work-done-progress-params partial-result-params)
  )

(define-message reference-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

(define-message reference-options (work-done-progress-options)
  )

(define-message reference-registration-options (text-document-registration-options reference-options)
  )

(define-message reference-params (text-document-position-params work-done-progress-params partial-result-params)
  (:context reference-context))

(define-message reference-context ()
  (:include-declaration boolean))

(define-message call-hierarchy-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

(define-message call-hierarchy-options (work-done-progress-options)
  )

(define-message call-hierarchy-registration-options (text-document-registration-options call-hierarchy-options static-registration-options)
  )

(define-message call-hierarchy-prepare-params (text-document-position-params work-done-progress-params)
  )

(define-message call-hierarchy-item ()
  (:name string)
  (:kind symbol-kind)
  (:tags (or (vector symbol-tag) null))
  (:detail (or string null))
  (:uri document-uri)
  (:range range)
  (:selection-range range)
  (:data (or lsp-any null)))

(define-message call-hierarchy-incoming-calls-params (work-done-progress-params partial-result-params)
  (:item call-hierarchy-item))

(define-message call-hierarchy-incoming-call ()
  (:from call-hierarchy-item)
  (:from-ranges (vector range)))

(define-message call-hierarchy-outgoing-calls-params (work-done-progress-params partial-result-params)
  (:item call-hierarchy-item))

(define-message call-hierarchy-outgoing-call ()
  (:to call-hierarchy-item)
  (:from-ranges (vector range)))

(define-message type-hierarchy-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

(define-message type-hierarchy-options (work-done-progress-options)
  )

(define-message type-hierarchy-registration-options (text-document-registration-options type-hierarchy-options static-registration-options)
  )

(define-message type-hierarchy-prepare-params (text-document-position-params work-done-progress-params)
  )

(define-message type-hierarchy-item ()
  (:name string)
  (:kind symbol-kind)
  (:tags (or (vector symbol-tag) null))
  (:detail (or string null))
  (:uri document-uri)
  (:range range)
  (:selection-range range)
  (:data (or lsp-any null)))

(define-message type-hierarchy-supertypes-params (work-done-progress-params partial-result-params)
  (:item type-hierarchy-item))

(define-message type-hierarchy-subtypes-params (work-done-progress-params partial-result-params)
  (:item type-hierarchy-item))

(define-message document-highlight-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

(define-message document-highlight-options (work-done-progress-options)
  )

(define-message document-highlight-registration-options (text-document-registration-options document-highlight-options)
  )

(define-message document-highlight-params (text-document-position-params work-done-progress-params partial-result-params)
  )

(define-message document-highlight ()
  (:range range)
  (:kind (or document-highlight-kind null)))

(defconstant +document-highlight-kind-text+ 1)
(defconstant +document-highlight-kind-read+ 2)
(defconstant +document-highlight-kind-write+ 3)

(define-message document-link-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:tooltip-support (boolean :optional t)))

(define-message document-link-options (work-done-progress-options)
  (:resolve-provider (boolean :optional t)))

(define-message document-link-registration-options (text-document-registration-options document-link-options)
  )

(define-message document-link-params (work-done-progress-params partial-result-params)
  (:text-document text-document-identifier))

(define-message document-link ()
  (:range range)
  (:target (or uri null))
  (:tooltip (or string null))
  (:data (or lsp-any null)))

(define-message hover-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:content-format (or (vector markup-kind) null)))

(define-message hover-options (work-done-progress-options)
  )

(define-message hover-registration-options (text-document-registration-options hover-options)
  )

(define-message hover-params (text-document-position-params work-done-progress-params)
  )

(define-message hover ()
  (:contents (or marked-string marked-string-vector markup-content))
  (:range (or range null)))

(define-message marked-string ()
  (:language string)
  (:value string))

(define-message code-lens-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

(define-message code-lens-options (work-done-progress-options)
  (:resolve-provider (boolean :optional t)))

(define-message code-lens-registration-options (text-document-registration-options code-lens-options)
  )

(define-message code-lens-params (work-done-progress-params partial-result-params)
  (:text-document text-document-identifier))

(define-message code-lens ()
  (:range range)
  (:command (or command null))
  (:data (or lsp-any null)))

(define-message code-lens-workspace-client-capabilities ()
  (:refresh-support (boolean :optional t)))

(define-message folding-range-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:range-limit (or uinteger null))
  (:line-folding-only (boolean :optional t))
  (:folding-range-kind (or folding-range-kind-options null))
  (:folding-range (or folding-range-options null)))

(define-message folding-range-options (work-done-progress-options)
  )

(define-message folding-range-registration-options (text-document-registration-options folding-range-options static-registration-options)
  )

(define-message folding-range-params (work-done-progress-params partial-result-params)
  (:text-document text-document-identifier))

(define-message folding-range-kind-options ()
  (:value-set (or (vector folding-range-kind) null)))

(define-message folding-range-options ()
  (:collapsed-text (boolean :optional t)))

(defvar *folding-range-kind-comment* "comment")
(defvar *folding-range-kind-imports* "imports")
(defvar *folding-range-kind-region* "region")


(define-message folding-range-kind ()
  (:value string))

(define-message folding-range ()
  (:start-line uinteger)
  (:start-character (or uinteger null))
  (:end-line uinteger)
  (:end-character (or uinteger null))
  (:kind (or folding-range-kind null))
  (:collapsed-text (or string null)))

(define-message selection-range-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

(define-message selection-range-options (work-done-progress-options)
  )

(define-message selection-range-registration-options (selection-range-options text-document-registration-options static-registration-options)
  )

(define-message selection-range-params (work-done-progress-params partial-result-params)
  (:text-document text-document-identifier)
  (:positions (vector position)))

(define-message selection-range ()
  (:range range)
  (:parent (or selection-range null)))

(define-message document-symbol-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:symbol-kind (or symbol-kind-options null))
  (:hierarchical-document-symbol-support (boolean :optional t))
  (:tag-support (or symbol-tag-options null))
  (:label-support (boolean :optional t)))

(define-message document-symbol-options (work-done-progress-options)
  (:label (or string null)))

(define-message document-symbol-registration-options (document-symbol-options text-document-registration-options)
  )

(define-message document-symbol-params (work-done-progress-params partial-result-params)
  (:text-document text-document-identifier))

(define-message symbol-kind ()
  (:value (member 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26)))

(define-message symbol-tag ()
  (:value (member 1)))

(define-message document-symbol ()
  (:name string)
  (:detail (or string null))
  (:kind symbol-kind)
  (:tags (or (vector symbol-tag) null))
  (:deprecated (boolean :optional t))
  (:range range)
  (:selection-range range)
  (:children (or (vector document-symbol) null)))

(define-message symbol-information ()
  (:name string)
  (:kind symbol-kind)
  (:tags (or (vector symbol-tag) null))
  (:deprecated (boolean :optional t))
  (:location location)
  (:container-name (or string null)))

(define-message semantic-token-types ()
  (:value (member "namespace" "type" "class" "enum" "interface" "struct" "typeParameter" "parameter" "variable" "property" "enumMember" "event" "function" "method" "macro" "keyword" "modifier" "comment" "string" "number" "regexp" "operator" "decorator")))

(define-message semantic-token-modifiers ()
  (:value (member "declaration" "definition" "readonly" "static" "deprecated" "abstract" "async" "modification" "documentation" "defaultLibrary")))

(define-message token-format ()
  (:value (member "relative")))

(define-message semantic-tokens-legend ()
  (:token-types (vector string))
  (:token-modifiers (vector string)))

(define-message semantic-tokens-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:requests semantic-tokens-requests)
  (:token-types (vector string))
  (:token-modifiers (vector string))
  (:formats (vector token-format))
  (:overlapping-token-support (boolean :optional t))
  (:multiline-token-support (boolean :optional t))
  (:server-cancel-support (boolean :optional t))
  (:augments-syntax-tokens (boolean :optional t)))

(define-message semantic-tokens-requests ()
  (:range (boolean :optional t))
  (:full (boolean :optional t)))

(define-message semantic-tokens-options (work-done-progress-options)
  (:legend semantic-tokens-legend)
  (:range (boolean :optional t))
  (:full (boolean :optional t)))

(define-message semantic-tokens-registration-options
    text-document-registration-options semantic-tokens-options
  static-registration-options)  ())

(define-message semantic-tokens-params (work-done-progress-params partial-result-params)
  (:text-document text-document-identifier))

(define-message semantic-tokens ()
  (:result-id (or string null))
  (:data (vector uinteger)))

(define-message semantic-tokens-partial-result ()
  (:data (vector uinteger)))

(define-message semantic-tokens-delta-params (work-done-progress-params partial-result-params)
  (:text-document text-document-identifier)
  (:previous-result-id string))

(define-message semantic-tokens-delta ()
  (:result-id (or string null))
  (:edits (vector semantic-tokens-edit)))

(define-message semantic-tokens-edit ()
  (:start uinteger)
  (:delete-count uinteger)
  (:data (or (vector uinteger) null)))

(define-message semantic-tokens-delta-partial-result ()
  (:edits (vector semantic-tokens-edit)))

(define-message semantic-tokens-range-params (work-done-progress-params partial-result-params)
  (:text-document text-document-identifier)
  (:range range))

(define-message semantic-tokens-workspace-client-capabilities ()
  (:refresh-support (boolean :optional t)))

(define-message inlay-hint-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:resolve-support (or inlay-hint-resolve-support null)))

(define-message inlay-hint-options (work-done-progress-options)
  (:resolve-provider (boolean :optional t)))

(define-message inlay-hint-registration-options
    inlay-hint-options text-document-registration-options static-registration-options)  ())

(define-message inlay-hint-params (work-done-progress-params)
  (:text-document text-document-identifier)
  (:range range))

(define-message inlay-hint ()
  (:position position)
  (:label (or string (vector inlay-hint-label-part)))
  (:kind (or inlay-hint-kind null))
  (:text-edits (or (vector text-edit) null))
  (:tooltip (or string markup-content null))
  (:padding-left (boolean :optional t))
  (:padding-right (boolean :optional t))
  (:data (or lsp-any null)))

(define-message inlay-hint-label-part ()
  (:value string)
  (:tooltip (or string markup-content null))
  (:location (or location null))
  (:command (or command null)))

(define-message inlay-hint-workspace-client-capabilities ()
  (:refresh-support (boolean :optional t)))

(define-message inline-value-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

(define-message inline-value-options (work-done-progress-options)
  )

(define-message inline-value-registration-options
    inline-value-options text-document-registration-options static-registration-options)  ())

(define-message inline-value-params (work-done-progress-params)
  (:text-document text-document-identifier)
  (:range range)
  (:context inline-value-context))

(define-message inline-value-context ()
  (:frame-id integer)
  (:stopped-location range))

(define-message inline-value-text ()
  (:range range)
  (:text string))

(define-message inline-value-variable-lookup ()
  (:range range)
  (:variable-name (or string null))
  (:case-sensitive-lookup boolean))

(define-message inline-value-evaluatable-expression ()
  (:range range)
  (:expression (or string null)))

(define-message inline-value-workspace-client-capabilities ()
  (:refresh-support (boolean :optional t)))
(define-message moniker-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

(define-message moniker-options (work-done-progress-options)
  )

(define-message moniker-registration-options
    text-document-registration-options moniker-options static-registration-options)  ())

(define-message moniker-params (text-document-position-params work-done-progress-params partial-result-params)
  )

(define-message uniqueness-level ()
  (:document (eql :document))
  (:project (eql :project))
  (:group (eql :group))
  (:scheme (eql :scheme))
  (:global (eql :global)))

(define-message moniker-kind ()
  (:import (eql :import))
  (:export (eql :export))
  (:local (eql :local)))

(define-message moniker ()
  (:scheme string)
  (:identifier string)
  (:unique uniqueness-level)
  (:kind (or moniker-kind null)))

(define-message completion-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:completion-item completion-item-capabilities)
  (:completion-item-kind completion-item-kind-capabilities)
  (:context-support (boolean :optional t))
  (:insert-text-mode insert-text-mode)
  (:completion-list completion-list-capabilities))

(define-message completion-options (work-done-progress-options)
  (:trigger-characters (vector string))
  (:all-commit-characters (vector string))
  (:resolve-provider (boolean :optional t))
  (:completion-item completion-item-options))

(define-message completion-registration-options
    text-document-registration-options completion-options static-registration-options)  ())

(define-message completion-params (text-document-position-params work-done-progress-params partial-result-params)
  (:context (or completion-context null)))

(define-message completion-trigger-kind ()
  (:invoked (eql 1))
  (:trigger-character (eql 2))
  (:trigger-for-incomplete-completions (eql 3)))

(define-message completion-context ()
  (:trigger-kind completion-trigger-kind)
  (:trigger-character (or string null)))

(define-message completion-list ()
  (:is-incomplete boolean)
  (:item-defaults (or completion-list-item-defaults null))
  (:items (vector completion-item)))

(define-message completion-list-item-defaults ()
  (:commit-characters (vector string))
  (:edit-range (or range null))
  (:insert-text-format (or insert-text-format null))
  (:insert-text-mode (or insert-text-mode null))
  (:data (or lsp-any null)))

(define-message insert-text-format ()
  (:plain-text (eql 1))
  (:snippet (eql 2)))

(define-message completion-item-tag ()
  (:deprecated (eql 1)))

(define-message insert-replace-edit ()
  (:new-text string)
  (:insert range)
  (:replace range))

(define-message insert-text-mode ()
  (:as-is (eql 1))
  (:adjust-indentation (eql 2)))

(define-message completion-item-label-details ()
  (:detail (or string null))
  (:description (or string null)))

(define-message completion-item ()
  (:label string)
  (:label-details (or completion-item-label-details null))
  (:kind (or completion-item-kind null))
  (:tags (vector completion-item-tag))
  (:detail (or string null))
  (:documentation (or string markup-content))
  (:deprecated (boolean :optional t))
  (:preselect (boolean :optional t))
  (:sort-text (or string null))
  (:filter-text (or string null))
  (:insert-text (or string null))
  (:insert-text-format (or insert-text-format null))
  (:insert-text-mode (or insert-text-mode null))
  (:text-edit (or text-edit insert-replace-edit))
  (:text-edit-text (or string null))
  (:additional-text-edits (vector text-edit))
  (:commit-characters (vector string))
  (:command (or command null))
  (:data (or lsp-any null)))

(define-message completion-item-kind ()
  (:text (eql 1))
  (:method (eql 2))
  (:function (eql 3))
  (:constructor (eql 4))
  (:field (eql 5))
  (:variable (eql 6))
  (:class (eql 7))
  (:interface (eql 8))
  (:module (eql 9))
  (:property (eql 10))
  (:unit (eql 11))
  (:value (eql 12))
  (:enum (eql 13))
  (:keyword (eql 14))
  (:snippet (eql 15))
  (:color (eql 16))
  (:file (eql 17))
  (:reference (eql 18))
  (:folder (eql 19))
  (:enum-member (eql 20))
  (:constant (eql 21))
  (:struct (eql 22))
  (:event (eql 23))
  (:operator (eql 24))
  (:type-parameter (eql 25)))

(define-message publish-diagnostics-client-capabilities ()
  (:related-information (boolean :optional t))
  (:tag-support (or (vector diagnostic-tag) null))
  (:version-support (boolean :optional t))
  (:code-description-support (boolean :optional t))
  (:data-support (boolean :optional t)))

(define-message publish-diagnostics-params ()
  (:uri document-uri)
  (:version (integer :optional t))
  (:diagnostics (vector diagnostic)))

(define-message diagnostic-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:related-document-support (boolean :optional t)))

(define-message diagnostic-options (work-done-progress-options)
  (:identifier (or string null))
  (:inter-file-dependencies boolean)
  (:workspace-diagnostics boolean))

(define-message diagnostic-registration-options
    text-document-registration-options diagnostic-options static-registration-options)  ())

(define-message document-diagnostic-params (work-done-progress-params partial-result-params)
  (:text-document text-document-identifier)
  (:identifier (or string null))
  (:previous-result-id (or string null)))

(define-message full-document-diagnostic-report ()
  (:kind (eql 'full))
  (:result-id (or string null))
  (:items (vector diagnostic)))

(define-message unchanged-document-diagnostic-report ()
  (:kind (eql 'unchanged))
  (:result-id string))

(define-message related-full-document-diagnostic-report (full-document-diagnostic-report)
  (:related-documents (hash-table string (or full-document-diagnostic-report unchanged-document-diagnostic-report))))

(define-message related-unchanged-document-diagnostic-report (unchanged-document-diagnostic-report)
  (:related-documents (hash-table string (or full-document-diagnostic-report unchanged-document-diagnostic-report))))

(define-message document-diagnostic-report-partial-result ()
  (:related-documents (hash-table string (or full-document-diagnostic-report unchanged-document-diagnostic-report))))

(define-message diagnostic-server-cancellation-data ()
  (:retrigger-request boolean))

(define-message workspace-diagnostic-params (work-done-progress-params partial-result-params)
  (:identifier (or string null))
  (:previous-result-ids (vector previous-result-id)))

(define-message previous-result-id ()
  (:uri document-uri)
  (:value string))

(define-message workspace-diagnostic-report ()
  (:items (vector workspace-document-diagnostic-report)))

(define-message workspace-full-document-diagnostic-report (full-document-diagnostic-report)
  (:uri document-uri)
  (:version (integer :optional t)))

(define-message workspace-unchanged-document-diagnostic-report (unchanged-document-diagnostic-report)
  (:uri document-uri)
  (:version (integer :optional t)))

(define-message workspace-document-diagnostic-report ()
  (:type (or workspace-full-document-diagnostic-report workspace-unchanged-document-diagnostic-report)))

(define-message workspace-diagnostic-report-partial-result ()
  (:items (vector workspace-document-diagnostic-report)))

(define-message diagnostic-workspace-client-capabilities ()
  (:refresh-support (boolean :optional t)))

(define-message signature-help-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:signature-information (or signature-information-options null))
  (:context-support (boolean :optional t)))

(define-message signature-help-options (work-done-progress-options)
  (:trigger-characters (or (vector string) null))
  (:retrigger-characters (or (vector string) null)))

(define-message signature-help-registration-options (text-document-registration-options signature-help-options)
  )

(define-message signature-help-params (text-document-position-params work-done-progress-params)
  (:context (or signature-help-context null)))

(define-message signature-help-trigger-kind ()
  (:invoked (eql 1))
  (:trigger-character (eql 2))
  (:content-change (eql 3)))

(define-message signature-help-context ()
  (:trigger-kind signature-help-trigger-kind)
  (:trigger-character (or string null))
  (:is-retrigger boolean)
  (:active-signature-help (or signature-help null)))

(define-message signature-help ()
  (:signatures (vector signature-information))
  (:active-signature (or uinteger null))
  (:active-parameter (or uinteger null)))

(define-message signature-information ()
  (:label string)
  (:documentation (or string markup-content))
  (:parameters (or (vector parameter-information) null))
  (:active-parameter (or uinteger null)))

(define-message parameter-information ()
  (:label (or string (vector uinteger)))
  (:documentation (or string markup-content)))

(define-message code-action-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:code-action-literal-support (or code-action-literal-options null))
  (:is-preferred-support (boolean :optional t))
  (:disabled-support (boolean :optional t))
  (:data-support (boolean :optional t))
  (:resolve-support (or code-action-resolve-options null))
  (:honors-change-annotations (boolean :optional t)))

(define-message code-action-options (work-done-progress-options)
  (:code-action-kinds (or (vector code-action-kind) null))
  (:resolve-provider (boolean :optional t)))

(define-message code-action-registration-options (text-document-registration-options code-action-options)
  )

(define-message code-action-params (work-done-progress-params partial-result-params)
  (:text-document text-document-identifier)
  (:range range)
  (:context code-action-context))


(define-message code-action-params (work-done-progress-params partial-result-params)
  (:text-document text-document-identifier)
  (:range range)
  (:context code-action-context))

(define-message code-action-kind ()
  (:value string))

(defparameter +code-action-kind-empty+ (make-instance 'code-action-kind :value ""))
(defparameter +code-action-kind-quick-fix+ (make-instance 'code-action-kind :value "quickfix"))
(defparameter +code-action-kind-refactor+ (make-instance 'code-action-kind :value "refactor"))
(defparameter +code-action-kind-refactor-extract+ (make-instance 'code-action-kind :value "refactor.extract"))
(defparameter +code-action-kind-refactor-inline+ (make-instance 'code-action-kind :value "refactor.inline"))
(defparameter +code-action-kind-refactor-rewrite+ (make-instance 'code-action-kind :value "refactor.rewrite"))
(defparameter +code-action-kind-source+ (make-instance 'code-action-kind :value "source"))
(defparameter +code-action-kind-source-organize-imports+ (make-instance 'code-action-kind :value "source.organizeImports"))
(defparameter +code-action-kind-source-fix-all+ (make-instance 'code-action-kind :value "source.fixAll"))

(define-message code-action-context ()
  (:diagnostics (vector diagnostic))
  (:only (or (vector code-action-kind) null))
  (:trigger-kind (or code-action-trigger-kind null)))

(define-message code-action-trigger-kind ()
  (:value (or 1 2)))

(defparameter +code-action-trigger-kind-invoked+ (make-instance 'code-action-trigger-kind :value 1))
(defparameter +code-action-trigger-kind-automatic+ (make-instance 'code-action-trigger-kind :value 2))

(define-message code-action ()
  (:title string)
  (:kind (or code-action-kind null))
  (:diagnostics (or (vector diagnostic) null))
  (:is-preferred (boolean :optional t))
  (:disabled (or (cons string string) null))
  (:edit (or workspace-edit null))
  (:command (or command null))
  (:data (or lsp-any null)))

(define-message document-color-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

(define-message document-color-options (work-done-progress-options)
  )

(define-message document-color-registration-options (text-document-registration-options static-registration-options document-color-options)
  )

(define-message document-color-params (work-done-progress-params partial-result-params)
  (:text-document text-document-identifier))

(define-message color-information ()
  (:range range)
  (:color color))

(define-message color ()
  (:red decimal)
  (:green decimal)
  (:blue decimal)
  (:alpha decimal))

(define-message color-presentation-params (work-done-progress-params partial-result-params)
  (:text-document text-document-identifier)
  (:color color)
  (:range range))

(define-message color-presentation ()
  (:label string)
  (:text-edit (or text-edit null))
  (:additional-text-edits (or (vector text-edit) null)))

(define-message document-formatting-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

(define-message document-formatting-options (work-done-progress-options)
  )

(define-message document-formatting-registration-options (text-document-registration-options document-formatting-options)
  )

(define-message document-formatting-params (work-done-progress-params)
  (:text-document text-document-identifier)
  (:options formatting-options))

(define-message formatting-options ()
  (:tab-size uinteger)
  (:insert-spaces boolean)
  (:trim-trailing-whitespace (boolean :optional t))
  (:insert-final-newline (boolean :optional t))
  (:trim-final-newlines (boolean :optional t))
  (:other-options (or (vector (cons string (or boolean integer string))) null)))

(define-message document-range-formatting-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

(define-message document-range-formatting-options (work-done-progress-options)
  )

(define-message document-range-formatting-registration-options (text-document-registration-options document-range-formatting-options)
  )

(define-message document-range-formatting-params (work-done-progress-params)
  (:text-document text-document-identifier)
  (:range range)
  (:options formatting-options))

(define-message document-on-type-formatting-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

(define-message document-on-type-formatting-options ()
  (:first-trigger-character string)
  (:more-trigger-character (or (vector string) null)))

(define-message document-on-type-formatting-registration-options (text-document-registration-options document-on-type-formatting-options)
  )

(define-message document-on-type-formatting-params ()
  (:text-document text-document-identifier)
  (:position position)
  (:ch string)
  (:options formatting-options))

(defparameter +prepare-support-default-behavior-identifier+ 1)

(define-message prepare-support-default-behavior ()
  (:value (or 1)))

(define-message rename-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:prepare-support (boolean :optional t))
  (:prepare-support-default-behavior (or prepare-support-default-behavior null))
  (:honors-change-annotations (boolean :optional t)))

(define-message rename-options (work-done-progress-options)
  (:prepare-provider (boolean :optional t)))

(define-message rename-registration-options (text-document-registration-options rename-options)
  )

(define-message rename-params (text-document-position-params work-done-progress-params)
  (:new-name string))

(define-message prepare-rename-params (text-document-position-params work-done-progress-params)
  )

(define-message linked-editing-range-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

(define-message linked-editing-range-options (work-done-progress-options)
  )

(define-message linked-editing-range-registration-options (text-document-registration-options linked-editing-range-options static-registration-options)
  )

(define-message linked-editing-range-params (text-document-position-params work-done-progress-params)
  )

(define-message linked-editing-ranges ()
  (:ranges (vector range))
  (:word-pattern (or string null)))

(define-message workspace-symbol-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:symbol-kind (or (cons symbol-kind (vector symbol-kind)) null))
  (:tag-support (or (cons symbol-tag (vector symbol-tag)) null))
  (:resolve-support (or (cons string (vector string)) null)))

(define-message workspace-symbol-options (work-done-progress-options)
  (:resolve-provider (boolean :optional t)))

(define-message workspace-symbol-registration-options (workspace-symbol-options)
  )

(define-message workspace-symbol-params (work-done-progress-params partial-result-params)
  (:query string))

(define-message workspace-symbol ()
  (:name string)
  (:kind symbol-kind)
  (:tags (or (vector symbol-tag) null))
  (:container-name (or string null))
  (:location (or location (cons document-uri null)))
  (:data (or lsp-any null)))

(define-message configuration-params ()
  (:items (vector configuration-item)))

(define-message configuration-item ()
  (:scope-uri (or uri null))
  (:section (or string null)))

(define-message did-change-configuration-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

(define-message workspace-folders-server-capabilities ()
  (:supported (boolean :optional t))
  (:change-notifications (or (or string boolean) null)))

(define-message did-change-workspace-folders-params ()
  (:event workspace-folders-change-event))

(define-message workspace-folders-change-event ()
  (:added (vector workspace-folder))
  (:removed (vector workspace-folder)))

(define-message file-operation-registration-options ()
  (:filters (vector file-operation-filter)))

(define-message file-operation-pattern ()
  (:glob string)
  (:matches (or file-operation-pattern-kind null))
  (:options (or file-operation-pattern-options null)))

(define-message file-operation-pattern-options ()
  (:ignore-case (boolean :optional t)))

(define-message file-operation-filter ()
  (:scheme (or string null))
  (:pattern file-operation-pattern))

(define-message create-files-params ()
  (:files (vector file-create)))

(define-message file-create ()
  (:uri string))

(define-message rename-files-params ()
  (:files (vector file-rename)))

(define-message file-rename ()
  (:old-uri string)
  (:new-uri string))

(define-message delete-files-params ()
  (:files (vector file-delete)))

(define-message file-delete ()
  (:uri string))

(define-message did-change-watched-files-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:relative-pattern-support (boolean :optional t)))

(define-message did-change-watched-files-registration-options ()
  (:watchers (vector file-system-watcher)))

(define-message file-system-watcher ()
  (:glob-pattern glob-pattern)
  (:kind (or watch-kind null)))

(define-message relative-pattern ()
  (:base-uri (or workspace-folder uri))
  (:pattern pattern))

(define-message did-change-watched-files-params ()
  (:changes (vector file-event)))

(define-message file-event ()
  (:uri document-uri)
  (:type file-change-type))

(define-message execute-command-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

(define-message execute-command-options (work-done-progress-options)
  (:commands (vector string)))

(define-message execute-command-registration-options (execute-command-options)
  )

(define-message execute-command-params (work-done-progress-params)
  (:command string)
  (:arguments (or (vector lsp-any) null)))

(define-message apply-workspace-edit-params ()
  (:label (or string null))
  (:edit workspace-edit))

(define-message apply-workspace-edit-result ()
  (:applied boolean)
  (:failure-reason (or string null))
  (:failed-change (or uinteger null)))

(define-message show-message-params ()
  (:type message-type)
  (:message string))

(define-message show-message-request-client-capabilities ()
  (:message-action-item (or message-action-item null)))

(define-message show-message-request-params ()
  (:type message-type)
  (:message string)
  (:actions (or (vector message-action-item) null)))

(define-message message-action-item ()
  (:title string))

(define-message show-document-client-capabilities ()
  (:support boolean))

(define-message show-document-params ()
  (:uri uri)
  (:external (boolean :optional t))
  (:take-focus (boolean :optional t))
  (:selection (or range null)))

(define-message show-document-result ()
  (:success boolean))

(define-message log-message-params ()
  (:type message-type)
  (:message string))

(define-message work-done-progress-create-params ()
  (:token progress-token))

(define-message work-done-progress-cancel-params ()
  (:token progress-token))
