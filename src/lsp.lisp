;;;; LSP message structure definitions, uses message.lisp

(in-package #:coalton-mode)

(define-primitive t)

(define-primitive string)

(define-primitive boolean)

(define-primitive integer)

(deftype uinteger () '(integer 0 #.(1- (expt 2 31))))

(define-primitive uinteger)

(define-message server-info ()
  (:name string)
  (:version (string :optional t)))

(define-message position ()
  (:line uinteger)
  (:character uinteger))

(define-message range ()
  (:start position)
  (:end position))

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

;;; Messages

(define-union progress-token (integer string))

(define-message work-done-progress-params ()
  (:work-done-token progress-token))

(define-message initialize-params (work-done-progress-params)
  (:process-id (integer :optional t))
  #++ (:client-info (client-info :optional t))
  (:locale (string :optional t))
  (:root-path (string :optional t))
  #++ (:root-uri (document-uri :optional t))
  (:initialization-options t)
  #++ (:capabilities client-capabilities)
  #++ (:trace (trace-value :optional t))
  #++ (:workspace-folders (workspace-folder :vector t :optional t)))

(define-message initialize-result ()
  (:capabilities server-capabilities)
  (:server-info server-info))

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
