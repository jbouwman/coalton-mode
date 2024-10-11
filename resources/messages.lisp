(defpackage #:coalton-mode/examples
  (:use #:cl)
  (:export #:initialize))

(in-package #:coalton-mode/examples)

(defvar initialize
  '((:JSONRPC . "2.0")
    (:METHOD . "initialize")
    (:PARAMS (:PROCESS-ID . 59127)
     (:ROOT-PATH . "/Users/jbouwman/git/coalton-mode")
     (:CLIENT-INFO (:NAME . "emacs")
      (:VERSION
       . "GNU Emacs 29.3 (build 1, aarch64-apple-darwin23.4.0, Carbon Version 170 AppKit 2487.5)
 of 2024-05-01"))
     (:ROOT-URI . "file:///Users/jbouwman/git/coalton-mode")
     (:CAPABILITIES (:GENERAL ("positionEncodings" "utf-32" "utf-16"))
      (:WORKSPACE
       (:WORKSPACE-EDIT (:DOCUMENT-CHANGES . T)
        (:RESOURCE-OPERATIONS "create" "rename" "delete"))
       (:APPLY-EDIT . T)
       (:SYMBOL
        ("symbolKind"
         ("valueSet" 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
                     24 25 26)))
       (:EXECUTE-COMMAND ("dynamicRegistration"))
       (:DID-CHANGE-WATCHED-FILES (:DYNAMIC-REGISTRATION . T))
       (:WORKSPACE-FOLDERS . T) (:CONFIGURATION . T)
       (:CODE-LENS (:REFRESH-SUPPORT . T))
       (:FILE-OPERATIONS (:DID-CREATE) (:WILL-CREATE) (:DID-RENAME . T)
        (:WILL-RENAME . T) (:DID-DELETE) (:WILL-DELETE)))
      (:TEXT-DOCUMENT
       (:DECLARATION (:DYNAMIC-REGISTRATION . T) (:LINK-SUPPORT . T))
       (:DEFINITION (:DYNAMIC-REGISTRATION . T) (:LINK-SUPPORT . T))
       (:REFERENCES (:DYNAMIC-REGISTRATION . T))
       (:IMPLEMENTATION (:DYNAMIC-REGISTRATION . T) (:LINK-SUPPORT . T))
       (:TYPE-DEFINITION (:DYNAMIC-REGISTRATION . T) (:LINK-SUPPORT . T))
       (:SYNCHRONIZATION (:WILL-SAVE . T) (:DID-SAVE . T)
        (:WILL-SAVE-WAIT-UNTIL . T))
       (:DOCUMENT-SYMBOL
        (:SYMBOL-KIND
         ("valueSet" 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
                     24 25 26))
        (:HIERARCHICAL-DOCUMENT-SYMBOL-SUPPORT . T))
       (:FORMATTING (:DYNAMIC-REGISTRATION . T))
       (:RANGE-FORMATTING (:DYNAMIC-REGISTRATION . T))
       (:ON-TYPE-FORMATTING (:DYNAMIC-REGISTRATION . T))
       (:RENAME (:DYNAMIC-REGISTRATION . T) (:PREPARE-SUPPORT . T))
       (:CODE-ACTION (:DYNAMIC-REGISTRATION . T) (:IS-PREFERRED-SUPPORT . T)
        (:CODE-ACTION-LITERAL-SUPPORT
         ("codeActionKind"
          ("valueSet" "" "quickfix" "refactor" "refactor.extract"
                      "refactor.inline" "refactor.rewrite" "source"
                      "source.organizeImports")))
        (:RESOLVE-SUPPORT ("properties" "edit" "command")) (:DATA-SUPPORT . T))
       (:COMPLETION
        (:COMPLETION-ITEM (:SNIPPET-SUPPORT)
         (:DOCUMENTATION-FORMAT "markdown" "plaintext")
         (:RESOLVE-ADDITIONAL-TEXT-EDITS-SUPPORT . T)
         (:INSERT-REPLACE-SUPPORT . T) (:DEPRECATED-SUPPORT . T)
         (:RESOLVE-SUPPORT
          ("properties" "documentation" "detail" "additionalTextEdits" "command"))
         (:INSERT-TEXT-MODE-SUPPORT ("valueSet" 1 2)))
        (:CONTEXT-SUPPORT . T) (:DYNAMIC-REGISTRATION . T))
       (:SIGNATURE-HELP
        (:SIGNATURE-INFORMATION
         (:PARAMETER-INFORMATION (:LABEL-OFFSET-SUPPORT . T)))
        (:DYNAMIC-REGISTRATION . T))
       (:DOCUMENT-LINK (:DYNAMIC-REGISTRATION . T) (:TOOLTIP-SUPPORT . T))
       (:HOVER (:CONTENT-FORMAT "markdown" "plaintext")
        (:DYNAMIC-REGISTRATION . T))
       (:FOLDING-RANGE (:DYNAMIC-REGISTRATION . T))
       (:SELECTION-RANGE (:DYNAMIC-REGISTRATION . T))
       (:CALL-HIERARCHY ("dynamicRegistration"))
       (:TYPE-HIERARCHY (:DYNAMIC-REGISTRATION . T))
       (:PUBLISH-DIAGNOSTICS (:RELATED-INFORMATION . T)
        (:TAG-SUPPORT ("valueSet" 1 2)) (:VERSION-SUPPORT . T))
       (:DIAGNOSTIC ("dynamicRegistration") ("relatedDocumentSupport"))
       (:LINKED-EDITING-RANGE (:DYNAMIC-REGISTRATION . T)))
      (:WINDOW (:WORK-DONE-PROGRESS . T) (:SHOW-DOCUMENT (:SUPPORT . T))))
     (:INITIALIZATION-OPTIONS) (:WORK-DONE-TOKEN . "1"))
    (:ID . 1)))
