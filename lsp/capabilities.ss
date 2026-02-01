;;; -*- Gerbil -*-
;;; LSP server capabilities declaration
(import ./types)
(export #t)

;; Returns the ServerCapabilities object for the initialize response
(def (server-capabilities)
  (hash
    ;; Full document sync — client sends entire text on each change
    ("textDocumentSync"
     (hash ("openClose" #t)
           ("change" TextDocumentSyncKind.Full)
           ("save" (hash ("includeText" #t)))))
    ;; Completion
    ("completionProvider"
     (hash ("triggerCharacters" ["(" ":" "/" "."])
           ("resolveProvider" #f)))
    ;; Hover
    ("hoverProvider" #t)
    ;; Go to definition
    ("definitionProvider" #t)
    ;; Find references
    ("referencesProvider" #t)
    ;; Document symbols (outline)
    ("documentSymbolProvider" #t)
    ;; Workspace symbols (search)
    ("workspaceSymbolProvider" #t)
    ;; Rename
    ("renameProvider"
     (hash ("prepareProvider" #t)))
    ;; Formatting
    ("documentFormattingProvider" #t)
    ;; Signature help
    ("signatureHelpProvider"
     (hash ("triggerCharacters" [" " "("])))
    ;; Code actions
    ("codeActionProvider" #t)
    ;; Document highlight
    ("documentHighlightProvider" #t)
    ;; Folding ranges
    ("foldingRangeProvider" #t)
    ;; Selection ranges
    ("selectionRangeProvider" #t)
    ;; Document links
    ("documentLinkProvider"
     (hash ("resolveProvider" #f)))))
