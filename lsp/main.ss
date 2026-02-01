;;; -*- Gerbil -*-
;;; gerbil-lsp entry point
(import :std/cli/getopt
        :std/sugar
        ./util/log
        ./server
        ./handlers/lifecycle
        ./handlers/sync
        ./handlers/diagnostics
        ./handlers/completion
        ./handlers/hover
        ./handlers/definition
        ./handlers/references
        ./handlers/symbols
        ./handlers/rename
        ./handlers/formatting
        ./handlers/signature)
(export main)

(def (main . args)
  (call-with-getopt gerbil-lsp-main args
    program: "gerbil-lsp"
    help: "Language Server Protocol server for Gerbil Scheme"
    (flag 'stdio "--stdio"
      help: "Use stdio transport (default)")
    (option 'log-level "--log-level"
      help: "Log level: debug, info, warn, error"
      default: "info")
    (flag 'version "--version"
      help: "Print version and exit")))

(def (gerbil-lsp-main opt)
  (when (hash-ref opt 'version #f)
    (displayln "gerbil-lsp 0.1.0")
    (exit 0))
  ;; Set log level
  (set-log-level! (log-level-from-string (hash-ref opt 'log-level "info")))
  ;; Register all handlers
  (register-all-handlers!)
  ;; Start the server on stdio
  (start-server))

;;; Register all LSP method handlers
(def (register-all-handlers!)
  ;; Lifecycle
  (register-request-handler! "initialize" handle-initialize)
  (register-notification-handler! "initialized" handle-initialized)
  (register-request-handler! "shutdown" handle-shutdown)
  (register-notification-handler! "exit" handle-exit)
  ;; Document sync
  (register-notification-handler! "textDocument/didOpen" handle-did-open)
  (register-notification-handler! "textDocument/didChange" handle-did-change)
  (register-notification-handler! "textDocument/didClose" handle-did-close)
  (register-notification-handler! "textDocument/didSave" handle-did-save)
  ;; Language features
  (register-request-handler! "textDocument/completion" handle-completion)
  (register-request-handler! "textDocument/hover" handle-hover)
  (register-request-handler! "textDocument/definition" handle-definition)
  (register-request-handler! "textDocument/references" handle-references)
  (register-request-handler! "textDocument/documentSymbol" handle-document-symbol)
  (register-request-handler! "workspace/symbol" handle-workspace-symbol)
  (register-request-handler! "textDocument/prepareRename" handle-prepare-rename)
  (register-request-handler! "textDocument/rename" handle-rename)
  (register-request-handler! "textDocument/formatting" handle-formatting)
  (register-request-handler! "textDocument/signatureHelp" handle-signature-help)
  (lsp-info "all handlers registered"))
