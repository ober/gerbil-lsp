;;; -*- Gerbil -*-
;;; Code action handler — textDocument/codeAction
(import :std/sugar
        ../util/log)
(export #t)

;;; Handle textDocument/codeAction
;;; Returns an empty array (no code actions supported yet)
(def (handle-code-action params)
  (lsp-debug "codeAction request")
  [])
