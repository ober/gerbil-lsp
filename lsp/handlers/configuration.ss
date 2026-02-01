;;; -*- Gerbil -*-
;;; Configuration handler — workspace/didChangeConfiguration
(import :std/sugar
        ../util/log)
(export #t)

;;; Handle workspace/didChangeConfiguration notification
;;; Accepts and logs the configuration change
(def (handle-did-change-configuration params)
  (lsp-debug "didChangeConfiguration: ~a" params)
  (void))
