;;; -*- Gerbil -*-
;;; LSP lifecycle handlers: initialize, initialized, shutdown, exit
(import :std/sugar
        :std/format
        ../util/log
        ../jsonrpc
        ../types
        ../capabilities
        ../state
        ../server
        ../analysis/index
        ../analysis/module)
(export #t)

;;; Handle "initialize" request
;;; Returns the InitializeResult with server capabilities
(def (handle-initialize params)
  (let ((root-uri (hash-ref params "rootUri" #f))
        (root-path (hash-ref params "rootPath" #f))
        (caps (hash-ref params "capabilities" (hash))))
    ;; Store workspace root
    (let ((root (or root-uri root-path)))
      (when root
        (set-workspace-root! (uri->file-path root))))
    ;; Store client capabilities
    (set-client-capabilities! caps)
    (lsp-info "initialize: workspace root = ~a" (workspace-root))
    ;; Return InitializeResult
    (hash ("capabilities" (server-capabilities))
          ("serverInfo" (hash ("name" "gerbil-lsp")
                              ("version" "0.1.0"))))))

;;; Handle "initialized" notification
;;; Client confirms initialization is complete — index workspace
(def (handle-initialized params)
  (set-initialized! #t)
  (lsp-info "server initialized")
  ;; Index all .ss files in the workspace for symbols
  (let ((root (workspace-root)))
    (when root
      (send-log-message! MessageType.Info
        (format "Indexing workspace: ~a" root))
      (with-catch
        (lambda (e)
          (lsp-warn "workspace indexing failed: ~a" e)
          (send-log-message! MessageType.Error
            (format "Workspace indexing failed: ~a" e)))
        (lambda ()
          (index-workspace! root)
          (send-log-message! MessageType.Info "Workspace indexing complete")))))
  (void))

;;; Handle "shutdown" request
;;; Prepare for exit, return null
(def (handle-shutdown params)
  (set-shutdown-requested! #t)
  (lsp-info "shutdown requested")
  (void))

;;; Handle "exit" notification
;;; Terminate the process
(def (handle-exit params)
  (lsp-info "exiting")
  (force-output (current-output-port))
  (exit (if (shutdown-requested?) 0 1)))

