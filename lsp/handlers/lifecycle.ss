;;; -*- Gerbil -*-
;;; LSP lifecycle handlers: initialize, initialized, shutdown, exit
(import :std/sugar
        ../util/log
        ../jsonrpc
        ../types
        ../capabilities
        ../state)
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
        (set-workspace-root! (uri->path root))))
    ;; Store client capabilities
    (set-client-capabilities! caps)
    (lsp-info "initialize: workspace root = ~a" (workspace-root))
    ;; Return InitializeResult
    (hash ("capabilities" (server-capabilities))
          ("serverInfo" (hash ("name" "gerbil-lsp")
                              ("version" "0.1.0"))))))

;;; Handle "initialized" notification
;;; Client confirms initialization is complete
(def (handle-initialized params)
  (set-initialized! #t)
  (lsp-info "server initialized")
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
  (exit (if (shutdown-requested?) 0 1)))

;;; Convert a file URI to a filesystem path
(def (uri->path uri)
  (cond
    ((string-prefix? "file://" uri)
     (let ((path (substring uri 7 (string-length uri))))
       ;; Handle URL-encoded characters (basic: %20 → space)
       (uri-decode path)))
    (else uri)))

;;; Convert a filesystem path to a file URI
(def (path->uri path)
  (string-append "file://" path))

;;; Basic URI percent-decoding
(def (uri-decode str)
  (let loop ((i 0) (acc '()))
    (cond
      ((>= i (string-length str))
       (list->string (reverse acc)))
      ((and (char=? (string-ref str i) #\%)
            (< (+ i 2) (string-length str)))
       (let ((hex (substring str (+ i 1) (+ i 3))))
         (let ((code (string->number hex 16)))
           (if code
             (loop (+ i 3) (cons (integer->char code) acc))
             (loop (+ i 1) (cons #\% acc))))))
      (else
       (loop (+ i 1) (cons (string-ref str i) acc))))))
