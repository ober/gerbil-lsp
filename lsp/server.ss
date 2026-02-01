;;; -*- Gerbil -*-
;;; LSP server main dispatch loop
(import :std/sugar
        :std/text/json
        ./util/log
        ./transport
        ./jsonrpc
        ./state)
(export #t)

;;; Method handler registry: method-string → handler-fn
;;; Request handlers: (params) → result (hash table or void)
;;; Notification handlers: (params) → void
(def *request-handlers* (make-hash-table))
(def *notification-handlers* (make-hash-table))

;;; Output port for sending messages (stdout)
(def *output-port* #f)

;;; Register a request handler (expects a response)
(def (register-request-handler! method handler)
  (hash-put! *request-handlers* method handler))

;;; Register a notification handler (no response expected)
(def (register-notification-handler! method handler)
  (hash-put! *notification-handlers* method handler))

;;; Send a notification from server to client
(def (send-notification! method params)
  (when *output-port*
    (let ((msg (make-notification method params)))
      (write-message *output-port* msg))))

;;; Main server loop — reads from stdin, dispatches, writes to stdout
(def (start-server (input-port (current-input-port))
                   (output-port (current-output-port)))
  (set! *output-port* output-port)
  (lsp-info "gerbil-lsp server starting")
  (let loop ()
    (let ((json-str (read-message input-port)))
      (if json-str
        (begin
          (handle-message json-str output-port)
          (loop))
        (begin
          (lsp-info "input stream closed, shutting down")
          (void))))))

;;; Handle a single JSON-RPC message
(def (handle-message json-str output-port)
  (let ((msg (parse-jsonrpc json-str)))
    (cond
      ((not msg)
       (lsp-error "failed to parse message")
       (write-message output-port
         (make-error-response (void) PARSE-ERROR "Parse error")))

      ;; Request — has id and method, needs response
      ((jsonrpc-request? msg)
       (handle-request msg output-port))

      ;; Notification — has method, no id, no response
      ((jsonrpc-notification? msg)
       (handle-notification msg))

      (else
       (lsp-warn "unexpected message type: ~a" json-str)))))

;;; Handle a request — dispatch and send response
(def (handle-request msg output-port)
  (let ((id (jsonrpc-id msg))
        (method (jsonrpc-method msg))
        (params (jsonrpc-params msg)))
    (lsp-debug "request: ~a (id=~a)" method id)
    (let ((handler (hash-ref *request-handlers* method #f)))
      (if handler
        (with-catch
          (lambda (e)
            (lsp-error "handler error for ~a: ~a" method e)
            (write-message output-port
              (make-error-response id INTERNAL-ERROR
                (format "Internal error: ~a" e))))
          (lambda ()
            (let ((result (handler params)))
              (write-message output-port
                (make-response id result)))))
        (begin
          (lsp-warn "no handler for request: ~a" method)
          (write-message output-port
            (make-error-response id METHOD-NOT-FOUND
              (string-append "Method not found: " method))))))))

;;; Handle a notification — dispatch, no response
(def (handle-notification msg)
  (let ((method (jsonrpc-method msg))
        (params (jsonrpc-params msg)))
    (lsp-debug "notification: ~a" method)
    (let ((handler (hash-ref *notification-handlers* method #f)))
      (if handler
        (with-catch
          (lambda (e)
            (lsp-error "handler error for ~a: ~a" method e))
          (lambda ()
            (handler params)))
        (lsp-debug "no handler for notification: ~a" method)))))
