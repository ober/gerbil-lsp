;;; -*- Gerbil -*-
;;; LSP server main dispatch loop
(import :std/format
        :std/sugar
        :std/text/json
        ./util/log
        ./transport
        ./jsonrpc
        ./state
        ./validation)
(export #t)

;;; Method handler registry: method-string → handler-fn
;;; Request handlers: (params) → result (hash table or void)
;;; Notification handlers: (params) → void
(def *request-handlers* (make-hash-table))
(def *notification-handlers* (make-hash-table))

;;; Output port for sending messages (stdout)
(def *output-port* #f)

;;; Mutex protecting write-message for concurrent access (main + diagnostics thread)
(def *output-mutex* (make-mutex 'output))

;;; Register a request handler (expects a response)
(def (register-request-handler! method handler)
  (hash-put! *request-handlers* method handler))

;;; Register a notification handler (no response expected)
(def (register-notification-handler! method handler)
  (hash-put! *notification-handlers* method handler))

;;; Send a notification from server to client (thread-safe)
(def (send-notification! method params)
  (when *output-port*
    (let ((msg (make-notification method params)))
      (mutex-lock! *output-mutex*)
      (with-catch
        (lambda (e)
          (mutex-unlock! *output-mutex*)
          (raise e))
        (lambda ()
          (write-message *output-port* msg)
          (mutex-unlock! *output-mutex*))))))

;;; LSP MessageType constants
(def MessageType.Error   1)
(def MessageType.Warning 2)
(def MessageType.Info    3)
(def MessageType.Log     4)

;;; Send a window/logMessage notification to the client
(def (send-log-message! type message)
  (send-notification! "window/logMessage"
    (hash ("type" type) ("message" message))))

;;; Send a $/progress notification
;;; kind: "begin", "report", or "end"
(def (send-progress! token kind
                     title: (title (void))
                     message: (message (void))
                     percentage: (percentage (void)))
  (let ((value (hash ("kind" kind))))
    (unless (void? title) (hash-put! value "title" title))
    (unless (void? message) (hash-put! value "message" message))
    (unless (void? percentage) (hash-put! value "percentage" percentage))
    (send-notification! "$/progress"
      (hash ("token" token) ("value" value)))))

;;; Send a request from server to client (thread-safe)
;;; Note: This is fire-and-forget; we don't track the response.
(def *next-server-request-id* 0)
(def (send-request! method params)
  (when *output-port*
    (let* ((id (begin (set! *next-server-request-id*
                        (+ *next-server-request-id* 1))
                      *next-server-request-id*))
           (msg (json-object->string
                  (hash ("jsonrpc" "2.0")
                        ("id" id)
                        ("method" method)
                        ("params" params)))))
      (mutex-lock! *output-mutex*)
      (with-catch
        (lambda (e)
          (mutex-unlock! *output-mutex*)
          (raise e))
        (lambda ()
          (write-message *output-port* msg)
          (mutex-unlock! *output-mutex*))))))

;;; Main server loop — reads from stdin, dispatches, writes to stdout
(def (start-server (input-port (current-input-port))
                   (output-port (current-output-port)))
  (set! *output-port* output-port)
  ;; Disable buffering on input port so that read-line (character I/O)
  ;; and read-subu8vector (byte I/O) can safely interleave in transport.
  (port-settings-set! input-port '(buffering: #f))
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
;;; Guards: reject if not initialized (except "initialize") or if shutdown requested
(def (handle-request msg output-port)
  (let ((id (jsonrpc-id msg))
        (method (jsonrpc-method msg))
        (params (jsonrpc-params msg)))
    (lsp-debug "request: ~a (id=~a)" method id)
    (cond
      ;; After shutdown, reject all requests
      ((shutdown-requested?)
       (lsp-warn "request after shutdown: ~a" method)
       (write-message output-port
         (make-error-response id INVALID-REQUEST "Server is shutting down")))
      ;; Before initialization, only "initialize" is allowed
      ((and (not (server-initialized?))
            (not (string=? method "initialize")))
       (lsp-warn "request before initialize: ~a" method)
       (write-message output-port
         (make-error-response id SERVER-NOT-INITIALIZED "Server not initialized")))
      ;; Normal dispatch
      (else
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
                 (validate-and-log! method result)
                 (write-message output-port
                   (make-response id result)))))
           (begin
             (lsp-warn "no handler for request: ~a" method)
             (write-message output-port
               (make-error-response id METHOD-NOT-FOUND
                 (string-append "Method not found: " method))))))))))

;;; Handle a notification — dispatch, no response
;;; After shutdown, only "exit" is processed; all others are ignored
(def (handle-notification msg)
  (let ((method (jsonrpc-method msg))
        (params (jsonrpc-params msg)))
    (lsp-debug "notification: ~a" method)
    (if (and (shutdown-requested?) (not (string=? method "exit")))
      (lsp-debug "ignoring notification after shutdown: ~a" method)
      (let ((handler (hash-ref *notification-handlers* method #f)))
        (if handler
          (with-catch
            (lambda (e)
              (lsp-error "handler error for ~a: ~a" method e))
            (lambda ()
              (handler params)))
          (lsp-debug "no handler for notification: ~a" method))))))
