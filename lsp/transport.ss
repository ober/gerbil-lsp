;;; -*- Gerbil -*-
;;; LSP stdio transport — Content-Length framing over stdin/stdout
(import :std/text/json
        :std/misc/string
        :std/format
        ./util/log)
(export #t)

(def +content-length-prefix+ "Content-Length: ")

;; Read one LSP message from port.
;; Returns the JSON string, or #f on EOF.
(def (read-message port)
  (let ((content-length (read-headers port)))
    (if content-length
      (let ((body (read-string content-length port)))
        (if (and (string? body) (= (string-length body) content-length))
          (begin
            (lsp-debug "recv: ~a" body)
            body)
          (begin
            (lsp-error "incomplete read: expected ~a bytes" content-length)
            #f)))
      #f)))

;; Read headers until blank line, extract Content-Length.
;; Returns content-length as integer, or #f on EOF.
(def (read-headers port)
  (let loop ((content-length #f))
    (let ((line (read-line port)))
      (cond
        ((eof-object? line) #f)
        ;; Blank line (possibly with \r) signals end of headers
        ((or (string=? line "") (string=? line "\r"))
         (or content-length
             (begin
               (lsp-error "no Content-Length header found")
               #f)))
        ;; Content-Length header
        ((string-prefix? +content-length-prefix+ line)
         (let* ((value-str (substring line
                             (string-length +content-length-prefix+)
                             (string-length line)))
                (trimmed (string-trim-eol value-str))
                ;; Also trim trailing \r if read-line didn't strip it
                (clean (if (and (> (string-length trimmed) 0)
                               (char=? (string-ref trimmed
                                         (- (string-length trimmed) 1))
                                       #\return))
                         (substring trimmed 0 (- (string-length trimmed) 1))
                         trimmed))
                (len (string->number clean)))
           (if len
             (loop len)
             (begin
               (lsp-error "invalid Content-Length: ~a" clean)
               (loop content-length)))))
        ;; Other headers — skip
        (else (loop content-length))))))

;; Write one LSP message to port.
(def (write-message port json-string)
  (let* ((body-bytes (string->bytes json-string))
         (content-length (u8vector-length body-bytes)))
    (lsp-debug "send: ~a" json-string)
    (fprintf port "Content-Length: ~a\r\n\r\n" content-length)
    (write-string json-string port)
    (force-output port)))
