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
;; Content-Length is in bytes (UTF-8), so we read raw bytes
;; and decode to a string.
(def (read-message port)
  (let ((content-length (read-headers port)))
    (if content-length
      (let* ((buf (make-u8vector content-length 0))
             (n (read-subu8vector buf 0 content-length port)))
        (if (= n content-length)
          (let ((body (bytes->string buf)))
            (lsp-debug "recv: ~a" body)
            body)
          (begin
            (lsp-error "incomplete read: expected ~a bytes, got ~a" content-length n)
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
;; Content-Length is the byte count of the UTF-8 body.
;; We write raw bytes for both header and body to avoid
;; encoding mismatches from port settings.
(def (write-message port json-string)
  (let* ((body-bytes (string->bytes json-string))
         (content-length (u8vector-length body-bytes))
         (header (string-append "Content-Length: "
                                (number->string content-length)
                                "\r\n\r\n"))
         (header-bytes (string->bytes header)))
    (lsp-debug "send: ~a" json-string)
    (write-subu8vector header-bytes 0 (u8vector-length header-bytes) port)
    (write-subu8vector body-bytes 0 content-length port)
    (force-output port)))
