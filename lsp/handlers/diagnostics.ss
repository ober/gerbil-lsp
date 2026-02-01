;;; -*- Gerbil -*-
;;; Diagnostics handler — compile Gerbil files and report errors
(import :std/sugar
        :std/misc/process
        :std/misc/ports
        :std/misc/string
        ../util/log
        ../util/position
        ../types
        ../state
        ../server)
(export #t)

;;; Publish diagnostics for a document
(def (publish-diagnostics-for uri)
  (let ((doc (get-document uri)))
    (when doc
      (let ((diags (collect-diagnostics uri doc)))
        (send-notification! "textDocument/publishDiagnostics"
          (hash ("uri" uri)
                ("diagnostics" (list->vector diags))))))))

;;; Collect diagnostics for a document
;;; Combines parse-level and compile-level diagnostics
(def (collect-diagnostics uri doc)
  (let ((text (document-text doc))
        (file-path (uri->file-path uri)))
    (append
      (parse-diagnostics text)
      (compile-diagnostics file-path text))))

;;; Quick parse diagnostics — try to read the file as S-expressions
(def (parse-diagnostics text)
  (let ((port (open-input-string text))
        (diags '()))
    (let loop ()
      (with-catch
        (lambda (e)
          (let ((msg (error-message e))
                (line (error-line e)))
            (set! diags
              (cons (make-diagnostic
                      (make-lsp-range (or line 0) 0 (or line 0) 1)
                      (or msg (format "~a" e))
                      severity: DiagnosticSeverity.Error
                      source: "gerbil-lsp/parse")
                    diags)))
          ;; Don't try to continue after parse error
          diags)
        (lambda ()
          (let ((form (read port)))
            (if (eof-object? form)
              diags
              (loop))))))
    diags))

;;; Extract error message from an exception
(def (error-message e)
  (with-catch
    (lambda (_) (format "~a" e))
    (lambda ()
      (cond
        ((datum-parsing-exception? e)
         (format "parse error: ~a" (datum-parsing-exception-kind e)))
        ((error-exception? e)
         (error-exception-message e))
        (else (format "~a" e))))))

;;; Try to extract line from an exception
(def (error-line e)
  (with-catch
    (lambda (_) #f)
    (lambda ()
      (cond
        ((datum-parsing-exception? e)
         (let ((re (datum-parsing-exception-readenv e)))
           ;; Readenv may have position info
           #f))
        (else #f)))))

;;; Compile diagnostics — run gxc on the file
(def (compile-diagnostics file-path text)
  ;; Only run on actual files (not untitled)
  (if (and (string? file-path) (file-exists? file-path))
    (run-gxc-diagnostics file-path)
    '()))

;;; Run gxc -S on a file and parse the error output
(def (run-gxc-diagnostics file-path)
  (with-catch
    (lambda (e)
      (lsp-debug "gxc diagnostics failed: ~a" e)
      '())
    (lambda ()
      (let ((result (run-process ["gxc" "-S" file-path]
                      stderr-redirection: #t
                      show-console: #f)))
        ;; gxc outputs errors to stderr
        ;; Parse the error output
        (if (and (string? result) (> (string-length result) 0))
          (parse-gxc-output result file-path)
          '())))))

;;; Parse gxc error output into diagnostics
(def (parse-gxc-output output file-path)
  (let ((lines (string-split-eol output))
        (diags '()))
    (for-each
      (lambda (line)
        (when (> (string-length line) 0)
          (let ((diag (parse-gxc-error-line line)))
            (when diag
              (set! diags (cons diag diags))))))
      lines)
    ;; If we couldn't parse structured errors but there is output,
    ;; report it as a generic error
    (if (and (null? diags) (> (string-length output) 0))
      (list (make-diagnostic
              (make-lsp-range 0 0 0 1)
              (string-trim-eol output)
              severity: DiagnosticSeverity.Error
              source: "gxc"))
      diags)))

;;; Parse a single gxc error line
;;; Format varies but common patterns:
;;;   file.ss:line:col: error message
;;;   *** ERROR IN procedure -- message
(def (parse-gxc-error-line line)
  (with-catch
    (lambda (e) #f)
    (lambda ()
      ;; Try pattern: path:line:col: message
      (let ((parts (string-split-colon line)))
        (if (and (>= (length parts) 4)
                 (string->number (cadr parts)))
          (let ((err-line (- (string->number (cadr parts)) 1)) ; 0-based
                (err-col (or (string->number (caddr parts)) 0))
                (msg (string-join (cdddr parts) ":")))
            (make-diagnostic
              (make-lsp-range err-line err-col err-line (+ err-col 1))
              (string-trim-eol msg)
              severity: DiagnosticSeverity.Error
              source: "gxc"))
          ;; Try pattern: *** ERROR ...
          (if (string-prefix? "***" line)
            (make-diagnostic
              (make-lsp-range 0 0 0 1)
              line
              severity: DiagnosticSeverity.Error
              source: "gxc")
            #f))))))

;;; Split a string on colons (simple)
(def (string-split-colon str)
  (let loop ((i 0) (start 0) (parts '()))
    (cond
      ((>= i (string-length str))
       (reverse (cons (substring str start i) parts)))
      ((char=? (string-ref str i) #\:)
       (loop (+ i 1) (+ i 1) (cons (substring str start i) parts)))
      (else (loop (+ i 1) start parts)))))

;;; Join strings with separator
(def (string-join strs sep)
  (if (null? strs) ""
    (let loop ((rest (cdr strs)) (acc (car strs)))
      (if (null? rest) acc
        (loop (cdr rest) (string-append acc sep (car rest)))))))

;;; Convert URI to file path
(def (uri->file-path uri)
  (if (string-prefix? "file://" uri)
    (substring uri 7 (string-length uri))
    uri))
