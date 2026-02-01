;;; -*- Gerbil -*-
;;; Diagnostics handler — compile Gerbil files and report errors
(import :std/format
        :std/sugar
        :std/misc/ports
        :std/misc/string
        ../util/log
        ../util/position
        ../types
        ../state
        ../server
        ../analysis/document
        ../analysis/module)
(export #t)

;;; Publish diagnostics for a document (full: parse + compile)
;;; Called on save — runs gxc and caches the results
(def (publish-diagnostics-for uri)
  (let ((doc (get-document uri)))
    (when doc
      (let ((diags (collect-diagnostics uri doc)))
        (send-notification! "textDocument/publishDiagnostics"
          (hash ("uri" uri)
                ("diagnostics" (list->vector diags))))))))

;;; Publish parse-only diagnostics for a document (fast, no gxc)
;;; Used on didChange for immediate feedback while typing.
;;; Merges parse diagnostics with cached gxc diagnostics so compiler
;;; errors don't disappear while editing.
(def (publish-parse-diagnostics uri text)
  (with-catch
    (lambda (e)
      (lsp-debug "parse diagnostics failed: ~a" e))
    (lambda ()
      (let ((parse-diags (parse-diagnostics text))
            (cached-gxc (get-gxc-diagnostics uri)))
        (send-notification! "textDocument/publishDiagnostics"
          (hash ("uri" uri)
                ("diagnostics"
                 (list->vector (append parse-diags cached-gxc)))))))))

;;; Collect diagnostics for a document
;;; Combines parse-level and compile-level diagnostics.
;;; Caches the gxc diagnostics for use during editing.
(def (collect-diagnostics uri doc)
  (let* ((text (document-text doc))
         (file-path (uri->file-path uri))
         (parse-diags (parse-diagnostics text))
         (gxc-diags (compile-diagnostics file-path text)))
    ;; Cache gxc diagnostics for merging during didChange
    (set-gxc-diagnostics! uri gxc-diags)
    (append parse-diags gxc-diags)))

;;; Quick parse diagnostics — try to read the file as S-expressions
(def (parse-diagnostics text)
  (let ((port (open-input-string text))
        (diags '()))
    (let loop ()
      (with-catch
        (lambda (e)
          (let ((msg (error-message e)))
            (let-values (((line col) (error-line-col e)))
              (let ((l (or line 0))
                    (c (or col 0)))
                (set! diags
                  (cons (make-diagnostic
                          (make-lsp-range l c l (+ c 1))
                          (or msg (format "~a" e))
                          severity: DiagnosticSeverity.Error
                          source: "gerbil-lsp/parse")
                        diags)))))
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

;;; Extract line and column from a parse exception
;;; Returns (values line col) with 0-based values, or (values #f #f)
;;; Gambit's readenv filepos encodes: lower 16 bits = 0-indexed line,
;;; upper bits = 1-indexed column at error point
(def (error-line-col e)
  (with-catch
    (lambda (_) (values #f #f))
    (lambda ()
      (if (datum-parsing-exception? e)
        (let* ((re (datum-parsing-exception-readenv e))
               (filepos (##readenv-current-filepos re)))
          (if (fixnum? filepos)
            (values (bitwise-and filepos #xFFFF)
                    (max 0 (- (arithmetic-shift filepos -16) 1)))
            (values #f #f)))
        (values #f #f)))))

;;; Compile diagnostics — run gxc on the file
(def (compile-diagnostics file-path text)
  ;; Only run on actual files (not untitled)
  (if (and (string? file-path) (file-exists? file-path))
    (run-gxc-diagnostics file-path)
    '()))

;;; Run gxc -S on a file and parse the error output.
;;; Uses open-process because run-process throws on non-zero exit,
;;; but gxc exits non-zero when there are compilation errors —
;;; exactly when we need to read its output.
(def (run-gxc-diagnostics file-path)
  (with-catch
    (lambda (e)
      (lsp-debug "gxc diagnostics failed: ~a" e)
      '())
    (lambda ()
      (let* ((proc (open-process
                      (list path: "gxc"
                            arguments: (list "-S" file-path)
                            stderr-redirection: #t
                            stdout-redirection: #t)))
             (output (read-all-as-string proc))
             (status (process-status proc)))
        ;; process-status returns raw waitpid status;
        ;; normal exit code N gives N*256, signals give lower byte.
        ;; Any non-zero status indicates failure.
        (if (and (not (= status 0))
                 (string? output)
                 (> (string-length output) 0))
          (parse-gxc-output output file-path)
          '())))))

;;; Parse gxc error output into diagnostics
(def (parse-gxc-output output file-path)
  (let ((lines (split-lines output))
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

;;; Split a string into lines
(def (split-lines text)
  (let loop ((i 0) (start 0) (lines '()))
    (cond
      ((>= i (string-length text))
       (reverse (cons (substring text start i) lines)))
      ((char=? (string-ref text i) #\newline)
       (loop (+ i 1) (+ i 1) (cons (substring text start i) lines)))
      (else
       (loop (+ i 1) start lines)))))
