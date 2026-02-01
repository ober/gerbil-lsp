;;; -*- Gerbil -*-
;;; Document synchronization handlers: didOpen, didChange, didClose, didSave
(import :std/sugar
        ../util/log
        ../state
        ../server
        ../analysis/document
        ../analysis/parser
        ../analysis/symbols
        ./diagnostics)
(export #t)

;;; Handle textDocument/didOpen
(def (handle-did-open params)
  (let* ((doc (make-document-from-open params))
         (uri (document-uri doc)))
    (lsp-info "didOpen: ~a" uri)
    (set-document! uri doc)
    ;; Analyze the document
    (analyze-document! uri doc)
    ;; Publish diagnostics
    (publish-diagnostics-for uri)))

;;; Handle textDocument/didChange
(def (handle-did-change params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (version (hash-ref td "version" 0))
         (changes (hash-ref params "contentChanges" '())))
    (let ((doc (get-document uri)))
      (when doc
        ;; Full sync: take the last content change (entire document)
        (let ((new-text (if (pair? changes)
                          (hash-ref (last changes) "text" (document-text doc))
                          (document-text doc))))
          (let ((updated (document-apply-full-change doc new-text version)))
            (set-document! uri updated)
            ;; Re-analyze symbols
            (analyze-document! uri updated)
            ;; Publish parse-level diagnostics (fast, no gxc)
            (publish-parse-diagnostics uri new-text)))))))

;;; Handle textDocument/didClose
(def (handle-did-close params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" "")))
    (lsp-info "didClose: ~a" uri)
    (remove-document! uri)
    (remove-file-symbols! uri)
    ;; Clear diagnostics
    (send-notification! "textDocument/publishDiagnostics"
      (hash ("uri" uri) ("diagnostics" [])))))

;;; Handle textDocument/didSave
(def (handle-did-save params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" "")))
    (lsp-debug "didSave: ~a" uri)
    ;; Update text if included
    (let ((text (hash-ref params "text" #f)))
      (when text
        (let ((doc (get-document uri)))
          (when doc
            (let ((updated (document-apply-full-change doc text
                             (document-version doc))))
              (set-document! uri updated)
              (analyze-document! uri updated))))))
    ;; Run diagnostics on save
    (publish-diagnostics-for uri)))

;;; Analyze a document: parse and extract symbols
(def (analyze-document! uri doc)
  (let* ((text (document-text doc))
         (forms (parse-source text))
         (syms (extract-symbols forms)))
    (set-file-symbols! uri syms)
    (lsp-debug "analyzed ~a: ~a symbols" uri (length syms))))

;;; Get the last element of a list
(def (last lst)
  (if (null? (cdr lst))
    (car lst)
    (last (cdr lst))))
