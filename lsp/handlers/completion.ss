;;; -*- Gerbil -*-
;;; Completion handler
(import :std/format
        :std/sugar
        ../util/log
        ../types
        ../state
        ../analysis/document
        ../analysis/symbols
        ../analysis/index
        ../analysis/completion-data)
(export #t)

;;; Handle textDocument/completion
(def (handle-completion params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (pos (hash-ref params "position" (hash)))
         (line (hash-ref pos "line" 0))
         (col (hash-ref pos "character" 0))
         (doc (get-document uri)))
    (if doc
      (let ((items (completion-candidates uri (document-text doc) line col)))
        (hash ("isIncomplete" #f)
              ("items" (list->vector items))))
      (hash ("isIncomplete" #f)
            ("items" (vector))))))

;;; Handle completionItem/resolve
;;; Enrich a completion item with documentation and auto-import
(def (handle-completion-resolve params)
  (let* ((label (hash-ref params "label" ""))
         (data (hash-ref params "data" #f))
         (result (hash-copy params)))
    ;; Add documentation if we can find the symbol
    (when data
      (let ((module-name (hash-ref data "module" #f))
            (sym-name (hash-ref data "name" label)))
        ;; Try to find documentation from indexed symbols
        (let ((defs (find-definitions-by-name sym-name)))
          (when (pair? defs)
            (let* ((def-entry (car defs))
                   (sym (cdr def-entry)))
              (let ((detail (sym-info-detail sym)))
                (when detail
                  (hash-put! result "documentation"
                    (hash ("kind" MarkupKind.Markdown)
                          ("value" (format "```scheme\n~a\n```\n\nDefined in `~a`"
                                     detail (car def-entry))))))))))))
    result))
