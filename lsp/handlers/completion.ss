;;; -*- Gerbil -*-
;;; Completion handler
(import :std/sugar
        ../util/log
        ../state
        ../analysis/document
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
