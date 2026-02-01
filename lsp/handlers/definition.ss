;;; -*- Gerbil -*-
;;; Go-to-definition handler
(import :std/sugar
        ../util/log
        ../util/position
        ../types
        ../state
        ../analysis/document
        ../analysis/symbols
        ../analysis/index)
(export #t)

;;; Handle textDocument/definition
(def (handle-definition params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (pos (hash-ref params "position" (hash)))
         (line (hash-ref pos "line" 0))
         (col (hash-ref pos "character" 0))
         (doc (get-document uri)))
    (if doc
      (let-values (((sym-name _start-col _end-col)
                    (symbol-at-position (document-text doc) line col)))
        (if sym-name
          (find-definition-location sym-name uri)
          (void)))
      (void))))

;;; Find the definition location for a symbol
;;; Returns a Location or a list of Locations, or void if not found
(def (find-definition-location name uri)
  ;; First search in current file
  (let ((local-syms (get-file-symbols uri)))
    (let ((found (find-sym-by-name name local-syms)))
      (if found
        (make-lsp-location uri
          (make-lsp-range (sym-info-line found) (sym-info-col found)
                          (sym-info-end-line found) (sym-info-end-col found)))
        ;; Search workspace
        (let ((defs (find-definitions-by-name name)))
          (if (pair? defs)
            (let* ((first-def (car defs))
                   (def-uri (car first-def))
                   (info (cdr first-def)))
              (make-lsp-location def-uri
                (make-lsp-range (sym-info-line info) (sym-info-col info)
                                (sym-info-end-line info)
                                (sym-info-end-col info))))
            (void)))))))

;;; Find a symbol by name in a list
(def (find-sym-by-name name syms)
  (let loop ((ss syms))
    (if (null? ss) #f
      (if (string=? name (sym-info-name (car ss)))
        (car ss)
        (loop (cdr ss))))))
