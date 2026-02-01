;;; -*- Gerbil -*-
;;; Rename handler — rename symbols across open documents
(import :std/sugar
        ../util/log
        ../util/position
        ../types
        ../state
        ../analysis/document)
(export #t)

;;; Handle textDocument/prepareRename
;;; Returns the range of the symbol that will be renamed, or error
(def (handle-prepare-rename params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (pos (hash-ref params "position" (hash)))
         (line (hash-ref pos "line" 0))
         (col (hash-ref pos "character" 0))
         (doc (get-document uri)))
    (if doc
      (let-values (((sym-name start-col end-col)
                    (symbol-at-position (document-text doc) line col)))
        (if sym-name
          (hash ("range" (make-lsp-range line start-col line end-col))
                ("placeholder" sym-name))
          (void)))
      (void))))

;;; Handle textDocument/rename
;;; Returns a WorkspaceEdit with text edits across documents
(def (handle-rename params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (pos (hash-ref params "position" (hash)))
         (line (hash-ref pos "line" 0))
         (col (hash-ref pos "character" 0))
         (new-name (hash-ref params "newName" ""))
         (doc (get-document uri)))
    (if (and doc (> (string-length new-name) 0))
      (let-values (((sym-name _start _end)
                    (symbol-at-position (document-text doc) line col)))
        (if sym-name
          (let ((edits (compute-rename-edits sym-name new-name)))
            (make-workspace-edit edits))
          (make-workspace-edit (hash))))
      (make-workspace-edit (hash)))))

;;; Compute rename edits across all open documents
;;; Returns a hash of uri → TextEdit[]
(def (compute-rename-edits old-name new-name)
  (let ((changes (make-hash-table)))
    (for-each
      (lambda (uri)
        (let ((doc (get-document uri)))
          (when doc
            (let ((edits (find-rename-edits-in-text
                           (document-text doc) old-name new-name)))
              (when (pair? edits)
                (hash-put! changes uri (list->vector edits)))))))
      (all-document-uris))
    changes))

;;; Find all positions to rename in a text
(def (find-rename-edits-in-text text old-name new-name)
  (let ((old-len (string-length old-name))
        (edits '()))
    (let line-loop ((i 0) (line-num 0) (line-start 0))
      (cond
        ((>= i (string-length text))
         ;; Process last line
         (set! edits
           (append edits
             (find-edits-in-line text line-start i line-num
                                 old-name old-len new-name)))
         (reverse edits))
        ((char=? (string-ref text i) #\newline)
         (set! edits
           (append edits
             (find-edits-in-line text line-start i line-num
                                 old-name old-len new-name)))
         (line-loop (+ i 1) (+ line-num 1) (+ i 1)))
        (else
         (line-loop (+ i 1) line-num line-start))))))

;;; Find rename edits in a single line
(def (find-edits-in-line text line-start line-end line-num
                          old-name old-len new-name)
  (let ((line-text (substring text line-start line-end))
        (edits '()))
    (let loop ((col 0))
      (if (> (+ col old-len) (string-length line-text))
        (reverse edits)
        (begin
          (when (and (string=? old-name
                       (substring line-text col (+ col old-len)))
                     (or (= col 0)
                         (not (symbol-char? (string-ref line-text (- col 1)))))
                     (or (= (+ col old-len) (string-length line-text))
                         (not (symbol-char?
                                (string-ref line-text (+ col old-len))))))
            (set! edits
              (cons (make-text-edit
                      (make-lsp-range line-num col line-num (+ col old-len))
                      new-name)
                    edits)))
          (loop (+ col 1)))))))


