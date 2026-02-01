;;; -*- Gerbil -*-
;;; Document state management — track open document text and metadata
(export #t)

;;; Document record
(defstruct document (uri version text language-id) transparent: #t)

;;; Create a document from didOpen params
(def (make-document-from-open params)
  (let ((td (hash-ref params "textDocument" (hash))))
    (make-document
      (hash-ref td "uri" "")
      (hash-ref td "version" 0)
      (hash-ref td "text" "")
      (hash-ref td "languageId" "gerbil"))))

;;; Apply full content changes to a document
;;; For TextDocumentSyncKind.Full, the entire text is replaced
(def (document-apply-full-change doc new-text version)
  (make-document
    (document-uri doc)
    version
    new-text
    (document-language-id doc)))

;;; Get the number of lines in a document
(def (document-line-count doc)
  (let ((text (document-text doc)))
    (let loop ((i 0) (count 1))
      (cond
        ((>= i (string-length text)) count)
        ((char=? (string-ref text i) #\newline)
         (loop (+ i 1) (+ count 1)))
        (else (loop (+ i 1) count))))))

;;; Get a specific line from the document (0-based)
(def (document-line-at doc line-number)
  (let ((text (document-text doc)))
    (let loop ((i 0) (cur-line 0) (start 0))
      (cond
        ((>= i (string-length text))
         (if (= cur-line line-number)
           (substring text start i)
           ""))
        ((char=? (string-ref text i) #\newline)
         (if (= cur-line line-number)
           (substring text start i)
           (loop (+ i 1) (+ cur-line 1) (+ i 1))))
        (else
         (loop (+ i 1) cur-line start))))))

;;; Split document text into lines
(def (document-lines doc)
  (let ((text (document-text doc)))
    (let loop ((i 0) (start 0) (lines '()))
      (cond
        ((>= i (string-length text))
         (reverse (cons (substring text start i) lines)))
        ((char=? (string-ref text i) #\newline)
         (loop (+ i 1) (+ i 1) (cons (substring text start i) lines)))
        (else
         (loop (+ i 1) start lines))))))
