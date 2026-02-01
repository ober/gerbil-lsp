;;; -*- Gerbil -*-
;;; Document formatting handler
(import :std/sugar
        ../util/log
        ../util/position
        ../types
        ../state
        ../analysis/document)
(export #t)

;;; Handle textDocument/formatting
;;; Returns TextEdit[] that replaces the entire document with formatted text
(def (handle-formatting params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (doc (get-document uri)))
    (if doc
      (let ((text (document-text doc)))
        (let ((formatted (format-gerbil-source text)))
          (if (and formatted (not (string=? formatted text)))
            (let ((last-line (count-lines text))
                  (last-col (last-line-length text)))
              (vector
                (make-text-edit
                  (make-lsp-range 0 0 last-line last-col)
                  formatted)))
            [])))
      [])))

;;; Format Gerbil source by pretty-printing each top-level form
(def (format-gerbil-source text)
  (with-catch
    (lambda (e)
      (lsp-debug "formatting error: ~a" e)
      #f)
    (lambda ()
      (let ((port (open-input-string text))
            (out (open-output-string)))
        (let loop ((first? #t))
          (let ((form (read port)))
            (if (eof-object? form)
              (get-output-string out)
              (begin
                (unless first? (newline out))
                (pretty-print form out)
                (loop #f)))))))))

;;; Count the number of lines in text (0-based last line number)
(def (count-lines text)
  (let loop ((i 0) (lines 0))
    (cond
      ((>= i (string-length text)) lines)
      ((char=? (string-ref text i) #\newline)
       (loop (+ i 1) (+ lines 1)))
      (else (loop (+ i 1) lines)))))

;;; Get the length of the last line
(def (last-line-length text)
  (let loop ((i (- (string-length text) 1)) (len 0))
    (cond
      ((< i 0) len)
      ((char=? (string-ref text i) #\newline) len)
      (else (loop (- i 1) (+ len 1))))))
