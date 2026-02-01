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

;;; Format Gerbil source preserving comments and blank lines.
;;; Strategy: identify regions that are comments/blank lines vs S-expressions.
;;; Only pretty-print the S-expression regions; pass comments through unchanged.
(def (format-gerbil-source text)
  (with-catch
    (lambda (e)
      (lsp-debug "formatting error: ~a" e)
      #f)
    (lambda ()
      (let ((lines (string-split-newline text))
            (out (open-output-string)))
        (let loop ((ls lines) (first? #t) (form-lines '()))
          (cond
            ;; End of input — flush any accumulated form lines
            ((null? ls)
             (when (pair? form-lines)
               (unless first? (newline out))
               (format-form-lines (reverse form-lines) out))
             (get-output-string out))
            ;; Comment line or blank line — flush form, emit as-is
            ((or (comment-line? (car ls))
                 (blank-line? (car ls)))
             ;; Flush any accumulated form
             (when (pair? form-lines)
               (unless first? (newline out))
               (format-form-lines (reverse form-lines) out)
               (set! form-lines '()))
             (unless (and first? (null? form-lines))
               (newline out))
             (display (car ls) out)
             (loop (cdr ls) #f '()))
            ;; Code line — accumulate for formatting
            (else
             (loop (cdr ls) first? (cons (car ls) form-lines)))))))))

;;; Format accumulated code lines by reading and pretty-printing forms
(def (format-form-lines lines out)
  (let ((text (string-join-newline lines)))
    (with-catch
      (lambda (e)
        ;; If formatting fails, emit original text unchanged
        (display text out))
      (lambda ()
        (let ((port (open-input-string text)))
          (let loop ((first? #t))
            (let ((form (read port)))
              (unless (eof-object? form)
                (unless first? (newline out))
                (pretty-print form out)
                (loop #f)))))))))

;;; Check if a line is a comment (starts with ; ignoring whitespace)
(def (comment-line? line)
  (let loop ((i 0))
    (cond
      ((>= i (string-length line)) #f)
      ((char=? (string-ref line i) #\;) #t)
      ((char-whitespace-simple? (string-ref line i)) (loop (+ i 1)))
      (else #f))))

;;; Check if a line is blank (only whitespace)
(def (blank-line? line)
  (let loop ((i 0))
    (cond
      ((>= i (string-length line)) #t)
      ((char-whitespace-simple? (string-ref line i)) (loop (+ i 1)))
      (else #f))))

(def (char-whitespace-simple? c)
  (or (char=? c #\space) (char=? c #\tab) (char=? c #\return)))

;;; Split text on newlines
(def (string-split-newline text)
  (let loop ((i 0) (start 0) (result '()))
    (cond
      ((>= i (string-length text))
       (reverse (cons (substring text start i) result)))
      ((char=? (string-ref text i) #\newline)
       (loop (+ i 1) (+ i 1) (cons (substring text start i) result)))
      (else (loop (+ i 1) start result)))))

;;; Join lines with newlines
(def (string-join-newline lines)
  (if (null? lines) ""
    (let loop ((rest (cdr lines)) (acc (car lines)))
      (if (null? rest) acc
        (loop (cdr rest) (string-append acc "\n" (car rest)))))))

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
