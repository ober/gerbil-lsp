;;; -*- Gerbil -*-
;;; On-type formatting handler — textDocument/onTypeFormatting
;;; Triggered on ), ], and newline to auto-indent
(import ../util/log
        ../util/position
        ../types
        ../state
        ../analysis/document)
(export #t)

;;; Handle textDocument/onTypeFormatting
;;; Returns TextEdit[] to fix indentation
(def (handle-on-type-formatting params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (pos (hash-ref params "position" (hash)))
         (line (hash-ref pos "line" 0))
         (col (hash-ref pos "character" 0))
         (ch (hash-ref params "ch" ""))
         (doc (get-document uri)))
    (if doc
      (let ((text (document-text doc)))
        (cond
          ;; On newline: calculate indent for new line
          ((string=? ch "\n")
           (let ((indent (calculate-indent text line)))
             (if (> indent 0)
               (let ((line-text (text-line-at text line)))
                 (let ((current-indent (count-leading-spaces line-text)))
                   (if (not (= current-indent indent))
                     (vector
                       (make-text-edit
                         (make-lsp-range line 0 line current-indent)
                         (make-spaces indent)))
                     (vector))))
               (vector))))
          ;; On ) or ]: no auto-edit needed, most editors handle this
          (else (vector))))
      (vector))))

;;; Calculate the desired indentation for a line based on paren depth
(def (calculate-indent text line)
  (if (= line 0) 0
    ;; Count open parens up to the previous line to determine depth
    (let ((offset (line-col->offset text line 0)))
      (let ((depth (paren-depth-at text offset)))
        (* depth 2)))))

;;; Count the paren depth at a given offset
;;; Returns the number of unclosed opening delimiters
(def (paren-depth-at text offset)
  (let ((limit (min offset (string-length text))))
    (let loop ((i 0) (depth 0) (in-string #f) (in-comment #f))
      (if (>= i limit) (max 0 depth)
        (let ((c (string-ref text i)))
          (cond
            ;; Handle string literals
            (in-string
             (if (char=? c #\")
               (loop (+ i 1) depth #f #f)
               ;; Skip escaped characters
               (if (and (char=? c #\\) (< (+ i 1) limit))
                 (loop (+ i 2) depth #t #f)
                 (loop (+ i 1) depth #t #f))))
            ;; Handle line comments
            (in-comment
             (if (char=? c #\newline)
               (loop (+ i 1) depth #f #f)
               (loop (+ i 1) depth #f #t)))
            ;; Start of string
            ((char=? c #\")
             (loop (+ i 1) depth #t #f))
            ;; Start of line comment
            ((char=? c #\;)
             (loop (+ i 1) depth #f #t))
            ;; Opening delimiters
            ((or (char=? c #\() (char=? c #\[))
             (loop (+ i 1) (+ depth 1) #f #f))
            ;; Closing delimiters
            ((or (char=? c #\)) (char=? c #\]))
             (loop (+ i 1) (- depth 1) #f #f))
            (else
             (loop (+ i 1) depth #f #f))))))))

;;; Count leading spaces on a line
(def (count-leading-spaces line)
  (let loop ((i 0))
    (if (or (>= i (string-length line))
            (not (char=? (string-ref line i) #\space)))
      i
      (loop (+ i 1)))))

;;; Create a string of N spaces
(def (make-spaces n)
  (make-string n #\space))
