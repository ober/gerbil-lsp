;;; -*- Gerbil -*-
;;; Signature help handler — show function signatures
(import :std/sugar
        ../util/log
        ../util/position
        ../types
        ../state
        ../analysis/document
        ../analysis/symbols
        ../analysis/index)
(export #t)

;;; Handle textDocument/signatureHelp
(def (handle-signature-help params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (pos (hash-ref params "position" (hash)))
         (line (hash-ref pos "line" 0))
         (col (hash-ref pos "character" 0))
         (doc (get-document uri)))
    (if doc
      (let ((call-info (find-enclosing-call (document-text doc) line col)))
        (if call-info
          (let* ((func-name (car call-info))
                 (arg-index (cdr call-info))
                 (sig (find-signature func-name uri)))
            (if sig
              (make-signature-help
                (vector sig)
                0
                (max 0 arg-index))
              (void)))
          (void)))
      (void))))

;;; Find the enclosing function call at a position
;;; Returns (func-name . arg-index) or #f
(def (find-enclosing-call text line col)
  (let ((offset (line-col->offset* text line col)))
    (if (not offset)
      #f
      ;; Walk backwards to find the opening paren of the enclosing call
      (let loop ((i (- offset 1)) (depth 0) (arg-count 0))
        (cond
          ((< i 0) #f)
          ((char=? (string-ref text i) #\))
           (loop (- i 1) (+ depth 1) arg-count))
          ((char=? (string-ref text i) #\()
           (if (= depth 0)
             ;; Found the opening paren — now extract the function name
             (let ((func-name (extract-func-name text (+ i 1))))
               (if func-name
                 (cons func-name arg-count)
                 #f))
             (loop (- i 1) (- depth 1) arg-count)))
          ;; Count spaces at depth 0 as argument separators
          ((and (= depth 0) (char-whitespace? (string-ref text i)))
           ;; Check if previous char was non-whitespace (argument boundary)
           (if (and (> i 0)
                    (not (char-whitespace? (string-ref text (- i 1))))
                    (not (char=? (string-ref text (- i 1)) #\()))
             (loop (- i 1) depth (+ arg-count 1))
             (loop (- i 1) depth arg-count)))
          (else
           (loop (- i 1) depth arg-count)))))))

;;; Extract function name starting at position (after opening paren)
(def (extract-func-name text start)
  (let ((len (string-length text)))
    (let loop ((i start) (chars '()))
      (cond
        ((>= i len) #f)
        ((or (char-whitespace? (string-ref text i))
             (char=? (string-ref text i) #\))
             (char=? (string-ref text i) #\())
         (if (null? chars) #f
           (list->string (reverse chars))))
        (else
         (loop (+ i 1) (cons (string-ref text i) chars)))))))

;;; Convert line/col to offset (simple implementation)
(def (line-col->offset* text line col)
  (let loop ((i 0) (cur-line 0) (cur-col 0))
    (cond
      ((and (= cur-line line) (= cur-col col)) i)
      ((>= i (string-length text)) #f)
      ((char=? (string-ref text i) #\newline)
       (loop (+ i 1) (+ cur-line 1) 0))
      (else
       (loop (+ i 1) cur-line (+ cur-col 1))))))

;;; Find a function signature for display
(def (find-signature func-name uri)
  ;; Look up in local symbols first
  (let ((local-syms (get-file-symbols uri)))
    (let ((found (find-func-sym func-name local-syms)))
      (if found
        (make-sig-from-sym found)
        ;; Search workspace
        (let ((defs (find-definitions-by-name func-name)))
          (if (pair? defs)
            (make-sig-from-sym (cdr (car defs)))
            #f))))))

;;; Find a function symbol by name
(def (find-func-sym name syms)
  (let loop ((ss syms))
    (if (null? ss) #f
      (let ((s (car ss)))
        (if (and (string=? name (sym-info-name s))
                 (= (sym-info-kind s) SymbolKind.Function))
          s
          (loop (cdr ss)))))))

;;; Create a SignatureInformation from a sym-info
(def (make-sig-from-sym s)
  (let ((detail (sym-info-detail s)))
    (if detail
      (make-signature-information detail
        documentation: (format "Defined as ~a" (sym-info-name s)))
      (make-signature-information
        (format "(~a ...)" (sym-info-name s))
        documentation: (format "~a" (sym-info-name s))))))

;;; Check if char is whitespace
(def (char-whitespace? c)
  (or (char=? c #\space) (char=? c #\tab)
      (char=? c #\newline) (char=? c #\return)))
