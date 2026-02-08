;;; -*- Gerbil -*-
;;; Semantic tokens handler — textDocument/semanticTokens/full
;;; Provides token classification for syntax highlighting
(import ../compat/compat
        ../util/log
        ../util/position
        ../util/string
        ../types
        ../state
        ../analysis/document)
(export #t)

;;; Semantic token type indices (must match the legend order)
(def SemanticTokenType.keyword   0)
(def SemanticTokenType.function  1)
(def SemanticTokenType.variable  2)
(def SemanticTokenType.parameter 3)
(def SemanticTokenType.type      4)
(def SemanticTokenType.macro     5)
(def SemanticTokenType.comment   6)
(def SemanticTokenType.string    7)
(def SemanticTokenType.number    8)
(def SemanticTokenType.operator  9)

;;; Semantic token modifier bit flags
(def SemanticTokenModifier.definition  1)
(def SemanticTokenModifier.readonly    2)

;;; Token type legend (order must match indices above)
(def *semantic-token-types*
  ["keyword" "function" "variable" "parameter" "type"
   "macro" "comment" "string" "number" "operator"])

;;; Token modifier legend
(def *semantic-token-modifiers*
  ["definition" "readonly"])

;;; Gerbil special forms and keywords for classification
(def *gerbil-special-forms*
  (let ((ht (make-hash-table)))
    (for-each (lambda (kw) (hash-put! ht kw #t))
      '("def" "define" "defn" "def*"
        "defstruct" "defclass" "defmethod" "defproto"
        "defrule" "defrules" "defsyntax" "defsyntax-call" "defsyntax-case"
        "defvalues" "defconst" "deferror-class"
        "deftable" "definterface" "implement"
        "lambda" "let" "let*" "letrec" "letrec*"
        "let-values" "let*-values"
        "if" "cond" "case" "when" "unless"
        "and" "or" "not"
        "begin" "begin0"
        "do" "do-while" "while" "do-with-lock"
        "for" "for*" "for/collect" "for/fold"
        "set!" "set!-values"
        "values" "receive" "call-with-values"
        "apply" "call/cc" "call-with-current-continuation"
        "with-catch" "with-exception-handler"
        "raise" "error"
        "import" "export"
        "require" "provide" "include"
        "quote" "quasiquote" "unquote" "unquote-splicing"
        "syntax" "syntax-rules" "syntax-case"
        "match" "with" "one-of"
        "try" "catch" "finally"
        "assert" "parameterize" "dynamic-wind" "guard"
        "delay" "force" "declare" "using" "with-methods"))
    ht))

;;; Gerbil macro-defining forms
(def *macro-def-forms*
  (let ((ht (make-hash-table)))
    (for-each (lambda (kw) (hash-put! ht kw #t))
      '("defrule" "defrules" "defsyntax" "defsyntax-call" "defsyntax-case"))
    ht))

;;; Gerbil type-defining forms
(def *type-def-forms*
  (let ((ht (make-hash-table)))
    (for-each (lambda (kw) (hash-put! ht kw #t))
      '("defstruct" "defclass" "deferror-class" "deftable" "definterface"))
    ht))

;;; Handle textDocument/semanticTokens/full
(def (handle-semantic-tokens-full params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (doc (get-document uri)))
    (if doc
      (let ((tokens (tokenize-source (document-text doc))))
        (hash ("data" (encode-semantic-tokens tokens))))
      (hash ("data" [])))))

;;; Tokenize source text into a list of (line col length type modifiers)
(def (tokenize-source text)
  (let ((tokens '())
        (len (string-length text)))
    (let loop ((i 0) (line 0) (col 0))
      (if (>= i len)
        (reverse tokens)
        (let ((c (string-ref text i)))
          (cond
            ;; Newline
            ((char=? c #\newline)
             (loop (+ i 1) (+ line 1) 0))

            ;; Comment: ; to end of line
            ((char=? c #\;)
             (let ((end (find-end-of-line text i len)))
               (set! tokens (cons (list line col (- end i)
                                        SemanticTokenType.comment 0)
                                  tokens))
               (loop end line (+ col (- end i)))))

            ;; String literal
            ((char=? c #\")
             (let ((end (find-end-of-string text (+ i 1) len)))
               (let ((str-len (- end i)))
                 ;; Count lines within string
                 (let-values (((end-line end-col) (count-span text i end line col)))
                   (set! tokens (cons (list line col str-len
                                            SemanticTokenType.string 0)
                                      tokens))
                   (loop end end-line end-col)))))

            ;; Character literal: #\x
            ((and (char=? c #\#)
                  (< (+ i 1) len)
                  (char=? (string-ref text (+ i 1)) #\\))
             (let ((end (find-end-of-char-literal text (+ i 2) len)))
               (set! tokens (cons (list line col (- end i)
                                        SemanticTokenType.string 0)
                                  tokens))
               (loop end line (+ col (- end i)))))

            ;; Boolean literal: #t or #f
            ((and (char=? c #\#)
                  (< (+ i 1) len)
                  (let ((nc (string-ref text (+ i 1))))
                    (or (char=? nc #\t) (char=? nc #\f))))
             (let ((end (find-end-of-token text (+ i 1) len)))
               (set! tokens (cons (list line col (- end i)
                                        SemanticTokenType.number 0)
                                  tokens))
               (loop end line (+ col (- end i)))))

            ;; Number: starts with digit, or - followed by digit, or . followed by digit
            ((or (char-numeric? c)
                 (and (char=? c #\-)
                      (< (+ i 1) len)
                      (char-numeric? (string-ref text (+ i 1))))
                 (and (char=? c #\.)
                      (< (+ i 1) len)
                      (char-numeric? (string-ref text (+ i 1)))))
             ;; Check this is not part of a symbol
             (if (and (> i 0)
                      (symbol-char? (string-ref text (- i 1)))
                      (not (token-start? text i)))
               ;; Part of a symbol, skip
               (let ((end (find-end-of-token text i len)))
                 (loop end line (+ col (- end i))))
               (let ((end (find-end-of-token text i len)))
                 (let ((tok (substring text i end)))
                   (if (string->number tok)
                     (begin
                       (set! tokens (cons (list line col (- end i)
                                                SemanticTokenType.number 0)
                                          tokens))
                       (loop end line (+ col (- end i))))
                     ;; Not actually a number, classify as symbol
                     (begin
                       (set! tokens (cons (classify-symbol-token tok line col)
                                          tokens))
                       (loop end line (+ col (- end i)))))))))

            ;; Symbol/identifier
            ((symbol-start-char? c)
             (let* ((end (find-end-of-token text i len))
                    (tok (substring text i end))
                    (token-info (classify-symbol-token tok line col)))
               (set! tokens (cons token-info tokens))
               (loop end line (+ col (- end i)))))

            ;; Quote characters: ' ` , ,@
            ((memv c '(#\' #\`))
             (loop (+ i 1) line (+ col 1)))

            ;; Skip parens, brackets, whitespace, etc.
            (else
             (loop (+ i 1) line (+ col 1)))))))))

;;; Classify a symbol token into a semantic token type
(def (classify-symbol-token name line col)
  (let ((name-len (string-length name)))
    (cond
      ;; Keyword / special form
      ((hash-key? *gerbil-special-forms* name)
       (list line col name-len SemanticTokenType.keyword 0))
      ;; Macro-defining form names (also keywords)
      ((hash-key? *macro-def-forms* name)
       (list line col name-len SemanticTokenType.keyword 0))
      ;; Type-related: names ending with ::t or ? (type predicates)
      ((and (> name-len 3) (string-suffix? "::t" name))
       (list line col name-len SemanticTokenType.type 0))
      ;; Constructor: make-*
      ((and (> name-len 5) (string-prefix? "make-" name))
       (list line col name-len SemanticTokenType.function 0))
      ;; Keyword symbol ending with :
      ((and (> name-len 1)
            (char=? (string-ref name (- name-len 1)) #\:))
       (list line col name-len SemanticTokenType.parameter 0))
      ;; UPPERCASE names are typically constants
      ((all-uppercase? name)
       (list line col name-len SemanticTokenType.variable
             SemanticTokenModifier.readonly))
      ;; Default: variable
      (else
       (list line col name-len SemanticTokenType.variable 0)))))

;;; Encode tokens into LSP delta-encoded format
;;; Input: list of (line col length type modifiers), not necessarily sorted
;;; Output: flat vector of [deltaLine deltaStartChar length tokenType tokenModifiers ...]
(def (encode-semantic-tokens tokens)
  ;; Sort by line then column
  (let ((sorted (sort-tokens tokens)))
    (let loop ((ts sorted) (prev-line 0) (prev-col 0) (result '()))
      (if (null? ts)
        (list->vector (reverse result))
        (let* ((tok (car ts))
               (line (car tok))
               (col (cadr tok))
               (length (caddr tok))
               (type (cadddr tok))
               (modifiers (car (cddddr tok)))
               (delta-line (- line prev-line))
               (delta-col (if (= delta-line 0) (- col prev-col) col)))
          (loop (cdr ts)
                line col
                (cons* modifiers type length delta-col delta-line result)))))))

;;; Sort tokens by line then column
(def (sort-tokens tokens)
  (sort tokens
    (lambda (a b)
      (let ((la (car a)) (lb (car b)))
        (if (= la lb)
          (< (cadr a) (cadr b))
          (< la lb))))))

;;; cons* helper: cons multiple items onto a list
(def (cons* a b c d e rest)
  (cons a (cons b (cons c (cons d (cons e rest))))))

;;; Find end of line from position i
(def (find-end-of-line text i len)
  (let loop ((j i))
    (if (or (>= j len) (char=? (string-ref text j) #\newline))
      j
      (loop (+ j 1)))))

;;; Find end of string literal (past closing quote)
(def (find-end-of-string text i len)
  (let loop ((j i))
    (cond
      ((>= j len) j)
      ((char=? (string-ref text j) #\\)
       (loop (+ j 2))) ;; skip escaped char
      ((char=? (string-ref text j) #\")
       (+ j 1))
      (else (loop (+ j 1))))))

;;; Find end of character literal
(def (find-end-of-char-literal text i len)
  (if (>= i len) i
    ;; Handle named chars like #\space, #\newline
    (let ((end (find-end-of-token text i len)))
      (if (> end i) end (+ i 1)))))

;;; Find end of a token (symbol, number, etc.)
(def (find-end-of-token text i len)
  (let loop ((j i))
    (if (or (>= j len) (token-delimiter? (string-ref text j)))
      j
      (loop (+ j 1)))))

;;; Check if character is a token delimiter
(def (token-delimiter? c)
  (or (char=? c #\space) (char=? c #\tab) (char=? c #\newline)
      (char=? c #\return) (char=? c #\() (char=? c #\))
      (char=? c #\[) (char=? c #\]) (char=? c #\{) (char=? c #\})
      (char=? c #\") (char=? c #\;)))

;;; Check if a character can start a symbol
(def (symbol-start-char? c)
  (or (char-alphabetic? c)
      (memv c '(#\_ #\! #\? #\* #\+ #\/ #\< #\> #\= #\. #\: #\#
                #\% #\& #\^ #\~ #\-))))

;;; Check if position is a token start (preceded by delimiter or start of text)
(def (token-start? text i)
  (or (= i 0)
      (token-delimiter? (string-ref text (- i 1)))))

;;; Check if a string is all uppercase letters (for constant detection)
(def (all-uppercase? name)
  (let ((len (string-length name)))
    (and (> len 1)
         (let loop ((i 0) (has-alpha? #f))
           (cond
             ((>= i len) has-alpha?)
             ((char-upper-case? (string-ref name i))
              (loop (+ i 1) #t))
             ((or (char=? (string-ref name i) #\_)
                  (char=? (string-ref name i) #\-)
                  (char-numeric? (string-ref name i)))
              (loop (+ i 1) has-alpha?))
             (else #f))))))

;;; Count lines/cols in a span of text
(def (count-span text start end cur-line cur-col)
  (let loop ((i start) (line cur-line) (col cur-col))
    (cond
      ((>= i end) (values line col))
      ((char=? (string-ref text i) #\newline)
       (loop (+ i 1) (+ line 1) 0))
      (else
       (loop (+ i 1) line (+ col 1))))))
