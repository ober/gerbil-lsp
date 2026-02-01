;;; -*- Gerbil -*-
;;; Completion candidate generation
(import :std/iter
        :std/sugar
        ../types
        ../util/log
        ../state
        ./symbols
        ./parser)
(export #t)

;;; Gerbil special forms and keywords
(def *gerbil-keywords*
  '("def" "define" "defn" "def*"
    "defstruct" "defclass" "defmethod" "defproto"
    "defrule" "defrules" "defsyntax"
    "defvalues" "defconst"
    "deferror-class"
    "lambda" "let" "let*" "letrec" "letrec*"
    "let-values" "let*-values"
    "if" "cond" "case" "when" "unless"
    "and" "or" "not"
    "begin" "begin0"
    "do" "do-while"
    "for" "for*" "for/collect" "for/fold"
    "while"
    "set!" "set!-values"
    "values" "receive" "call-with-values"
    "apply" "call/cc" "call-with-current-continuation"
    "with-catch" "with-exception-handler"
    "raise" "error"
    "import" "export"
    "require" "provide"
    "include"
    "quote" "quasiquote" "unquote" "unquote-splicing"
    "syntax" "syntax-rules" "syntax-case"
    "match" "with"
    "try" "catch" "finally"
    "assert"
    "parameterize"
    "dynamic-wind"
    "guard"
    "delay" "force"
    "declare"
    "using" "with-methods"))

;;; Generate completion candidates for a position in a document
(def (completion-candidates uri text line col)
  (let ((prefix (get-completion-prefix text line col))
        (result '()))
    (lsp-debug "completion prefix: ~s" prefix)
    ;; Local symbols from this file
    (let ((file-syms (get-file-symbols uri)))
      (for-each
        (lambda (s)
          (when (or (not prefix) (string-prefix? prefix (sym-info-name s)))
            (set! result
              (cons (sym-info->completion-item s) result))))
        file-syms))
    ;; Keywords
    (for-each
      (lambda (kw)
        (when (or (not prefix) (string-prefix? prefix kw))
          (set! result
            (cons (hash ("label" kw)
                        ("kind" CompletionItemKind.Keyword)
                        ("detail" "keyword"))
                  result))))
      *gerbil-keywords*)
    ;; Symbols from all indexed files
    (hash-for-each
      (lambda (other-uri syms)
        (unless (string=? other-uri uri)
          (for-each
            (lambda (s)
              (when (or (not prefix) (string-prefix? prefix (sym-info-name s)))
                (set! result
                  (cons (sym-info->completion-item s detail-uri: other-uri)
                        result))))
            syms)))
      *symbol-index*)
    result))

;;; Convert a sym-info to a CompletionItem
(def (sym-info->completion-item s detail-uri: (detail-uri #f))
  (let ((kind (sym-kind->completion-kind (sym-info-kind s)))
        (detail (or (sym-info-detail s)
                    (and detail-uri (string-append "from " detail-uri))
                    "")))
    (hash ("label" (sym-info-name s))
          ("kind" kind)
          ("detail" detail))))

;;; Map SymbolKind to CompletionItemKind
(def (sym-kind->completion-kind sk)
  (cond
    ((= sk SymbolKind.Function) CompletionItemKind.Function)
    ((= sk SymbolKind.Variable) CompletionItemKind.Variable)
    ((= sk SymbolKind.Constant) CompletionItemKind.Variable)
    ((= sk SymbolKind.Struct)   CompletionItemKind.Struct)
    ((= sk SymbolKind.Class)    CompletionItemKind.Class)
    ((= sk SymbolKind.Method)   CompletionItemKind.Method)
    (else CompletionItemKind.Text)))

;;; Get the symbol prefix at the cursor for filtering
;;; Returns a string prefix or #f if no prefix
(def (get-completion-prefix text line col)
  (let ((line-text (get-line-text text line)))
    (if (or (string=? line-text "") (= col 0))
      #f
      (let loop ((i (min (- col 1) (- (string-length line-text) 1)))
                 (acc '()))
        (if (or (< i 0) (not (completion-char? (string-ref line-text i))))
          (if (null? acc) #f (list->string acc))
          (loop (- i 1) (cons (string-ref line-text i) acc)))))))

;;; Get a line from text by line number
(def (get-line-text text line-num)
  (let loop ((i 0) (cur 0) (start 0))
    (cond
      ((>= i (string-length text))
       (if (= cur line-num) (substring text start i) ""))
      ((char=? (string-ref text i) #\newline)
       (if (= cur line-num)
         (substring text start i)
         (loop (+ i 1) (+ cur 1) (+ i 1))))
      (else (loop (+ i 1) cur start)))))

;;; Characters that can appear in completion prefixes
(def (completion-char? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (memv c '(#\- #\_ #\! #\? #\* #\+ #\/ #\< #\> #\= #\. #\: #\#))))
