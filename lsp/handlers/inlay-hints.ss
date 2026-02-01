;;; -*- Gerbil -*-
;;; Inlay hints handler — textDocument/inlayHint
;;; Shows parameter names at function call sites
(import :std/format
        :std/sugar
        ../util/log
        ../util/position
        ../types
        ../state
        ../analysis/document
        ../analysis/parser
        ../analysis/symbols
        ../analysis/index)
(export #t)

;;; InlayHintKind constants
(def InlayHintKind.Type      1)
(def InlayHintKind.Parameter 2)

;;; Handle textDocument/inlayHint
;;; Returns InlayHint[] for the visible range
(def (handle-inlay-hint params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (range (hash-ref params "range" (hash)))
         (start (hash-ref range "start" (hash)))
         (end (hash-ref range "end" (hash)))
         (start-line (hash-ref start "line" 0))
         (end-line (hash-ref end "line" 0))
         (doc (get-document uri)))
    (if doc
      (let* ((text (document-text doc))
             (forms (parse-source text))
             (hints (collect-inlay-hints forms text uri start-line end-line)))
        (list->vector hints))
      [])))

;;; Collect inlay hints from parsed forms within a line range
(def (collect-inlay-hints forms text uri start-line end-line)
  (let ((result '()))
    (for-each
      (lambda (lf)
        (let ((form (located-form-form lf))
              (fl (located-form-line lf)))
          ;; Only process forms that overlap the visible range
          (when (and (<= fl end-line)
                     (>= (located-form-end-line lf) start-line))
            (let ((hints (extract-call-hints form text uri fl)))
              (set! result (append result hints))))))
      forms)
    result))

;;; Extract inlay hints from a form (recursively walks subforms)
;;; Looks for function calls where we know the parameter names
(def (extract-call-hints form text uri context-line)
  (if (not (pair? form))
    '()
    (let ((result '()))
      ;; Check if this is a function call with arguments
      (when (and (symbol? (car form))
                 (pair? (cdr form)))
        (let* ((func-name (symbol->string (car form)))
               (args (cdr form))
               (param-names (lookup-param-names func-name uri)))
          (when (and param-names (pair? param-names))
            (set! result (append result
                           (generate-param-hints func-name args param-names
                                                  text context-line))))))
      ;; Recurse into subforms
      (for-each
        (lambda (sub)
          (when (pair? sub)
            (set! result (append result
                           (extract-call-hints sub text uri context-line)))))
        (if (pair? form) (cdr form) '()))
      result)))

;;; Look up parameter names for a function
;;; Returns a list of parameter name strings, or #f
(def (lookup-param-names func-name uri)
  ;; First check local symbols
  (let ((local-syms (get-file-symbols uri)))
    (let ((found (find-func-with-params func-name local-syms)))
      (if found found
        ;; Check workspace symbols
        (let ((defs (find-definitions-by-name func-name)))
          (let loop ((ds defs))
            (if (null? ds) #f
              (let ((info (cdr (car ds))))
                (let ((params (extract-param-names-from-detail info)))
                  (if params params
                    (loop (cdr ds))))))))))))

;;; Find a function symbol and extract its parameter names
(def (find-func-with-params name syms)
  (let loop ((ss syms))
    (if (null? ss) #f
      (let ((s (car ss)))
        (if (and (string=? name (sym-info-name s))
                 (= (sym-info-kind s) SymbolKind.Function))
          (extract-param-names-from-detail s)
          (loop (cdr ss)))))))

;;; Extract parameter names from a sym-info's detail string
;;; Detail is like "(func-name arg1 arg2 . rest)"
(def (extract-param-names-from-detail info)
  (let ((detail (sym-info-detail info)))
    (if (and detail (> (string-length detail) 0))
      (with-catch
        (lambda (e) #f)
        (lambda ()
          (let ((form (read (open-input-string detail))))
            (if (and (pair? form) (pair? (cdr form)))
              (let ((raw-params (cdr form)))
                (param-list->names raw-params))
              #f))))
      #f)))

;;; Convert a parameter list to a list of name strings
;;; Handles: (a b c), (a b . rest), ((a default) b), keyword: args
(def (param-list->names params)
  (cond
    ((null? params) '())
    ((symbol? params) (list (string-append (symbol->string params) "...")))
    ((pair? params)
     (let ((param (car params)))
       (cons
         (cond
           ((pair? param) (format "~a" (car param)))
           ((symbol? param)
            (let ((s (symbol->string param)))
              ;; Skip keyword parameters ending in :
              (if (and (> (string-length s) 0)
                       (char=? (string-ref s (- (string-length s) 1)) #\:))
                s
                s)))
           (else (format "~a" param)))
         (param-list->names (cdr params)))))
    (else '())))

;;; Generate InlayHint objects for parameter names at call sites
;;; We need to find the actual source positions of arguments in the text
(def (generate-param-hints func-name args param-names text context-line)
  (let ((hints '())
        (params param-names)
        (arg-index 0))
    ;; Skip creating hints for common well-known functions where hints are noise
    (if (member func-name '("cons" "car" "cdr" "list" "vector" "hash"
                             "+" "-" "*" "/" "=" "<" ">" "<=" ">="
                             "eq?" "equal?" "string=?" "not" "and" "or"
                             "display" "displayln" "newline" "write"
                             "set!" "hash-ref" "hash-put!"))
      '()
      ;; Only show hints if we have at least 2 params (single-arg is obvious)
      (if (< (length param-names) 2)
        '()
        (let loop ((as args) (ps params) (idx 0))
          (if (or (null? as) (null? ps))
            (reverse hints)
            (let* ((param-name (car ps))
                   ;; Skip keyword params (they're self-documenting)
                   (is-keyword? (and (> (string-length param-name) 0)
                                     (char=? (string-ref param-name
                                               (- (string-length param-name) 1))
                                             #\:))))
              (if is-keyword?
                ;; Skip keyword and its value
                (loop (if (pair? (cdr as)) (cddr as) '())
                      (if (pair? (cdr ps)) (cddr ps) '())
                      (+ idx 2))
                (begin
                  (set! hints (cons (make-inlay-hint
                                      context-line
                                      param-name
                                      InlayHintKind.Parameter)
                                    hints))
                  (loop (cdr as) (cdr ps) (+ idx 1)))))))))))

;;; Create an InlayHint object
;;; Position is approximate — placed at start of line + offset
(def (make-inlay-hint line label kind)
  (hash ("position" (make-lsp-position line 0))
        ("label" (string-append label ":"))
        ("kind" kind)
        ("paddingRight" #t)))
