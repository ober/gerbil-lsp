;;; -*- Gerbil -*-
;;; Symbol extraction from parsed Gerbil source forms
(import :std/format
        :std/iter
        :std/sugar
        ../types
        ../util/log
        ./parser)
(export #t)

;;; A symbol definition record
(defstruct sym-info (name kind line col end-line end-col detail) transparent: #t)

;;; Extract all symbol definitions from a list of located-forms
(def (extract-symbols located-forms)
  (let ((result '()))
    (for-each
      (lambda (lf)
        (let ((syms (extract-form-symbols (located-form-form lf)
                                           (located-form-line lf)
                                           (located-form-col lf)
                                           (located-form-end-line lf)
                                           (located-form-end-col lf))))
          (set! result (append result syms))))
      located-forms)
    result))

;;; Extract symbols from a single form
(def (extract-form-symbols form line col end-line end-col)
  (if (not (pair? form)) '()
    (let ((head (car form)))
      (cond
        ;; (def name ...) or (def (name args...) body...)
        ((memq head '(def define defn))
         (extract-def-symbol form line col end-line end-col))
        ;; (def* name ...)
        ((eq? head 'def*)
         (extract-def*-symbol form line col end-line end-col))
        ;; (defstruct name ...)
        ((eq? head 'defstruct)
         (extract-defstruct-symbol form line col end-line end-col))
        ;; (defclass name ...)
        ((eq? head 'defclass)
         (extract-defclass-symbol form line col end-line end-col))
        ;; (defmethod {name type} ...)
        ((eq? head 'defmethod)
         (extract-defmethod-symbol form line col end-line end-col))
        ;; (defrule/defrules/defsyntax name ...)
        ((memq head '(defrule defrules defsyntax defsyntax-call))
         (extract-macro-symbol form line col end-line end-col))
        ;; (defvalues (name ...) expr)
        ((eq? head 'defvalues)
         (extract-defvalues-symbols form line col end-line end-col))
        ;; (defconst name value)
        ((eq? head 'defconst)
         (extract-const-symbol form line col end-line end-col))
        ;; (deferror-class name ...)
        ((eq? head 'deferror-class)
         (extract-error-class-symbol form line col end-line end-col))
        (else '())))))

;;; Extract from (def name expr) or (def (name . args) body...)
(def (extract-def-symbol form line col end-line end-col)
  (if (< (length form) 2) '()
    (let ((target (cadr form)))
      (cond
        ;; (def (name arg ...) body...)
        ((pair? target)
         (let ((name (car target)))
           (if (symbol? name)
             (list (make-sym-info (symbol->string name)
                                  SymbolKind.Function line col end-line end-col
                                  (format-def-signature target)))
             '())))
        ;; (def name expr)
        ((symbol? target)
         (list (make-sym-info (symbol->string target)
                              SymbolKind.Variable line col end-line end-col
                              #f)))
        (else '())))))

;;; Extract from (def* name clause ...)
(def (extract-def*-symbol form line col end-line end-col)
  (if (< (length form) 2) '()
    (let ((name (cadr form)))
      (if (symbol? name)
        (list (make-sym-info (symbol->string name)
                             SymbolKind.Function line col end-line end-col
                             "(multi-arity function)"))
        '()))))

;;; Extract from (defstruct name (fields...) options...)
(def (extract-defstruct-symbol form line col end-line end-col)
  (if (< (length form) 2) '()
    (let ((name (cadr form)))
      (cond
        ;; (defstruct (name super) ...)
        ((pair? name)
         (let ((actual-name (car name)))
           (if (symbol? actual-name)
             (list (make-sym-info (symbol->string actual-name)
                                  SymbolKind.Struct line col end-line end-col
                                  (format "struct ~a" actual-name)))
             '())))
        ((symbol? name)
         (list (make-sym-info (symbol->string name)
                              SymbolKind.Struct line col end-line end-col
                              (format "struct ~a" name))))
        (else '())))))

;;; Extract from (defclass name ...)
(def (extract-defclass-symbol form line col end-line end-col)
  (if (< (length form) 2) '()
    (let ((name (cadr form)))
      (cond
        ((pair? name)
         (let ((actual-name (car name)))
           (if (symbol? actual-name)
             (list (make-sym-info (symbol->string actual-name)
                                  SymbolKind.Class line col end-line end-col
                                  (format "class ~a" actual-name)))
             '())))
        ((symbol? name)
         (list (make-sym-info (symbol->string name)
                              SymbolKind.Class line col end-line end-col
                              (format "class ~a" name))))
        (else '())))))

;;; Extract from (defmethod {name type} ...)
(def (extract-defmethod-symbol form line col end-line end-col)
  (if (< (length form) 2) '()
    (let ((spec (cadr form)))
      (cond
        ;; {name type} is read as a hash-table dispatch form
        ;; In Gerbil, (defmethod {method-name Type} ...) uses braces
        ((pair? spec)
         (let ((name (car spec)))
           (if (symbol? name)
             (list (make-sym-info (symbol->string name)
                                  SymbolKind.Method line col end-line end-col
                                  "method"))
             '())))
        ((symbol? spec)
         (list (make-sym-info (symbol->string spec)
                              SymbolKind.Method line col end-line end-col
                              "method")))
        (else '())))))

;;; Extract from macro definitions
(def (extract-macro-symbol form line col end-line end-col)
  (if (< (length form) 2) '()
    (let ((name (cadr form)))
      (if (symbol? name)
        (list (make-sym-info (symbol->string name)
                             SymbolKind.Function line col end-line end-col
                             "macro"))
        '()))))

;;; Extract from (defvalues (a b c) expr)
(def (extract-defvalues-symbols form line col end-line end-col)
  (if (< (length form) 2) '()
    (let ((names (cadr form)))
      (if (pair? names)
        (filter-map
          (lambda (n)
            (if (symbol? n)
              (make-sym-info (symbol->string n)
                             SymbolKind.Variable line col end-line end-col
                             "values binding")
              #f))
          names)
        '()))))

;;; Extract from (defconst name value)
(def (extract-const-symbol form line col end-line end-col)
  (if (< (length form) 2) '()
    (let ((name (cadr form)))
      (if (symbol? name)
        (list (make-sym-info (symbol->string name)
                             SymbolKind.Constant line col end-line end-col
                             "constant"))
        '()))))

;;; Extract from (deferror-class name ...)
(def (extract-error-class-symbol form line col end-line end-col)
  (if (< (length form) 2) '()
    (let ((name (cadr form)))
      (if (symbol? name)
        (list (make-sym-info (symbol->string name)
                             SymbolKind.Class line col end-line end-col
                             "error class"))
        '()))))

;;; Extract import specs from located forms
(def (extract-imports located-forms)
  (let ((imports '()))
    (for-each
      (lambda (lf)
        (let ((form (located-form-form lf)))
          (when (and (pair? form) (eq? (car form) 'import))
            (for-each
              (lambda (spec)
                (set! imports (cons spec imports)))
              (cdr form)))))
      located-forms)
    (reverse imports)))

;;; Extract export specs from located forms
(def (extract-exports located-forms)
  (let ((exports '()))
    (for-each
      (lambda (lf)
        (let ((form (located-form-form lf)))
          (when (and (pair? form) (eq? (car form) 'export))
            (for-each
              (lambda (spec)
                (set! exports (cons spec exports)))
              (cdr form)))))
      located-forms)
    (reverse exports)))

;;; Find a symbol by name in a list of sym-info
(def (find-sym-by-name name syms)
  (let loop ((ss syms))
    (if (null? ss) #f
      (if (string=? name (sym-info-name (car ss)))
        (car ss)
        (loop (cdr ss))))))

;;; Format a definition signature for display
(def (format-def-signature target)
  (if (pair? target)
    (let ((name (car target))
          (args (cdr target)))
      (format "(~a~a)" name
        (if (null? args) ""
          (string-append " " (format-args args)))))
    #f))

(def (format-args args)
  (cond
    ((null? args) "")
    ((symbol? args) (format ". ~a" args))
    ((pair? args)
     (string-append
       (format "~a" (car args))
       (if (null? (cdr args)) ""
         (if (pair? (cdr args))
           (string-append " " (format-args (cdr args)))
           (format " . ~a" (cdr args))))))
    (else (format "~a" args))))
