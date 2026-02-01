;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/inlay-hints
(import :std/test
        :lsp/lsp/types
        :lsp/lsp/state
        :lsp/lsp/analysis/symbols
        :lsp/lsp/handlers/inlay-hints)

(export inlay-hints-test-suite)

(def inlay-hints-test-suite
  (test-suite "lsp/handlers/inlay-hints"

    ;; --- InlayHintKind constants ---
    (test-case "InlayHintKind constants"
      (check InlayHintKind.Type => 1)
      (check InlayHintKind.Parameter => 2))

    ;; --- param-list->names ---
    (test-case "param-list->names: simple params"
      (let ((names (param-list->names '(a b c))))
        (check (length names) => 3)
        (check-equal? (car names) "a")
        (check-equal? (cadr names) "b")
        (check-equal? (caddr names) "c")))

    (test-case "param-list->names: rest param"
      (let ((names (param-list->names 'rest)))
        (check (length names) => 1)
        (check-equal? (car names) "rest...")))

    (test-case "param-list->names: empty"
      (check-equal? (param-list->names '()) '()))

    (test-case "param-list->names: dotted pair"
      (let ((names (param-list->names '(a b . rest))))
        (check (>= (length names) 3) => #t)))

    ;; --- extract-param-names-from-detail ---
    (test-case "extract-param-names-from-detail: function sig"
      (let* ((info (make-sym-info "add" SymbolKind.Function 0 0 0 10
                                   "(add a b)"))
             (params (extract-param-names-from-detail info)))
        (check (pair? params) => #t)
        (check (length params) => 2)
        (check-equal? (car params) "a")
        (check-equal? (cadr params) "b")))

    (test-case "extract-param-names-from-detail: no args"
      (let* ((info (make-sym-info "foo" SymbolKind.Function 0 0 0 10
                                   "(foo)"))
             (params (extract-param-names-from-detail info)))
        (check (or (not params) (null? params)) => #t)))

    (test-case "extract-param-names-from-detail: no detail"
      (let* ((info (make-sym-info "bar" SymbolKind.Variable 0 0 0 5 #f))
             (params (extract-param-names-from-detail info)))
        (check (not params) => #t)))

    ;; --- lookup-param-names ---
    (test-case "lookup-param-names: finds in indexed symbols"
      (let ((uri "file:///test-hints.ss"))
        (set-file-symbols! uri
          (list (make-sym-info "my-add" SymbolKind.Function 0 0 0 10
                                "(my-add x y)")))
        (let ((params (lookup-param-names "my-add" uri)))
          (check (pair? params) => #t)
          (check (length params) => 2))
        ;; Cleanup
        (remove-file-symbols! uri)))

    (test-case "lookup-param-names: not found"
      (check (lookup-param-names "nonexistent-func" "file:///test.ss") => #f))

    ;; --- make-inlay-hint ---
    (test-case "make-inlay-hint: creates correct structure"
      (let ((hint (make-inlay-hint 5 "param" InlayHintKind.Parameter)))
        (check (hash-table? hint) => #t)
        (check-equal? (hash-ref hint "label") "param:")
        (check (hash-ref hint "kind") => InlayHintKind.Parameter)
        (check (hash-ref hint "paddingRight") => #t)
        (let ((pos (hash-ref hint "position")))
          (check (hash-ref pos "line") => 5))))

    ;; --- generate-param-hints ---
    (test-case "generate-param-hints: skips common functions"
      (let ((hints (generate-param-hints "cons" '(1 2) '("car" "cdr")
                                          "" 0)))
        (check (length hints) => 0)))

    (test-case "generate-param-hints: skips single-param functions"
      (let ((hints (generate-param-hints "my-func" '(1) '("x")
                                          "" 0)))
        (check (length hints) => 0)))

    (test-case "generate-param-hints: generates for multi-param"
      (let ((hints (generate-param-hints "my-add" '(1 2) '("x" "y")
                                          "" 0)))
        (check (length hints) => 2)))
  ))

(def main
  (lambda ()
    (run-tests! inlay-hints-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
