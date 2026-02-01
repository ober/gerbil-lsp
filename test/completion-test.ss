;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/completion and completion-data
(import :std/test
        :lsp/lsp/types
        :lsp/lsp/state
        :lsp/lsp/analysis/document
        :lsp/lsp/analysis/parser
        :lsp/lsp/analysis/symbols
        :lsp/lsp/analysis/completion-data
        :lsp/lsp/handlers/completion)

(export completion-test-suite)

(def completion-test-suite
  (test-suite "lsp/handlers/completion"

    ;; --- get-completion-prefix ---
    (test-case "get-completion-prefix: simple prefix"
      (check-equal? (get-completion-prefix "(def" 0 4) "def"))

    (test-case "get-completion-prefix: after paren"
      (check-equal? (get-completion-prefix "(de" 0 3) "de"))

    (test-case "get-completion-prefix: at start of line"
      (check (get-completion-prefix "foo" 0 0) => #f))

    (test-case "get-completion-prefix: empty text"
      (check (get-completion-prefix "" 0 0) => #f))

    (test-case "get-completion-prefix: multi-line"
      (check-equal? (get-completion-prefix "line1\nfoo" 1 3) "foo"))

    ;; --- sym-kind->completion-kind ---
    (test-case "sym-kind->completion-kind: function"
      (check (sym-kind->completion-kind SymbolKind.Function)
             => CompletionItemKind.Function))

    (test-case "sym-kind->completion-kind: variable"
      (check (sym-kind->completion-kind SymbolKind.Variable)
             => CompletionItemKind.Variable))

    (test-case "sym-kind->completion-kind: struct"
      (check (sym-kind->completion-kind SymbolKind.Struct)
             => CompletionItemKind.Struct))

    (test-case "sym-kind->completion-kind: class"
      (check (sym-kind->completion-kind SymbolKind.Class)
             => CompletionItemKind.Class))

    (test-case "sym-kind->completion-kind: method"
      (check (sym-kind->completion-kind SymbolKind.Method)
             => CompletionItemKind.Method))

    (test-case "sym-kind->completion-kind: unknown"
      (check (sym-kind->completion-kind 999)
             => CompletionItemKind.Text))

    ;; --- sym-info->completion-item ---
    (test-case "sym-info->completion-item: basic"
      (let* ((s (make-sym-info "my-func" SymbolKind.Function 0 0 0 10
                                "(my-func x y)"))
             (item (sym-info->completion-item s)))
        (check-equal? (hash-ref item "label") "my-func")
        (check (hash-ref item "kind") => CompletionItemKind.Function)
        (check-equal? (hash-ref item "detail") "(my-func x y)")))

    (test-case "sym-info->completion-item: with detail-uri"
      (let* ((s (make-sym-info "foo" SymbolKind.Variable 0 0 0 5 #f))
             (item (sym-info->completion-item s detail-uri: "file:///other.ss")))
        (check-equal? (hash-ref item "label") "foo")
        (check-equal? (hash-ref item "detail") "from file:///other.ss")))

    ;; --- handle-completion: integration ---
    (test-case "handle-completion: returns items for open document"
      (let* ((uri "file:///test-comp.ss")
             (text "(def (add a b) (+ a b))\n(ad")
             (doc (make-document uri 1 text "gerbil"))
             (forms (parse-source text))
             (syms (extract-symbols forms)))
        (set-document! uri doc)
        (set-file-symbols! uri syms)
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))
                             ("position" (hash ("line" 1) ("character" 3)))))
               (result (handle-completion params)))
          (check (hash-table? result) => #t)
          (let ((items (hash-ref result "items" [])))
            ;; Should have at least the "add" symbol from the file
            (check (> (vector-length items) 0) => #t)))
        ;; Cleanup
        (remove-document! uri)
        (remove-file-symbols! uri)))

    (test-case "handle-completion: returns empty for missing document"
      (let* ((params (hash ("textDocument" (hash ("uri" "file:///nonexistent.ss")))
                           ("position" (hash ("line" 0) ("character" 0)))))
             (result (handle-completion params)))
        (check (hash-table? result) => #t)
        (check (vector-length (hash-ref result "items" [])) => 0)))
  ))

(def main
  (lambda ()
    (run-tests! completion-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
