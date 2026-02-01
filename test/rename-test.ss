;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/rename
(import :std/test
        :lsp/lsp/types
        :lsp/lsp/util/position
        :lsp/lsp/handlers/rename)

(export rename-test-suite)

(def rename-test-suite
  (test-suite "lsp/handlers/rename"

    ;; --- find-edits-in-line ---
    (test-case "find-edits-in-line: single occurrence"
      (let ((edits (find-edits-in-line "(def foo 1)" 0 11 0
                                        "foo" 3 "bar")))
        (check (length edits) => 1)
        (let ((edit (car edits)))
          (check-equal? (hash-ref edit "newText") "bar"))))

    (test-case "find-edits-in-line: multiple occurrences"
      (let ((edits (find-edits-in-line "(+ x x)" 0 7 0
                                        "x" 1 "y")))
        (check (length edits) => 2)))

    (test-case "find-edits-in-line: no match"
      (let ((edits (find-edits-in-line "(def foo 1)" 0 11 0
                                        "bar" 3 "baz")))
        (check (length edits) => 0)))

    (test-case "find-edits-in-line: respects word boundaries"
      (let ((edits (find-edits-in-line "foobar foo" 0 10 0
                                        "foo" 3 "baz")))
        ;; Only standalone "foo" should match
        (check (length edits) => 1)))

    (test-case "find-edits-in-line: empty line"
      (let ((edits (find-edits-in-line "" 0 0 0
                                        "foo" 3 "bar")))
        (check (length edits) => 0)))

    ;; --- find-rename-edits-in-text ---
    (test-case "find-rename-edits-in-text: multi-line"
      (let ((edits (find-rename-edits-in-text
                     "(def (add a b) (+ a b))\n(add 1 2)"
                     "add" "plus")))
        ;; "add" appears twice: definition and call
        (check (>= (length edits) 2) => #t)
        (for-each
          (lambda (edit)
            (check-equal? (hash-ref edit "newText") "plus"))
          edits)))

    (test-case "find-rename-edits-in-text: no matches"
      (let ((edits (find-rename-edits-in-text
                     "(def x 1)"
                     "nonexistent" "new-name")))
        (check (length edits) => 0)))

    (test-case "find-rename-edits-in-text: single line"
      (let ((edits (find-rename-edits-in-text
                     "(set! x (+ x 1))"
                     "x" "y")))
        ;; "x" appears twice
        (check (length edits) => 2)))
  ))

(def main
  (lambda ()
    (run-tests! rename-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
