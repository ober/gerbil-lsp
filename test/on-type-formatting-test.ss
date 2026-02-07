;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/on-type-formatting
(import :std/test
        :lsp/lsp/state
        :lsp/lsp/analysis/document
        :lsp/lsp/handlers/on-type-formatting)

(export on-type-formatting-test-suite)

(def on-type-formatting-test-suite
  (test-suite "lsp/handlers/on-type-formatting"

    ;; --- paren-depth-at ---
    (test-case "paren-depth-at: no parens"
      (check (paren-depth-at "hello" 5) => 0))

    (test-case "paren-depth-at: one open"
      (check (paren-depth-at "(hello" 6) => 1))

    (test-case "paren-depth-at: balanced"
      (check (paren-depth-at "(hello)" 7) => 0))

    (test-case "paren-depth-at: nested"
      (check (paren-depth-at "((hello)" 8) => 1))

    (test-case "paren-depth-at: ignores parens in strings"
      (check (paren-depth-at "\"(\"" 3) => 0))

    (test-case "paren-depth-at: ignores parens in comments"
      (check (paren-depth-at "; (\n" 4) => 0))

    (test-case "paren-depth-at: partial offset"
      ;; At offset 5, only the first ( at position 0 has been counted
      (check (paren-depth-at "(def (foo x) x)" 5) => 1))

    ;; --- calculate-indent ---
    (test-case "calculate-indent: line 0"
      (check (calculate-indent "(def x 1)" 0) => 0))

    (test-case "calculate-indent: after open paren"
      (check (calculate-indent "(def (foo x)\n  x)" 1) => 2))

    ;; --- count-leading-spaces ---
    (test-case "count-leading-spaces: no spaces"
      (check (count-leading-spaces "hello") => 0))

    (test-case "count-leading-spaces: 2 spaces"
      (check (count-leading-spaces "  hello") => 2))

    (test-case "count-leading-spaces: all spaces"
      (check (count-leading-spaces "   ") => 3))

    (test-case "count-leading-spaces: empty string"
      (check (count-leading-spaces "") => 0))

    ;; --- make-spaces ---
    (test-case "make-spaces: 0 spaces"
      (check-equal? (make-spaces 0) ""))

    (test-case "make-spaces: 4 spaces"
      (check-equal? (make-spaces 4) "    "))

    ;; --- handle-on-type-formatting: integration ---
    (test-case "handle-on-type-formatting: returns edits on newline"
      (let* ((uri "file:///test-otf.ss")
             (text "(def (foo x)\n  x)")
             (doc (make-document uri 1 text "gerbil")))
        (set-document! uri doc)
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))
                             ("position" (hash ("line" 1) ("character" 0)))
                             ("ch" "\n")
                             ("options" (hash ("tabSize" 2)
                                              ("insertSpaces" #t)))))
               (result (handle-on-type-formatting params)))
          (check (vector? result) => #t))
        (remove-document! uri)))

    (test-case "handle-on-type-formatting: empty for close paren"
      (let* ((uri "file:///test-otf2.ss")
             (text "(def x 1)")
             (doc (make-document uri 1 text "gerbil")))
        (set-document! uri doc)
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))
                             ("position" (hash ("line" 0) ("character" 8)))
                             ("ch" ")")
                             ("options" (hash ("tabSize" 2)
                                              ("insertSpaces" #t)))))
               (result (handle-on-type-formatting params)))
          (check (vector? result) => #t)
          (check (vector-length result) => 0))
        (remove-document! uri)))

    (test-case "handle-on-type-formatting: empty for missing document"
      (let* ((params (hash ("textDocument" (hash ("uri" "file:///nonexistent.ss")))
                           ("position" (hash ("line" 0) ("character" 0)))
                           ("ch" "\n")
                           ("options" (hash))))
             (result (handle-on-type-formatting params)))
        (check (vector? result) => #t)
        (check (vector-length result) => 0)))
  ))

(def main
  (lambda ()
    (run-tests! on-type-formatting-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
