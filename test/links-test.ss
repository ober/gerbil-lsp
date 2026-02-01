;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/links
(import :std/test
        :lsp/lsp/analysis/parser
        :lsp/lsp/handlers/links)

(export links-test-suite)

(def links-test-suite
  (test-suite "lsp/handlers/links"

    ;; --- format-import-spec ---
    (test-case "format-import-spec: symbol"
      (check-equal? (format-import-spec ':std/sugar) ":std/sugar"))

    (test-case "format-import-spec: pair with symbol head"
      (check-equal? (format-import-spec '(only-in :std/sugar when-let))
                    "only-in"))

    (test-case "format-import-spec: non-symbol non-pair"
      (check-equal? (format-import-spec 42) ""))

    ;; --- collect-document-links ---
    (test-case "collect-document-links: no imports"
      (let* ((text "(def x 1)")
             (forms (parse-source text)))
        (check-equal? (collect-document-links forms "/tmp/test.ss") '())))

    (test-case "collect-document-links: with imports"
      ;; This test checks structure, resolution depends on filesystem
      (let* ((text "(import :std/sugar)")
             (forms (parse-source text))
             (links (collect-document-links forms "/tmp/test.ss")))
        ;; Links may be empty if the module can't be resolved,
        ;; but the function should not error
        (check (list? links) => #t)))

    ;; --- make-import-link ---
    (test-case "make-import-link: unresolvable returns #f"
      ;; A bogus import spec that can't be resolved
      (check (make-import-link ':nonexistent/module "/tmp/test.ss" 1) => #f))

    (test-case "make-import-link: relative import unresolvable"
      ;; Relative import to non-existing file
      (check (make-import-link './nonexistent "/tmp/test.ss" 1) => #f))
  ))

(def main
  (lambda ()
    (run-tests! links-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
