;;; -*- Gerbil -*-
;;; Code action handler — textDocument/codeAction
;;; Provides organize imports and add missing import actions
(import :std/format
        :std/sugar
        :std/sort
        ../util/log
        ../util/position
        ../util/string
        ../types
        ../state
        ../analysis/document
        ../analysis/parser
        ../analysis/symbols
        ../analysis/completion-data)
(export #t)

;;; CodeActionKind constants
(def CodeActionKind.QuickFix            "quickfix")
(def CodeActionKind.Source              "source")
(def CodeActionKind.SourceOrganizeImports "source.organizeImports")

;;; Handle textDocument/codeAction
;;; Returns CodeAction[] with available actions (lazy — no edit, just data)
(def (handle-code-action params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (range (hash-ref params "range" (hash)))
         (context (hash-ref params "context" (hash)))
         (doc (get-document uri)))
    (if doc
      (let ((text (document-text doc))
            (actions '()))
        ;; Always offer "organize imports" if there are imports
        (let ((organize (make-organize-imports-action uri text)))
          (when organize
            (set! actions (cons organize actions))))
        ;; Offer "add missing import" based on diagnostics
        (let ((diags (hash-ref context "diagnostics" [])))
          (let ((import-actions (make-add-import-actions uri text diags)))
            (set! actions (append actions import-actions)))
          ;; Offer "remove unused import" for unused-import diagnostics
          (let ((unused-actions (make-remove-unused-import-actions uri text diags)))
            (set! actions (append actions unused-actions))))
        (list->vector actions))
      [])))

;;; Handle codeAction/resolve
;;; Receives a code action with data, computes the edit, returns the full action
(def (handle-code-action-resolve params)
  (let ((data (hash-ref params "data" #f)))
    (if (not data)
      params  ;; No data — return as-is
      (let ((type (hash-ref data "type" ""))
            (uri (hash-ref data "uri" "")))
        (with-catch
          (lambda (e)
            (lsp-debug "code action resolve failed: ~a" e)
            params)
          (lambda ()
            (let ((doc (get-document uri)))
              (if (not doc)
                params
                (let ((text (document-text doc)))
                  (cond
                    ((string=? type "organize-imports")
                     (let ((edit (compute-organize-imports-edit uri text)))
                       (if edit
                         (begin (hash-put! params "edit" edit) params)
                         params)))
                    ((string=? type "add-import")
                     (let* ((module-name (hash-ref data "module" ""))
                            (edit (compute-add-import-edit uri text module-name)))
                       (hash-put! params "edit" edit)
                       params))
                    ((string=? type "remove-import")
                     (let* ((line (hash-ref data "line" 0))
                            (edit (make-remove-import-edit uri text
                                    (hash-ref data "importName" "") line)))
                       (if edit
                         (begin (hash-put! params "edit" edit) params)
                         params)))
                    (else params)))))))))))

;;; Create an "Organize Imports" code action (lazy — data only, no edit)
(def (make-organize-imports-action uri text)
  (let ((forms (parse-source text)))
    (let ((import-forms (find-import-forms forms)))
      (if (null? import-forms)
        #f
        (let ((all-specs (collect-all-import-specs import-forms)))
          (if (< (length all-specs) 2)
            #f
            (hash
              ("title" "Organize Imports")
              ("kind" CodeActionKind.SourceOrganizeImports)
              ("data" (hash ("type" "organize-imports")
                            ("uri" uri))))))))))

;;; Find all top-level import forms
(def (find-import-forms forms)
  (let loop ((fs forms) (result '()))
    (if (null? fs)
      (reverse result)
      (let ((lf (car fs)))
        (if (and (pair? (located-form-form lf))
                 (eq? (car (located-form-form lf)) 'import))
          (loop (cdr fs) (cons lf result))
          (loop (cdr fs) result))))))

;;; Collect all import specs from multiple import forms
(def (collect-all-import-specs import-forms)
  (let ((specs '()))
    (for-each
      (lambda (lf)
        (let ((form (located-form-form lf)))
          (when (pair? (cdr form))
            (for-each
              (lambda (spec) (set! specs (cons spec specs)))
              (cdr form)))))
      import-forms)
    (reverse specs)))

;;; Sort import specs alphabetically by their string representation
(def (sort-import-specs specs)
  (let ((pairs (map (lambda (s) (cons (import-spec->sort-key s) s)) specs)))
    (let ((sorted (sort pairs (lambda (a b) (string<? (car a) (car b))))))
      (map cdr sorted))))

;;; Get a sort key for an import spec
(def (import-spec->sort-key spec)
  (cond
    ((symbol? spec) (symbol->string spec))
    ((pair? spec) (format "~a" spec))
    (else "")))

;;; Format sorted imports as a single import form
(def (format-organized-imports specs)
  (if (null? specs)
    ""
    (let ((out (open-output-string)))
      (display "(import" out)
      (for-each
        (lambda (spec)
          (display "\n        " out)
          (write spec out))
        specs)
      (display ")" out)
      (get-output-string out))))

;;; Compute the workspace edit for organizing imports
(def (compute-organize-imports-edit uri text)
  (let ((forms (parse-source text)))
    (let ((import-forms (find-import-forms forms)))
      (if (null? import-forms)
        #f
        (let ((all-specs (collect-all-import-specs import-forms)))
          (if (< (length all-specs) 2)
            #f
            (let* ((sorted-specs (sort-import-specs all-specs))
                   (first-import (car import-forms))
                   (last-import (last-elem import-forms))
                   (start-line (located-form-line first-import))
                   (end-line (located-form-end-line last-import))
                   (end-col (located-form-end-col last-import))
                   (new-text (format-organized-imports sorted-specs)))
              (hash ("changes"
                     (hash (uri
                            (vector
                              (make-text-edit
                                (make-lsp-range start-line 0 end-line end-col)
                                new-text)))))))))))))

;;; Compute the workspace edit for adding an import
(def (compute-add-import-edit uri text module-name)
  (let* ((forms (parse-source text))
         (insert-pos (find-import-insert-position forms text)))
    (hash ("changes"
           (hash (uri
                  (vector
                    (make-text-edit
                      (make-lsp-range (car insert-pos) (cdr insert-pos)
                                      (car insert-pos) (cdr insert-pos))
                      (format "\n(import ~a)" module-name)))))))))

;;; Create "Add missing import" code actions from diagnostics
;;; Looks for unbound identifier errors and suggests imports
(def (make-add-import-actions uri text diags)
  (let ((actions '()))
    (for-each
      (lambda (diag)
        (let ((msg (if (vector? diag)
                     ""
                     (hash-ref diag "message" ""))))
          ;; Look for "unbound identifier" patterns
          (let ((sym-name (extract-unbound-symbol msg)))
            (when sym-name
              (let ((suggestions (suggest-import-for sym-name)))
                (for-each
                  (lambda (module-name)
                    (set! actions
                      (cons (make-add-import-action uri text sym-name module-name)
                            actions)))
                  suggestions))))))
      (if (vector? diags) (vector->list diags) '()))
    actions))

;;; Extract unbound symbol name from an error message
(def (extract-unbound-symbol msg)
  (cond
    ;; Pattern: "unbound identifier: foo"
    ((string-contains msg "unbound identifier")
     (let ((parts (string-split-on-colon msg)))
       (if (>= (length parts) 2)
         (string-trim-whitespace (list-ref-safe parts (- (length parts) 1)))
         #f)))
    ;; Pattern: "Unbound variable: foo"
    ((string-contains msg "Unbound variable")
     (let ((parts (string-split-on-colon msg)))
       (if (>= (length parts) 2)
         (string-trim-whitespace (list-ref-safe parts (- (length parts) 1)))
         #f)))
    (else #f)))

;;; Suggest module imports for a symbol
;;; Uses the stdlib symbols list from completion-data
(def (suggest-import-for sym-name)
  (let ((result '()))
    (for-each
      (lambda (entry)
        (when (string=? (car entry) sym-name)
          (set! result (cons (cadr entry) result))))
      *stdlib-symbols*)
    (reverse result)))

;;; Create a single "Add import" code action (lazy — data only)
(def (make-add-import-action uri text sym-name module-name)
  (hash
    ("title" (format "Add import ~a from ~a" sym-name module-name))
    ("kind" CodeActionKind.QuickFix)
    ("data" (hash ("type" "add-import")
                  ("uri" uri)
                  ("symbol" sym-name)
                  ("module" module-name)))))

;;; Find where to insert a new import statement
;;; Returns (line . col) — after the last existing import, or at line 0
(def (find-import-insert-position forms text)
  (let ((last-import-end 0))
    (for-each
      (lambda (lf)
        (let ((form (located-form-form lf)))
          (when (and (pair? form) (eq? (car form) 'import))
            (set! last-import-end (located-form-end-line lf)))))
      forms)
    (cons last-import-end 0)))

;;; Split string on colon
(def (string-split-on-colon str)
  (let loop ((i 0) (start 0) (parts '()))
    (cond
      ((>= i (string-length str))
       (reverse (cons (substring str start i) parts)))
      ((char=? (string-ref str i) #\:)
       (loop (+ i 1) (+ i 1) (cons (substring str start i) parts)))
      (else (loop (+ i 1) start parts)))))

;;; Trim leading/trailing whitespace from a string
(def (string-trim-whitespace str)
  (let* ((len (string-length str))
         (start (let loop ((i 0))
                  (if (or (>= i len)
                          (not (char-whitespace-trim? (string-ref str i))))
                    i
                    (loop (+ i 1)))))
         (end (let loop ((i (- len 1)))
                (if (or (< i start)
                        (not (char-whitespace-trim? (string-ref str i))))
                  (+ i 1)
                  (loop (- i 1))))))
    (substring str start end)))

(def (char-whitespace-trim? c)
  (or (char=? c #\space) (char=? c #\tab) (char=? c #\newline) (char=? c #\return)))

;;; Safe list-ref
(def (list-ref-safe lst n)
  (let loop ((l lst) (i 0))
    (cond
      ((null? l) "")
      ((= i n) (car l))
      (else (loop (cdr l) (+ i 1))))))

;;; Get last element of a list
(def (last-elem lst)
  (if (null? (cdr lst))
    (car lst)
    (last-elem (cdr lst))))

;;; Create "Remove unused import" code actions from diagnostics (lazy — data only)
(def (make-remove-unused-import-actions uri text diags)
  (let ((actions '()))
    (for-each
      (lambda (diag)
        (when (hash-table? diag)
          (let ((code (hash-ref diag "code" ""))
                (msg (hash-ref diag "message" "")))
            (when (string=? code "unused-import")
              (let ((import-name (extract-unused-import-name msg)))
                (when import-name
                  (let ((range (hash-ref diag "range" #f)))
                    (when range
                      (let* ((start (hash-ref range "start" (hash)))
                             (line (hash-ref start "line" 0)))
                        (set! actions
                          (cons (hash
                                  ("title" (format "Remove unused import: ~a" import-name))
                                  ("kind" CodeActionKind.QuickFix)
                                  ("diagnostics" (vector diag))
                                  ("data" (hash ("type" "remove-import")
                                                ("uri" uri)
                                                ("importName" import-name)
                                                ("line" line))))
                                actions)))))))))))
      (if (vector? diags) (vector->list diags) '()))
    actions))

;;; Extract the import name from an "Unused import: X" message
(def (extract-unused-import-name msg)
  (if (string-prefix? "Unused import: " msg)
    (substring msg 15 (string-length msg))
    #f))

;;; Create a workspace edit that removes an import spec from the file
(def (make-remove-import-edit uri text import-name line)
  (let* ((lines (string-split-lines text))
         (line-text (if (< line (length lines))
                      (list-ref lines line)
                      ""))
         ;; Check if this line is a standalone import like (import :foo/bar)
         ;; or part of a multi-spec import
         (trimmed (string-trim-whitespace line-text)))
    ;; Simple case: the import spec is on its own line within a multi-line import
    ;; Remove the entire line
    (if (and (> line 0) (< line (length lines)))
      (hash ("changes"
             (hash (uri
                    (vector
                      (make-text-edit
                        (make-lsp-range line 0 (+ line 1) 0)
                        ""))))))
      #f)))

;;; String split into lines helper
(def (string-split-lines text)
  (let loop ((i 0) (start 0) (lines '()))
    (cond
      ((>= i (string-length text))
       (reverse (cons (substring text start i) lines)))
      ((char=? (string-ref text i) #\newline)
       (loop (+ i 1) (+ i 1) (cons (substring text start i) lines)))
      (else
       (loop (+ i 1) start lines)))))

