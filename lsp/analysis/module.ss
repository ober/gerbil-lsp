;;; -*- Gerbil -*-
;;; Module resolution — resolve import specs to file paths and exports
(import :std/sugar
        :std/iter
        :std/misc/ports
        :std/misc/path
        ../util/log
        ../state
        ./parser
        ./symbols)
(export #t)

;;; Resolve an import spec to a filesystem path
;;; Import specs can be:
;;;   :std/text/json     — standard library module
;;;   ./foo              — relative to current file
;;;   ../bar             — relative to parent
;;;   :mypackage/module  — installed package
(def (resolve-import-spec spec current-file-path)
  (cond
    ;; Symbol like :std/text/json
    ((symbol? spec)
     (let ((str (symbol->string spec)))
       (cond
         ;; Relative imports: ./foo or ../foo
         ((string-prefix? "./" str)
          (resolve-relative-import str current-file-path))
         ((string-prefix? "../" str)
          (resolve-relative-import str current-file-path))
         ;; Standard library or package
         ((string-prefix? ":" str)
          (resolve-std-module str))
         (else #f))))
    ;; Complex import spec like (only-in :std/text/json read-json)
    ((pair? spec)
     (let ((head (car spec)))
       (cond
         ((memq head '(only-in except-in rename-in prefix-in))
          (if (> (length spec) 1)
            (resolve-import-spec (cadr spec) current-file-path)
            #f))
         (else
          (resolve-import-spec head current-file-path)))))
    (else #f)))

;;; Resolve a relative import path
(def (resolve-relative-import rel-path current-file-path)
  (let* ((dir (path-directory current-file-path))
         (base (string-append dir "/" rel-path ".ss")))
    (let ((resolved (path-normalize base)))
      (if (file-exists? resolved)
        resolved
        ;; Try without .ss extension (it might already have it)
        (let ((alt (path-normalize (string-append dir "/" rel-path))))
          (if (file-exists? alt) alt #f))))))

;;; Resolve a standard library or package module
;;; Returns the source path if available, #f otherwise
(def (resolve-std-module module-str)
  (let* ((clean (if (string-prefix? ":" module-str)
                  (substring module-str 1 (string-length module-str))
                  module-str))
         ;; Try Gerbil installation paths
         (gerbil-home (getenv "GERBIL_HOME" "/opt/gerbil"))
         (src-path (string-append gerbil-home "/src/" clean ".ss")))
    (if (file-exists? src-path)
      src-path
      ;; Try user's .gerbil/pkg
      (let ((pkg-path (string-append (getenv "HOME" "") "/.gerbil/pkg/"
                                      clean ".ss")))
        (if (file-exists? pkg-path) pkg-path #f)))))

;;; Get exports for a module, using cache
(def (get-or-resolve-module-exports module-spec current-file)
  (let ((cached (get-module-exports module-spec)))
    (or cached
        (let ((path (resolve-import-spec module-spec current-file)))
          (if path
            (let ((exports (analyze-file-exports path)))
              (set-module-exports! module-spec exports)
              exports)
            '())))))

;;; Analyze a file to get its exported symbols
(def (analyze-file-exports file-path)
  (with-catch
    (lambda (e)
      (lsp-debug "failed to analyze exports of ~a: ~a" file-path e)
      '())
    (lambda ()
      (let* ((text (read-file-string file-path))
             (forms (parse-source text))
             (syms (extract-symbols forms))
             (exports (extract-exports forms)))
        ;; If (export #t), all symbols are exported
        (if (member #t exports)
          syms
          ;; Otherwise filter to only exported names
          (let ((export-names (filter symbol? exports)))
            (if (null? export-names)
              syms  ; if no explicit exports, assume all
              (filter
                (lambda (s)
                  (member (string->symbol (sym-info-name s)) export-names))
                syms))))))))

;;; Get all symbols available from imports in a document
(def (get-imported-symbols located-forms current-file)
  (let ((imports (extract-imports located-forms))
        (result '()))
    (for-each
      (lambda (spec)
        (let ((exports (get-or-resolve-module-exports spec current-file)))
          (set! result (append result exports))))
      imports)
    result))

;;; Convert a URI to a file path with percent-decoding
(def (uri->file-path uri)
  (if (string-prefix? "file://" uri)
    (uri-decode (substring uri 7 (string-length uri)))
    uri))

;;; Convert a filesystem path to a file URI
(def (path->uri path)
  (string-append "file://" path))

;;; Basic URI percent-decoding (%20 → space, etc.)
(def (uri-decode str)
  (let loop ((i 0) (acc '()))
    (cond
      ((>= i (string-length str))
       (list->string (reverse acc)))
      ((and (char=? (string-ref str i) #\%)
            (< (+ i 2) (string-length str)))
       (let ((hex (substring str (+ i 1) (+ i 3))))
         (let ((code (string->number hex 16)))
           (if code
             (loop (+ i 3) (cons (integer->char code) acc))
             (loop (+ i 1) (cons #\% acc))))))
      (else
       (loop (+ i 1) (cons (string-ref str i) acc))))))
