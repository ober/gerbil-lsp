;;; -*- Gerbil -*-
;;; Workspace-wide symbol index
(import :std/iter
        :std/sugar
        :std/misc/ports
        ../util/log
        ../util/position
        ../state
        ./parser
        ./symbols)
(export #t)

;;; Index all .ss files in the workspace
(def (index-workspace! root)
  (when root
    (lsp-info "indexing workspace: ~a" root)
    (let ((files (find-gerbil-files root)))
      (lsp-info "found ~a Gerbil files" (length files))
      (for-each
        (lambda (path)
          (with-catch
            (lambda (e)
              (lsp-debug "failed to index ~a: ~a" path e))
            (lambda ()
              (index-file-by-path! path))))
        files))))

;;; Index a single file by its filesystem path
(def (index-file-by-path! path)
  (let* ((uri (string-append "file://" path))
         (text (read-file-string path))
         (forms (parse-source text))
         (syms (extract-symbols forms)))
    (set-file-symbols! uri syms)
    (lsp-debug "indexed ~a: ~a symbols" path (length syms))))

;;; Find all .ss files under a directory recursively
(def (find-gerbil-files root)
  (let ((result '()))
    (scan-directory root
      (lambda (path)
        (when (and (string-suffix? ".ss" path)
                   (not (string-contains path "/.gerbil/"))
                   (not (string-contains path "/build-deps/")))
          (set! result (cons path result)))))
    (reverse result)))

;;; Recursively scan a directory, calling proc on each file path
(def (scan-directory dir proc)
  (with-catch
    (lambda (e)
      (lsp-debug "scan-directory error ~a: ~a" dir e))
    (lambda ()
      (let ((entries (directory-files dir)))
        (for-each
          (lambda (entry)
            (let ((full-path (string-append dir "/" entry)))
              (if (file-directory? full-path)
                (unless (string-prefix? "." entry)
                  (scan-directory full-path proc))
                (proc full-path))))
          entries)))))

;;; Check if a string ends with a suffix
(def (string-suffix? suffix str)
  (let ((slen (string-length suffix))
        (len (string-length str)))
    (and (>= len slen)
         (string=? suffix (substring str (- len slen) len)))))

;;; Check if a string contains a substring
(def (string-contains haystack needle)
  (let ((hlen (string-length haystack))
        (nlen (string-length needle)))
    (let loop ((i 0))
      (cond
        ((> (+ i nlen) hlen) #f)
        ((string=? needle (substring haystack i (+ i nlen))) #t)
        (else (loop (+ i 1)))))))

;;; Check if a path is a directory
(def (file-directory? path)
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (eq? (file-info-type (file-info path)) 'directory))))

;;; Find all definitions matching a name across the workspace
(def (find-definitions-by-name name)
  (let ((result '()))
    (hash-for-each
      (lambda (uri syms)
        (for-each
          (lambda (s)
            (when (string=? (sym-info-name s) name)
              (set! result (cons (cons uri s) result))))
          syms))
      *symbol-index*)
    result))

;;; Find all references to a symbol name across indexed files
;;; This does text-based search with word boundary detection
(def (find-references-by-name name)
  (let ((result '()))
    (hash-for-each
      (lambda (uri _syms)
        (let ((doc (get-document uri)))
          (when doc
            (let ((text (document-text doc))
                  (name-len (string-length name)))
              (let ((lines (string-split-lines text)))
                (let line-loop ((ls lines) (line-num 0))
                  (unless (null? ls)
                    (let ((line-text (car ls)))
                      (find-in-line line-text name name-len uri line-num
                                    (lambda (col)
                                      (set! result
                                        (cons (list uri line-num col
                                                    (+ col name-len))
                                              result))))
                      (line-loop (cdr ls) (+ line-num 1))))))))))
      *symbol-index*)
    result))

;;; Find occurrences of name in a line with word boundary checking
(def (find-in-line text name name-len uri line-num callback)
  (let ((text-len (string-length text)))
    (let loop ((i 0))
      (when (< (+ i name-len) (+ text-len 1))
        (when (and (string=? name (substring text i (+ i name-len)))
                   (or (= i 0) (not (symbol-char? (string-ref text (- i 1)))))
                   (or (= (+ i name-len) text-len)
                       (not (symbol-char? (string-ref text (+ i name-len))))))
          (callback i))
        (loop (+ i 1))))))

;;; Split text into lines
(def (string-split-lines text)
  (let loop ((i 0) (start 0) (lines '()))
    (cond
      ((>= i (string-length text))
       (reverse (cons (substring text start i) lines)))
      ((char=? (string-ref text i) #\newline)
       (loop (+ i 1) (+ i 1) (cons (substring text start i) lines)))
      (else
       (loop (+ i 1) start lines)))))
