;;; -*- Gerbil -*-
;;; S-expression parser for Gerbil source
;;; Parses source text into a list of top-level forms with position info
(import :std/sugar
        :std/iter
        ../util/log)
(export #t)

;;; A parsed form with source location
(defstruct located-form (form line col end-line end-col) transparent: #t)

;;; Parse source text into a list of top-level S-expressions.
;;; Returns list of located-form records.
;;; Handles parse errors gracefully — returns partial results.
(def (parse-source text)
  (let ((port (open-input-string text)))
    (let loop ((forms '()) (last-line 0))
      (let ((pos (get-port-position port)))
        (with-catch
          (lambda (e)
            (lsp-debug "parse error at ~a: ~a" pos e)
            ;; Skip past the error and try to continue
            (skip-to-next-form port)
            (loop forms (+ last-line 1)))
          (lambda ()
            (let ((form (read port)))
              (if (eof-object? form)
                (reverse forms)
                (let* ((end-pos (get-port-position port))
                       (start-line (position-line pos))
                       (start-col (position-col pos))
                       (end-line (position-line end-pos))
                       (end-col (position-col end-pos)))
                  (loop (cons (make-located-form form start-line start-col
                                                  end-line end-col)
                              forms)
                        end-line))))))))))

;;; Get position info from a port
;;; Gambit ports use 1-based line/column; LSP uses 0-based, so subtract 1
(def (get-port-position port)
  (let ((line (input-port-line port))
        (col (input-port-column port)))
    (cons (max 0 (- line 1)) (max 0 (- col 1)))))

(def (position-line pos) (car pos))
(def (position-col pos) (cdr pos))

;;; Skip characters until we find something that looks like the start
;;; of a new top-level form (opening paren at the start of a line)
(def (skip-to-next-form port)
  (let loop ()
    (let ((c (read-char port)))
      (cond
        ((eof-object? c) (void))
        ;; Found a newline — peek ahead for start of form
        ((char=? c #\newline)
         (let ((next (peek-char port)))
           (cond
             ((eof-object? next) (void))
             ((char=? next #\() (void))  ; let the reader take over
             ((char=? next #\;) (loop))  ; skip comment lines
             (else (loop)))))
        (else (loop))))))

;;; Parse source text into just the raw S-expressions (no position info)
(def (parse-source-forms text)
  (let ((port (open-input-string text)))
    (let loop ((forms '()))
      (with-catch
        (lambda (e)
          (reverse forms))
        (lambda ()
          (let ((form (read port)))
            (if (eof-object? form)
              (reverse forms)
              (loop (cons form forms)))))))))

;;; Find the top-level form at a given line number
(def (form-at-line forms line)
  (let loop ((fs forms))
    (if (null? fs) #f
      (let ((f (car fs)))
        (if (and (>= line (located-form-line f))
                 (<= line (located-form-end-line f)))
          f
          (loop (cdr fs)))))))

;;; Check if a form is a definition form
(def (definition-form? form)
  (and (pair? form)
       (symbol? (car form))
       (memq (car form)
             '(def define defn def*
               defstruct defclass
               defmethod defproto
               defrule defrules defsyntax defsyntax-call
               defvalues defconst
               deferror-class))))
