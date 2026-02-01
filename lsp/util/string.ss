;;; -*- Gerbil -*-
;;; Shared string utilities used across the LSP server
(export #t)

;;; Split text into lines on newline characters
(def (string-split-lines text)
  (let loop ((i 0) (start 0) (lines '()))
    (cond
      ((>= i (string-length text))
       (reverse (cons (substring text start i) lines)))
      ((char=? (string-ref text i) #\newline)
       (loop (+ i 1) (+ i 1) (cons (substring text start i) lines)))
      (else
       (loop (+ i 1) start lines)))))

;;; Check if haystack contains needle as a substring
(def (string-contains haystack needle)
  (let ((hlen (string-length haystack))
        (nlen (string-length needle)))
    (let loop ((i 0))
      (cond
        ((> (+ i nlen) hlen) #f)
        ((string=? needle (substring haystack i (+ i nlen))) #t)
        (else (loop (+ i 1)))))))

;;; Case-insensitive substring search
(def (string-contains-ci haystack needle)
  (let ((h (string-downcase haystack))
        (n (string-downcase needle)))
    (let ((hlen (string-length h))
          (nlen (string-length n)))
      (let loop ((i 0))
        (cond
          ((> (+ i nlen) hlen) #f)
          ((string=? n (substring h i (+ i nlen))) #t)
          (else (loop (+ i 1))))))))

;;; Check if a string ends with a suffix
(def (string-suffix? suffix str)
  (let ((slen (string-length suffix))
        (len (string-length str)))
    (and (>= len slen)
         (string=? suffix (substring str (- len slen) len)))))

;;; Join strings with a separator
(def (string-join strs sep)
  (if (null? strs) ""
    (let loop ((rest (cdr strs)) (acc (car strs)))
      (if (null? rest) acc
        (loop (cdr rest) (string-append acc sep (car rest)))))))

;;; Join lines with newline separator
(def (string-join-newline lines)
  (string-join lines "\n"))

;;; Take at most N elements from a list
(def (take-at-most lst n)
  (if (or (null? lst) (<= n 0))
    '()
    (cons (car lst) (take-at-most (cdr lst) (- n 1)))))
