;;; -*- Gerbil -*-
;;; Global LSP server state
(export #t)

;;; Server lifecycle state
(def *initialized* #f)
(def *shutdown-requested* #f)
(def *workspace-root* #f)
(def *client-capabilities* #f)

;;; Document store: uri (string) → document record (hash table)
(def *documents* (make-hash-table))

;;; Symbol index: populated by analysis
(def *symbol-index* (make-hash-table))

;;; Module cache: module-path → export list
(def *module-cache* (make-hash-table))

;;; --- Document operations ---

(def (get-document uri)
  (hash-ref *documents* uri #f))

(def (set-document! uri doc)
  (hash-put! *documents* uri doc))

(def (remove-document! uri)
  (hash-remove! *documents* uri))

(def (all-document-uris)
  (hash-keys *documents*))

;;; --- State accessors ---

(def (server-initialized?)
  *initialized*)

(def (set-initialized! v)
  (set! *initialized* v))

(def (shutdown-requested?)
  *shutdown-requested*)

(def (set-shutdown-requested! v)
  (set! *shutdown-requested* v))

(def (workspace-root)
  *workspace-root*)

(def (set-workspace-root! root)
  (set! *workspace-root* root))

(def (client-capabilities)
  *client-capabilities*)

(def (set-client-capabilities! caps)
  (set! *client-capabilities* caps))

;;; --- Symbol index operations ---

(def (get-file-symbols uri)
  (hash-ref *symbol-index* uri '()))

(def (set-file-symbols! uri symbols)
  (hash-put! *symbol-index* uri symbols))

(def (remove-file-symbols! uri)
  (hash-remove! *symbol-index* uri))

(def (all-indexed-symbols)
  (let ((result '()))
    (hash-for-each
      (lambda (uri syms)
        (for-each (lambda (s) (set! result (cons (cons uri s) result))) syms))
      *symbol-index*)
    result))

;;; --- Module cache operations ---

(def (get-module-exports module-path)
  (hash-ref *module-cache* module-path #f))

(def (set-module-exports! module-path exports)
  (hash-put! *module-cache* module-path exports))
