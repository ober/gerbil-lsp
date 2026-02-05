;;; gerbil-lsp.el --- Eglot integration for Gerbil Scheme -*- lexical-binding: t -*-

;; Author: gerbil-lsp contributors
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (eglot "1.12"))
;; Keywords: languages, gerbil, scheme, lsp
;; URL: https://github.com/ober/gerbil-lsp

;;; Commentary:

;; This package provides Eglot (LSP) integration for Gerbil Scheme
;; via the gerbil-lsp language server.
;;
;; Features supported:
;;   - Completion with trigger characters ( : / .
;;   - Hover documentation
;;   - Go to definition / implementation
;;   - Find references
;;   - Document & workspace symbols
;;   - Rename (with prepare support)
;;   - Formatting (full, range, on-type)
;;   - Signature help
;;   - Code actions (quickfix, organize imports)
;;   - Document highlight
;;   - Folding ranges
;;   - Selection ranges
;;   - Document links
;;   - Semantic tokens (10 token types, 2 modifiers)
;;   - Inlay hints (parameter names, requires Eglot 1.15+)
;;   - Call hierarchy (incoming/outgoing)
;;   - Type hierarchy (supertypes/subtypes)
;;   - Code lenses (reference counts, test runners)
;;   - Pull diagnostics
;;   - Custom commands (run test, show references)
;;
;; Usage:
;;   1. Build gerbil-lsp: cd /path/to/gerbil-lsp && make build
;;   2. Ensure the binary is on your PATH or set `gerbil-lsp-server-path'
;;   3. Add to your Emacs config:
;;        (require 'gerbil-lsp)
;;   4. Open a .ss file in gerbil-mode and run M-x eglot
;;
;; Or enable automatically:
;;   (add-hook 'gerbil-mode-hook #'eglot-ensure)

;;; Code:

(require 'eglot)
(require 'project)
(require 'seq)

;; ---------------------------------------------------------------------
;; Customization group
;; ---------------------------------------------------------------------

(defgroup gerbil-lsp nil
  "Gerbil LSP client settings."
  :group 'gerbil
  :prefix "gerbil-lsp-")

(defcustom gerbil-lsp-server-path "gerbil-lsp"
  "Path to the gerbil-lsp executable.
If the binary is not on your PATH, set this to the absolute path,
e.g., \"/home/user/.gerbil/bin/gerbil-lsp\"."
  :type 'string
  :group 'gerbil-lsp)

(defcustom gerbil-lsp-log-level "info"
  "Log level for gerbil-lsp server.
One of: debug, info, warn, error."
  :type '(choice (const "debug")
                 (const "info")
                 (const "warn")
                 (const "error"))
  :group 'gerbil-lsp)

(defcustom gerbil-lsp-enable-inlay-hints t
  "Enable inlay hints (parameter name annotations at call sites).
Requires Emacs 29.1+ and Eglot 1.15+."
  :type 'boolean
  :group 'gerbil-lsp)

(defcustom gerbil-lsp-enable-code-lenses t
  "Enable code lenses (reference counts, test runners above definitions)."
  :type 'boolean
  :group 'gerbil-lsp)

(defcustom gerbil-lsp-auto-start nil
  "When non-nil, automatically start Eglot in `gerbil-mode' buffers."
  :type 'boolean
  :group 'gerbil-lsp)

;; ---------------------------------------------------------------------
;; Server command
;; ---------------------------------------------------------------------

(defun gerbil-lsp--server-command (&rest _args)
  "Return the command to start the gerbil-lsp server."
  (list gerbil-lsp-server-path
        "--stdio"
        "--log-level" gerbil-lsp-log-level))

;; Register gerbil-lsp with Eglot for gerbil-mode
(add-to-list 'eglot-server-programs
             `(gerbil-mode . ,#'gerbil-lsp--server-command))

;; ---------------------------------------------------------------------
;; Semantic token face mapping
;; ---------------------------------------------------------------------

;; The server provides 10 token types and 2 modifiers:
;;   Types:     keyword, function, variable, parameter, type,
;;              macro, comment, string, number, operator
;;   Modifiers: definition, readonly

(defface gerbil-lsp-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for Gerbil keywords and special forms."
  :group 'gerbil-lsp)

(defface gerbil-lsp-function-face
  '((t :inherit font-lock-function-name-face))
  "Face for Gerbil function names."
  :group 'gerbil-lsp)

(defface gerbil-lsp-variable-face
  '((t :inherit font-lock-variable-name-face))
  "Face for Gerbil variables."
  :group 'gerbil-lsp)

(defface gerbil-lsp-parameter-face
  '((t :inherit font-lock-variable-name-face :slant italic))
  "Face for Gerbil keyword parameters."
  :group 'gerbil-lsp)

(defface gerbil-lsp-type-face
  '((t :inherit font-lock-type-face))
  "Face for Gerbil types (structs, classes, type predicates)."
  :group 'gerbil-lsp)

(defface gerbil-lsp-macro-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for Gerbil macros."
  :group 'gerbil-lsp)

(defface gerbil-lsp-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for Gerbil comments."
  :group 'gerbil-lsp)

(defface gerbil-lsp-string-face
  '((t :inherit font-lock-string-face))
  "Face for Gerbil string literals."
  :group 'gerbil-lsp)

(defface gerbil-lsp-number-face
  '((t :inherit font-lock-number-face))
  "Face for Gerbil numeric literals."
  :group 'gerbil-lsp)

(defface gerbil-lsp-operator-face
  '((t :inherit font-lock-operator-face))
  "Face for Gerbil operators."
  :group 'gerbil-lsp)

(defface gerbil-lsp-definition-face
  '((t :inherit bold))
  "Face modifier for definitions."
  :group 'gerbil-lsp)

(defface gerbil-lsp-readonly-face
  '((t :inherit font-lock-constant-face))
  "Face modifier for readonly/constant values."
  :group 'gerbil-lsp)

;; Map server token types to Emacs faces for Eglot's semantic highlighting.
;; Eglot uses `eglot--semantic-token-modifier-faces' and
;; `eglot--semantic-token-type-faces' (Eglot 1.15+).
(defvar gerbil-lsp-semantic-token-faces
  '(("keyword"   . gerbil-lsp-keyword-face)
    ("function"  . gerbil-lsp-function-face)
    ("variable"  . gerbil-lsp-variable-face)
    ("parameter" . gerbil-lsp-parameter-face)
    ("type"      . gerbil-lsp-type-face)
    ("macro"     . gerbil-lsp-macro-face)
    ("comment"   . gerbil-lsp-comment-face)
    ("string"    . gerbil-lsp-string-face)
    ("number"    . gerbil-lsp-number-face)
    ("operator"  . gerbil-lsp-operator-face))
  "Mapping from gerbil-lsp semantic token types to Emacs faces.")

(defvar gerbil-lsp-semantic-token-modifier-faces
  '(("definition" . gerbil-lsp-definition-face)
    ("readonly"   . gerbil-lsp-readonly-face))
  "Mapping from gerbil-lsp semantic token modifiers to Emacs faces.")

;; ---------------------------------------------------------------------
;; Custom commands: gerbil-lsp.runTest, gerbil-lsp.showReferences
;; ---------------------------------------------------------------------

(defun gerbil-lsp-run-test ()
  "Run the test suite at point via the gerbil-lsp server.
The server locates the test definition file and executes
`gerbil test` on it, reporting results via window/showMessage."
  (interactive)
  (unless (eglot-current-server)
    (user-error "No active Eglot session"))
  (let* ((sym (thing-at-point 'symbol t)))
    (unless sym
      (user-error "No symbol at point"))
    (jsonrpc-request (eglot--current-server-or-lose)
                     :workspace/executeCommand
                     `(:command "gerbil-lsp.runTest"
                       :arguments ,(vector sym)))))

(defun gerbil-lsp-show-references ()
  "Show reference count for the symbol at point.
Uses the gerbil-lsp.showReferences custom command."
  (interactive)
  (unless (eglot-current-server)
    (user-error "No active Eglot session"))
  (let* ((pos (eglot--pos-to-lsp-position))
         (uri (eglot--path-to-uri (buffer-file-name))))
    (jsonrpc-request (eglot--current-server-or-lose)
                     :workspace/executeCommand
                     `(:command "gerbil-lsp.showReferences"
                       :arguments ,(vector uri
                                          (plist-get pos :line)
                                          (plist-get pos :character))))))

;; ---------------------------------------------------------------------
;; Call hierarchy commands
;; ---------------------------------------------------------------------

(defun gerbil-lsp-incoming-calls ()
  "Show incoming calls to the symbol at point."
  (interactive)
  (unless (eglot-current-server)
    (user-error "No active Eglot session"))
  (let* ((server (eglot--current-server-or-lose))
         (params (eglot--TextDocumentPositionParams))
         (items (jsonrpc-request server :textDocument/prepareCallHierarchy params)))
    (if (and items (> (length items) 0))
        (let* ((item (aref items 0))
               (incoming (jsonrpc-request server :callHierarchy/incomingCalls
                                          `(:item ,item))))
          (if (and incoming (> (length incoming) 0))
              (gerbil-lsp--display-call-hierarchy "Incoming Calls" incoming 'from)
            (message "No incoming calls found")))
      (message "No call hierarchy item at point"))))

(defun gerbil-lsp-outgoing-calls ()
  "Show outgoing calls from the symbol at point."
  (interactive)
  (unless (eglot-current-server)
    (user-error "No active Eglot session"))
  (let* ((server (eglot--current-server-or-lose))
         (params (eglot--TextDocumentPositionParams))
         (items (jsonrpc-request server :textDocument/prepareCallHierarchy params)))
    (if (and items (> (length items) 0))
        (let* ((item (aref items 0))
               (outgoing (jsonrpc-request server :callHierarchy/outgoingCalls
                                          `(:item ,item))))
          (if (and outgoing (> (length outgoing) 0))
              (gerbil-lsp--display-call-hierarchy "Outgoing Calls" outgoing 'to)
            (message "No outgoing calls found")))
      (message "No call hierarchy item at point"))))

(defun gerbil-lsp--display-call-hierarchy (title items direction)
  "Display call hierarchy ITEMS in a buffer.
TITLE is the buffer heading.  DIRECTION is `from' or `to',
indicating which field contains the target CallHierarchyItem."
  (let ((buf (get-buffer-create (format "*gerbil-lsp: %s*" title))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "== %s ==\n\n" title))
        (seq-doseq (entry items)
          (let* ((field (if (eq direction 'from) :from :to))
                 (target (plist-get entry field))
                 (name (plist-get target :name))
                 (kind (plist-get target :kind))
                 (uri (plist-get target :uri))
                 (range (plist-get target :range))
                 (start (plist-get range :start))
                 (line (1+ (plist-get start :line)))
                 (file (eglot--uri-to-path uri)))
            (insert-text-button
             (format "%s (kind %s)" name kind)
             'action (gerbil-lsp--make-goto-action file line)
             'follow-link t)
            (insert (format "  %s:%d\n" (file-name-nondirectory file) line))))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

(defun gerbil-lsp--make-goto-action (file line)
  "Return a button action that opens FILE at LINE."
  (lambda (_btn)
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- line))))

;; ---------------------------------------------------------------------
;; Type hierarchy commands
;; ---------------------------------------------------------------------

(defun gerbil-lsp-supertypes ()
  "Show supertypes of the type at point."
  (interactive)
  (unless (eglot-current-server)
    (user-error "No active Eglot session"))
  (let* ((server (eglot--current-server-or-lose))
         (params (eglot--TextDocumentPositionParams))
         (items (jsonrpc-request server :textDocument/prepareTypeHierarchy params)))
    (if (and items (> (length items) 0))
        (let* ((item (aref items 0))
               (supers (jsonrpc-request server :typeHierarchy/supertypes
                                        `(:item ,item))))
          (if (and supers (> (length supers) 0))
              (gerbil-lsp--display-type-hierarchy "Supertypes" supers)
            (message "No supertypes found")))
      (message "No type hierarchy item at point"))))

(defun gerbil-lsp-subtypes ()
  "Show subtypes of the type at point."
  (interactive)
  (unless (eglot-current-server)
    (user-error "No active Eglot session"))
  (let* ((server (eglot--current-server-or-lose))
         (params (eglot--TextDocumentPositionParams))
         (items (jsonrpc-request server :textDocument/prepareTypeHierarchy params)))
    (if (and items (> (length items) 0))
        (let* ((item (aref items 0))
               (subs (jsonrpc-request server :typeHierarchy/subtypes
                                      `(:item ,item))))
          (if (and subs (> (length subs) 0))
              (gerbil-lsp--display-type-hierarchy "Subtypes" subs)
            (message "No subtypes found")))
      (message "No type hierarchy item at point"))))

(defun gerbil-lsp--display-type-hierarchy (title items)
  "Display type hierarchy ITEMS in a buffer with TITLE."
  (let ((buf (get-buffer-create (format "*gerbil-lsp: %s*" title))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "== %s ==\n\n" title))
        (seq-doseq (item items)
          (let* ((name (plist-get item :name))
                 (kind (plist-get item :kind))
                 (uri (plist-get item :uri))
                 (range (plist-get item :range))
                 (start (plist-get range :start))
                 (line (1+ (plist-get start :line)))
                 (file (eglot--uri-to-path uri)))
            (insert-text-button
             (format "%s (kind %s)" name kind)
             'action (gerbil-lsp--make-goto-action file line)
             'follow-link t)
            (insert (format "  %s:%d\n" (file-name-nondirectory file) line))))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

;; ---------------------------------------------------------------------
;; Selection range (expand/shrink)
;; ---------------------------------------------------------------------

(defvar-local gerbil-lsp--selection-range-stack nil
  "Stack of previous selection ranges for shrinking.")

(defun gerbil-lsp-expand-selection ()
  "Expand the selection to the next enclosing syntactic unit.
Uses LSP selectionRange to expand structurally."
  (interactive)
  (unless (eglot-current-server)
    (user-error "No active Eglot session"))
  (let* ((server (eglot--current-server-or-lose))
         (params `(:textDocument ,(eglot--TextDocumentIdentifier)
                   :positions ,(vector (eglot--pos-to-lsp-position))))
         (result (jsonrpc-request server :textDocument/selectionRange params)))
    (when (and result (> (length result) 0))
      (let* ((sel (aref result 0))
             (current-start (point))
             (current-end (if (use-region-p) (mark) (point)))
             (range (gerbil-lsp--find-expanding-range sel current-start current-end)))
        (when range
          (push (cons current-start current-end) gerbil-lsp--selection-range-stack)
          (let* ((start (plist-get range :start))
                 (end (plist-get range :end))
                 (beg-pos (eglot--lsp-position-to-point start))
                 (end-pos (eglot--lsp-position-to-point end)))
            (goto-char beg-pos)
            (set-mark end-pos)
            (activate-mark)))))))

(defun gerbil-lsp-shrink-selection ()
  "Shrink the selection back to the previous range."
  (interactive)
  (if gerbil-lsp--selection-range-stack
      (let ((prev (pop gerbil-lsp--selection-range-stack)))
        (goto-char (car prev))
        (if (= (car prev) (cdr prev))
            (deactivate-mark)
          (set-mark (cdr prev))
          (activate-mark)))
    (deactivate-mark)))

(defun gerbil-lsp--find-expanding-range (selection-range current-start current-end)
  "Walk SELECTION-RANGE to find a range strictly larger than CURRENT-START..CURRENT-END."
  (let ((range (plist-get selection-range :range))
        (parent (plist-get selection-range :parent)))
    (if range
        (let* ((start (plist-get range :start))
               (end (plist-get range :end))
               (beg-pos (eglot--lsp-position-to-point start))
               (end-pos (eglot--lsp-position-to-point end))
               (cur-min (min current-start current-end))
               (cur-max (max current-start current-end)))
          (if (and (<= beg-pos cur-min) (>= end-pos cur-max)
                   (or (< beg-pos cur-min) (> end-pos cur-max)))
              range
            (when parent
              (gerbil-lsp--find-expanding-range parent current-start current-end))))
      nil)))

;; ---------------------------------------------------------------------
;; Code lens display
;; ---------------------------------------------------------------------

(defvar-local gerbil-lsp--code-lens-overlays nil
  "List of code lens overlays in the current buffer.")

(defun gerbil-lsp-refresh-code-lenses ()
  "Refresh code lenses in the current buffer."
  (interactive)
  (unless (eglot-current-server)
    (user-error "No active Eglot session"))
  (gerbil-lsp--clear-code-lenses)
  (when gerbil-lsp-enable-code-lenses
    (let* ((server (eglot--current-server-or-lose))
           (params `(:textDocument ,(eglot--TextDocumentIdentifier)))
           (lenses (jsonrpc-request server :textDocument/codeLens params)))
      (when lenses
        (seq-doseq (lens lenses)
          (gerbil-lsp--render-code-lens lens))))))

(defun gerbil-lsp--clear-code-lenses ()
  "Remove all code lens overlays."
  (dolist (ov gerbil-lsp--code-lens-overlays)
    (delete-overlay ov))
  (setq gerbil-lsp--code-lens-overlays nil))

(defun gerbil-lsp--render-code-lens (lens)
  "Render a single code LENS as an overlay above its range."
  (let* ((range (plist-get lens :range))
         (command (plist-get lens :command))
         (start (plist-get range :start))
         (line (plist-get start :line))
         (title (plist-get command :title))
         (cmd-name (plist-get command :command))
         (cmd-args (plist-get command :arguments)))
    (when (and title cmd-name)
      (save-excursion
        (goto-char (point-min))
        (forward-line line)
        (let* ((bol (line-beginning-position))
               (ov (make-overlay bol bol))
               (text (propertize (concat title "\n")
                                 'face '(:height 0.85 :foreground "dim gray")
                                 'cursor t)))
          ;; Make it clickable
          (put-text-property 0 (length title) 'mouse-face 'highlight text)
          (put-text-property 0 (length title) 'keymap
                             (let ((map (make-sparse-keymap)))
                               (define-key map [mouse-1]
                                 (lambda (_event)
                                   (interactive "e")
                                   (gerbil-lsp--execute-lens-command cmd-name cmd-args)))
                               (define-key map (kbd "RET")
                                 (lambda ()
                                   (interactive)
                                   (gerbil-lsp--execute-lens-command cmd-name cmd-args)))
                               map)
                             text)
          (overlay-put ov 'before-string text)
          (overlay-put ov 'gerbil-lsp-code-lens t)
          (push ov gerbil-lsp--code-lens-overlays))))))

(defun gerbil-lsp--execute-lens-command (command args)
  "Execute a code lens COMMAND with ARGS via the LSP server."
  (when (eglot-current-server)
    (jsonrpc-request (eglot--current-server-or-lose)
                     :workspace/executeCommand
                     `(:command ,command :arguments ,args))))

;; ---------------------------------------------------------------------
;; Minor mode and keybindings
;; ---------------------------------------------------------------------

(defvar gerbil-lsp-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Custom commands
    (define-key map (kbd "C-c l t") #'gerbil-lsp-run-test)
    (define-key map (kbd "C-c l r") #'gerbil-lsp-show-references)
    ;; Call hierarchy
    (define-key map (kbd "C-c l c i") #'gerbil-lsp-incoming-calls)
    (define-key map (kbd "C-c l c o") #'gerbil-lsp-outgoing-calls)
    ;; Type hierarchy
    (define-key map (kbd "C-c l h s") #'gerbil-lsp-supertypes)
    (define-key map (kbd "C-c l h b") #'gerbil-lsp-subtypes)
    ;; Selection range
    (define-key map (kbd "C-c l +") #'gerbil-lsp-expand-selection)
    (define-key map (kbd "C-c l -") #'gerbil-lsp-shrink-selection)
    ;; Code lenses
    (define-key map (kbd "C-c l l") #'gerbil-lsp-refresh-code-lenses)
    ;; Standard Eglot commands on convenient keys
    (define-key map (kbd "C-c l a") #'eglot-code-actions)
    (define-key map (kbd "C-c l n") #'eglot-rename)
    (define-key map (kbd "C-c l f") #'eglot-format-buffer)
    (define-key map (kbd "C-c l d") #'xref-find-definitions)
    (define-key map (kbd "C-c l R") #'xref-find-references)
    (define-key map (kbd "C-c l i") #'eglot-find-implementation)
    (define-key map (kbd "C-c l w") #'xref-find-apropos)
    map)
  "Keymap for `gerbil-lsp-mode'.")

;;;###autoload
(define-minor-mode gerbil-lsp-mode
  "Minor mode for gerbil-lsp specific features.
Provides keybindings, code lenses, inlay hints, and semantic
token highlighting for Gerbil Scheme via Eglot.

\\{gerbil-lsp-mode-map}"
  :lighter " GLSP"
  :keymap gerbil-lsp-mode-map
  (if gerbil-lsp-mode
      (progn
        ;; Enable inlay hints if available and configured
        (when (and gerbil-lsp-enable-inlay-hints
                   (fboundp 'eglot-inlay-hints-mode))
          (eglot-inlay-hints-mode 1))
        ;; Refresh code lenses after a short delay
        (when gerbil-lsp-enable-code-lenses
          (add-hook 'after-save-hook #'gerbil-lsp--maybe-refresh-lenses nil t)))
    ;; Cleanup on disable
    (gerbil-lsp--clear-code-lenses)
    (when (fboundp 'eglot-inlay-hints-mode)
      (eglot-inlay-hints-mode -1))
    (remove-hook 'after-save-hook #'gerbil-lsp--maybe-refresh-lenses t)))

(defun gerbil-lsp--maybe-refresh-lenses ()
  "Refresh code lenses if an Eglot session is active."
  (when (and gerbil-lsp-enable-code-lenses (eglot-current-server))
    (gerbil-lsp-refresh-code-lenses)))

;; ---------------------------------------------------------------------
;; Eglot hooks: auto-activate minor mode, semantic token faces
;; ---------------------------------------------------------------------

(defun gerbil-lsp--eglot-managed-hook ()
  "Hook for gerbil-mode buffers when Eglot managed mode changes.
Activates `gerbil-lsp-mode' when Eglot connects and deactivates
it when Eglot disconnects."
  (when (derived-mode-p 'gerbil-mode)
    (if (bound-and-true-p eglot--managed-mode)
        (progn
          ;; Activate gerbil-lsp-mode
          (gerbil-lsp-mode 1)
          ;; Configure semantic token faces
          (when (boundp 'eglot--semantic-tokens-faces-override)
            (setq-local eglot--semantic-tokens-faces-override
                        (mapcar (lambda (entry)
                                  (cons (car entry) (cdr entry)))
                                gerbil-lsp-semantic-token-faces)))
          ;; Initial code lens refresh
          (when gerbil-lsp-enable-code-lenses
            (let ((buf (current-buffer)))
              (run-with-idle-timer 1 nil
                                   (lambda ()
                                     (when (buffer-live-p buf)
                                       (with-current-buffer buf
                                         (gerbil-lsp--maybe-refresh-lenses))))))))
      ;; Eglot disconnected — clean up
      (gerbil-lsp-mode -1))))

(add-hook 'eglot-managed-mode-hook #'gerbil-lsp--eglot-managed-hook)

;; ---------------------------------------------------------------------
;; Auto-start support
;; ---------------------------------------------------------------------

(defun gerbil-lsp--maybe-auto-start ()
  "Start Eglot automatically if `gerbil-lsp-auto-start' is non-nil."
  (when gerbil-lsp-auto-start
    (eglot-ensure)))

(add-hook 'gerbil-mode-hook #'gerbil-lsp--maybe-auto-start)

;; ---------------------------------------------------------------------
;; Project configuration (.gerbil-lsp.json) support
;; ---------------------------------------------------------------------

(defun gerbil-lsp-project-config-file ()
  "Find the .gerbil-lsp.json file for the current project, if any."
  (when-let* ((root (gerbil-lsp--project-root))
              (config-file (expand-file-name ".gerbil-lsp.json" root)))
    (when (file-exists-p config-file)
      config-file)))

(defun gerbil-lsp--project-root ()
  "Find the Gerbil project root by looking for gerbil.pkg."
  (when buffer-file-name
    (locate-dominating-file buffer-file-name "gerbil.pkg")))

(defun gerbil-lsp-edit-project-config ()
  "Open or create the .gerbil-lsp.json configuration file.
If the file doesn't exist, creates it with a template."
  (interactive)
  (let* ((root (or (gerbil-lsp--project-root)
                   (and buffer-file-name
                        (file-name-directory buffer-file-name))
                   default-directory))
         (config-file (expand-file-name ".gerbil-lsp.json" root)))
    (if (file-exists-p config-file)
        (find-file config-file)
      (find-file config-file)
      (insert "{\n"
              "  \"gxc-path\": \"gxc\",\n"
              "  \"gxc-flags\": [\"-O\"],\n"
              "  \"loadpath\": [],\n"
              "  \"diagnostics-delay\": 1500,\n"
              "  \"disabled-features\": []\n"
              "}\n")
      (goto-char (point-min))
      (message "Created .gerbil-lsp.json template — save and restart Eglot to apply"))))

;; ---------------------------------------------------------------------
;; Workspace symbols with preview
;; ---------------------------------------------------------------------

(defun gerbil-lsp-workspace-symbol (query)
  "Search workspace symbols matching QUERY."
  (interactive "sWorkspace symbol query: ")
  (unless (eglot-current-server)
    (user-error "No active Eglot session"))
  (let* ((server (eglot--current-server-or-lose))
         (result (jsonrpc-request server :workspace/symbol
                                 `(:query ,query))))
    (if (and result (> (length result) 0))
        (let* ((items (append result nil))
               (candidates
                (mapcar (lambda (item)
                          (let* ((name (plist-get item :name))
                                 (kind (plist-get item :kind))
                                 (loc (plist-get item :location))
                                 (uri (plist-get loc :uri))
                                 (range (plist-get loc :range))
                                 (start (plist-get range :start))
                                 (line (1+ (plist-get start :line)))
                                 (file (eglot--uri-to-path uri)))
                            (cons (format "%s  [%s]  %s:%d"
                                          name
                                          (gerbil-lsp--symbol-kind-name kind)
                                          (file-name-nondirectory file)
                                          line)
                                  item)))
                        items))
               (selected (completing-read "Symbol: " candidates nil t))
               (item (cdr (assoc selected candidates))))
          (when item
            (let* ((loc (plist-get item :location))
                   (uri (plist-get loc :uri))
                   (range (plist-get loc :range))
                   (start (plist-get range :start))
                   (file (eglot--uri-to-path uri))
                   (line (plist-get start :line)))
              (find-file file)
              (goto-char (point-min))
              (forward-line line))))
      (message "No symbols found matching \"%s\"" query))))

(defun gerbil-lsp--symbol-kind-name (kind)
  "Return a human-readable name for a SymbolKind number KIND."
  (pcase kind
    (1 "File") (2 "Module") (3 "Namespace") (4 "Package")
    (5 "Class") (6 "Method") (7 "Property") (8 "Field")
    (9 "Constructor") (10 "Enum") (11 "Interface") (12 "Function")
    (13 "Variable") (14 "Constant") (15 "String") (16 "Number")
    (17 "Boolean") (18 "Array") (19 "Object") (20 "Key")
    (21 "Null") (22 "EnumMember") (23 "Struct") (24 "Event")
    (25 "Operator") (26 "TypeParameter")
    (_ (format "%d" kind))))

;; Add workspace symbol binding to the keymap
(define-key gerbil-lsp-mode-map (kbd "C-c l s") #'gerbil-lsp-workspace-symbol)

;; ---------------------------------------------------------------------
;; Organize imports code action
;; ---------------------------------------------------------------------

(defun gerbil-lsp-organize-imports ()
  "Organize imports in the current buffer via code action."
  (interactive)
  (unless (eglot-current-server)
    (user-error "No active Eglot session"))
  (eglot-code-actions nil nil "source.organizeImports" t))

(define-key gerbil-lsp-mode-map (kbd "C-c l o") #'gerbil-lsp-organize-imports)

;; ---------------------------------------------------------------------
;; Provide
;; ---------------------------------------------------------------------

(provide 'gerbil-lsp)

;;; gerbil-lsp.el ends here
