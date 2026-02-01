;;; gerbil-lsp.el --- Eglot integration for Gerbil Scheme -*- lexical-binding: t -*-

;; Author: gerbil-lsp contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (eglot "1.12"))
;; Keywords: languages, gerbil, scheme, lsp
;; URL: https://github.com/ober/gerbil-lsp

;;; Commentary:

;; This package provides Eglot (LSP) integration for Gerbil Scheme
;; via the gerbil-lsp language server.
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

(defun gerbil-lsp--server-command ()
  "Return the command to start the gerbil-lsp server."
  (list gerbil-lsp-server-path
        "--stdio"
        "--log-level" gerbil-lsp-log-level))

;; Register gerbil-lsp with Eglot for gerbil-mode
(add-to-list 'eglot-server-programs
             `(gerbil-mode . ,#'gerbil-lsp--server-command))

(provide 'gerbil-lsp)

;;; gerbil-lsp.el ends here
