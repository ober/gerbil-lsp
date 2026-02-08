#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

;;; Detect Gerbil version and select the appropriate compat shim
(let* ((vs (gerbil-version-string))
       (parts (string-split vs #\.))
       (minor (string->number (cadr parts)))
       (src (if (>= minor 19)
              "lsp/compat/compat-v19.ss"
              "lsp/compat/compat-v18.ss"))
       (dst "lsp/compat/compat.ss"))
  (displayln "gerbil-lsp build: detected Gerbil v0." minor
             " — using " src)
  (when (file-exists? dst) (delete-file dst))
  (copy-file src dst))

(defbuild-script
  '("lsp/compat/version"
    "lsp/compat/compat"
    "lsp/util/log"
    "lsp/util/position"
    "lsp/util/string"
    "lsp/transport"
    "lsp/jsonrpc"
    "lsp/types"
    "lsp/validation"
    "lsp/capabilities"
    "lsp/state"
    "lsp/analysis/document"
    "lsp/analysis/parser"
    "lsp/analysis/symbols"
    "lsp/analysis/module"
    "lsp/analysis/cache"
    "lsp/analysis/project-config"
    "lsp/analysis/index"
    "lsp/analysis/completion-data"
    "lsp/handlers/lifecycle"
    "lsp/handlers/sync"
    "lsp/handlers/diagnostics"
    "lsp/handlers/completion"
    "lsp/handlers/hover"
    "lsp/handlers/definition"
    "lsp/handlers/references"
    "lsp/handlers/symbols"
    "lsp/handlers/rename"
    "lsp/handlers/formatting"
    "lsp/handlers/signature"
    "lsp/handlers/code-action"
    "lsp/handlers/configuration"
    "lsp/handlers/highlight"
    "lsp/handlers/folding"
    "lsp/handlers/selection"
    "lsp/handlers/links"
    "lsp/handlers/semantic-tokens"
    "lsp/handlers/inlay-hints"
    "lsp/handlers/call-hierarchy"
    "lsp/handlers/type-definition"
    "lsp/handlers/implementation"
    "lsp/handlers/type-hierarchy"
    "lsp/handlers/code-lens"
    "lsp/handlers/on-type-formatting"
    "lsp/handlers/pull-diagnostics"
    "lsp/handlers/execute-command"
    "lsp/handlers/will-rename"
    "lsp/server"
    (exe: "lsp/main" bin: "gerbil-lsp")))
