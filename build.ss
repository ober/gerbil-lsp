#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("lsp/util/log"
    "lsp/util/position"
    "lsp/transport"
    "lsp/jsonrpc"
    "lsp/types"
    "lsp/capabilities"
    "lsp/state"
    "lsp/analysis/document"
    "lsp/analysis/parser"
    "lsp/analysis/symbols"
    "lsp/analysis/module"
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
    "lsp/server"
    (exe: "lsp/main" bin: "gerbil-lsp")))
