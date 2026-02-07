# gerbil-lsp

A Language Server Protocol (LSP) implementation for [Gerbil Scheme](https://cons.io), providing IDE features through any LSP-compatible editor. Primary integration is with Emacs via [Eglot](https://github.com/joaotavora/eglot).

## Features

| Feature           | LSP Method                        | Description                                               |
|-------------------|-----------------------------------|-----------------------------------------------------------|
| Diagnostics       | `textDocument/publishDiagnostics` | Compilation errors via `gxc` and parse error detection    |
| Completion        | `textDocument/completion`         | Symbols from current file, workspace, and Gerbil keywords |
| Hover             | `textDocument/hover`              | Symbol info with kind, signature, and source location     |
| Go to Definition  | `textDocument/definition`         | Jump to symbol definition across workspace                |
| Find References   | `textDocument/references`         | Locate all occurrences of a symbol                        |
| Document Symbols  | `textDocument/documentSymbol`     | Outline view of definitions in current file               |
| Workspace Symbols | `workspace/symbol`                | Search definitions across all indexed files               |
| Rename            | `textDocument/rename`             | Rename a symbol across all open documents                 |
| Formatting        | `textDocument/formatting`         | Format via Gambit's `pretty-print`                        |
| Signature Help    | `textDocument/signatureHelp`      | Function signatures while typing arguments                |

### Symbol Recognition

The analysis engine recognizes these Gerbil definition forms:

- `def`, `define`, `defn`, `def*` -- functions and variables
- `defstruct` -- struct types
- `defclass` -- class types
- `defmethod` -- methods
- `defrule`, `defrules`, `defsyntax` -- macros
- `defvalues` -- multiple value bindings
- `defconst` -- constants
- `deferror-class` -- error types

### Module Resolution

Import resolution supports:

- Standard library modules: `:std/text/json`, `:std/sugar`, etc.
- Relative imports: `./foo`, `../bar`
- Package modules: `:mypackage/module`
- Complex import forms: `only-in`, `except-in`, `rename-in`, `prefix-in`

## Requirements

- [Gerbil Scheme](https://cons.io) v0.18+ with `gxpkg`
- A C compiler (gcc/clang) for linking the executable
- OpenSSL development libraries (typically already installed)
- Emacs 29.1+ with Eglot (for Emacs integration)

## Building

```bash
git clone https://github.com/ober/gerbil-lsp.git
cd gerbil-lsp
make build
```

On macOS with Homebrew, the Makefile automatically locates OpenSSL. If linking fails with `library 'ssl' not found`, set the library path manually:

```bash
LIBRARY_PATH="$(brew --prefix openssl@3)/lib" make build
```

The compiled binary is placed at `.gerbil/bin/gerbil-lsp`.

### Install

Copy the binary to your PATH or Gerbil bin directory:

```bash
make install   # copies to ~/.gerbil/bin/gerbil-lsp
```

## Emacs Setup

### 1. Load the Eglot integration

Add to your Emacs init file (`~/.emacs.d/init.el` or `~/.emacs`):

```elisp
;; Point to where you cloned gerbil-lsp
(add-to-list 'load-path "/path/to/gerbil-lsp/emacs")
(require 'gerbil-lsp)
```

### 2. (Optional) Auto-start on gerbil-mode

```elisp
(add-hook 'gerbil-mode-hook #'eglot-ensure)
```

### 3. (Optional) Customize server path

If `gerbil-lsp` is not on your PATH:

```elisp
(setq gerbil-lsp-server-path "/path/to/gerbil-lsp/.gerbil/bin/gerbil-lsp")
```

### 4. (Optional) Set log level

```elisp
(setq gerbil-lsp-log-level "debug")  ;; debug | info | warn | error
```

### Usage

Open any `.ss` file in `gerbil-mode` and run `M-x eglot`. The LSP server starts automatically and provides:

- **Diagnostics** -- errors appear as underlines and in the minibuffer
- **Completion** -- trigger with `(`, `:`, `/`, `.` or invoke via `C-M-i`
- **Hover** -- `M-x eldoc` or hover with mouse
- **Go to Definition** -- `M-.`
- **Find References** -- `M-?`
- **Rename** -- `M-x eglot-rename`
- **Format** -- `M-x eglot-format-buffer`
- **Document Symbols** -- `M-x imenu`

## CLI Usage

```
gerbil-lsp [options]

Options:
  --stdio                Use stdio transport (default)
  --log-level <level>    Log level: debug, info, warn, error (default: info)
  --version              Print version and exit
  -h, --help             Display help
```

The server communicates via JSON-RPC 2.0 over stdin/stdout using Content-Length framing (standard LSP transport). All log output goes to stderr to keep the transport channel clean.

## Architecture

```
lsp/
├── main.ss                 Entry point, CLI parsing, handler registration
├── server.ss               JSON-RPC dispatch loop (read -> dispatch -> respond)
├── transport.ss            stdio Content-Length framing
├── jsonrpc.ss              JSON-RPC 2.0 message codec
├── types.ss                LSP protocol type constructors
├── capabilities.ss         Server capability declaration
├── state.ss                Global state (documents, symbol index, module cache)
│
├── util/
│   ├── log.ss              Logging to stderr
│   └── position.ss         Line/column and range utilities
│
├── analysis/
│   ├── document.ss         Document text buffer tracking
│   ├── parser.ss           S-expression parser with position info
│   ├── symbols.ss          Symbol extraction from parsed forms
│   ├── module.ss           Module resolution (imports/exports)
│   ├── index.ss            Workspace-wide symbol index
│   └── completion-data.ss  Completion candidate generation
│
└── handlers/
    ├── lifecycle.ss        initialize, shutdown, exit
    ├── sync.ss             didOpen, didChange, didClose, didSave
    ├── diagnostics.ss      Compile errors via gxc
    ├── completion.ss       textDocument/completion
    ├── hover.ss            textDocument/hover
    ├── definition.ss       textDocument/definition
    ├── references.ss       textDocument/references
    ├── symbols.ss          documentSymbol + workspace/symbol
    ├── rename.ss           textDocument/rename
    ├── formatting.ss       textDocument/formatting
    └── signature.ss        textDocument/signatureHelp
```

### Data Flow

1. **Transport** reads LSP messages from stdin (Content-Length framing)
2. **JSON-RPC** layer parses the JSON and classifies as request or notification
3. **Server** dispatches to the registered handler by method name
4. **Handlers** use the **analysis** layer to inspect documents and symbols
5. **Server** serializes the response and writes it back via **transport**

### State Management

The server maintains global state in `lsp/state.ss`:

| State | Type | Description |
|-------|------|-------------|
| `*documents*` | `uri -> document` | Open document text buffers |
| `*symbol-index*` | `uri -> sym-info list` | Extracted symbols per file |
| `*module-cache*` | `module-path -> exports` | Cached module export lists |
| `*workspace-root*` | `string` | Workspace root directory |

Documents are re-analyzed on every change (full text sync). The symbol index is updated incrementally as files are opened and modified.

### Dependencies

The server uses these Gerbil standard library modules:

| Module | Purpose |
|--------|---------|
| `:std/text/json` | JSON serialization |
| `:std/format` | String formatting |
| `:std/sugar` | `when-let`, `with-catch`, etc. |
| `:std/iter` | `for`, `for/collect`, `for-each` |
| `:std/error` | Exception types |
| `:std/cli/getopt` | CLI argument parsing |
| `:std/misc/process` | Spawning `gxc` for diagnostics |
| `:std/misc/ports` | File reading utilities |
| `:std/misc/string` | String utilities |
| `:std/misc/path` | Path manipulation |

## How It Works

### Diagnostics

On file open and save, the server runs two levels of checking:

1. **Parse-level**: Attempts to read the file as S-expressions using Gambit's `read`. Reports syntax errors with position info.
2. **Compile-level**: Runs `gxc -S` on the file and parses its error output into structured diagnostics with file, line, column, and message.

### Completion

Completion candidates come from three sources, filtered by the prefix at the cursor:

1. **Local symbols** -- definitions extracted from the current file
2. **Workspace symbols** -- definitions from all indexed `.ss` files
3. **Keywords** -- 75+ Gerbil special forms and keywords (def, lambda, let, if, cond, match, import, export, etc.)

Trigger characters: `(`, `:`, `/`, `.`

### Hover

When hovering over a symbol, the server:

1. Identifies the symbol at the cursor position using word-boundary detection
2. Searches local file symbols, then workspace-wide definitions
3. Returns a markdown code block showing the signature and kind

### Formatting

The formatter reads each top-level S-expression and outputs it through Gambit's `pretty-print`. This handles indentation and line wrapping but does not preserve comments (a known limitation of `read`-based formatting).

## Other Editor Support

While primary integration is with Emacs/Eglot, `gerbil-lsp` implements standard LSP over stdio and should work with any LSP client:

### Neovim (nvim-lspconfig)

```lua
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

configs.gerbil_lsp = {
  default_config = {
    cmd = { 'gerbil-lsp', '--stdio' },
    filetypes = { 'gerbil', 'scheme' },
    root_dir = lspconfig.util.root_pattern('gerbil.pkg', '.git'),
  },
}

lspconfig.gerbil_lsp.setup{}
```

### VS Code

Create a `.vscode/settings.json` or use a generic LSP client extension configured with:

```json
{
  "command": "gerbil-lsp",
  "args": ["--stdio"],
  "languages": ["scheme"]
}
```

## Development

### Clean and rebuild

```bash
make clean
make build
```

### Debug logging

Run with verbose logging to see all JSON-RPC messages:

```bash
gerbil-lsp --stdio --log-level debug 2>lsp-debug.log
```

### Manual testing

Send raw LSP messages via stdin:

```bash
INIT='{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":1,"rootUri":"file:///tmp","capabilities":{}}}'
printf "Content-Length: %d\r\n\r\n%s" "${#INIT}" "$INIT" | gerbil-lsp --stdio 2>/dev/null
```

### Project structure for development

```
gerbil-lsp/
├── gerbil.pkg          Package definition (package: lsp)
├── build.ss            Build script listing all modules
├── Makefile            Build/clean/install targets
├── lsp/                All source code (23 modules)
├── emacs/              Emacs integration
└── test/               Test files (placeholder)
```

## Known Limitations

- **Full document sync only** -- the entire document text is sent on each change (no incremental sync yet)
- **Formatting strips comments** -- `pretty-print` operates on S-expressions after `read`, which discards comments
- **No incremental indexing** -- workspace symbols are only indexed from open documents, not scanned on startup
- **Diagnostics require saved files** -- `gxc` compilation runs on the filesystem copy, not the editor buffer
- **Rename is limited to open documents** -- closed files in the workspace are not updated

## License

MIT
