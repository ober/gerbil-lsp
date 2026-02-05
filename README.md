# gerbil-lsp

A Language Server Protocol (LSP) implementation for [Gerbil Scheme](https://cons.io), providing IDE features through any LSP-compatible editor. Primary integration is with Emacs via [Eglot](https://github.com/joaotavora/eglot).

## Features

| Feature            | LSP Method                        | Description                                                                 |
|--------------------|-----------------------------------|-----------------------------------------------------------------------------|
| Diagnostics        | `textDocument/publishDiagnostics` | Compilation errors via `gxc` and parse error detection, debounced on change |
| Completion         | `textDocument/completion`         | Symbols from current file, workspace, and Gerbil keywords with auto-import  |
| Hover              | `textDocument/hover`              | Symbol info with kind, signature, and source location                       |
| Go to Definition   | `textDocument/definition`         | Jump to symbol definition across workspace and stdlib                       |
| Go to Declaration  | `textDocument/declaration`        | Jump to declaration (same as definition in Gerbil)                          |
| Go to Type Def     | `textDocument/typeDefinition`     | Navigate from constructors/predicates/accessors to their type               |
| Find References    | `textDocument/references`         | Locate all occurrences of a symbol                                          |
| Document Symbols   | `textDocument/documentSymbol`     | Outline view of definitions in current file                                 |
| Workspace Symbols  | `workspace/symbol`                | Search definitions across all indexed files                                 |
| Rename             | `textDocument/rename`             | Scope-aware rename, skips strings and comments                              |
| Formatting         | `textDocument/formatting`         | Format via Gambit's `pretty-print`                                          |
| Signature Help     | `textDocument/signatureHelp`      | Function signatures while typing arguments                                  |
| Code Action        | `textDocument/codeAction`         | Quick fixes and refactoring actions (with lazy resolution)                  |
| Code Lens          | `textDocument/codeLens`           | Inline actions (run test, show references)                                  |
| Execute Command    | `workspace/executeCommand`        | Run test files, show reference counts                                       |
| Document Highlight | `textDocument/documentHighlight`  | Highlight occurrences of symbol under cursor                                |
| Folding Range      | `textDocument/foldingRange`       | Code folding for top-level forms                                            |
| Selection Range    | `textDocument/selectionRange`     | Expand/shrink selection by syntax                                           |
| Document Link      | `textDocument/documentLink`       | Clickable module import paths                                               |
| Inlay Hints        | `textDocument/inlayHint`          | Inline type/parameter hints (with lazy resolution)                          |
| File Rename        | `workspace/willRenameFiles`       | Automatically update import paths when `.ss` files are renamed              |

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
- Workspace-local packages: `.gerbil/lib/` in workspace root
- `GERBIL_LOADPATH` directories

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

The Emacs integration is provided by `eglot-gerbil`, shipped in the `emacs/` directory of this repository. It registers `gerbil-lsp` with Eglot and adds support for features that Eglot does not handle natively (code lenses, call hierarchy, type hierarchy, selection ranges, etc.).

### 1. Load eglot-gerbil

Add to your Emacs init file (`~/.emacs.d/init.el` or `~/.emacs`):

```elisp
(add-to-list 'load-path "/path/to/gerbil-lsp/emacs")
(require 'eglot-gerbil)
```

### 2. (Optional) Auto-start on gerbil-mode

```elisp
(setq eglot-gerbil-auto-start t)
```

Or use the standard Eglot hook:

```elisp
(add-hook 'gerbil-mode-hook #'eglot-ensure)
```

### 3. (Optional) Customize server path

If `gerbil-lsp` is not on your PATH:

```elisp
(setq eglot-gerbil-server-path "/path/to/gerbil-lsp/.gerbil/bin/gerbil-lsp")
```

### 4. (Optional) Set log level

```elisp
(setq eglot-gerbil-log-level "debug")  ;; debug | info | warn | error
```

### 5. (Optional) Configure features

```elisp
(setq eglot-gerbil-enable-inlay-hints t)   ;; parameter name hints (default: t)
(setq eglot-gerbil-enable-code-lenses t)   ;; reference counts & test runners (default: t)
```

### Usage

Open any `.ss` file in `gerbil-mode` and run `M-x eglot`. The `eglot-gerbil-mode` minor mode activates automatically, providing keybindings and feature integration.

#### Standard LSP Features (handled by Eglot)

These work out of the box -- Eglot supports them natively:

| Feature | Key | Command |
|---------|-----|---------|
| Diagnostics | (automatic) | Errors appear as underlines |
| Completion | `C-M-i` | Trigger with `(` `:` `/` `.` |
| Hover | (automatic) | Via eldoc |
| Go to Definition | `M-.` or `C-c l d` | `xref-find-definitions` |
| Go to Declaration | `C-c l D` | `eglot-find-declaration` |
| Go to Type Definition | `C-c l T` | `eglot-find-typeDefinition` |
| Find References | `M-?` or `C-c l R` | `xref-find-references` |
| Rename | `C-c l n` | `eglot-rename` |
| Format Buffer | `C-c l f` | `eglot-format-buffer` |
| Code Actions | `C-c l a` | `eglot-code-actions` |
| Implementation | `C-c l i` | `eglot-find-implementation` |
| Document Symbols | `M-x imenu` | Outline via imenu |

#### Extended Features (via eglot-gerbil)

Eglot has no built-in support for these LSP capabilities. The `eglot-gerbil` package bridges the gap:

| Feature | Key | Command |
|---------|-----|---------|
| Run Test | `C-c l t` | `eglot-gerbil-run-test` |
| Show References | `C-c l r` | `eglot-gerbil-show-references` |
| Incoming Calls | `C-c l c i` | `eglot-gerbil-incoming-calls` |
| Outgoing Calls | `C-c l c o` | `eglot-gerbil-outgoing-calls` |
| Supertypes | `C-c l h s` | `eglot-gerbil-supertypes` |
| Subtypes | `C-c l h b` | `eglot-gerbil-subtypes` |
| Expand Selection | `C-c l +` | `eglot-gerbil-expand-selection` |
| Shrink Selection | `C-c l -` | `eglot-gerbil-shrink-selection` |
| Refresh Lenses | `C-c l l` | `eglot-gerbil-refresh-code-lenses` |
| Workspace Symbols | `C-c l s` | `eglot-gerbil-workspace-symbol` |
| Organize Imports | `C-c l o` | `eglot-gerbil-organize-imports` |
| Edit Config | `C-c l p` | `eglot-gerbil-edit-project-config` |

#### Code Lenses

Code lenses appear above function definitions showing reference counts and
"Run test" buttons for test suites. They refresh automatically on save.
Click or press `RET` on a lens to execute its action.

#### Inlay Hints

Parameter name hints appear at function call sites when the function has
2+ parameters. Requires Emacs 29.1+ and Eglot 1.15+.

#### Semantic Tokens

The server provides semantic highlighting with 10 token types. The
eglot-gerbil package defines custom faces (`eglot-gerbil-keyword-face`,
`eglot-gerbil-macro-face`, etc.) for each type. Customize them via
`M-x customize-group RET eglot-gerbil`.

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
│   ├── position.ss         Line/column and range utilities
│   └── string.ss           String utilities and text region classification
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
    ├── diagnostics.ss      Compile errors via gxc + debounced on-change
    ├── completion.ss       textDocument/completion with auto-import
    ├── hover.ss            textDocument/hover
    ├── definition.ss       textDocument/definition (local + stdlib)
    ├── references.ss       textDocument/references
    ├── symbols.ss          documentSymbol + workspace/symbol
    ├── rename.ss           Scope-aware rename (skips strings/comments)
    ├── formatting.ss       textDocument/formatting
    ├── signature.ss        textDocument/signatureHelp
    ├── execute-command.ss  workspace/executeCommand (run test, show refs)
    ├── code-action.ss      textDocument/codeAction + codeAction/resolve
    ├── code-lens.ss        textDocument/codeLens
    ├── document-highlight.ss  textDocument/documentHighlight
    ├── document-link.ss    textDocument/documentLink
    ├── folding-range.ss    textDocument/foldingRange
    ├── selection-range.ss  textDocument/selectionRange
    ├── inlay-hints.ss      textDocument/inlayHint + inlayHint/resolve
    ├── type-definition.ss  textDocument/typeDefinition
    ├── will-rename.ss      workspace/willRenameFiles
    └── pull-diagnostics.ss textDocument/diagnostic (pull model)
```

### Data Flow

1. **Transport** reads LSP messages from stdin (Content-Length framing)
2. **JSON-RPC** layer parses the JSON and classifies as request or notification
3. **Server** dispatches to the registered handler by method name
4. **Handlers** use the **analysis** layer to inspect documents and symbols
5. **Server** serializes the response and writes it back via **transport**

### State Management

The server maintains global state in `lsp/state.ss`:

| State                     | Type                     | Description                                  |
|---------------------------|--------------------------|----------------------------------------------|
| `*documents*`             | `uri -> document`        | Open document text buffers                   |
| `*symbol-index*`          | `uri -> sym-info list`   | Extracted symbols per file                   |
| `*module-cache*`          | `module-path -> exports` | Cached module export lists                   |
| `*workspace-root*`        | `string`                 | Workspace root directory                     |
| `*last-completion-uri*`   | `string`                 | URI of last completion request (for resolve) |
| `*debounce-thread*`       | `thread`                 | Active debounced diagnostics thread          |
| `*file-text-cache*`       | `uri -> string`          | Cached file contents for analysis            |
| `*gxc-diagnostics-cache*` | `uri -> diagnostics`     | Cached gxc compilation results               |

Documents are re-analyzed on every change (full text sync). The symbol index is updated incrementally as files are opened and modified.

### Dependencies

The server uses these Gerbil standard library modules:

| Module              | Purpose                          |
|---------------------|----------------------------------|
| `:std/text/json`    | JSON serialization               |
| `:std/format`       | String formatting                |
| `:std/sugar`        | `when-let`, `with-catch`, etc.   |
| `:std/iter`         | `for`, `for/collect`, `for-each` |
| `:std/error`        | Exception types                  |
| `:std/cli/getopt`   | CLI argument parsing             |
| `:std/misc/process` | Spawning `gxc` for diagnostics   |
| `:std/misc/ports`   | File reading utilities           |
| `:std/misc/string`  | String utilities                 |
| `:std/misc/path`    | Path manipulation                |

## How It Works

### Diagnostics

On file open and save, the server runs two levels of checking:

1. **Parse-level**: Attempts to read the file as S-expressions using Gambit's `read`. Reports syntax errors with position info.
2. **Compile-level**: Runs `gxc -S` on the file and parses its error output into structured diagnostics with file, line, column, and message.

Compile-level diagnostics are also triggered on text changes with a configurable debounce delay (default: 1500ms). After the user stops typing, the server waits for the delay period before running `gxc`, avoiding redundant compilations during active editing.

### Completion

Completion candidates come from three sources, filtered by the prefix at the cursor:

1. **Local symbols** -- definitions extracted from the current file
2. **Workspace symbols** -- definitions from all indexed `.ss` files
3. **Keywords** -- 75+ Gerbil special forms and keywords (def, lambda, let, if, cond, match, import, export, etc.)

Trigger characters: `(`, `:`, `/`, `.`

**Auto-import**: When a completion item comes from a known stdlib module (e.g., `read-json` from `:std/text/json`), accepting the completion via `completionItem/resolve` automatically inserts the corresponding `(import ...)` statement if not already present.

### Hover

When hovering over a symbol, the server:

1. Identifies the symbol at the cursor position using word-boundary detection
2. Searches local file symbols, then workspace-wide definitions
3. Returns a markdown code block showing the signature and kind

### Rename

Rename is scope-aware:

- **Local variables** (parameters, `let` bindings): renames are restricted to the enclosing form, preventing unintended changes to same-named symbols elsewhere
- **Global symbols**: renames apply across all open documents
- **String and comment filtering**: occurrences inside string literals and comments are never renamed

### Formatting

The formatter reads each top-level S-expression and outputs it through Gambit's `pretty-print`. This handles indentation and line wrapping but does not preserve comments (a known limitation of `read`-based formatting).

## Other Editor Support

While primary integration is with Emacs via `eglot-gerbil` (see `emacs/` directory), `gerbil-lsp` implements standard LSP over stdio and should work with any LSP client:

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

## Project Configuration

Create a `.gerbil-lsp.json` file in your workspace root to customize LSP behavior:

```json
{
  "gxc-path": "/custom/path/to/gxc",
  "gxc-flags": ["-O"],
  "loadpath": ["./libs", "~/.gerbil/lib"],
  "diagnostics-delay": 2000,
  "disabled-features": ["unused-import-warnings"]
}
```

| Option | Type | Description |
|--------|------|-------------|
| `gxc-path` | string | Path to gxc compiler (default: "gxc") |
| `gxc-flags` | string[] | Extra flags to pass to gxc (e.g., "-O") |
| `loadpath` | string[] | Directories to add to GERBIL_LOADPATH |
| `diagnostics-delay` | number | Debounce delay for diagnostics in ms (default: 1500) |
| `disabled-features` | string[] | Features to disable for this project |

The server also reads `gerbil.pkg` to extract package name and dependencies.

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
├── lsp/                All source code (30+ modules)
├── emacs/              Emacs integration (eglot-gerbil.el)
└── test/               Test suites
```

## Known Limitations

- **Full document sync only** -- the entire document text is sent on each change (no incremental sync yet)
- **No semantic type inference** -- analysis is purely syntactic; no cross-module type information

## Recent Improvements

- **Persistent index cache** -- workspace symbols are cached to `.gerbil-lsp-cache/` for fast startup
- **Comment-preserving formatting** -- blank lines, comment lines, and inline comments are preserved
- **Inter-file dependency tracking** -- when a file changes, dependents are automatically re-diagnosed
- **Temp file diagnostics** -- unsaved buffer content can be diagnosed via temporary files
- **Multi-root workspace support** -- `workspaceFolders` are fully supported with add/remove notifications
- **Workspace-wide rename** -- renames apply to all indexed files, not just open documents

## License

MIT
