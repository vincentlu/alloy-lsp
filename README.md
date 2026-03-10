# alloy-lsp

An Emacs package integrating the [Alloy 6](https://alloytools.org/) formal
specification language with [lsp-mode](https://emacs-lsp.github.io/lsp-mode/).

Connects to the Alloy language server (bundled in the official
[Alloy distribution](https://github.com/AlloyTools/org.alloytools.alloy))
and provides an experience similar to the
[VS Code Alloy extension](https://github.com/s-arash/VSCodeAlloyExtension):
CodeLens execution, streaming output, and instance viewing.

## Features

- **CodeLens execution** — click or invoke commands directly from the buffer
- **Streaming output** — execution results in a dedicated `*Alloy*` buffer
- **Instance links** — click to open counterexamples/instances in the Alloy visualizer
- **Auto-download** — the server JAR is downloaded from the official Alloy release on first use
- **Command discovery** — `alloy-lsp-list-commands` shows a chooser of runnable commands

## Installation

Add `alloy-lsp.el` to your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/alloy-lsp")
(require 'alloy-lsp)
(add-hook 'alloy-mode-hook #'alloy-lsp-ensure)
```

The server JAR (`org.alloytools.alloy.dist.jar`) is downloaded automatically on
first use. To use a custom JAR:

```elisp
(setq alloy-lsp-server-command
      '("java" "-jar" "/path/to/org.alloytools.alloy.dist.jar" "ls"))
```

Requires Java on your `PATH`.

## Keybindings

| Key         | Command                              |
|-------------|--------------------------------------|
| `C-c a e`   | Execute command under cursor         |
| `C-c a a`   | Execute all commands in file         |
| `C-c a c`   | List commands (chooser)              |
| `C-c a l`   | Open latest instance                 |
| `C-c a s`   | Stop execution                       |

In the `*Alloy*` output buffer: `c` to clear, `s` to stop.

## Major mode

This package provides LSP integration only. For syntax highlighting and
indentation, use one of:

- [alloy-mode](https://github.com/vincentlu/alloy-mode) — works on any Emacs 25.1+
- [alloy-ts-mode](https://github.com/vincentlu/alloy-ts-mode) — tree-sitter-based (Emacs 29+),
  using the [tree-sitter-alloy](https://github.com/vincentlu/tree-sitter-alloy) grammar

## License

[GPL-3.0-or-later](LICENSE)
