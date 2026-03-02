# Alloy LSP Custom Protocol

Reference: `VSCodeAlloyExtension/` (VS Code extension) and
`org.alloytools.alloy/org.alloytools.alloy.lsp/src/main/java/org/alloytools/alloy/lsp/provider/` (server).

## Transport

Standard LSP JSON-RPC over stdio (VS Code uses TCP; Emacs uses stdio via
`lsp-stdio-connection`). Standard LSP lifecycle applies (`initialize`,
`initialized`, `textDocument/didOpen`, `didChange`, etc.).

## Standard LSP features used

- `textDocument/codeLens` — primary "Execute" affordance (used by this plugin)
- `textDocument/documentLink` — disabled (`lsp-enable-links nil`); server
  returns VS Code-specific `command:` URIs that lsp-mode cannot handle

---

## Client → Server custom notifications

All are sent as JSON-RPC notifications (no response expected). The server uses
`@JsonRequest` handlers but they work fine fire-and-forget.

### `ExecuteAlloyCommand`

Params: JSON array `[uri, ind, line, char]`

| Field | Type | Description |
|-------|------|-------------|
| `uri` | string | Document URI, e.g. `"file:///path/to/test.als"` |
| `ind` | number | Command index (0-based); `-1` means execute all |
| `line` | number | 0-based line of the command |
| `char` | number | 0-based character offset |

Results arrive asynchronously via `alloy/showExecutionOutput`.

### `StopExecution`

Params: none. Cancels the current run; server sends `RunCompleted` with
message `"Stopped"`.

### `OpenModel`

Params: a string (the link value from a prior `showExecutionOutput`
notification). Triggers server-side visualization via `doVisualize(link)`.
Link prefixes: `"XML: "` → load instance into VizGUI; also `"CORE: "`,
`"CNF: "`, `"POS: "`.

### `ListAlloyCommands`

Params: document URI string. Server replies asynchronously via
`alloy/commandsListResult`.

---

## Server → Client custom notifications

### `alloy/showExecutionOutput`

Streaming execution output/progress/results. Params:

```json
{
  "message":     "string | null",
  "messageType": number,
  "bold":        boolean,
  "replaceLast": boolean,
  "lineBreak":   boolean,
  "link":        "string | null"
}
```

`messageType` values:

| Value | Name | Meaning |
|-------|------|---------|
| 0 | RunStarted | Execution began |
| 1 | RunInProgress | Progress update |
| 2 | RunResult | A result (instance found / no counterexample) |
| 3 | RunCompleted | Execution finished; add visual separator |
| 4 | Warning | Error/warning; render with error face |

Rendering rules:
- `replaceLast == true` → replace previously inserted output item (progress updates)
- `lineBreak == false` → do not append `\n` (default: `true`)
- `bold == true` → bold face
- `link` present → render as clickable button; clicking sends `OpenModel(link)`
- `messageType == 3` → insert horizontal rule after output

`RunCompleted` `message` values: `""` (normal), `"failure"` (error),
`"Stopped"` (cancelled).

### `alloy/commandsListResult`

Reply to `ListAlloyCommands`. Params:

```json
{
  "commands": [
    {
      "title": "string",
      "command": {
        "command":   "ExecuteAlloyCommand",
        "arguments": [uri, ind, line, char]
      }
    }
  ]
}
```

Client presents `commands[*].title` in a chooser; on selection sends
`ExecuteAlloyCommand` with the provided arguments.

---

## CodeLens / "Execute" affordance

The server returns CodeLens entries with:
- `command.command == "ExecuteAlloyCommand"`
- `command.title == "Execute"`
- `command.arguments == [uri, ind, line, char]`

lsp-mode normally dispatches CodeLens clicks to `workspace/executeCommand`
(which the Alloy server does not implement). This plugin intercepts clicks via
`:action-handlers` in the lsp-mode client registration and sends the
`ExecuteAlloyCommand` custom notification instead.
