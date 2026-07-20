# Identifier Syntax

Most subcommands take a single identifier that names a package, module, or
symbol:

```
<pkg>[-<version>][/<Module.Path>][/<symbol>]
```

The version is optional and attaches with a **hyphen** (the usual Haskell
`name-version` convention). A bare name resolves to whatever the plan /
store / Hackage considers current.

Examples:

- `async` — package (current version)
- `async-2.2.6` — version-pinned package
- `async-2.2.6/Control.Concurrent.Async` — module
- `async-2.2.6/Control.Concurrent.Async/concurrently` — symbol
- `async/Control.Concurrent.Async/concurrently` — same symbol, unpinned
- `my-project/MyProject.Internal/helper` — a symbol from the **local** project

> **Version detection.** A trailing segment is treated as a version only
> when it looks like one (digits and dots). So a hyphenated package name
> such as `my-project` is parsed as a whole name, while `async-2.2.6`
> splits into name `async` + version `2.2.6`. A cabal store hash suffix
> (`async-2.2.6-<hash>`) is recognised and stripped automatically.

> **Note:** the `@version` form is **not** accepted — use the hyphen.

## Sub-libraries

The [doc-browser server](../server/index.md) addresses sublibs as
`pkg:sublib` in URLs (e.g. `/pkg/happy-lib:frontend`). The CLI path does
**not** yet handle the `:<sublib>` suffix in identifier arguments — that's
planned. For now, query a sublib by browsing it in the server UI.
