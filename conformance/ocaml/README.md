# OCaml conformance harness

The conformance harness in OCaml, on Protocol's reader (`xynolog_protocol`). It is the dune package `xynolog-conformance`, a dune project of its own here, with its own `dune-project` and `dune.lock`, so a repository that vendors Protocol never sees it or its dependencies. It reaches the reader through `protocol`, a link to this repository's root, vendored as a consumer vendors Protocol; the workspace and the format are the root's, through `dune-workspace` and `.ocamlformat`, links to the root's. It holds the whole gateway suite, the whole dispatcher suite and the whole librarian suite.

| Directory | What it is |
|---|---|
| `bin/` | The program, `xynolog-conformance`, which every suite is linked into. |
| `harness/` | The library: stand-ins, stores, processes, the bus-stream client, the broker and its proxy, OTLP, the census gates and the runner's glue. |
| `harness-test/` | The harness's own cases: each census gate, stream flushing, abrupt closes, large bodies, OTLP decoding, the two Neo4j reads, the runner's bound. |
| `gateway/` | The gateway suite: [its README](gateway/README.md) says what it checks and what it needs. |
| `dispatcher/` | The dispatcher suite: [its README](dispatcher/README.md) says what it checks and what it needs. |
| `librarian/` | The librarian suite: [its README](librarian/README.md) says what it checks and what it needs. |

Build it, and run a suite against an implementations manifest, the one the TypeScript suites take:

```bash
dune build --root conformance/ocaml
```

```bash
./conformance/ocaml/_build/default/bin/main.exe librarian --implementations /path/to/implementations.json
```

The repository's root has a `dune-workspace` too, and dune takes the outermost as the workspace, so every dune command here names `--root conformance/ocaml`. Run the program from the repository's root, where `specs/src` is. testo's own arguments follow the manifest: `-s <text>` selects cases, `-j <n>` sets the workers, and `status` and `approve` are its subcommands, as in `... --implementations <manifest> status -a`.

The librarian suite and the Neo4j case of `harness-test` read the stores named by `NEO4J_HOST`, `NEO4J_PASSWORD`, `QDRANT_HOST` and `QDRANT_API_KEY`. The Neo4j's default database is the suite's: `kb-` and the first 60 hex digits of the SHA-256 of `did:web:example.org:test-kb`. The librarian suite runs one world at a time; the gateway and dispatcher suites run four workers unless `-j` says otherwise.

`dune test --root conformance/ocaml` runs every case of `harness-test` that needs no store and no manifest (`-t "not stores"`), the census gates among them; `make reader` and `make validate` run it.
