# OCaml conformance harness

The conformance harness in OCaml, on Protocol's reader (`xynolog_protocol`). It is the dune package `xynolog-conformance`, built from this repository's `dune-project` and `dune.lock` beside the reader. It holds the gateway's broker case and the Librarian's telemetry, search and gather cases, ported from the TypeScript suites, which remain the suites of record for every case not yet ported.

| Directory | What it is |
|---|---|
| `bin/` | The program, `xynolog-conformance`, which every suite is linked into. |
| `harness/` | The library: stand-ins, stores, processes, the bus-stream client, OTLP, the census gates and the runner's glue. |
| `harness-test/` | The harness's own cases: each census gate, stream flushing, abrupt closes, large bodies, OTLP decoding, the two Neo4j reads, the runner's bound. |
| `gateway/` | The gateway suite: the broker case. |
| `librarian/` | The librarian suite: the telemetry, search and gather cases. |

Build it, and run a suite against the implementations manifest the TypeScript suites take:

```bash
dune build ./conformance/ocaml
```

```bash
./_build/default/conformance/ocaml/bin/main.exe librarian --implementations /path/to/implementations.json
```

Run it from the repository's root, where `specs/src` is. testo's own arguments follow the manifest: `-s <text>` selects cases, `-j <n>` sets the workers, and `status` and `approve` are its subcommands, as in `... --implementations <manifest> status -a`.

The librarian suite and the Neo4j case of `harness-test` read the stores the TypeScript librarian suite reads, named by `NEO4J_HOST`, `NEO4J_PASSWORD`, `QDRANT_HOST` and `QDRANT_API_KEY`. The Neo4j's default database is the suite's: `kb-` and the first 60 hex digits of the SHA-256 of `did:web:example.org:test-kb`. The librarian suite runs one world at a time, as its TypeScript project does; the gateway suite runs four workers unless `-j` says otherwise.

`dune test` runs every case of `harness-test` that needs no store and no manifest (`-t "not stores"`), the census gates among them; `make validate` runs it.
