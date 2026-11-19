# OCaml conformance harness

A spike of the conformance harness in OCaml, on Protocol's reader (`xynolog_protocol`). It is the dune package `xynolog-conformance`, built from this repository's `dune-project` and `dune.lock` beside the reader. It ports the gateway's broker case and the Librarian's telemetry, search and gather cases from the TypeScript suites, which remain the suites of record.

| Directory | What it is |
|---|---|
| `harness/` | The library: stand-ins, stores, processes, the bus-stream client, OTLP and the runner's glue. |
| `harness-test/` | The harness's own cases: stream flushing, abrupt closes, large bodies, OTLP decoding, the two Neo4j reads, the runner's bound. |
| `gateway/` | The broker case. |
| `librarian/` | The Librarian's telemetry, search and gather cases. |

Build it and run a suite with the same implementations manifest the TypeScript suites take, after testo's own arguments:

```bash
dune build ./conformance/ocaml
```

```bash
./_build/default/conformance/ocaml/librarian/main.exe -e implementations=/path/to/implementations.json
```

The Librarian suite and the Neo4j case of `harness-test` read the stores the TypeScript Librarian suite reads, named by `NEO4J_HOST`, `NEO4J_PASSWORD`, `QDRANT_HOST` and `QDRANT_API_KEY`. The Librarian suite runs one world at a time, as its TypeScript project does; the others run four workers unless `-j` says otherwise.
