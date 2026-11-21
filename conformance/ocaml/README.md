# OCaml conformance harness

The conformance harness in OCaml, on Protocol's reader (`xynolog_protocol`). It is the dune package `xynolog-conformance`, a dune project of its own here, with its own `dune-project` and `dune.lock`, so a repository that vendors Protocol never sees it or its dependencies. It reaches the reader through `protocol`, a link to this repository's root, vendored as a consumer vendors Protocol; the workspace and the format are the root's, through `dune-workspace` and `.ocamlformat`, links to the root's. It holds every suite.

| Directory | What it is |
|---|---|
| `bin/` | The program, `xynolog-conformance`, which every suite is linked into. |
| `harness/` | The library: stand-ins, stores, processes, the bus-stream client, the broker and its proxy, OTLP, the census gates and the runner's glue. |
| `harness-test/` | The harness's own cases: each census gate, stream flushing, abrupt closes, large bodies, the client proxy, OTLP decoding in either encoding, the two Neo4j reads, the runner's bound, git run by hand beside an Archivist that holds the index, the implementations manifest and the command line that names it, the issuer's grants, the ports handed out, values held to their schemas and telemetry rows. |
| `gateway/` | The gateway suite: [its README](gateway/README.md) says what it checks and what it needs. |
| `dispatcher/` | The dispatcher suite: [its README](dispatcher/README.md) says what it checks and what it needs. |
| `archivist/` | The archivist suite: [its README](archivist/README.md) says what it checks and what it needs. |
| `librarian/` | The librarian suite: [its README](librarian/README.md) says what it checks and what it needs. |
| `smelter/` | The smelter suite: [its README](smelter/README.md) says what it checks and what it needs. |
| `weaver/` | The weaver suite: [its README](weaver/README.md) says what it checks and what it needs. |
| `worker_service/` | The worker-service suite: [its README](worker_service/README.md) says what it checks and what it needs. |
| `sdk/` | The sdk suite's code. Its cases, and what it checks and needs, are in [`../sdk`](../sdk/README.md). |
| `worker/` | The worker suite's code. Its cases, and what it checks and needs, are in [`../worker`](../worker/README.md). |
| `protocol` | A link to this repository's root, vendored so the harness builds against the reader. |

Build it, and run a suite against an implementations manifest ([the conformance README](../README.md#implementation-manifest) says what it holds):

```bash
dune build --root conformance/ocaml
```

```bash
./conformance/ocaml/_build/default/bin/main.exe librarian --implementations /path/to/implementations.json
```

The repository's root has a `dune-workspace` too, and dune takes the outermost as the workspace, so every dune command here names `--root conformance/ocaml`. Run the program from the repository's root, where `specs/src` is. testo's own arguments follow the manifest: `-s <text>` selects cases, `-j <n>` sets the workers, and `status` and `approve` are its subcommands, as in `... --implementations <manifest> status -a`.

The weaver and librarian suites read the Neo4j named by `NEO4J_HOST` and `NEO4J_PASSWORD`, and the smelter and librarian suites the Qdrant named by `QDRANT_HOST` and `QDRANT_API_KEY`; each suite's README says what it does to them and how many workers it runs. The Neo4j case of `harness-test` reads `NEO4J_HOST` and `NEO4J_PASSWORD` too.

`dune test --root conformance/ocaml` runs every case of `harness-test` that needs no store and no manifest (`-t "not stores"`), the census gates among them; `make reader` and `make validate` run it.
