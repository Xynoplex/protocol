# Conformance suites

Black-box suites exercise implementations through the protocol surfaces they expose. The suites do not build or import implementations; each consumer repository supplies launch commands and SDK drivers in its own manifest.

Every suite runs on the OCaml harness ([`ocaml`](ocaml/README.md)), from the same manifest: `gateway`, the HTTP API, bus streams, authorization and limits ([`ocaml/gateway`](ocaml/gateway/README.md)); `dispatcher`, job admission and claims, the durable queue, its clocks, and health ([`ocaml/dispatcher`](ocaml/dispatcher/README.md)); `archivist`, HTTP and bus behavior, persisted events, and stored resources ([`ocaml/archivist`](ocaml/archivist/README.md)); `librarian`, search and gathering across a running record and index ([`ocaml/librarian`](ocaml/librarian/README.md)); `smelter`, vector indexing, anchored text, provider interactions, and telemetry ([`ocaml/smelter`](ocaml/smelter/README.md)); `weaver`, graph indexing, bus behavior, health, and telemetry ([`ocaml/weaver`](ocaml/weaver/README.md)); `worker-service`, Worker services, the jobs they run, provider interactions, health, and telemetry ([`ocaml/worker_service`](ocaml/worker_service/README.md)); `sdk`, SDK drivers as clients of a running gateway ([`sdk`](sdk/README.md)); and `worker`, SDK worker drivers as clients of a dispatcher and gateway ([`worker`](worker/README.md)).

## Implementation manifest

Every run names one JSON manifest with `--implementations`; there is no default product manifest. Start from [`example.implementations.json`](example.implementations.json). Nothing in `../specs/src` states its shape: the harness's decoder, [`ocaml/harness/implementations.ml`](ocaml/harness/implementations.ml), is its one statement, and refuses a member it does not know, a missing one and a wrong type. Commands are argument arrays, not shell strings. Relative command arguments and an optional `modelCatalogue` path resolve from the manifest's directory. Build programs, drivers, wrappers, and their runtime environments in the owning consumer repositories.

Example from this repository's root:

```sh
dune build --root conformance/ocaml
./conformance/ocaml/_build/default/bin/main.exe worker-service --implementations /path/to/worker-service.implementations.json
```

The manifest can name several implementations in one run. A suite refuses to start if the manifest is missing or malformed. It does not infer a current/default configuration.

## Runtime requirements

The gateway, dispatcher, and Archivist suites need their corresponding commands and NATS Server 2.10 or newer. SDK and worker suites need the drivers named in `sdks`. Worker, Weaver, Smelter, and Librarian suites need their service commands and the external stores and tools documented by each suite. Weaver and Librarian use Neo4j; Smelter and Librarian use Qdrant. Some suites clear the database instance they receive. Use dedicated test instances.

The suites read Protocol through its reader (`xynolog_protocol`), from `../specs/src` as they run: every table through the model its schema gives, and every value held to its schema by the reader's own validator. Language-neutral case tables also live in `../specs/src`; implementation repositories should include applicable tables in their own tests.
