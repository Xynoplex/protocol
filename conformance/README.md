# Conformance suites

Black-box suites exercise implementations through the protocol surfaces they expose. The suites do not build or import implementations; each consumer repository supplies launch commands and SDK drivers in its own manifest.

| Suite | Checks | Command |
| --- | --- | --- |
| `gateway` | HTTP API, bus streams, authorization, and limits | `test:gateway` |
| `dispatcher` | Job claims, durable queue behavior, and health | `test:dispatcher` |
| `archivist` | HTTP and bus behavior, persisted events, and stored resources | `test:archivist` |
| `sdk` | SDK drivers as clients of a running gateway | `test:sdk` |
| `worker` | SDK worker drivers as clients of a dispatcher and gateway | `test:worker` |
| `worker-service` | Worker services, provider interactions, and health | `test:worker-service` |
| `weaver` | Graph indexing, bus behavior, health, and telemetry | `test:weaver` |
| `smelter` | Vector indexing, anchored text, provider interactions, and telemetry | `test:smelter` |
| `librarian` | Search and gathering across a running record and index | `test:librarian` |

## Implementation manifest

Every run names one JSON manifest with `--implementations`; there is no default product manifest. Start from [`example.implementations.json`](example.implementations.json). Its validated shape is [`harness/implementations.ts`](harness/implementations.ts). Commands are argument arrays, not shell strings. Relative command arguments and an optional `modelCatalogue` path resolve from the manifest's directory. Build programs, drivers, wrappers, and their runtime environments in the owning consumer repositories.

Example from this repository's root:

```sh
npm ci --prefix conformance
npm run --prefix conformance test:gateway -- -- --implementations /path/to/gateway.implementations.json
```

The manifest can name several implementations in one run. A suite refuses to start if the manifest is missing or malformed. It does not infer a current/default configuration.

## Runtime requirements

The gateway, dispatcher, and Archivist suites need their corresponding commands and NATS Server 2.10 or newer. SDK and worker suites need the drivers named in `sdks`. Worker, Weaver, Smelter, and Librarian suites need their service commands and the external stores and tools documented by each suite. Weaver and Librarian use Neo4j; Smelter and Librarian use Qdrant. Some suites clear the database instance they receive. Use dedicated test instances.

The suite reads Protocol through what its reader writes from `../specs/src` (`make bundle` at the repository root, which `make conformance-typecheck` and `make conformance-fixtures` run first): the bundles, from which it derives the gateway's types; and, in `harness/generated/protocol`, every table's value and type and every service schema's type (`protocol.ts`), and the reader's own validator compiled to JavaScript (`validate.cjs`), which holds a service's values to the services' bundle (`harness/services.ts`). Language-neutral case tables also live in `../specs/src`; implementation repositories should include applicable tables in their own tests.
