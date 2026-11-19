# Specifications

`src/` is the machine-readable protocol source of truth. It contains the gateway and Archivist OpenAPI documents, shared schemas and registries, configuration and identity formats, and language-neutral behavior case tables.

## Build the bundles

The OpenAPI bundle files are generated and ignored by Git. From the repository root:

```sh
make install
make bundle
```

`specs/openapi.json` and `specs/archivist.openapi.json` are outputs for type generation and conformance runs. Edit only files under `src/`. `make lint` applies the Redocly rules; `make reader` runs the reader, which holds the protocol's own rules, including each table's schema. Every JSON file under `src/` is spelled as `JSON.stringify(value, null, 2)` and a newline write it, so an edit made with any tool changes only what it means to change; `make reader` refuses any other spelling.

## Layout

`src/openapi.json`, `src/paths/` and `src/components/` are the gateway's public API: what it serves over HTTP and carries on the bus. Its document lists every schema of `components/schemas/`, and the client's discovery document's (`discovery/`); a component schema refers only to component schemas, and so does every route of the gateway's. `src/control/` is the launcher's control API. A service's own schemas, such as its configuration document and its health answer, are in its area: `archivist/` (beside Archivist's own document), `dispatcher/`, `gateway/`, `librarian/`, `smelter/`, `weaver/` and `worker/`, and the stores' and the embedding model's in `graph/`, `vectors/` and `embedding/`. A component schema that only Archivist's document reaches is Archivist's, so its place is `archivist/`. A service's schema is a file at the top of its area, named in PascalCase, that is no table's schema; the reader names the services' areas (`Places.services`), reads their schemas as `Spec.services`, and refuses a service's area that holds none. The reader (`make reader`) holds all of this.

## Case tables

A file under `src/` whose name ends in `cases.json` states behavior case by case. Its `$comment` describes the rule, and its cases state it; a table names no consumer, and each consumer records which of its tests runs it. [`src/CaseTable.json`](src/CaseTable.json) is the schema of what every case table carries. Each consumer repository owns its implementation tests and uses the pinned protocol revision as input.

## Table schemas

A table's schema sits beside it, named by the table's area and its file's stem in PascalCase: `jobs/retry-cases.json`'s schema is `jobs/JobsRetryCases.json`. A case table's schema extends `CaseTable.json` through `allOf`, admits each of its properties, and states what a case is; a `why`, wherever a table's schema states one, is `CaseTable.json`'s. The reader (`reader/`, `make reader`) derives each table's typed model from its schema, decodes the table through it and validates it against its schema; a schema it can derive no model from is refused. The form it derives from: every object closed, any two objects of a union told apart by a tag or by a property one requires and the other does not state, and at most one member of each other type beside them, a value that may be null written as JSON Schema 2020-12 writes one (a type that may be null is `"type": [T, "null"]`, a `$ref` that may be null is `{ "anyOf": [{ "$ref": … }, { "type": "null" }] }`, and a `oneOf` that may be null has `{ "type": "null" }` as its last member), a value that is JSON by nature marked so in its `$comment`, a value that is a JSON Schema a `$ref` to [`src/JsonSchema.json`](src/JsonSchema.json), which the reader reads as a schema (an annotation, a `$comment`, `title` or `description`, may sit beside a table's `$ref`), and each member of a union that no tag tells apart given a `title`, which names its constructor. Every table has its schema: a table is a JSON file at the top of an area of `src/`, named in kebab case, but an area's `openapi.json`, and a schema that extends `CaseTable.json` or states its `$comment` has its table beside it.

Cases that require live processes belong in [`../conformance/`](../conformance/README.md). The conformance suites are black-box and obtain build and launch commands from a consumer-provided manifest.

## Ownership and consumers

The [`contract/`](../contract/README.md) directory gives the normative prose. SDK type generators, service implementations, CLI configuration handling, and Desktop integrations stay in their respective repositories. See [`../OWNERSHIP.md`](../OWNERSHIP.md) for the complete boundary and generator ownership map.

Consumers pin this repository as a Git submodule at `protocol/`. Do not fetch `main` implicitly in a build. Update the pin deliberately, regenerate consumer-owned outputs, and run that consumer's checks.
