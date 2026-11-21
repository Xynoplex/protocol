# Specifications

`src/` is the machine-readable protocol source of truth. It contains the gateway and Archivist OpenAPI documents, shared schemas and registries, configuration and identity formats, and language-neutral behavior case tables.

## Build the bundles

The OpenAPI bundle files are generated and ignored by Git. From the repository root:

```sh
make bundle
```

The reader (`xynolog-protocol bundle`) writes `specs/openapi.json` and `specs/archivist.openapi.json`, the outputs for type generation and conformance runs. A bundle names nothing outside itself: a path item is written in its place, and every other file a `$ref` names is a component, by the name its document lists it under, or, unlisted, by its stem, as a schema when a schema names it and otherwise as the kind of its directory (`schemas/` or `responses/`). Edit only files under `src/`. `make bundle` writes the bundles; `make reader` runs the reader, which holds the protocol's own rules, OpenAPI's among them (`reader/lib/protocol_rules.ml`), and each table's schema. Every JSON file under `src/` is spelled as `JSON.stringify(value, null, 2)` and a newline write it, so an edit made with any tool changes only what it means to change; `make reader` refuses any other spelling.

## Layout

`src/openapi.json`, `src/paths/` and `src/components/` are the gateway's public API: what it serves over HTTP and carries on the bus. A path item's file, in `paths/` or `archivist/paths/`, is named by its operation's `operationId` in kebab case (`getStatus` is `paths/get-status.json`); a path item of several operations is named by what their `operationId`s share after the verb. A response's file, like a schema's, is its name in PascalCase, the name its document lists it by, and no two responses share one. Its document lists every schema of `components/schemas/`, and the client's discovery document's (`discovery/`); a component schema refers only to component schemas, and so does every route of the gateway's. `src/control/` is the launcher's control API. A service's own schemas, such as its configuration document and its health answer, are in its area: `archivist/` (beside Archivist's own document), `dispatcher/`, `gateway/`, `librarian/`, `smelter/`, `weaver/` and `worker/`, and the stores' and the embedding model's in `graph/`, `vectors/` and `embedding/`. A component schema that only Archivist's document reaches is Archivist's, so its place is `archivist/`. A service's schema is a file at the top of its area, named in PascalCase, that is no table's schema; the reader names the services' areas (`Places.services`), reads their schemas as `Spec.services`, and refuses a service's area that holds none. Every directory of `src/`, and every file but a schema's, a response's and a `README.md`, is named in kebab case: a `README.md` is the one exception. The reader (`make reader`) holds all of this.

## Schemas and references

Every JSON Schema file states first the dialect it is written in, `"$schema": "https://json-schema.org/draft/2020-12/schema"`, and then its `$id`, its file's name under `https://xynolog.org/protocol/`: `components/schemas/JobId.json`'s is `https://xynolog.org/protocol/JobId.json`. A `$ref` names a schema by its `$id`, with a fragment when it names a member within one, and a discriminator's mapping maps each value to a schema's `$id`. So a schema keeps its name wherever its file is, and no two schema files share a name. A path item or a response is named by its path from the file that names it, as OpenAPI names one. `$id` and `$schema` appear nowhere else. A table names no schema, since its schema is found by its place beside it; a schema a table holds within it states neither; and a bundle carries neither, naming each component by `#/components/…`. The reader (`make reader`) holds all of this.

## Case tables

A file under `src/` whose name ends in `cases.json` states behavior case by case. Its `$comment` describes the rule, and its cases state it; a table names no consumer, and each consumer records which of its tests runs it. [`src/CaseTable.json`](src/CaseTable.json) is the schema of what every case table carries. Each consumer repository owns its implementation tests and uses the pinned protocol revision as input.

## Table schemas

A table's schema sits beside it, named by the table's area and its file's stem in PascalCase: `jobs/retry-cases.json`'s schema is `jobs/JobsRetryCases.json`. A case table's schema extends `CaseTable.json` through `allOf`, admits each of its properties, and states what a case is; a `why`, wherever a table's schema states one, is `CaseTable.json`'s. The reader (`reader/`, `make reader`) derives each table's typed model from its schema, decodes the table through it and validates it against its schema; a schema it can derive no model from is refused. The form it derives from: every object closed, any two objects of a union told apart by a tag or by a property one requires and the other does not state, and at most one member of each other type beside them, a value that may be null written as JSON Schema 2020-12 writes one (a type that may be null is `"type": [T, "null"]`, a `$ref` that may be null is `{ "anyOf": [{ "$ref": … }, { "type": "null" }] }`, and a `oneOf` that may be null has `{ "type": "null" }` as its last member), a value that is JSON by nature marked so in its `$comment`, a value that is a JSON Schema a `$ref` to [`src/JsonSchema.json`](src/JsonSchema.json), which the reader reads as a schema (an annotation, a `$comment`, `title` or `description`, may sit beside a table's `$ref`), and each member of a union that no tag tells apart given a `title`, which names its constructor. Every table has its schema: a table is a JSON file at the top of an area of `src/`, named in kebab case, but an area's `openapi.json`, and a schema that extends `CaseTable.json` or states its `$comment` has its table beside it.

Cases that require live processes belong in [`../conformance/`](../conformance/README.md). The conformance suites are black-box and obtain build and launch commands from a consumer-provided manifest. The SDK and worker suites' cases, `conformance/sdk/wire`, `conformance/sdk/live` and `conformance/worker/cases`, are one file each, and [`src/conformance/`](src/conformance/) states their format (`ConformanceCase.json`, whose steps are `ConformanceStep.json`'s) and every line a driver writes (`ConformanceDriverLine.json`). The reader reads each case through its model, validates it against its schema, and holds each operation it names to the gateway's document and each channel and request to the bus registry.

## Ownership and consumers

The [`contract/`](../contract/README.md) directory gives the normative prose. SDK type generators, service implementations, CLI configuration handling, and Desktop integrations stay in their respective repositories ([the root README](../README.md#what-is-here)).

Consumers pin this repository as a Git submodule at `protocol/`. Do not fetch `main` implicitly in a build. Update the pin deliberately, regenerate consumer-owned outputs, and run that consumer's checks.
