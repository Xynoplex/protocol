# Specifications

`src/` is the machine-readable protocol source of truth. It contains the gateway and Archivist OpenAPI documents, shared schemas and registries, configuration and identity formats, and language-neutral behavior case tables.

## Build the bundles

The OpenAPI bundle files are generated and ignored by Git. From the repository root:

```sh
make install
make bundle
```

`specs/openapi.json` and `specs/archivist.openapi.json` are outputs for type generation and conformance runs. Edit only files under `src/`. `make lint` applies the Redocly rules; `make check` runs protocol-specific consistency gates, including each table's schema.

## Case tables

A file under `src/` whose name ends in `cases.json` states behavior case by case. Its `$comment` describes the rule, and its cases state it; a table names no consumer, and each consumer records which of its tests runs it. [`src/CaseTable.json`](src/CaseTable.json) is the schema of what every case table carries. Each consumer repository owns its implementation tests and uses the pinned protocol revision as input.

## Table schemas

A table's schema sits beside it, named by the table's area and its file's stem in PascalCase: `jobs/retry-cases.json`'s schema is `jobs/JobsRetryCases.json`. A case table's schema extends `CaseTable.json` through `allOf`, admits each of its properties, and states what a case is; a `why`, wherever a table's schema states one, is `CaseTable.json`'s. The reader (`reader/`, `make reader`) derives each table's typed model from its schema, decodes the table through it and validates it against its schema; a schema it can derive no model from is refused. The form it derives from: every object closed, any two objects of a union told apart by a tag or by a property one requires and the other does not state, and at most one member of each other type beside them, a value that may be null marked `"nullable": true` as the components mark one (a `$ref` that may be null is `{ "nullable": true, "allOf": [{ "$ref": … }] }`, and a `oneOf` that may be null has `"nullable": true` beside it), a value that is JSON by nature marked so in its `$comment`, a value that is a JSON Schema a `$ref` to [`src/JsonSchema.json`](src/JsonSchema.json), which the reader reads as a schema (a `$comment` may sit beside a table's `$ref`, which nothing reads), and each member of a union that no tag tells apart given a `title`, which names its constructor. Every table has its schema: a table is a JSON file at the top of an area of `src/`, named in kebab case, but an area's `openapi.json`, and a schema that extends `CaseTable.json` or states its `$comment` has its table beside it.

Cases that require live processes belong in [`../conformance/`](../conformance/README.md). The conformance suites are black-box and obtain build and launch commands from a consumer-provided manifest.

## Ownership and consumers

The [`contract/`](../contract/README.md) directory gives the normative prose. SDK type generators, service implementations, CLI configuration handling, and Desktop integrations stay in their respective repositories. See [`../OWNERSHIP.md`](../OWNERSHIP.md) for the complete boundary and generator ownership map.

Consumers pin this repository as a Git submodule at `protocol/`. Do not fetch `main` implicitly in a build. Update the pin deliberately, regenerate consumer-owned outputs, and run that consumer's checks.
