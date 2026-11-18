# Specifications

`src/` is the machine-readable protocol source of truth. It contains the gateway and Archivist OpenAPI documents, shared schemas and registries, configuration and identity formats, and language-neutral behavior case tables.

## Build the bundles

The OpenAPI bundle files are generated and ignored by Git. From the repository root:

```sh
make install
make bundle
```

`specs/openapi.json` and `specs/archivist.openapi.json` are outputs for type generation and conformance runs. Edit only files under `src/`. `make lint` applies the Redocly rules; `make check` runs protocol-specific consistency gates, including case-table structure.

## Case tables

A file under `src/` whose name ends in `cases.json` states behavior case by case. Its `$comment` describes the rule; `runBy` names consumer-owned test locations, while `absent` and `notYetReached` explain SDK coverage gaps. [`src/CaseTable.json`](src/CaseTable.json) is the schema of what every case table carries. Each consumer repository owns its implementation tests and uses the pinned protocol revision as input.

## Table schemas

A table's schema sits beside it, named by the table's area and its file's stem in PascalCase: `jobs/retry-cases.json`'s schema is `jobs/JobsRetryCases.json`. A case table's schema extends `CaseTable.json` through `allOf`, admits each of its properties, and states what a case is; a `why`, wherever a table's schema states one, is `CaseTable.json`'s. `scripts/checks/specs/check-tables.mjs` validates each table against its schema, and holds each schema to a form a reader can derive a typed model from: every object closed, any two members of a union told apart by a tag or by a property one requires and the other does not state, a value that may be null marked `"nullable": true` as the components mark one (a `$ref` that may be null is `{ "nullable": true, "allOf": [{ "$ref": … }] }`), and a value that is JSON by nature marked so in its `$comment`. The case tables that have no schema yet are held to what `CaseTable.json` states by `scripts/checks/specs/check-case-tables.mjs`.

Cases that require live processes belong in [`../conformance/`](../conformance/README.md). The conformance suites are black-box and obtain build and launch commands from a consumer-provided manifest.

## Ownership and consumers

The [`contract/`](../contract/README.md) directory gives the normative prose. SDK type generators, service implementations, CLI configuration handling, and Desktop integrations stay in their respective repositories. See [`../OWNERSHIP.md`](../OWNERSHIP.md) for the complete boundary and generator ownership map.

Consumers pin this repository as a Git submodule at `protocol/`. Do not fetch `main` implicitly in a build. Update the pin deliberately, regenerate consumer-owned outputs, and run that consumer's checks.
