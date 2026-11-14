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

A file under `src/` whose name ends in `cases.json` states behavior case by case. Its `$comment` describes the rule; `runBy` names consumer-owned test locations, while `absent` and `notYetReached` explain SDK coverage gaps. The protocol repository checks the table's shape. Each consumer repository owns its implementation tests and uses the pinned protocol revision as input.

Cases that require live processes belong in [`../conformance/`](../conformance/README.md). The conformance suites are black-box and obtain build and launch commands from a consumer-provided manifest.

## Ownership and consumers

The [`contract/`](../contract/README.md) directory gives the normative prose. SDK type generators, service implementations, CLI configuration handling, and Desktop integrations stay in their respective repositories. See [`../OWNERSHIP.md`](../OWNERSHIP.md) for the complete boundary and generator ownership map.

Consumers pin this repository as a Git submodule at `protocol/`. Do not fetch `main` implicitly in a build. Update the pin deliberately, regenerate consumer-owned outputs, and run that consumer's checks.
