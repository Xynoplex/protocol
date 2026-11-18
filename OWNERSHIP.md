# Ownership and extraction inventory

This repository owns compatibility contracts shared by independently released SDK, image, CLI, and Desktop repositories. The machine-readable inventory in [`OWNERSHIP-INVENTORY.csv`](OWNERSHIP-INVENTORY.csv) assigns every extracted contract, specification, conformance, and validation file.

## Boundaries

- `contract/` and `specs/src/` are normative. Changing them can affect interoperability and follows the protocol versioning rules in the root README.
- `conformance/` contains language-neutral behavior cases and a black-box harness. It does not build consumer code or require a monorepo checkout.
- `specs/*.json` bundles are generated from `specs/src/`; generated outputs are never the source of truth.
- `scripts/checks/specs/` validates protocol-owned sources only.
- SDK, image, CLI, and Desktop implementation code, package generators, launch commands, and deployment configuration stay with those repositories.

## Generator ownership

The previous monorepo `packages/codegen/groups.txt` grouped generators by historical implementation layout. Its groups are assigned by generated output owner:

| Group | Owner |
| --- | --- |
| `bus`, `typescript`, `python`, `go`, `haskell`, `ocaml`, `unicode-case`, `encoding-cases` | `xynolog/sdk` when outputs are SDK source; protocol schema authoring/bundle checks remain here. |
| `stack-go`, `mcp-tools-go`, `control`, `discovery-go` | `xynolog/cli` |
| `model-catalogue` | `xynolog/images` |
| Protocol OpenAPI bundling and lint | `xynolog/protocol` |

A generator that produces outputs in more than one repository must be split or moved to the consumer that owns those outputs; a generated file must have one authoritative generator. The `openapi-schema.mjs` helper is protocol-owned only for generating protocol bundles. If consumers need it to generate implementation types, they consume the published protocol schema as input and own their generation invocation.

## Provenance

The inventory documents the current ownership boundary. See [`NOTICE`](NOTICE) for the pre-2026-08-27 source provenance and post-boundary Xynolog ownership statement. The extracted Git history was reconstructed as a single cutoff snapshot followed by applicable post-cutoff commits; rewritten commits are signed and retain their original authorship and messages.
