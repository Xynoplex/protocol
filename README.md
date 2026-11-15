# Xynolog Protocol

The cross-implementation contracts shared by the Xynolog SDKs, services,
CLI, Desktop app, and independent implementations.

| Directory | Contents |
| --- | --- |
| [`contract/`](contract/README.md) | Normative behavior and compatibility rules in prose. |
| [`specs/`](specs/README.md) | OpenAPI definitions, registries, schemas, configuration formats, and language-neutral case tables. |
| [`conformance/`](conformance/README.md) | Black-box suites for implementations that provide their own launch commands and SDK drivers. |

## Use this repository

Consumers should pin a specific protocol revision. Do not build against the
moving `main` branch. The canonical consumer path is a Git submodule named
`protocol`, pinned to the commit that the consumer supports:

```sh
git submodule add https://github.com/xynolog/protocol.git protocol
git submodule update --init --recursive
```

The selected revision is part of the consumer's compatibility contract. Update
it deliberately, regenerate any owned output, and run that consumer's checks.

## Implement the protocol

1. Start with [`contract/README.md`](contract/README.md) and the relevant
   service or SDK surface.
2. Generate types and validators from `specs/src/`; do not maintain a second
   authoritative copy.
3. Run the language-neutral case tables in your implementation's own tests.
4. Run the applicable black-box suite in `conformance/` with an
   implementations manifest that names your build and driver commands.

The conformance kit does not contain Xynolog's process launch commands. Each
implementation repository owns its manifest and build setup. See
[`conformance/README.md`](conformance/README.md) for its schema and suite
requirements.

## Versioning

Protocol releases use Semantic Versioning. A patch release clarifies or fixes a
definition without changing the behavior a conforming implementation must
provide. A minor release adds an optional capability or case while preserving
existing behavior. A major release changes a required wire shape, meaning, or
compatibility guarantee. A case-table addition that exposes a new requirement
is a compatibility change and must be described in the release notes.

Consumers pin the exact commit (or its release tag) they have implemented.
The Git revision remains authoritative if a release tag and a consumer pin
ever disagree.

## Source and compatibility

The repository name and documentation use Xynolog. Historical wire
identifiers such as `xynolog` remain where changing them would break deployed
clients or persisted data. Such values are protocol compatibility details,
not a claim about current product branding.

Coverage reporting is not enabled in protocol CI: the executable conformance suites exercise consumer processes and external stores supplied by each implementation, so a protocol-only coverage percentage would not represent implementation coverage. The CI workflow validates authored specs, generated bundles, and harness types.

See [`NOTICE`](NOTICE) for source provenance and the ownership boundary.
