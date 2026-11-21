# Xynolog Protocol

The cross-implementation contracts shared by the Xynolog SDKs, services,
CLI, Desktop app, and independent implementations.

## What is here

| Path | What it is |
| --- | --- |
| [`contract/`](contract/README.md) | Normative behavior and compatibility rules, in prose. |
| [`specs/src/`](specs/README.md) | Normative machine-readable definitions: the gateway's and Archivist's OpenAPI documents, the launcher's control API, schemas, registries, configuration formats, and language-neutral case tables. |
| [`reader/`](reader/README.md) | The reader, Protocol's one reader of itself, in OCaml: it reads `specs/src` into a typed model, holds it to its rules, and bundles the OpenAPI documents. Consumers read Protocol through it. |
| [`conformance/`](conformance/README.md) | Black-box suites that hold an implementation to Protocol, run by one OCaml program, `xynolog-conformance`, and the case files of the SDK and worker suites. |
| `build-tools/` | A Git submodule, [xynolog/build-tools](https://github.com/xynolog/build-tools): the OCaml toolchain's image, the container runtime's detection, and the scripts CI shares with the other repositories. |
| `scripts/` | `make-target.sh`, which runs each `make` target in the OCaml toolchain's container. |
| [`NOTICE`](NOTICE), [`HISTORY-MAP.md`](HISTORY-MAP.md) | Provenance and authorship, and how the history was extracted from its source. |

Only `contract/` and `specs/src/` are normative. The bundles in `specs/`
(`openapi.json`, `archivist.openapi.json`) are generated from `specs/src/`,
and Git ignores them. Implementation code, SDK generators, launch commands and
deployment configuration live in the repositories that own them: sdk, images,
cli and desktop.

## Work in this repository

Clone it with its submodule, since every `make` target needs `build-tools/`:

```sh
git clone --recurse-submodules https://github.com/xynolog/protocol.git
```

Every target runs in the OCaml toolchain's container, so the host needs only
a container runtime: `container`, `docker` or `podman`, the first that
responds, or the one `CONTAINER_RUNTIME` names.

| Target | What it does |
| --- | --- |
| `make validate` | The default, and what CI runs: everything `make reader` does, then `make bundle`. |
| `make reader` | Builds and tests the reader, reads every file of `specs/src` with it (`xynolog-protocol check`), then builds the OCaml conformance harness and runs its cases that need no store. |
| `make bundle` | Writes `specs/openapi.json` and `specs/archivist.openapi.json` (`xynolog-protocol bundle`). |
| `make help` | Lists the targets and variables. |

To change Protocol, see [`CONTRIBUTING.md`](CONTRIBUTING.md).

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

An OCaml consumer reads Protocol through the reader's library, the dune
package `xynolog-protocol`, by vendoring the pinned submodule: sdk's
generator does this with `(vendored_dirs protocol)`. The library gives each
table a typed model derived from its schema, so a consumer restates none of
Protocol's shapes.

## Implement the protocol

1. Start with [`contract/README.md`](contract/README.md) and the relevant
   service or SDK surface.
2. Generate types and validators from `specs/src/`, or read it through the
   reader; do not maintain a second authoritative copy.
3. Run the language-neutral case tables in your implementation's own tests.
4. Run the applicable black-box suite with `xynolog-conformance` and an
   implementations manifest that names your build and driver commands.

The conformance kit does not contain Xynolog's process launch commands. Each
implementation repository owns its manifest and build setup. See
[`conformance/README.md`](conformance/README.md) for the manifest and each
suite's requirements.

## CI and provenance

CI runs `make validate` and scans every lockfile for advisories nobody has
judged (`build-tools/advisories.py`). Coverage reporting is not enabled: the
executable conformance suites exercise consumer processes and external stores
supplied by each implementation, so a protocol-only coverage percentage would
not represent implementation coverage.

See [`NOTICE`](NOTICE) for source provenance and authorship.
