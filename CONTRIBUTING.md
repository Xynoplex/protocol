# Contributing

Protocol changes affect independently released consumers. Keep one authoritative definition here, update the relevant language-neutral cases with behavior changes, and link consumer implementation work to the exact protocol commit it supports.

## Change process

1. Describe the compatibility effect in the change.
2. Update normative prose and/or `specs/src/`; do not edit generated bundles.
3. Add or adjust case tables when behavior changes. A table states its cases; each consumer records which of its tests runs it.
4. Run `make validate`. The conformance integration suites require implementation manifests and isolated external services; run those from the consumer repositories that provide them.
5. Apply the Semantic Versioning rules in the root README and record breaking changes explicitly.

Consumers must pin a specific Git revision or release tag as a `protocol/` submodule. No generated output or build should silently follow `main`.
