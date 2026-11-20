#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd -P)"
source "$ROOT/build-tools/container-runtime.sh"
RUNTIME="$(detect_container_runtime)"
TARGET="${1:-}"

case "$TARGET" in
  install|bundle|lint|reader|validate) ;;
  *)
    echo 'Usage: scripts/make-target.sh {install|bundle|lint|reader|validate}' >&2
    exit 2
    ;;
esac

# The reader, in the OCaml toolchain's image, on a copy of the checkout.
# "check": it builds, its layout is ocamlformat's, its tests pass, and it
# reads every file of specs/src, each table what its schema says, the
# contract documents that name what the specs state, the bus registry's
# spelling, and the ownership inventory against what Git tracks, so the copy
# carries .git; and the OCaml conformance harness, a dune project of its own
# in conformance/ocaml, builds, its layout is ocamlformat's, and its cases
# that need no store pass, the census among them. "bundle": it builds, and
# bundles each OpenAPI document of specs/src into specs/.
# Both reach the checkout as a tar stream on standard output, its progress on
# standard error, so they are the checkout's user's. "check-and-bundle": both.
reader() {
  bash "$ROOT/build-tools/ocaml/toolchain-image.sh" "$ROOT" "$ROOT/conformance/ocaml" >&2
  "$RUNTIME" run --rm \
    --volume "$ROOT:/source:ro" \
    --workdir /tmp \
    --memory 4g \
    xynolog-ocaml-toolchain bash -euc '
      exec 3>&1 1>&2
      mkdir /tmp/protocol
      tar -C /source --exclude=./node_modules --exclude=./_build --exclude=./conformance/ocaml/_build -cf - . | tar -C /tmp/protocol -xf -
      git config --global --add safe.directory /tmp/protocol
      cd /tmp/protocol
      printf "==> Building the reader\n"
      dune build @all
      if [[ "$0" != bundle ]]; then
        dune build @fmt
        printf "==> Testing the reader\n"
        dune test
        printf "==> Reading the specifications\n"
        dune exec -- xynolog-protocol check .
        printf "==> Building the conformance harness\n"
        dune build --root conformance/ocaml @all
        dune build --root conformance/ocaml @fmt
        printf "==> Testing the conformance harness\n"
        dune test --root conformance/ocaml
      fi
      if [[ "$0" != check ]]; then
        printf "==> Bundling the specifications\n"
        dune exec -- xynolog-protocol bundle .
        tar -cf - specs/openapi.json specs/archivist.openapi.json >&3
      fi' "$1"
}

case "$TARGET" in
  reader)
    reader check
    exit
    ;;
  bundle)
    reader bundle | tar -C "$ROOT" -xf -
    exit
    ;;
  validate) reader check-and-bundle | tar -C "$ROOT" -xf - ;;
  lint) reader bundle | tar -C "$ROOT" -xf - ;;
esac

case "$TARGET" in
  install) COMMAND='npm ci --silent --no-audit --no-fund --no-update-notifier --loglevel=error >/dev/null' ;;
  lint) COMMAND='npm run --silent specs:lint' ;;
  validate)
    COMMAND='printf "==> Installing protocol dependencies\n"; npm ci --silent --no-audit --no-fund --no-update-notifier --loglevel=error >/dev/null; printf "==> Linting specifications\n"; npm run --silent specs:lint'
    ;;
esac

"$RUNTIME" run --rm \
  --volume "$ROOT:/workspace" \
  --workdir /workspace \
  --user "$(id -u):$(id -g)" \
  --env HOME=/tmp \
  --env npm_config_cache=/tmp/npm-cache \
  node:26-bookworm bash -euc "$COMMAND"
