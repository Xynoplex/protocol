#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd -P)"
source "$ROOT/build-tools/container-runtime.sh"
RUNTIME="$(detect_container_runtime)"
TARGET="${1:-}"

case "$TARGET" in
  install|conformance-install|bundle|lint|conformance-typecheck|conformance-fixtures|reader|validate) ;;
  *)
    echo 'Usage: scripts/make-target.sh {install|conformance-install|bundle|lint|conformance-typecheck|conformance-fixtures|reader|validate}' >&2
    exit 2
    ;;
esac

# The reader, in the OCaml toolchain's image, on a copy of the checkout: it
# builds, its layout is ocamlformat's, its tests pass, and it reads every file
# of specs/src, each table what its schema says, the contract documents that
# name what the specs state, the bus registry's spelling, and the ownership
# inventory against what Git tracks, so the copy carries .git.
reader() {
  bash "$ROOT/build-tools/ocaml/toolchain-image.sh" "$ROOT"
  "$RUNTIME" run --rm \
    --volume "$ROOT:/source:ro" \
    --workdir /tmp \
    --memory 4g \
    xynolog-ocaml-toolchain bash -euc '
      mkdir /tmp/protocol
      tar -C /source --exclude=./node_modules --exclude=./conformance/node_modules --exclude=./_build -cf - . | tar -C /tmp/protocol -xf -
      cd /tmp/protocol
      git config --global --add safe.directory /tmp/protocol
      printf "==> Building the reader\n"
      dune build @all
      dune build @fmt
      printf "==> Testing the reader\n"
      dune test
      printf "==> Reading the specifications\n"
      dune exec -- xynolog-protocol check .'
}

if [[ "$TARGET" == reader ]]; then
  reader
  exit
fi
if [[ "$TARGET" == validate ]]; then
  reader
fi

case "$TARGET" in
  install) COMMAND='npm ci --silent --no-audit --no-fund --no-update-notifier --loglevel=error >/dev/null' ;;
  conformance-install) COMMAND='npm ci --prefix conformance --silent --no-audit --no-fund --no-update-notifier --loglevel=error >/dev/null' ;;
  bundle) COMMAND='npm run --silent specs:bundle' ;;
  lint) COMMAND='npm run --silent specs:lint' ;;
  conformance-typecheck)
    COMMAND='npm ci --prefix conformance --silent --no-audit --no-fund --no-update-notifier --loglevel=error >/dev/null && npm run --silent specs:bundle && npm run --silent typecheck --prefix conformance'
    ;;
  conformance-fixtures)
    COMMAND='npm ci --prefix conformance --silent --no-audit --no-fund --no-update-notifier --loglevel=error >/dev/null && npm run --silent specs:bundle && npm run --silent test:fixtures --prefix conformance'
    ;;
  validate)
    COMMAND='printf "==> Installing protocol dependencies\n"; npm ci --silent --no-audit --no-fund --no-update-notifier --loglevel=error >/dev/null; printf "==> Installing conformance dependencies\n"; npm ci --prefix conformance --silent --no-audit --no-fund --no-update-notifier --loglevel=error >/dev/null; printf "==> Bundling specifications\n"; npm run --silent specs:bundle; printf "==> Linting specifications\n"; npm run --silent specs:lint; printf "==> Type-checking conformance harness\n"; npm run --silent typecheck --prefix conformance; printf "==> Running conformance fixtures\n"; npm run --silent test:fixtures --prefix conformance'
    ;;
esac

"$RUNTIME" run --rm \
  --volume "$ROOT:/workspace" \
  --workdir /workspace \
  --user "$(id -u):$(id -g)" \
  --env HOME=/tmp \
  --env npm_config_cache=/tmp/npm-cache \
  node:26-bookworm bash -euc "$COMMAND"
