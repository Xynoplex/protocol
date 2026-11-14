#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd -P)"
source "$ROOT/scripts/container-runtime.sh"
RUNTIME="$(detect_container_runtime)"
TARGET="${1:-}"

case "$TARGET" in
  install|conformance-install|bundle|lint|check|conformance-typecheck|conformance-fixtures|validate) ;;
  *)
    echo 'Usage: scripts/make-target.sh {install|conformance-install|bundle|lint|check|conformance-typecheck|conformance-fixtures|validate}' >&2
    exit 2
    ;;
esac

case "$TARGET" in
  install) COMMAND='npm ci' ;;
  conformance-install) COMMAND='npm ci --prefix conformance' ;;
  bundle) COMMAND='npm run specs:bundle' ;;
  lint) COMMAND='npm run specs:lint' ;;
  check) COMMAND='npm run specs:check' ;;
  conformance-typecheck) COMMAND='npm ci --prefix conformance && npm run specs:bundle && npm run typecheck --prefix conformance' ;;
  conformance-fixtures) COMMAND='npm ci --prefix conformance && npm run specs:bundle && npm run test:fixtures --prefix conformance' ;;
  validate)
    COMMAND='npm ci && npm ci --prefix conformance && npm run specs:bundle && npm run specs:lint && npm run specs:check && npm run typecheck --prefix conformance && npm run test:fixtures --prefix conformance'
    ;;
esac

"$RUNTIME" run --rm \
  --volume "$ROOT:/workspace" \
  --workdir /workspace \
  --user "$(id -u):$(id -g)" \
  --env HOME=/tmp \
  --env npm_config_cache=/tmp/npm-cache \
  node:26-bookworm bash -euc "$COMMAND"
