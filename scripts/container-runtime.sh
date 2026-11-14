#!/usr/bin/env bash

runtime_ready() {
  local runtime="$1"
  if [[ "$runtime" == container ]]; then
    "$runtime" system status >/dev/null 2>&1
  else
    "$runtime" info >/dev/null 2>&1
  fi
}

detect_container_runtime() {
  local candidate
  if [[ -n "${CONTAINER_RUNTIME:-}" ]]; then
    if ! command -v "$CONTAINER_RUNTIME" >/dev/null 2>&1; then
      echo "Selected runtime '$CONTAINER_RUNTIME' is not installed or not on PATH." >&2
      return 1
    fi
    if ! runtime_ready "$CONTAINER_RUNTIME"; then
      echo "Selected runtime '$CONTAINER_RUNTIME' is not responding." >&2
      case "$CONTAINER_RUNTIME" in
        container) echo 'Start it with: container system start' >&2 ;;
        docker)
          if [[ "$(uname -s)" == Darwin ]]; then echo 'Start Docker Desktop with: open -a Docker' >&2
          else echo 'Start Docker Engine or check the selected Docker context.' >&2; fi ;;
        podman)
          if [[ "$(uname -s)" == Darwin ]]; then echo 'Start its VM with: podman machine start' >&2
          else echo 'Check the selected Podman connection with: podman info' >&2; fi ;;
      esac
      return 1
    fi
    printf '%s\n' "$CONTAINER_RUNTIME"
    return
  fi

  for candidate in container docker podman; do
    if command -v "$candidate" >/dev/null 2>&1 && runtime_ready "$candidate"; then
      printf '%s\n' "$candidate"
      return
    fi
  done
  echo 'Install Apple Container, Docker, or Podman, or set CONTAINER_RUNTIME to its executable.' >&2
  return 1
}
