.DEFAULT_GOAL := validate

.PHONY: help bundle reader validate

# `make help` prints the `##` lines below, in order (build-tools/make-help.awk).
# Every target runs the reader in the OCaml toolchain's container
# (scripts/make-target.sh).
##@ Xynolog Protocol: make [target] [VARIABLE=value ...]
##@ Variables
## CONTAINER_RUNTIME=name: container|docker|podman; else the first to respond

help:
	@awk -f build-tools/make-help.awk Makefile Makefile

##@ Validate
## validate: Everything reader does, then bundle specs/src into specs/openapi.json and specs/archivist.openapi.json
validate:
	bash scripts/make-target.sh validate

##@ Steps
## bundle: Bundle specs/src into specs/openapi.json and specs/archivist.openapi.json with the reader
bundle:
	bash scripts/make-target.sh bundle

## reader: Build and test the reader, and read every file of the specifications with it, each table through the model its schema gives, the contract documents that name what they state, the bus registry's spelling and the ownership inventory, then build the OCaml conformance harness and run its cases that need no store
reader:
	bash scripts/make-target.sh reader
