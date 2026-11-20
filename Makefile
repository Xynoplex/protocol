.DEFAULT_GOAL := validate

.PHONY: help install bundle lint reader validate

# `make help` prints the `##` lines below, in order (build-tools/make-help.awk).
# Every target runs in a container (scripts/make-target.sh): the reader in the
# OCaml toolchain's, the npm tasks in a Node one.
##@ Xynolog Protocol: make [target] [VARIABLE=value ...]
##@ Variables
## CONTAINER_RUNTIME=name: container|docker|podman; else the first to respond

help:
	@awk -f build-tools/make-help.awk Makefile Makefile

##@ Validate
## validate: Read and bundle the specifications with the reader, then install the protocol's npm dependencies and lint the bundles
validate:
	bash scripts/make-target.sh validate

##@ Steps
## install: Install the protocol's npm dependencies
install:
	bash scripts/make-target.sh install

## bundle: Bundle specs/src into specs/openapi.json and specs/archivist.openapi.json with the reader
bundle:
	bash scripts/make-target.sh bundle

## lint: Bundle with the reader, then apply the Redocly rules to the bundles
lint:
	bash scripts/make-target.sh lint

## reader: Build and test the reader, and read every file of the specifications with it, each table through the model its schema gives, the contract documents that name what they state, the bus registry's spelling and the ownership inventory, then build the OCaml conformance harness and run its cases that need no store
reader:
	bash scripts/make-target.sh reader
