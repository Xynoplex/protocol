.DEFAULT_GOAL := validate

.PHONY: help install conformance-install bundle lint conformance-typecheck conformance-fixtures reader validate

# `make help` prints the `##` lines below, in order (build-tools/make-help.awk).
# Every target runs its npm task in a Node container (scripts/make-target.sh).
##@ Xynolog Protocol: make [target] [VARIABLE=value ...]
##@ Variables
## CONTAINER_RUNTIME=name: container|docker|podman; else the first to respond

help:
	@awk -f build-tools/make-help.awk Makefile Makefile

##@ Validate
## validate: Read the specifications with the reader, then install, bundle, lint, type-check the conformance harness and run its fixtures
validate:
	bash scripts/make-target.sh validate

##@ Steps
## install: Install the protocol's npm dependencies
install:
	bash scripts/make-target.sh install

## conformance-install: Install the conformance harness's npm dependencies
conformance-install:
	bash scripts/make-target.sh conformance-install

## bundle: Bundle specs/src into specs/openapi.json and specs/archivist.openapi.json
bundle:
	bash scripts/make-target.sh bundle

## lint: Apply the Redocly rules to the specifications
lint:
	bash scripts/make-target.sh lint

## conformance-typecheck: Bundle, then type-check the conformance harness
conformance-typecheck:
	bash scripts/make-target.sh conformance-typecheck

## conformance-fixtures: Bundle, then run the conformance fixtures
conformance-fixtures:
	bash scripts/make-target.sh conformance-fixtures

## reader: Build and test the reader, and read every file of the specifications with it, each table through the model its schema gives, the contract documents that name what they state, the bus registry's spelling and the ownership inventory
reader:
	bash scripts/make-target.sh reader
