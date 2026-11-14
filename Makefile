.PHONY: install conformance-install bundle lint check conformance-typecheck conformance-fixtures validate

.DEFAULT_GOAL := validate

install:
	bash scripts/make-target.sh install

conformance-install:
	bash scripts/make-target.sh conformance-install

bundle:
	bash scripts/make-target.sh bundle

lint:
	bash scripts/make-target.sh lint

check:
	bash scripts/make-target.sh check

conformance-typecheck:
	bash scripts/make-target.sh conformance-typecheck

conformance-fixtures:
	bash scripts/make-target.sh conformance-fixtures

validate:
	bash scripts/make-target.sh validate
