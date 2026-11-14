.PHONY: install conformance-install bundle lint check conformance-typecheck validate

install:
	npm ci

conformance-install:
	npm ci --prefix conformance

bundle:
	npm run specs:bundle

lint:
	npm run specs:lint

check:
	npm run specs:check

conformance-typecheck: conformance-install bundle
	npm run typecheck --prefix conformance

validate: install bundle lint check conformance-typecheck
