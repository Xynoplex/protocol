#!/usr/bin/env node
// lint:spec-registry-form — how specs/src/bus/registry.json is spelled.
//
// JSON has more than one faithful spelling of one document, and the tools
// that edit this file disagree: `JSON.stringify` writes an em-dash as the
// character, Python's `json.dumps` as the six bytes `—`. A one-line
// change made by the other tool lands as a whole-file re-encoding, which
// conflicts with every branch touching the registry, the file branches are
// most likely to share, and hides the change under review.
//
// It fails when the file's bytes are not `JSON.stringify(value, null, 2)` and
// a newline. `--fix` rewrites the file in that form.

import { readFileSync, writeFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const REPO = resolve(fileURLToPath(import.meta.url), '../../../..');
const FILE = 'specs/src/bus/registry.json';
const path = resolve(REPO, FILE);

const raw = readFileSync(path, 'utf8');
const canonical = `${JSON.stringify(JSON.parse(raw), null, 2)}\n`;

if (raw === canonical) {
  console.log(`lint:spec-registry-form: ${FILE} is spelled as JSON.stringify(value, null, 2) writes it.`);
} else if (process.argv.includes('--fix')) {
  writeFileSync(path, canonical);
  console.log(`lint:spec-registry-form: ${FILE} rewritten as JSON.stringify(value, null, 2) writes it.`);
} else {
  console.error(
    `lint:spec-registry-form: ${FILE} is not spelled as JSON.stringify(value, null, 2) and a newline write it.\n` +
      `  Its content may be right; its bytes (escapes, indentation, the final newline) are not, and left so it\n` +
      `  lands as a whole-file diff. Rewrite it with: node scripts/checks/specs/check-registry-form.mjs --fix`,
  );
  process.exit(1);
}
