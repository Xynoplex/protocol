#!/usr/bin/env node
// Validate protocol case-table structure without assuming consumer repositories
// share a checkout or directory layout.
import { existsSync, readdirSync, readFileSync, statSync } from 'node:fs';
import { join, relative, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const REPO = resolve(fileURLToPath(import.meta.url), '../../../..');
const SPECS = resolve(REPO, 'specs/src');
const COUNTED_OFFSETS = new Set([
  'text/offset-cases.json', 'text/chunk-cases.json', 'annotations/reconcile-cases.json',
  'annotations/pdf-locate-cases.json', 'annotations/builder-cases.json', 'annotations/id-cases.json',
  'worker/chunk-plan-cases.json', 'worker/parser-cases.json', 'worker/citation-cases.json',
  'librarian/window-cases.json',
]);
const failures = [];
const files = [];
function walk(directory) {
  for (const name of readdirSync(directory)) {
    if (name.startsWith('.') || ['node_modules', 'target', 'build', 'bin', '_build'].includes(name)) continue;
    const path = join(directory, name);
    if (statSync(path).isDirectory()) walk(path);
    else if (name.endsWith('cases.json')) files.push(path);
  }
}
if (existsSync(SPECS)) walk(SPECS);
for (const file of files) {
  const relativePath = relative(SPECS, file).split('\\').join('/');
  let table;
  try { table = JSON.parse(readFileSync(file, 'utf8')); }
  catch (error) { failures.push(`${relativePath} is not valid JSON: ${error.message}`); continue; }
  if (typeof table.$comment !== 'string' || !table.$comment.trim()) failures.push(`${relativePath} must explain its normative purpose in $comment`);
  if ('cases' in table && (!Array.isArray(table.cases) || table.cases.length === 0)) failures.push(`${relativePath} must contain at least one case`);
  if (!table.runBy || typeof table.runBy !== 'object' || Array.isArray(table.runBy) || Object.keys(table.runBy).length === 0) {
    failures.push(`${relativePath} must name at least one runner in runBy`);
  } else {
    for (const [runner, sources] of Object.entries(table.runBy)) {
      if (!Array.isArray(sources) || sources.length === 0 || sources.some((source) => typeof source !== 'string' || !source.trim())) {
        failures.push(`${relativePath}: runBy.${runner} must name one or more consumer-owned test/source locations`);
      }
    }
  }
  for (const field of ['absent', 'notYetReached']) {
    if (table[field] === undefined) continue;
    if (table[field] === null || typeof table[field] !== 'object' || Array.isArray(table[field])) {
      failures.push(`${relativePath}: ${field} must map implementation names to explanations`);
      continue;
    }
    for (const [implementation, explanation] of Object.entries(table[field])) {
      if (typeof explanation !== 'string' || !explanation.trim()) failures.push(`${relativePath}: ${field}.${implementation} must explain its status`);
    }
  }
  const stated = new Set(Object.keys(table.runBy ?? {}));
  for (const field of ['absent', 'notYetReached']) for (const implementation of Object.keys(table[field] ?? {})) {
    if (stated.has(implementation)) failures.push(`${relativePath}: ${implementation} cannot be both runBy and ${field}`);
    stated.add(implementation);
  }
  if (COUNTED_OFFSETS.has(relativePath) && !/[\u{10000}-\u{10FFFF}]/u.test(readFileSync(file, 'utf8'))) {
    failures.push(`${relativePath} states an offset/count rule but has no non-BMP example to distinguish code points from UTF-16 units or bytes`);
  }
}
if (failures.length) {
  for (const failure of failures) console.error(`✗ ${failure}`);
  process.exit(1);
}
console.log(`✓ ${files.length} protocol case tables have descriptions, cases, and portable runner declarations`);
