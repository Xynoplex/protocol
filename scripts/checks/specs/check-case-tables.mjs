#!/usr/bin/env node
// Validate protocol case-table structure without assuming consumer repositories
// share a checkout or directory layout. A case table that has its schema is
// check-tables.mjs's, which holds it to the schema; this holds the others.
import { existsSync, readdirSync, readFileSync, statSync } from 'node:fs';
import { join, relative, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { hasSchema } from './tables.mjs';

const REPO = resolve(fileURLToPath(import.meta.url), '../../../..');
const SPECS = resolve(REPO, 'specs/src');
const failures = [];
const files = [];
let held = 0;
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
  if (hasSchema(relativePath)) continue;
  held++;
  let table;
  try { table = JSON.parse(readFileSync(file, 'utf8')); }
  catch (error) { failures.push(`${relativePath} is not valid JSON: ${error.message}`); continue; }
  if (typeof table.$comment !== 'string' || !table.$comment.trim()) failures.push(`${relativePath} must explain its normative purpose in $comment`);
  if ('cases' in table && (!Array.isArray(table.cases) || table.cases.length === 0)) failures.push(`${relativePath} must contain at least one case`);
}
if (failures.length) {
  for (const failure of failures) console.error(`✗ ${failure}`);
  process.exit(1);
}
console.log(`✓ ${held} protocol case tables without a schema have descriptions and cases`);
