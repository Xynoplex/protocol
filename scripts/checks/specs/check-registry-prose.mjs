#!/usr/bin/env node
// lint:spec-registry-prose — the prose specs/src/bus/registry.json states.
//
// The registry's prose is read by every SDK's generator, and each lays it out
// for its own language: its comment markers, its indentation, its width. So
// the registry states it in no language's shape, as plain text. Its shape (a
// docs entry's `lead` of blocks and its `trailing` note, prose as paragraphs,
// each a text or `{ "items": [text, ...] }`) is its schema's
// (bus/BusRegistry.json), which the reader in reader/ holds it to. This holds what
// no schema says of it:
//
//   - a text is one line of words: no line break, no tab, no space at either
//     end or two together (no indent, no alignment), no comment marker
//     (`//`, `/*`, `*/`, `*`, `#`, `--`), no list marker, no `{@link}`;
//   - a break does not lead a listing (the first channel of each of
//     `channelOrder`'s lists, the first operation), nor follow another.

import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const REPO = resolve(fileURLToPath(import.meta.url), '../../../..');
const registry = JSON.parse(readFileSync(resolve(REPO, 'specs/src/bus/registry.json'), 'utf8'));

const failures = [];
const fail = (at, message) => failures.push(`${at}: ${message}`);
const MARKUP = /^(\/\/|\/\*|\*|#|--|- |\d+\. )|\*\/|\{@link/;

function text(value, at) {
  if (/[\n\r\t]/.test(value)) fail(at, 'breaks a line or holds a tab; a writer lays the text out');
  if (value !== value.trim() || value.includes('  ')) fail(at, 'has a space at an end or two together: an indent or an alignment');
  if (MARKUP.test(value)) fail(at, 'holds comment or list markup; a writer writes its own');
}

function prose(value, at) {
  value.forEach((paragraph, i) => {
    const where = `${at}/${i}`;
    if (typeof paragraph === 'string') return text(paragraph, where);
    paragraph.items.forEach((item, j) => text(item, `${where}/items/${j}`));
  });
}

const BLOCKS = { section: text, comment: prose, doc: prose };

function docs(value, at, first) {
  if (value.trailing !== '') text(value.trailing, `${at}/trailing`);
  value.lead.forEach((block, i) => {
    const where = `${at}/lead/${i}`;
    if (block === '') {
      if (i === 0 && first) fail(where, 'breaks the listing before its first entry');
      if (value.lead[i - 1] === '') fail(where, 'breaks the listing where it is already broken');
      return;
    }
    const [kind] = Object.keys(block);
    BLOCKS[kind](block[kind], `${where}/${kind}`);
  });
}

const index = new Map(registry.channels.map((channel, i) => [channel.channel, i]));
const firstOf = (list) => list.length > 0 ? index.get(list[0]) : undefined;
const firstInEventMap = firstOf(registry.channelOrder.eventMap);
const firstInSchemas = firstOf(registry.channelOrder.schemas);

registry.channels.forEach((channel, i) => {
  docs(channel.docs, `/channels/${i}/docs`, i === firstInEventMap);
  docs(channel.schemaDocs, `/channels/${i}/schemaDocs`, i === firstInSchemas);
});
registry.operations.forEach((operation, i) => docs(operation.docs, `/operations/${i}/docs`, i === 0));
for (const [name, value] of Object.entries(registry.docs)) prose(value, `/docs/${name}`);
for (const holder of ['audience', 'effect', 'kind', 'inProcess']) prose(registry[holder].doc, `/${holder}/doc`);

if (failures.length > 0) {
  console.error(`lint:spec-registry-prose: ${failures.length} failure(s) in specs/src/bus/registry.json:\n${failures.map((line) => `  - ${line}`).join('\n')}`);
  process.exit(1);
}
console.log('lint:spec-registry-prose: the bus registry states its prose as plain text, in no language\'s shape.');
