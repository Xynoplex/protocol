#!/usr/bin/env node
// lint:spec-registry-prose — the prose specs/src/bus/registry.json states.
//
// The registry's prose is read by every SDK's generator, and each lays it out
// for its own language: its comment markers, its indentation, its width. So
// the registry states it in no language's shape, as plain text:
//
//   - a docs entry (a channel's `docs` and `schemaDocs`, an operation's
//     `docs`) is `lead`, what is written before the entry, and `trailing`, a
//     note after it on its line or "" for none;
//   - a lead is a list of blocks: "" where the listing breaks, a `section`
//     that heads a part of it, a `comment` on what follows, or a `doc` that
//     documents the entry itself;
//   - a comment, a doc, and each of `docs.*`, `audience.doc`, `effect.doc`,
//     `kind.doc` and `inProcess.doc`, is a list of paragraphs, each a text or
//     `{ "items": [text, ...] }`;
//   - a text is one line of words: no line break, no tab, no space at either
//     end or two together (no indent, no alignment), no comment marker
//     (`//`, `/*`, `*/`, `*`, `#`, `--`), no list marker, no `{@link}`.
//
// It fails, too, on a break that leads a listing (the first channel of each
// of `channelOrder`'s lists, the first operation) or follows another.

import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const REPO = resolve(fileURLToPath(import.meta.url), '../../../..');
const registry = JSON.parse(readFileSync(resolve(REPO, 'specs/src/bus/registry.json'), 'utf8'));

const failures = [];
const fail = (at, message) => failures.push(`${at}: ${message}`);
const isObject = (value) => typeof value === 'object' && value !== null && !Array.isArray(value);
const keys = (value, at, allowed) => {
  for (const key of Object.keys(value)) if (!allowed.includes(key)) fail(at, `has \`${key}\`, which is no member here`);
  for (const key of allowed) if (!(key in value)) fail(at, `lacks \`${key}\``);
};

const MARKUP = /^(\/\/|\/\*|\*|#|--|- |\d+\. )|\*\/|\{@link/;

function text(value, at) {
  if (typeof value !== 'string' || value === '') return fail(at, 'is no text: a string of words');
  if (/[\n\r\t]/.test(value)) fail(at, 'breaks a line or holds a tab; a writer lays the text out');
  if (value !== value.trim() || value.includes('  ')) fail(at, 'has a space at an end or two together: an indent or an alignment');
  if (MARKUP.test(value)) fail(at, 'holds comment or list markup; a writer writes its own');
}

function prose(value, at) {
  if (!Array.isArray(value) || value.length === 0) return fail(at, 'is no prose: a list of paragraphs');
  value.forEach((paragraph, i) => {
    const where = `${at}/${i}`;
    if (typeof paragraph === 'string') return text(paragraph, where);
    if (!isObject(paragraph)) return fail(where, 'is neither a text nor a list of items');
    keys(paragraph, where, ['items']);
    if (!Array.isArray(paragraph.items) || paragraph.items.length === 0) return fail(`${where}/items`, 'is no list of texts');
    paragraph.items.forEach((item, j) => text(item, `${where}/items/${j}`));
  });
}

const BLOCKS = { section: text, comment: prose, doc: prose };

function docs(value, at, first) {
  if (!isObject(value)) return fail(at, 'is no docs entry');
  keys(value, at, ['lead', 'trailing']);
  if (value.trailing !== '') text(value.trailing, `${at}/trailing`);
  if (!Array.isArray(value.lead)) return fail(`${at}/lead`, 'is no list of blocks');
  value.lead.forEach((block, i) => {
    const where = `${at}/lead/${i}`;
    if (block === '') {
      if (i === 0 && first) fail(where, 'breaks the listing before its first entry');
      if (value.lead[i - 1] === '') fail(where, 'breaks the listing where it is already broken');
      return;
    }
    const kinds = isObject(block) ? Object.keys(block) : [];
    if (kinds.length !== 1 || !(kinds[0] in BLOCKS)) return fail(where, 'is neither "" nor one of `section`, `comment` and `doc`');
    BLOCKS[kinds[0]](block[kinds[0]], `${where}/${kinds[0]}`);
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
