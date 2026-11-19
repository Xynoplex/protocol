#!/usr/bin/env node
// lint:spec-offset-units — the spec says what an offset into a text counts.
//
// A text offset counts Unicode code points from the start of the text, and so
// does every length worked out from one: the W3C Web Annotation rule, and the
// same number in every language. specs/src/text/offset-cases.json holds the
// counting itself, and each implementation runs it. This gate holds the
// wording: a member that carries an offset or a length of text and does not
// say its unit leaves a reader to assume their own language's, and a
// JavaScript string, a Rust `str` and a Python `str` each count differently.
//
// It fails when:
//   - a member listed below is gone, or its description does not say
//     "code points". These are the members that carry an offset or a length of
//     text on the wire, in the record, in a store or in what a client hands
//     its caller;
//   - any description of a component schema, a client type or a service's
//     schema (one at the top of an area that is no table's) speaks of an
//     offset, of characters, or of code units, and does not say "code points". A member
//     added beside the listed ones is held by this, whatever it is called;
//   - a table's schema, named here as reader/schema/places.ml names it, is not
//     there: this check's copy of that rule no longer agrees with the reader's;
//   - a case table listed below, whose cases count offsets or lengths of text,
//     has no character outside the Basic Multilingual Plane: only such a
//     character tells a count of code points from one of UTF-16 code units or
//     of bytes.
//
// It reads the source files, so it cannot pass on a stale bundle.

import { existsSync, readdirSync, readFileSync, statSync } from 'node:fs';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const REPO = resolve(fileURLToPath(import.meta.url), '../../../..');
const SPECS = 'specs/src';
const AREAS = readdirSync(resolve(REPO, SPECS))
  .filter((name) => name !== 'components' && name !== 'paths' && statSync(resolve(REPO, SPECS, name)).isDirectory())
  .map((name) => `${SPECS}/${name}`);
const DIRECTORIES = [`${SPECS}/components/schemas`, `${SPECS}/client/types`, ...AREAS];
const pascal = (words) => words.split('-').map((word) => word[0].toUpperCase() + word.slice(1)).join('');
/** The schemas of an area's tables, named as reader/schema/places.ml names them: jobs/retry-cases.json's is JobsRetryCases.json. */
const tableSchemas = new Set(
  AREAS.flatMap((area) => {
    const name = area.slice(`${SPECS}/`.length);
    return readdirSync(resolve(REPO, area))
      .filter((file) => /^[a-z][a-z0-9-]*\.json$/.test(file) && file !== 'openapi.json')
      .map((file) => `${area}/${pascal(name)}${pascal(file.slice(0, -'.json'.length))}.json`);
  }),
);

/** The members that carry an offset into a text, or a length of one: schema, then the path to the member. */
const MEMBERS = [
  ['TextPositionSelector', ['properties', 'start']],
  ['TextPositionSelector', ['properties', 'end']],
  ['PdfTextItem', ['properties', 'start']],
  ['PdfTextItem', ['properties', 'end']],
  ['AnchoredTextExtractedEntry', ['properties', 'lines', 'items', 'properties', 'words']],
  ['UnitCursor', ['properties', 'next']],
  ['GatherAnnotationOptions', ['properties', 'contextWindow']],
  ['TextSpan', ['allOf', 0, 'properties', 'start']],
  ['TextSpan', ['allOf', 0, 'properties', 'end']],
];

/** The case tables that count offsets or lengths of text, from specs/src. */
const COUNTING_TABLES = [
  'text/offset-cases.json', 'text/chunk-cases.json', 'annotations/reconcile-cases.json',
  'annotations/pdf-locate-cases.json', 'annotations/builder-cases.json', 'annotations/id-cases.json',
  'worker/chunk-plan-cases.json', 'worker/parser-cases.json', 'worker/citation-cases.json',
  'librarian/window-cases.json',
];

const SAYS_THE_UNIT = /code points?/i;
const SPEAKS_OF_A_COUNT_OF_TEXT = /\boffsets?\b|\bcharacters?\b|\bchars?\b|utf-16|\bcode units?\b/i;

const failures = [];
for (const schema of tableSchemas) {
  if (!existsSync(resolve(REPO, schema))) failures.push(`${schema} is not there: this check names a table's schema as reader/schema/places.ml does, and the two no longer agree`);
}
/** Every schema by its file, with its name. Two files may share a name (stack/'s and components/' KnowledgeBase). */
const schemas = new Map(
  DIRECTORIES.flatMap((directory) =>
    readdirSync(resolve(REPO, directory))
      .filter((name) => /^[A-Z].*\.json$/.test(name) && !tableSchemas.has(`${directory}/${name}`))
      .map((name) => [
        `${directory}/${name}`,
        { name: name.slice(0, -'.json'.length), document: JSON.parse(readFileSync(resolve(REPO, directory, name), 'utf8')) },
      ]),
  ),
);
/** The one file of the schema a member below names. */
const named = (schema) => {
  const files = [...schemas].filter(([, { name }]) => name === schema).map(([file]) => file);
  if (files.length > 1) failures.push(`${schema} is listed below, and names ${files.length} files: ${files.join(', ')}`);
  return files[0];
};

/** The listed members, as the sweep names a place: it leaves them to the check above. */
const listed = new Set(MEMBERS.map(([schema, path]) => `${named(schema)} #/${path.join('/')}`));

for (const [schema, path] of MEMBERS) {
  const file = named(schema);
  const at = `${file ?? schema} #/${path.join('/')}`;
  const member = path.reduce((node, name) => node?.[name], schemas.get(file)?.document);
  if (member === undefined) {
    failures.push(`${at} is listed as carrying an offset or a length of text, and is not there`);
  } else if (typeof member.description !== 'string' || !SAYS_THE_UNIT.test(member.description)) {
    failures.push(`${at} carries an offset or a length of text, and its description does not say that it counts code points`);
  }
}

function sweep(node, pointer, file) {
  if (Array.isArray(node)) {
    node.forEach((member, index) => sweep(member, `${pointer}/${index}`, file));
    return;
  }
  if (node === null || typeof node !== 'object') return;
  const { description } = node;
  const speaks = typeof description === 'string' && SPEAKS_OF_A_COUNT_OF_TEXT.test(description) && !SAYS_THE_UNIT.test(description);
  if (speaks && !listed.has(`${file} ${pointer}`)) {
    failures.push(
      `${file} ${pointer} speaks of an offset or of characters and does not say "code points": ${JSON.stringify(description.slice(0, 90))}`,
    );
  }
  for (const [name, member] of Object.entries(node)) sweep(member, `${pointer}/${name}`, file);
}
for (const [file, { document }] of schemas) sweep(document, '#', file);

for (const table of COUNTING_TABLES) {
  if (!/[\u{10000}-\u{10FFFF}]/u.test(readFileSync(resolve(REPO, 'specs/src', table), 'utf8'))) {
    failures.push(`specs/src/${table} counts offsets or lengths of text and has no character outside the Basic Multilingual Plane to tell code points from UTF-16 code units or bytes`);
  }
}

if (failures.length > 0) {
  for (const message of failures) console.error(`✗ ${message}`);
  process.exit(1);
}
console.log(`✓ ${MEMBERS.length} members that carry an offset or a length of text say that they count code points, no description of ${schemas.size} schemas leaves the unit out, and ${COUNTING_TABLES.length} tables that count text have a character outside the Basic Multilingual Plane`);
