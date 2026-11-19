// The Archivist spec gate.
//
// What the Archivist keeps and how it is started is stated in specs/ for a
// second implementation to read instead of the code. The check fails when a
// part of that statement is missing, or when two parts that restate one fact
// disagree:
//
//   - a file the record keeps has no schema, Archivist's own (specs/src/archivist)
//     or a component (specs/src/components/schemas);
//   - ArchivistConfig defaults anything, or leaves optional anything but a
//     job's or an actor's engine, which is absent when it has none;
//   - the engines' job types are not exactly JobType, or their mark
//     motivations not exactly Motivation;
//   - the anchored-text entry's provenance is not ExtractedText's, or its
//     decline classes are not ExtractionDeclined's;
//   - the shard table is missing (the reader in reader/ holds it to its schema);
//   - the protocol's account of the Archivist is missing, or does not name a
//     schema, a table, a persisted event or a route of its HTTP surface.
//
// It reads the source files, so it cannot pass on a stale bundle.

import { existsSync, readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const REPO = resolve(fileURLToPath(import.meta.url), '../../../..');
const PLACES = ['specs/src/archivist', 'specs/src/components/schemas'].map((place) => resolve(REPO, place));
const read = (path) => JSON.parse(readFileSync(path, 'utf8'));
const fileOf = (name) => PLACES.map((place) => resolve(place, `${name}.json`)).find((file) => existsSync(file));
const schema = (name) => read(fileOf(name));

const failures = [];
const fail = (message) => failures.push(message);
const same = (a, b) => JSON.stringify(a) === JSON.stringify(b);
/** A node read from [file], each reference in it naming the file it resolves to: two schemas in different directories that name one schema say the same. */
const anchored = (node, file) => {
  if (Array.isArray(node)) return node.map((member) => anchored(member, file));
  if (node === null || typeof node !== 'object') return node;
  return Object.fromEntries(
    Object.entries(node).map(([key, value]) => [key, key === '$ref' ? resolve(file, '..', value) : anchored(value, file)]),
  );
};

const RECORD_SCHEMAS = [
  'ResourceView',
  'ResourceAnnotations',
  'EntityTypesProjection',
  'TagSchemasProjection',
  'ProfilesProjection',
  'StorageUriEntry',
  'AnchoredTextEntry',
  'AnchoredTextExtractedEntry',
  'AnchoredTextDeclinedEntry',
];
const CONFIG_SCHEMAS = ['ArchivistConfig', 'ArchivistEngines', 'Engine'];
const TABLES = ['specs/src/archivist/shard-cases.json'];
const PROTOCOL = 'contract/ARCHIVIST.md';

const present = [];
for (const name of [...RECORD_SCHEMAS, ...CONFIG_SCHEMAS]) {
  if (fileOf(name) === undefined) fail(`${name} has no schema in specs/src/archivist or specs/src/components/schemas`);
  else present.push(name);
}
const has = (name) => present.includes(name);

if (has('ArchivistConfig') && has('ArchivistEngines')) {
  const everythingStated = (node, path, optionalAllowed) => {
    if ('default' in node) fail(`ArchivistConfig defaults ${path}`);
    if (node.type !== 'object') return;
    for (const [name, property] of Object.entries(node.properties ?? {})) {
      const at = path ? `${path}.${name}` : name;
      if (!node.required?.includes(name) && !optionalAllowed) fail(`ArchivistConfig leaves ${at} optional`);
      const resolved = property.$ref === './ArchivistEngines.json' ? schema('ArchivistEngines') : property;
      everythingStated(resolved, at, at === 'engines.jobs' || at === 'engines.jobs.mark' || at === 'engines.actors');
    }
  };
  everythingStated(schema('ArchivistConfig'), '', false);

  const engineJobTypes = Object.keys(schema('ArchivistEngines').properties.jobs.properties);
  const jobTypes = schema('JobType').enum;
  if ([...engineJobTypes].sort().join() !== [...jobTypes].sort().join()) {
    fail(`the engines' job types are [${engineJobTypes}], JobType is [${jobTypes}]`);
  }
  const engineMotivations = Object.keys(schema('ArchivistEngines').properties.jobs.properties.mark.properties);
  const motivations = schema('Motivation').enum;
  if ([...engineMotivations].sort().join() !== [...motivations].sort().join()) {
    fail(`the engines' mark motivations are [${engineMotivations}], Motivation is [${motivations}]`);
  }
}

if (has('AnchoredTextEntry')) {
  const entry = fileOf('AnchoredTextEntry');
  const [extracted, declined] = read(entry).oneOf.map((member) => {
    const file = resolve(entry, '..', member.$ref);
    return anchored(read(file), file);
  });
  const provenance = Object.fromEntries(
    Object.entries(anchored(schema('ExtractedText'), fileOf('ExtractedText')).allOf[1].properties).filter(([name]) => name !== 'kind'),
  );
  for (const [name, property] of Object.entries(provenance)) {
    if (!same(extracted.properties[name], property)) fail(`AnchoredTextEntry's ${name} is not ExtractedText's`);
  }
  if (!same(declined.properties.declined, anchored(schema('ExtractionDeclined'), fileOf('ExtractionDeclined')).properties.declined)) {
    fail(`AnchoredTextEntry's declined is not ExtractionDeclined's`);
  }
}

for (const path of TABLES) if (!existsSync(resolve(REPO, path))) fail(`${path} is missing`);

if (!existsSync(resolve(REPO, PROTOCOL))) {
  fail(`${PROTOCOL} is missing`);
} else {
  const protocol = readFileSync(resolve(REPO, PROTOCOL), 'utf8');
  const named = (what, term) => { if (!protocol.includes(term)) fail(`${PROTOCOL} does not name ${what} ${term}`); };
  for (const name of [...RECORD_SCHEMAS.filter((name) => !/^AnchoredText.+Entry$/.test(name)), 'ArchivistConfig']) named('the schema', name);
  for (const path of TABLES) named('the table', path.split('/').pop());
  const registry = read(resolve(REPO, 'specs/src/bus/registry.json'));
  const persisted = registry.channels
    .filter((channel) => channel.shape === 'storedEvent')
    .map((channel) => channel.channel);
  if (persisted.length === 0) fail('the bus registry names no persisted event: the check has lost what it reads');
  for (const type of persisted) named('the persisted event', `\`${type}\``);
  const surface = read(resolve(REPO, 'specs/src/archivist/openapi.json'));
  for (const path of Object.keys(surface.paths)) named('the route', `\`${path}\``);
}

if (failures.length > 0) {
  for (const message of failures) console.error(`✗ ${message}`);
  process.exit(1);
}
console.log(`✓ the Archivist is specified (${RECORD_SCHEMAS.length} file-format schemas, its configuration document, the shared table, ${PROTOCOL})`);
