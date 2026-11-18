// The client types' spec gate.
//
// `specs/src/client/types/` states the shapes every SDK hands its callers,
// and `specs/src/session/ClientSecretForm.json` the form a service account
// posts with a client secret. Each restates facts the spec states elsewhere,
// and the check fails when a restatement disagrees:
//
//   - telemetry's `anchor.method` values (service-telemetry/telemetry.json)
//     are anything but AnchorMethod's words;
//   - a reconciled span of annotations/reconcile-cases.json names a search
//     when its method is not `fuzzy-match`, or names none when it is. That
//     its method is an AnchorMethod and its search a MatchQuality is its
//     schema's, through ReconciledSpan, which check-tables.mjs holds it to;
//   - FrameRefusalReason names a reason no case of bus/frame-cases.json is
//     refused for. That each case is refused for one it names is the
//     table's schema's;
//   - annotations/reader-cases.json's `readers` is not, in order, what its
//     schema says a case `reads`, or names one that is neither a reader of
//     client/surface.json's annotations module nor one of its functions that
//     read an annotation (`is…`);
//   - annotations/builder-cases.json's `builders` is not the annotations
//     module's other functions;
//   - DeviceCode is anything but what control/UserAction.json carries less its
//     `requestId`, its `expiresInSeconds` named `expiresIn`;
//   - ClientSecretForm's grant is not client-assertion-cases.json's grant.
//
// It reads the source files, so it cannot pass on a stale bundle.

import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const REPO = resolve(fileURLToPath(import.meta.url), '../../../..');
const read = (path) => JSON.parse(readFileSync(resolve(REPO, path), 'utf8'));
const clientType = (name) => read(`specs/src/client/types/${name}.json`);
const same = (a, b) => JSON.stringify(a) === JSON.stringify(b);

const failures = [];
const fail = (message) => failures.push(message);

const anchorMethods = clientType('AnchorMethod').enum;

const telemetry = read('specs/src/service-telemetry/telemetry.json');
const anchorAttributes = [...telemetry.spans, ...telemetry.metrics]
  .flatMap((row) => (row.attributes ?? []).map((attribute) => ({ row: row.name, attribute })))
  .filter(({ attribute }) => attribute.key === 'anchor.method');
if (anchorAttributes.length === 0) fail('telemetry.json has no `anchor.method` attribute');
for (const { row, attribute } of anchorAttributes) {
  if (!same(attribute.values, anchorMethods)) {
    fail(`telemetry.json's ${row} lists anchor.method values ${JSON.stringify(attribute.values)}, not AnchorMethod's ${JSON.stringify(anchorMethods)}`);
  }
}

const reconcile = read('specs/src/annotations/reconcile-cases.json');
for (const [index, { reconciled }] of reconcile.cases.entries()) {
  if (reconciled === null) continue;
  const at = `reconcile-cases.json case ${index}`;
  const { anchorMethod, matchQuality } = reconciled;
  if (anchorMethod === 'fuzzy-match' && matchQuality === undefined) fail(`${at} is a fuzzy match, and names no search`);
  if (anchorMethod !== 'fuzzy-match' && matchQuality !== undefined) fail(`${at} names a search, ${JSON.stringify(matchQuality)}, and is not a fuzzy match`);
}

const refusalReasons = clientType('FrameRefusalReason').enum;
const frames = read('specs/src/bus/frame-cases.json');
const refusedFor = new Set(frames.cases.filter((c) => c.refused).map((c) => c.reason));
for (const reason of refusalReasons) {
  if (!refusedFor.has(reason)) fail(`FrameRefusalReason names ${JSON.stringify(reason)}, and no case of frame-cases.json is refused for it`);
}

const annotations = read('specs/src/client/surface.json').modules.find((module) => module.module === 'annotations');
const readerTable = read('specs/src/annotations/reader-cases.json');
const reads = Object.keys(read('specs/src/annotations/AnnotationsReaderCases.json').properties.cases.items.properties.reads.properties);
if (!same(readerTable.readers, reads)) {
  fail(`reader-cases.json's readers ${JSON.stringify(readerTable.readers)} are not, in order, what its schema says a case reads ${JSON.stringify(reads)}`);
}
// Which of the module's readers every SDK has is each SDK repository's to
// record, and so is that the table names each of those.
const surfaceReaders = new Set(annotations.members
  .filter((member) => member.kind === 'reader' || (member.kind === 'function' && /^is[A-Z]/.test(member.member)))
  .map((member) => member.member));
for (const reader of readerTable.readers) {
  if (!surfaceReaders.has(reader)) fail(`reader-cases.json names the reader ${JSON.stringify(reader)}, which client/surface.json's annotations module does not have`);
}
const builders = read('specs/src/annotations/builder-cases.json').builders;
const surfaceBuilders = annotations.members
  .filter((member) => member.kind === 'function' && !/^is[A-Z]/.test(member.member))
  .map((member) => member.member);
if (!same(builders, surfaceBuilders)) {
  fail(`builder-cases.json's builders ${JSON.stringify(builders)} are not client/surface.json's other annotations functions ${JSON.stringify(surfaceBuilders)}`);
}

const userAction = read('specs/src/control/UserAction.json');
const deviceCode = clientType('DeviceCode');
const asDeviceCode = (name) => (name === 'expiresInSeconds' ? 'expiresIn' : name);
const carried = Object.entries(userAction.properties).filter(([name]) => name !== 'requestId');
const typeOf = (member) => (member ? member.type : undefined);
const deviceMembers = Object.keys(deviceCode.properties);
const carriedMembers = carried.map(([name]) => asDeviceCode(name));
if (!same([...deviceMembers].sort(), [...carriedMembers].sort())) {
  fail(`DeviceCode's members ${JSON.stringify(deviceMembers)} are not UserAction's less requestId ${JSON.stringify(carriedMembers)}`);
}
for (const [name, member] of carried) {
  const own = deviceCode.properties[asDeviceCode(name)];
  if (typeOf(own) !== typeOf(member)) fail(`DeviceCode's ${asDeviceCode(name)} is ${typeOf(own)}, and UserAction's ${name} is ${typeOf(member)}`);
}
const carriedRequired = userAction.required.filter((name) => name !== 'requestId').map(asDeviceCode).sort();
if (!same([...deviceCode.required].sort(), carriedRequired)) {
  fail(`DeviceCode requires ${JSON.stringify(deviceCode.required)}, and UserAction, less requestId, requires ${JSON.stringify(carriedRequired)}`);
}

const form = read('specs/src/session/ClientSecretForm.json');
const assertion = read('specs/src/session/client-assertion-cases.json');
if (!same(form.properties.grant_type.enum, [assertion.grantType])) {
  fail(`ClientSecretForm's grant_type is ${JSON.stringify(form.properties.grant_type.enum)}, not client-assertion-cases.json's ${JSON.stringify(assertion.grantType)}`);
}

if (failures.length > 0) {
  for (const message of failures) console.error(`✗ ${message}`);
  process.exit(1);
}
console.log(
  `✓ the client types agree with what restates them (${anchorMethods.length} anchor methods, ${reconcile.cases.length} reconcile cases, ${refusalReasons.length} refusal reasons, ${reads.length} readers, ${builders.length} builders, DeviceCode, the client secret form)`,
);
