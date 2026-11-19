// The services' schemas the suites read, as an OpenAPI document of their own.
//
// A service's schemas are in its area of specs/src, and neither the gateway's
// document nor Archivist's lists them, so neither bundle has them. This writes
// harness/generated/services.openapi.json, which lists each schema
// harness/service-schemas.json names, found by its name at the top of an area.
// It refuses a name no area has, and a name two areas have.

import { mkdirSync, readFileSync, readdirSync, statSync, writeFileSync } from 'node:fs';
import { dirname, relative, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const CONFORMANCE = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const SPECS = resolve(CONFORMANCE, '../specs/src');
export const SERVICES_DOCUMENT = resolve(CONFORMANCE, 'harness/generated/services.openapi.json');

export function writeServicesDocument(): string {
  const { schemas: names } = JSON.parse(readFileSync(resolve(CONFORMANCE, 'harness/service-schemas.json'), 'utf8')) as { schemas: string[] };
  const areas = readdirSync(SPECS).filter(
    (name) => name !== 'components' && name !== 'paths' && statSync(resolve(SPECS, name)).isDirectory(),
  );
  const refusals: string[] = [];
  const schemas: Record<string, { $ref: string }> = {};
  for (const name of names) {
    const found = areas.filter((area) => readdirSync(resolve(SPECS, area)).includes(`${name}.json`));
    if (found.length !== 1) {
      refusals.push(`${name} is at the top of ${found.length} areas of specs/src (${found.join(', ') || 'none'}), not one`);
      continue;
    }
    schemas[name] = { $ref: relative(dirname(SERVICES_DOCUMENT), resolve(SPECS, found[0]!, `${name}.json`)) };
  }
  if (refusals.length > 0) throw new Error(`harness/service-schemas.json:\n${refusals.map((r) => `  ${r}`).join('\n')}`);
  const document = {
    openapi: '3.0.0',
    info: { title: "The services' own schemas the conformance suites read", version: '0.0.0' },
    paths: {},
    components: { schemas },
  };
  mkdirSync(dirname(SERVICES_DOCUMENT), { recursive: true });
  writeFileSync(SERVICES_DOCUMENT, `${JSON.stringify(document, null, 2)}\n`);
  return SERVICES_DOCUMENT;
}

if (process.argv[1] === fileURLToPath(import.meta.url)) writeServicesDocument();
