/**
 * Once per run: bundle the specs the suite checks against — the gateway's API,
 * the Archivist's HTTP surface its stand-in Archivist is held to, and the
 * schemas at the top of specs/src's areas that the suites read
 * (services-document.ts) — and provide each bundle's path.
 *
 * The bundles are made here, from `specs/src`, rather than read from the
 * gitignored bundles in `specs/`: a stale bundle would let the suite pass
 * against a spec that is not the source.
 */
import { execFileSync } from 'node:child_process';
import { mkdtempSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import type { TestProject } from 'vitest/node';
import { writeServicesDocument } from './services-document';
import { REPO_ROOT } from './paths';

declare module 'vitest' {
  export interface ProvidedContext {
    specPath: string;
    archivistSpecPath: string;
    servicesSpecPath: string;
  }
}

export default function setup(project: TestProject): () => void {
  const dir = mkdtempSync(join(tmpdir(), 'conformance-spec-'));
  const bundle = (source: string, name: string) => {
    const out = join(dir, name);
    execFileSync(join(REPO_ROOT, 'node_modules/.bin/redocly'), ['bundle', source, '-o', out], { cwd: REPO_ROOT, stdio: 'ignore' });
    return out;
  };
  project.provide('specPath', bundle('specs/src/openapi.json', 'openapi.json'));
  project.provide('archivistSpecPath', bundle('specs/src/archivist/openapi.json', 'archivist.openapi.json'));
  project.provide('servicesSpecPath', bundle(writeServicesDocument(), 'services.openapi.json'));
  return () => rmSync(dir, { recursive: true, force: true });
}
