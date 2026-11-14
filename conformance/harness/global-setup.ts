/**
 * Once per run: bundle the specs the suite checks against — the gateway's API,
 * and the Archivist's HTTP surface its stand-in Archivist is held to — and
 * refuse to start when the required NATS broker is missing. Consumer commands
 * and implementations are supplied by the manifest.
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
import type { Implementations } from './implementations';
import { REPO_ROOT } from './paths';

declare module 'vitest' {
  export interface ProvidedContext {
    /** How a gateway is started: the configuration provides it. */
    gatewayCommand: Implementations['gateway'];
    dispatcherCommand: Implementations['dispatcher'];
    specPath: string;
    archivistSpecPath: string;
  }
}

export default function setup(project: TestProject): () => void {
  try {
    execFileSync('nats-server', ['--version'], { stdio: 'ignore' });
  } catch {
    throw new Error('nats-server is not on PATH. The NATS cases run a real broker; install nats-server 2.10 or later.');
  }

  const dir = mkdtempSync(join(tmpdir(), 'conformance-spec-'));
  const bundle = (source: string, name: string) => {
    const out = join(dir, name);
    execFileSync(join(REPO_ROOT, 'node_modules/.bin/redocly'), ['bundle', source, '-o', out], { cwd: REPO_ROOT, stdio: 'ignore' });
    return out;
  };
  project.provide('specPath', bundle('specs/src/openapi.json', 'openapi.json'));
  project.provide('archivistSpecPath', bundle('specs/src/archivist/openapi.json', 'archivist.openapi.json'));
  return () => rmSync(dir, { recursive: true, force: true });
}
