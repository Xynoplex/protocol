/**
 * Once per run: name the specs the suite checks against — the gateway's API
 * and the Archivist's HTTP surface its stand-in Archivist is held to, as the
 * reader bundles them into `specs/` (`make bundle`, which the conformance
 * targets run first), and the services' own schemas the suites read
 * (services-document.ts), bundled here — and refuse to start when a bundle or
 * the required NATS broker is missing. Consumer commands and implementations
 * are supplied by the manifest.
 */
import { execFileSync } from 'node:child_process';
import { existsSync, mkdtempSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import type { TestProject } from 'vitest/node';
import { writeServicesDocument } from './services-document';
import type { Implementations } from './implementations';
import { REPO_ROOT } from './paths';

declare module 'vitest' {
  export interface ProvidedContext {
    /** How a gateway is started: the configuration provides it. */
    gatewayCommand: Implementations['gateway'];
    dispatcherCommand: Implementations['dispatcher'];
    specPath: string;
    archivistSpecPath: string;
    servicesSpecPath: string;
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
  const bundled = (name: string) => {
    const path = join(REPO_ROOT, 'specs', name);
    if (!existsSync(path)) throw new Error(`specs/${name} is missing. The reader writes it: make bundle.`);
    return path;
  };
  project.provide('specPath', bundled('openapi.json'));
  project.provide('archivistSpecPath', bundled('archivist.openapi.json'));
  project.provide('servicesSpecPath', bundle(writeServicesDocument(), 'services.openapi.json'));
  return () => rmSync(dir, { recursive: true, force: true });
}
