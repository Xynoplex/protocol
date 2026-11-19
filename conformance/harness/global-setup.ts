/**
 * Once per run: refuse to start when the required NATS broker is missing, then
 * bundle the specs the suite checks against (spec-setup.ts). Consumer
 * commands and implementations are supplied by the manifest.
 */
import { execFileSync } from 'node:child_process';
import type { TestProject } from 'vitest/node';
import type { Implementations } from './implementations';
import bundleSpecs from './spec-setup';

declare module 'vitest' {
  export interface ProvidedContext {
    /** How a gateway is started: the configuration provides it. */
    gatewayCommand: Implementations['gateway'];
    dispatcherCommand: Implementations['dispatcher'];
  }
}

export default function setup(project: TestProject): () => void {
  try {
    execFileSync('nats-server', ['--version'], { stdio: 'ignore' });
  } catch {
    throw new Error('nats-server is not on PATH. The NATS cases run a real broker; install nats-server 2.10 or later.');
  }
  return bundleSpecs(project);
}
