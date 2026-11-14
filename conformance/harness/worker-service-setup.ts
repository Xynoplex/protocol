/**
 * Once per worker-service run: refuse to start when a Worker service cannot
 * run, so a missing build reads as that, never as every case failing.
 */
import type { TestProject } from 'vitest/node';
import type { Implementations } from './implementations';

declare module 'vitest' {
  export interface ProvidedContext {
    /** How each implementation of the Worker service is started: the configuration provides them. */
    workerServices: Implementations['workerServices'];
  }
}

export default function setup(project: TestProject): void {
  const services = project.getProvidedContext().workerServices;
  if (Object.keys(services).length === 0) throw new Error('The implementations file names no Worker service.');

}
