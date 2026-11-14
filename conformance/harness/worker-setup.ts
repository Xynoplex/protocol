/** The Worker suite runs only drivers supplied by the implementation manifest. */
import type { TestProject } from 'vitest/node';
import type { Implementations } from './implementations';

declare module 'vitest' {
  export interface ProvidedContext {
    sdkDrivers: Implementations['sdks'];
  }
}

export default function setup(project: TestProject): void {
  const workers = Object.values(project.getProvidedContext().sdkDrivers).filter((drivers) => drivers.worker !== undefined);
  if (workers.length === 0) throw new Error('The implementations file names no SDK Worker drivers. Provide at least one `worker` driver in `sdks`.');
}
