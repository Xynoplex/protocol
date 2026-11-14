/** The SDK suite never builds implementation code: the manifest supplies its drivers. */
import type { TestProject } from 'vitest/node';
import type { Implementations } from './implementations';

declare module 'vitest' {
  export interface ProvidedContext {
    sdkDrivers: Implementations['sdks'];
  }
}

export default function setup(project: TestProject): void {
  if (Object.keys(project.getProvidedContext().sdkDrivers).length === 0) {
    throw new Error('The implementations file names no SDK drivers. Provide at least one driver in `sdks`.');
  }
}
