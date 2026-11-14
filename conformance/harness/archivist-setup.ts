/**
 * Once per Archivist run, check the Git runtime dependency used by staging cases.
 */
import { execFileSync } from 'node:child_process';
import type { Implementations } from './implementations';

declare module 'vitest' {
  export interface ProvidedContext {
    /** How an Archivist is started: the configuration provides it. */
    archivistCommand: Implementations['archivist'];
  }
}

export default function setup(): void {
  try {
    execFileSync('git', ['--version'], { stdio: 'ignore' });
  } catch {
    throw new Error('git is not on PATH. The staging cases run it, as an Archivist whose knowledge base syncs git does.');
  }
}
