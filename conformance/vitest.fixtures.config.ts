import { defineConfig } from 'vitest/config';

/** Harness-level tests that need only protocol fixtures, the bundled specs, and local Node tools. */
export default defineConfig({
  test: {
    pool: 'forks',
    globalSetup: ['harness/spec-setup.ts'],
    include: [
      'harness/graph-codec.test.ts',
      'harness/implementations.test.ts',
      'harness/issuer.test.ts',
      'harness/git.test.ts',
      'harness/net.test.ts',
      'harness/otlp.test.ts',
      'harness/telemetry.test.ts',
    ],
  },
});
