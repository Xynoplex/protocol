import { defineConfig } from 'vitest/config';
import { readImplementations } from './harness/implementations';

// What the suites test: the implementations file the run names after vitest's
// own arguments, `-- --implementations <file>` (harness/implementations.ts).
const implementations = readImplementations(process.argv);
if (implementations.modelCatalogue) process.env['XYNOLOG_CONFORMANCE_MODEL_CATALOGUE'] = implementations.modelCatalogue;

// Every file boots its own processes, issuer, Archivist and broker on ports of
// its own, so files run in parallel; the cases inside a file share them and
// run in order.
export default defineConfig({
  test: {
    pool: 'forks',
    maxWorkers: 4,
    testTimeout: 60_000,
    hookTimeout: 60_000,
    projects: [
      {
        extends: true,
        test: {
          name: 'sdk',
          // The client proxy is the SDK suite's, and is held against a real gateway.
          include: ['sdk/**/*.test.ts', 'harness/client-proxy.test.ts'],
          provide: { gatewayCommand: implementations.gateway, sdkDrivers: implementations.sdks },
          globalSetup: ['harness/global-setup.ts'],
        },
      },
      {
        extends: true,
        test: {
          name: 'worker',
          include: ['worker/**/*.test.ts'],
          provide: { gatewayCommand: implementations.gateway, sdkDrivers: implementations.sdks },
          globalSetup: ['harness/global-setup.ts'],
        },
      },
    ],
  },
});
