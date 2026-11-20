/**
 * A real NATS broker for the NATS-plane cases: a mocked broker would prove
 * nothing about subjects, queue groups or the key-value tables. Spawned from
 * PATH on a free port, with JetStream. Like the broker the launcher runs, it
 * takes a message twice as large as the largest emit, so the envelope a frame
 * is published in always fits.
 */
import { spawn, type ChildProcess } from 'node:child_process';
import { mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { freePort, waitForTcp } from './net';
import { spec } from './spec';

export interface Broker {
  readonly url: string;
  readonly port: number;
  stop(): Promise<void>;
}

export async function startBroker(): Promise<Broker> {
  const port = await freePort();
  const store = mkdtempSync(join(tmpdir(), 'conformance-nats-'));
  // nats-server takes its largest message from a config file and from no flag.
  const config = join(store, 'xynolog.conf');
  writeFileSync(config, `max_payload: ${2 * spec().limits('post', '/bus/emit')['maxBodyBytes']!}\n`);
  const child: ChildProcess = spawn('nats-server', ['-c', config, '-a', '127.0.0.1', '-p', String(port), '-js', '-sd', store], { stdio: 'ignore' });
  await waitForTcp('127.0.0.1', port, 10_000);
  return {
    url: `nats://127.0.0.1:${port}`,
    port,
    async stop() {
      if (child.exitCode === null) {
        const exited = new Promise((resolve) => child.once('exit', resolve));
        child.kill('SIGTERM');
        await exited;
      }
      rmSync(store, { recursive: true, force: true });
    },
  };
}
