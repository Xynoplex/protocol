/**
 * The services' own schemas — their configuration documents, health answers
 * and Archivist's anchored-text entry — as Protocol's reader holds a value to
 * them: its validator (reader/js), over the services' bundle it writes
 * (specs/services.json). Their types are the reader's too (ServiceSchemas).
 */
import { readFileSync } from 'node:fs';
import { inject } from 'vitest';
import validate from './generated/protocol/validate.cjs';
import type { ServiceSchemas } from './generated/protocol/protocol';

export type { ServiceSchemas };

let check: ((schema: string, value: string) => string[]) | undefined;

/** What the reader refuses of `value` as the service schema `name`: each refusal, a JSON Pointer into it and why. */
export function serviceRefusals(name: keyof ServiceSchemas, value: unknown): string[] {
  check ??= validate.validator(readFileSync(inject('servicesBundlePath'), 'utf8'));
  return check(name, JSON.stringify(value));
}

/** Whether the reader accepts `value` as the service schema `name`. */
export function isService<N extends keyof ServiceSchemas>(name: N, value: unknown): value is ServiceSchemas[N] {
  return serviceRefusals(name, value).length === 0;
}
