/**
 * The reader's validator, compiled to JavaScript (reader/js), over the
 * services' bundle the reader writes: what services.ts holds every service's
 * value to. These hold its export to what validate.d.cts says of it.
 */
import { readFileSync } from 'node:fs';
import { join } from 'node:path';
import { describe, expect, it } from 'vitest';
import validate from './generated/protocol/validate.cjs';
import { REPO_ROOT } from './paths';

const check = validate.validator(readFileSync(join(REPO_ROOT, 'specs/services.json'), 'utf8'));

describe("the reader's validator", () => {
  it('accepts a value its schema holds', () => {
    expect(check('ServiceAddress', JSON.stringify({ host: 'localhost', port: 8080 }))).toEqual([]);
  });

  it('refuses each thing its schema refuses, at the pointer into the value', () => {
    expect(check('ServiceAddress', JSON.stringify({ host: '', port: 70000, colour: 'red' }))).toEqual([
      '/host is shorter than 1 code points',
      '/port is more than 65535',
      '/colour is a member the schema does not state',
    ]);
  });

  it('counts a string in code points, and reads an integer past 2^31 as an integer', () => {
    expect(check('ServiceAddress', JSON.stringify({ host: '😀', port: 5_000_000_000 }))).toEqual(['/port is more than 65535']);
  });

  it('throws for a schema the bundle does not name', () => {
    expect(() => check('NoSuchSchema', '{}')).toThrow('the bundle names no schema NoSuchSchema');
  });
});
