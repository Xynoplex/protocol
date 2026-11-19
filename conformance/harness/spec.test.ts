/**
 * The tables the harness reads, each read through its schema (readTable):
 * each is what its schema states, and what a case asks of it is there.
 */
import { expect, it } from 'vitest';
import { identifiers, kbIdentity, readTable, serviceEnvironment, SUITE_KB } from './spec';

it('reads each service its environment variables from service-environment/variables.json', () => {
  for (const service of ['gateway', 'dispatcher', 'archivist'] as const) expect(serviceEnvironment(service).length, service).toBeGreaterThan(0);
});

it('reads a kind of id the strings it takes and refuses from identifiers/kinds.json', () => {
  const { accepts, refuses } = identifiers('ResourceId');
  expect(accepts.length).toBeGreaterThan(0);
  expect(refuses.length).toBeGreaterThan(0);
  expect(() => identifiers('NoSuchId')).toThrow('identifiers/kinds.json names no kind NoSuchId');
});

it("reads the suite's knowledge base its identity from kb-identity/cases.json", () => {
  const kb = kbIdentity(SUITE_KB);
  expect(kb.resource.startsWith('https://')).toBe(true);
  expect(() => kbIdentity('no such case')).toThrow('kb-identity/cases.json has no "no such case" case that declares an identity');
});

it('reads every section of errors/codes.json its codes', () => {
  const { $comment, ...sections } = readTable('errors/codes.json', 'ErrorsCodes');
  expect($comment.length).toBeGreaterThan(0);
  for (const [name, section] of Object.entries(sections)) expect(section.codes.length, name).toBeGreaterThan(0);
});
