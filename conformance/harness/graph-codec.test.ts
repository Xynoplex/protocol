/**
 * The harness's reading of the graph, held to
 * specs/src/graph/codec-cases.json: the table every implementation of the
 * graph's codecs runs, the Weaver's among them. It is what makes
 * harness/graph-codec.ts a reader a Weaver can be judged by.
 *
 * The harness reads the graph and writes nothing to it, so it runs the parts
 * a reader has: a row read as an annotation, a row read as a resource, and
 * what the graph is meant to hold of an annotation the record holds. The
 * parts that are a writer's (the row an annotation is stored as, an
 * annotation updated, a reference resolved, a resource stored) are run by the
 * writer's codec.
 */
import { describe, expect, it } from 'vitest';
import { graphCodecCases, type GraphCodecCases } from './generated/protocol/protocol';
import { intendedGraphAnnotation, readAnnotation, readResource } from './graph-codec';

/** The cases of one part, each with why it is there. */
function part<K extends Exclude<keyof GraphCodecCases.CasesItem, 'why'>>(key: K): Array<{ why: string; given: NonNullable<GraphCodecCases.CasesItem[K]> }> {
  return graphCodecCases.cases.flatMap((one) => {
    const given = one[key];
    return given === undefined ? [] : [{ why: one.why, given }];
  });
}

const annotationStored = part('annotationStored');
const annotationRead = part('annotationRead');
const resourceRead = part('resourceRead');

describe('the graph, as the harness reads it', () => {
  it('is held to a table whose every case is of one part, with cases of each part read here', () => {
    for (const { why, ...parts } of graphCodecCases.cases) expect(Object.keys(parts), why).toHaveLength(1);
    expect(annotationStored.length, 'annotationStored').toBeGreaterThan(0);
    expect(annotationRead.length, 'annotationRead').toBeGreaterThan(0);
    expect(resourceRead.length, 'resourceRead').toBeGreaterThan(0);
  });

  describe('what the graph is meant to hold of an annotation', () => {
    it.each(annotationStored)('$why', ({ given }) => {
      expect(intendedGraphAnnotation(given.annotation)).toEqual(given.holds);
    });
  });

  describe("an annotation's row, read", () => {
    it.each(annotationRead)('$why', ({ given }) => {
      if ('refused' in given) expect(() => readAnnotation(given.properties, given.tags)).toThrow(given.refused);
      else expect(readAnnotation(given.properties, given.tags)).toEqual(given.annotation);
    });
  });

  describe("a resource's row, read", () => {
    it.each(resourceRead)('$why', ({ given }) => {
      if ('refused' in given) expect(() => readResource(given.properties)).toThrow(given.refused);
      else expect(readResource(given.properties)).toEqual(given.resource);
    });
  });
});
