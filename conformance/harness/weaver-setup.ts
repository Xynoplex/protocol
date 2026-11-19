/**
 * Once per weaver run: refuse to start when a Weaver, the Archivist or the
 * graph database the run needs is missing, so each reads as that, never as
 * every case failing.
 *
 * The run is told where its neo4j is, in NEO4J_HOST and NEO4J_PASSWORD. It
 * starts none: the database is a server, and a run inside a container cannot
 * start another. A run without them fails here and says so. It is not skipped.
 * The suite CLEARS the database it is given, so it is given one of its own
 * (the launcher repository documents a compatible Neo4j image). The graph is
 * in the database specs/src/graph/database-cases.json names for the suite's
 * knowledge base, which a Neo4j somebody else runs must already hold: it is
 * started with that as its default database.
 */
import type { TestProject } from 'vitest/node';
import type { Implementations } from './implementations';

declare module 'vitest' {
  export interface ProvidedContext {
    /** How each implementation of the Weaver is started: the configuration provides them. */
    weaverServices: Implementations['weaverServices'];
    /** Where the run's graph database is. */
    graphDatabase: { host: string; password: string };
  }
}

export default function setup(project: TestProject): void {
  const services = project.getProvidedContext().weaverServices;
  if (Object.keys(services).length === 0) throw new Error('The implementations file names no Weaver.');


  const host = process.env['NEO4J_HOST'];
  const password = process.env['NEO4J_PASSWORD'];
  if (!host || !password) {
    const missing = [...(host ? [] : ['NEO4J_HOST']), ...(password ? [] : ['NEO4J_PASSWORD'])];
    throw new Error(
      `The weaver suite runs against a real neo4j, which it clears, and ${missing.join(' and ')} ${missing.length > 1 ? 'are' : 'is'} not set. ` +
        'Start one for it alone (a compatible Neo4j image; NEO4J_AUTH=neo4j/<password>, and as its default database the one specs/src/graph/database-cases.json names for the suite\'s knowledge base) ' +
        'and set NEO4J_HOST to its address and NEO4J_PASSWORD to that password.',
    );
  }
  project.provide('graphDatabase', { host, password });
}
