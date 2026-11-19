/**
 * Once per librarian run: refuse to start when a Librarian, the Weaver, the
 * Smelter or a database the run needs is missing, so each reads as that,
 * never as every case failing.
 *
 * The run is told where its neo4j is, in NEO4J_HOST and NEO4J_PASSWORD, and
 * where its Qdrant is, in QDRANT_HOST, with the API key it was started with,
 * in QDRANT_API_KEY. It starts neither: each is a server, and a run inside a
 * container cannot start another. A run without the four fails here and
 * names the ones that are missing. It is not skipped. The Qdrant is run with
 * its JWT access control on, as the launcher runs one, and the suite mints
 * the tokens of the services it starts from the key. The suite CLEARS the
 * graph database and the index it is given, so it is given ones of its own
 * (the CLI and images repositories document compatible database images). The
 * graph is in the database specs/src/graph/database-cases.json names for the
 * suite's knowledge base, which a Neo4j somebody else runs must already hold:
 * it is started with that as its default database.
 */
import type { TestProject } from 'vitest/node';
import type { Implementations } from './implementations';
import { guarded, QDRANT_PORT } from './qdrant';

declare module 'vitest' {
  export interface ProvidedContext {
    /** How each implementation of the Librarian is started: the configuration provides them. */
    librarianServices: Implementations['librarianServices'];
  }
}

export default async function setup(project: TestProject): Promise<void> {
  const provided = project.getProvidedContext();
  if (Object.keys(provided.librarianServices).length === 0) throw new Error('The implementations file names no Librarian: its `librarianServices` is empty.');


  const graphHost = process.env['NEO4J_HOST'];
  const graphPassword = process.env['NEO4J_PASSWORD'];
  const vectorHost = process.env['QDRANT_HOST'];
  const vectorKey = process.env['QDRANT_API_KEY'];
  if (!graphHost || !graphPassword || !vectorHost || !vectorKey) {
    const missing = [
      ...(graphHost ? [] : ['NEO4J_HOST']),
      ...(graphPassword ? [] : ['NEO4J_PASSWORD']),
      ...(vectorHost ? [] : ['QDRANT_HOST']),
      ...(vectorKey ? [] : ['QDRANT_API_KEY']),
    ];
    throw new Error(
      `The librarian suite runs against a real neo4j and a real Qdrant, both of which it clears, and ${missing.join(' and ')} ${missing.length > 1 ? 'are' : 'is'} not set. ` +
        'Start a neo4j for it alone (a compatible Neo4j image with NEO4J_AUTH=neo4j/<password>, and as its default database the one specs/src/graph/database-cases.json names for the suite\'s knowledge base) and set NEO4J_HOST to its address and NEO4J_PASSWORD to that password. ' +
        `Start a Qdrant for it alone (\`cli/internal/launcher/scripts/qdrant-image.sh\` prints the image; its REST port is ${QDRANT_PORT}), ` +
        'with QDRANT__SERVICE__API_KEY=<key> and QDRANT__SERVICE__JWT_RBAC=true, and set QDRANT_HOST to its address and QDRANT_API_KEY to its key.',
    );
  }
  const vectorDatabase = { host: vectorHost, key: vectorKey };
  // Asked once here, so a vector database that is not there, refuses the key or asks for none reads as that.
  await guarded(vectorDatabase);
  project.provide('graphDatabase', { host: graphHost, password: graphPassword });
  project.provide('vectorDatabase', vectorDatabase);
}
