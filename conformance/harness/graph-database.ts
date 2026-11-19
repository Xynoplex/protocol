/**
 * The database of a graph store that holds a knowledge base's graph, named as
 * specs/src/graph/database-cases.json says and held to its cases by
 * graph-database.test.ts. The launcher names it, so a world names it to the
 * Weaver and to the Librarian it launches, and connects to it itself.
 */
import { createHash } from 'node:crypto';

/** `kb-` and the first 60 lower-case hex digits of the SHA-256 of the DID's UTF-8 bytes. */
export function graphDatabaseOf(kb: string): string {
  return `kb-${createHash('sha256').update(kb, 'utf8').digest('hex').slice(0, 60)}`;
}
