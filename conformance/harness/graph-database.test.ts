/** The graph's database as the harness names it, held to specs/src/graph/database-cases.json. */
import { describe, expect, it } from 'vitest';
import { graphDatabaseCases } from './generated/protocol/protocol';
import { graphDatabaseOf } from './graph-database';

describe("a knowledge base's graph database, as the harness names it", () => {
  it.each(graphDatabaseCases.cases)('$why', ({ kb, database }) => {
    expect(graphDatabaseOf(kb)).toBe(database);
  });
});
