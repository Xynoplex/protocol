// Where a table's schema is: beside the table, named by the table's area and
// its file's stem in PascalCase, so `jobs/retry-cases.json`'s schema is
// `jobs/JobsRetryCases.json`. The name is the type a reader derives from the
// schema, and differs from the table's file by more than case.
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

export const SPECS = resolve(fileURLToPath(import.meta.url), '../../../../specs/src');

/** The schema every case table's schema extends, from specs/src. */
export const CASE_TABLE = 'CaseTable.json';

/** A table states its rule case by case when its file's name ends so (specs/README.md, "Case tables"). */
export const isCaseTable = (table) => table.endsWith('cases.json');

const pascal = (words) => words.split('-').map((word) => word[0].toUpperCase() + word.slice(1)).join('');

/** The schema of `table` (a path from specs/src, `<area>/<stem>.json`), from specs/src. */
export function schemaOf(table) {
  const match = /^([a-z][a-z0-9-]*)\/([a-z][a-z0-9-]*)\.json$/.exec(table);
  if (!match) throw new Error(`${table} is not <area>/<stem>.json, where a table is`);
  return `${match[1]}/${pascal(match[1])}${pascal(match[2])}.json`;
}
