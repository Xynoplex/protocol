import { execFileSync } from 'node:child_process';
import { readFileSync } from 'node:fs';

function parseCsv(text) {
  const rows = [];
  let row = [];
  let field = '';
  let quoted = false;
  for (let index = 0; index < text.length; index++) {
    const char = text[index];
    if (quoted) {
      if (char === '"' && text[index + 1] === '"') { field += '"'; index++; }
      else if (char === '"') quoted = false;
      else field += char;
    } else if (char === '"' && field.length === 0) quoted = true;
    else if (char === ',') { row.push(field); field = ''; }
    else if (char === '\n') {
      row.push(field.replace(/\r$/, ''));
      if (row.some((value) => value !== '')) rows.push(row);
      row = [];
      field = '';
    } else field += char;
  }
  if (field || row.length > 0) { row.push(field); rows.push(row); }
  return rows;
}

const rows = parseCsv(readFileSync('OWNERSHIP-INVENTORY.csv', 'utf8'));
const [header, ...entries] = rows;
if (header?.join(',') !== 'path,classification,owner,notes') throw new Error('OWNERSHIP-INVENTORY.csv has an unexpected header');
const listed = entries.map((entry) => entry[0]);
const duplicate = listed.find((path, index) => listed.indexOf(path) !== index);
if (duplicate) throw new Error(`ownership inventory lists ${duplicate} more than once`);
const tracked = execFileSync('git', ['ls-files', '-z'], { encoding: 'utf8' }).split('\0').filter(Boolean);
const missing = tracked.filter((path) => !listed.includes(path));
const stale = listed.filter((path) => !tracked.includes(path));
if (missing.length || stale.length || entries.some((entry) => entry.length !== 4 || entry.some((value) => !value))) {
  throw new Error([
    missing.length ? `unclassified tracked files:\n${missing.map((path) => `  ${path}`).join('\n')}` : '',
    stale.length ? `inventory entries not tracked by Git:\n${stale.map((path) => `  ${path}`).join('\n')}` : '',
    entries.some((entry) => entry.length !== 4 || entry.some((value) => !value)) ? 'an inventory row is incomplete' : '',
  ].filter(Boolean).join('\n'));
}
console.log(`✓ ${tracked.length} tracked files have an owner and classification`);
