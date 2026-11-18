#!/usr/bin/env node
// lint:spec-tables — every table that has its schema is what its schema says,
// and its schema is one a reader can derive a typed model from.
//
// A table's schema is found by its place (tables.mjs). Each one is read twice:
//   - as a schema of the table dialect, written so a reader can derive a type
//     from every node of it. Its keywords mean the same in OpenAPI 3.0 and in
//     JSON Schema 2020-12 but one: `nullable`, OpenAPI 3.0's, as the
//     components write it, which 2020-12 writes as a `type` that lists
//     "null". A node is one of:
//       - a `$ref` and nothing beside it, which OpenAPI 3.0 would not read;
//       - `{ "nullable": true, "allOf": [{ "$ref": … }] }`: what the `$ref`
//         names, or null, as the components write one;
//       - `{ "$comment": … }` alone: a value that is JSON by nature, such as an
//         expected annotation, whose comment says what it is;
//       - a `oneOf` of closed objects, any two of them told apart: by a
//         property both require and fix to different values (a `const` or a
//         one-member `enum`), or by a property one requires and the other
//         does not state; beside them, at most one member of each other
//         type, told apart by its type; and `"nullable": true` beside the
//         `oneOf` when its value may be null;
//       - a node of one `type`, with only the keywords that type takes, and
//         `"nullable": true` when its value may be null. An
//         object with `properties` is closed (`additionalProperties: false`);
//         one without is a map, whose `additionalProperties` is its values'
//         schema. An array states its `items`;
//   - as the schema the table is validated against, following each `$ref`
//     into the schema it names, whose keywords are OpenAPI 3.0's as the
//     components write them.
//
// A case table's schema extends CaseTable.json: its `allOf` is that and only
// that, and it admits each of CaseTable's properties with `true`, except
// `cases`, which it states as an array of its own cases, or leaves out when
// it has none. `additionalProperties` does not see through `allOf`, so the
// admissions are what keeps the closed table open to what CaseTable states.
// Any other table's schema requires `$comment`, stated as CaseTable's. A
// `why`, wherever a table's schema states one, is CaseTable's.
//
// A case table also names no implementation in more than one of `runBy`,
// `absent` and `notYetReached`, which no schema can say.
//
// `format` and `discriminator` are read as annotations: the type and the
// `oneOf` decide.

import { readdirSync, readFileSync, statSync } from 'node:fs';
import { dirname, join, relative, resolve } from 'node:path';
import { CASE_TABLE, SPECS, hasSchema, isCaseTable, restated, schemaOf } from './tables.mjs';

const failures = [];
const fail = (file, pointer, message) => failures.push(`${file}#${pointer} ${message}`);

const documents = new Map();
function load(file) {
  if (!documents.has(file)) documents.set(file, JSON.parse(readFileSync(resolve(SPECS, file), 'utf8')));
  return documents.get(file);
}

const escape = (token) => String(token).replaceAll('~', '~0').replaceAll('/', '~1');
const unescape = (token) => token.replaceAll('~1', '/').replaceAll('~0', '~');

/** The file and pointer `$ref` names, from the file it is written in. */
function target(file, ref) {
  const [path, fragment = ''] = ref.split('#');
  return { file: path ? relative(SPECS, resolve(SPECS, dirname(file), path)).split('\\').join('/') : file, pointer: fragment };
}

function at(file, pointer) {
  let node = load(file);
  for (const token of pointer.split('/').slice(1).map(unescape)) {
    if (node === null || typeof node !== 'object' || !(token in node)) return undefined;
    node = node[token];
  }
  return node;
}

function resolveRef(file, ref) {
  const named = target(file, ref);
  const node = at(named.file, named.pointer);
  if (node === undefined) throw new Error(`${file}: $ref ${ref} names nothing`);
  return { ...named, node };
}

const isObject = (value) => value !== null && typeof value === 'object' && !Array.isArray(value);
const equal = (a, b) => JSON.stringify(canonical(a)) === JSON.stringify(canonical(b));
function canonical(value) {
  if (Array.isArray(value)) return value.map(canonical);
  if (isObject(value)) return Object.fromEntries(Object.keys(value).sort().map((key) => [key, canonical(value[key])]));
  return value;
}

const TYPES = {
  string: (value) => typeof value === 'string',
  integer: (value) => Number.isInteger(value),
  number: (value) => typeof value === 'number',
  boolean: (value) => typeof value === 'boolean',
  object: isObject,
  array: Array.isArray,
};

// The table dialect: what a table's schema may say, and of which type.
const ANNOTATIONS = ['$comment', 'title', 'description'];
const TAKES = {
  string: ['enum', 'const', 'minLength', 'maxLength', 'pattern'],
  integer: ['enum', 'const', 'minimum', 'maximum'],
  number: ['enum', 'const', 'minimum', 'maximum'],
  boolean: ['enum', 'const'],
  object: ['properties', 'required', 'additionalProperties', 'minProperties', 'maxProperties'],
  array: ['items', 'minItems', 'maxItems', 'uniqueItems'],
};

/** The object schema `node` is, following `$ref`s, or nothing. */
function objectOf(file, node) {
  while (isObject(node) && '$ref' in node) ({ file, node } = resolveRef(file, node.$ref));
  return isObject(node) && node.type === 'object' && isObject(node.properties) ? node : undefined;
}

/** The type other than an object that the schema `node` is, following `$ref`s, or nothing. */
function scalarOf(file, node) {
  while (isObject(node) && '$ref' in node) ({ file, node } = resolveRef(file, node.$ref));
  return isObject(node) && typeof node.type === 'string' && node.type !== 'object' ? node.type : undefined;
}

/** The value `node` fixes for a property, by `const` or a one-member `enum`. */
const fixed = (node) => (!isObject(node) ? undefined : 'const' in node ? node.const : node.enum?.length === 1 ? node.enum[0] : undefined);

/** Whether no value is both of two closed objects: one property both require and fix to different values, or one requires and the other does not state. */
function apart(one, other) {
  const requires = (member, name) => (member.required ?? []).includes(name);
  return [...new Set([...Object.keys(one.properties), ...Object.keys(other.properties)])].some((name) =>
    (requires(one, name) && !(name in other.properties)) ||
    (requires(other, name) && !(name in one.properties)) ||
    (requires(one, name) && requires(other, name) && fixed(one.properties[name]) !== undefined &&
      fixed(other.properties[name]) !== undefined && !equal(fixed(one.properties[name]), fixed(other.properties[name]))));
}

function derivable(file, pointer, node) {
  if (!isObject(node)) return fail(file, pointer, 'is no schema a type can be derived from: a boolean schema is only an admission of CaseTable\'s');
  const keys = Object.keys(node);
  if ('$ref' in node) {
    if (keys.length > 1) fail(file, pointer, 'has keywords beside its $ref, which OpenAPI 3.0 does not read');
    resolveRef(file, node.$ref);
    return;
  }
  if (keys.length === 1 && keys[0] === '$comment') {
    if (typeof node.$comment !== 'string' || !/\S/.test(node.$comment)) fail(file, pointer, 'is JSON by nature, and must say what it is in $comment');
    return;
  }
  if ('allOf' in node) {
    for (const key of keys) if (!['allOf', 'nullable', ...ANNOTATIONS].includes(key)) fail(file, `${pointer}/${escape(key)}`, 'is beside a nullable $ref, which is what the $ref names or null');
    if (node.nullable !== true || !Array.isArray(node.allOf) || node.allOf.length !== 1 || !isObject(node.allOf[0]) || !('$ref' in node.allOf[0])) {
      return fail(file, pointer, 'must be { "nullable": true, "allOf": [{ "$ref": … }] }: an allOf in a table\'s schema is a $ref that may be null');
    }
    derivable(file, `${pointer}/allOf/0`, node.allOf[0]);
    return;
  }
  if ('oneOf' in node) {
    for (const key of keys) if (!['oneOf', 'nullable', ...ANNOTATIONS].includes(key)) fail(file, `${pointer}/${escape(key)}`, 'is beside a oneOf, which says what each member is');
    if ('nullable' in node && node.nullable !== true) fail(file, `${pointer}/nullable`, 'must be true, or be left out of a oneOf whose value is never null');
    if (!Array.isArray(node.oneOf) || node.oneOf.length < 2) return fail(file, `${pointer}/oneOf`, 'must name two members or more');
    node.oneOf.forEach((member, index) => derivable(file, `${pointer}/oneOf/${index}`, member));
    const scalars = node.oneOf.map((member) => scalarOf(file, member)).filter((type) => type !== undefined);
    if (new Set(scalars).size !== scalars.length) return fail(file, `${pointer}/oneOf`, 'has two members of one type other than an object, which nothing tells apart');
    const members = node.oneOf.filter((member) => scalarOf(file, member) === undefined).map((member) => objectOf(file, member));
    if (members.some((member) => member === undefined || member.additionalProperties !== false)) {
      return fail(file, `${pointer}/oneOf`, 'must be of closed objects and of at most one member of each other type, so a value is one member or none');
    }
    members.forEach((member, index) => members.slice(index + 1).forEach((other, offset) => {
      if (!apart(member, other)) fail(file, `${pointer}/oneOf`, `has members ${index} and ${index + 1 + offset}, which no property tells apart`);
    }));
    return;
  }
  if (typeof node.type !== 'string' || !(node.type in TAKES)) {
    return fail(file, `${pointer}/type`, `must be one of ${Object.keys(TAKES).join(', ')}, or the node a $ref, a oneOf or { "$comment": … }`);
  }
  for (const key of keys) {
    if (key !== 'type' && key !== 'nullable' && !ANNOTATIONS.includes(key) && !TAKES[node.type].includes(key)) {
      fail(file, `${pointer}/${escape(key)}`, `is no keyword of the table dialect for a ${node.type}`);
    }
  }
  if ('nullable' in node && node.nullable !== true) fail(file, `${pointer}/nullable`, 'must be true, or be left out of a node whose value is never null');
  for (const key of ['enum', 'const'].filter((key) => key in node)) {
    const values = key === 'enum' ? node.enum : [node.const];
    if (!Array.isArray(values) || values.length === 0) { fail(file, `${pointer}/enum`, 'must list one value or more'); continue; }
    for (const value of values) if (!TYPES[node.type](value)) fail(file, `${pointer}/${key}`, `holds ${JSON.stringify(value)}, which is no ${node.type}`);
  }
  if (node.type === 'array') {
    if (!('items' in node)) fail(file, pointer, 'is an array, and must state its items');
    else derivable(file, `${pointer}/items`, node.items);
  }
  if (node.type === 'object') {
    if ('properties' in node) {
      if (node.additionalProperties !== false) fail(file, pointer, 'has properties, and must be closed with "additionalProperties": false');
      for (const [name, property] of Object.entries(node.properties)) derivable(file, `${pointer}/properties/${escape(name)}`, property);
    } else if (!isObject(node.additionalProperties)) {
      fail(file, pointer, 'is a map, and must state its values\' schema in additionalProperties');
    } else derivable(file, `${pointer}/additionalProperties`, node.additionalProperties);
    for (const name of node.required ?? []) if (!(name in (node.properties ?? {}))) fail(file, `${pointer}/required`, `requires ${name}, which it does not state`);
  }
}

/** A table schema's top level: CaseTable's admissions for a case table, CaseTable's `$comment` for any other. */
function tableSchema(table, file, schema) {
  if (!isObject(schema) || schema.type !== 'object' || !isObject(schema.properties)) return fail(file, '', 'must be a closed object: a table is one');
  const own = { ...schema };
  const caseTable = load(CASE_TABLE);
  const toCaseTable = relative(dirname(resolve(SPECS, file)), resolve(SPECS, CASE_TABLE)).split('\\').join('/');
  if (isCaseTable(table)) {
    if (!Array.isArray(schema.allOf) || schema.allOf.length !== 1 || !equal(schema.allOf[0], { $ref: toCaseTable })) {
      fail(file, '/allOf', `must be [{ "$ref": "${toCaseTable}" }] and nothing else: a case table is a CaseTable`);
    }
    delete own.allOf;
    const properties = { ...schema.properties };
    for (const name of Object.keys(caseTable.properties)) {
      if (name === 'cases') {
        if (properties.cases === true) fail(file, '/properties/cases', 'must say what a case is, or be left out of a table that has no cases');
      } else if (properties[name] !== true) fail(file, `/properties/${escape(name)}`, 'is CaseTable\'s, and must be admitted with true: CaseTable states it');
      else delete properties[name];
    }
    own.properties = properties;
  } else {
    if ('allOf' in schema) fail(file, '/allOf', 'extends nothing: only a case table\'s schema extends CaseTable');
    if (!equal(schema.properties.$comment, { $ref: toCaseTable + '#/properties/$comment' }) || !(schema.required ?? []).includes('$comment')) {
      fail(file, '/properties/$comment', `must be required, and be { "$ref": "${toCaseTable}#/properties/$comment" }`);
    }
  }
  const why = { $ref: toCaseTable + '#/properties/cases/items/properties/why' };
  for (const [pointer, node] of nodes('', own)) {
    if (isObject(node.properties) && 'why' in node.properties && !equal(node.properties.why, why)) {
      fail(file, `${pointer}/properties/why`, `is CaseTable's, and must be { "$ref": "${why.$ref}" }`);
    }
  }
  derivable(file, '', own);
}

/** Each node a schema states in itself, with its pointer: itself, its properties, its items, its map's values and its members. */
function* nodes(pointer, node) {
  if (!isObject(node)) return;
  yield [pointer, node];
  for (const [name, property] of Object.entries(isObject(node.properties) ? node.properties : {})) yield* nodes(`${pointer}/properties/${escape(name)}`, property);
  yield* nodes(`${pointer}/items`, node.items);
  yield* nodes(`${pointer}/additionalProperties`, node.additionalProperties);
  for (const key of ['oneOf', 'allOf']) for (const [index, member] of (Array.isArray(node[key]) ? node[key] : []).entries()) yield* nodes(`${pointer}/${key}/${index}`, member);
}

// Validation, in OpenAPI 3.0's reading of the keywords the components use.
const VALIDATION = new Set([
  '$ref', '$comment', 'title', 'description', 'type', 'format', 'enum', 'const', 'default', 'example', 'nullable',
  'properties', 'required', 'additionalProperties', 'propertyNames', 'minProperties', 'maxProperties',
  'items', 'minItems', 'maxItems', 'uniqueItems', 'minLength', 'maxLength', 'pattern', 'minimum', 'maximum',
  'allOf', 'anyOf', 'oneOf', 'not', 'discriminator',
]);

/** The ways `value` is not what `node` (written in `file`) says, each as [pointer into the value, message]. */
function errors(file, node, value, path) {
  if (node === true) return [];
  if (node === false) return [[path, 'is admitted by no schema']];
  if (!isObject(node)) throw new Error(`${file}: a schema is an object or a boolean`);
  for (const key of Object.keys(node)) if (!VALIDATION.has(key)) throw new Error(`${file}: ${key} is no keyword Protocol's schemas use`);
  if ('$ref' in node) {
    const named = resolveRef(file, node.$ref);
    return errors(named.file, named.node, value, path);
  }
  if (value === null && node.nullable === true) return [];
  const found = [];
  const say = (message) => found.push([path, message]);
  if ('type' in node) {
    const types = Array.isArray(node.type) ? node.type : [node.type];
    if (!types.some((type) => TYPES[type](value))) return [[path, `is ${JSON.stringify(value)}, which is no ${types.join(' or ')}`]];
  }
  if ('enum' in node && !node.enum.some((member) => equal(member, value))) say(`is ${JSON.stringify(value)}, which is none of ${JSON.stringify(node.enum)}`);
  if ('const' in node && !equal(node.const, value)) say(`is ${JSON.stringify(value)}, not ${JSON.stringify(node.const)}`);
  if (typeof value === 'string') {
    const length = [...value].length;
    if ('minLength' in node && length < node.minLength) say(`is shorter than ${node.minLength} code points`);
    if ('maxLength' in node && length > node.maxLength) say(`is longer than ${node.maxLength} code points`);
    if ('pattern' in node && !new RegExp(node.pattern, 'u').test(value)) say(`does not match ${node.pattern}`);
  }
  if (typeof value === 'number') {
    if ('minimum' in node && value < node.minimum) say(`is less than ${node.minimum}`);
    if ('maximum' in node && value > node.maximum) say(`is more than ${node.maximum}`);
  }
  if (Array.isArray(value)) {
    if ('minItems' in node && value.length < node.minItems) say(`has fewer than ${node.minItems} items`);
    if ('maxItems' in node && value.length > node.maxItems) say(`has more than ${node.maxItems} items`);
    if (node.uniqueItems === true && new Set(value.map((item) => JSON.stringify(canonical(item)))).size !== value.length) say('repeats an item');
    if ('items' in node) value.forEach((item, index) => found.push(...errors(file, node.items, item, `${path}/${index}`)));
  }
  if (isObject(value)) {
    const names = Object.keys(value);
    if ('minProperties' in node && names.length < node.minProperties) say(`has fewer than ${node.minProperties} members`);
    if ('maxProperties' in node && names.length > node.maxProperties) say(`has more than ${node.maxProperties} members`);
    for (const name of node.required ?? []) if (!(name in value)) say(`lacks ${name}`);
    for (const name of names) {
      const member = `${path}/${escape(name)}`;
      if ('propertyNames' in node) found.push(...errors(file, node.propertyNames, name, member).map(([, message]) => [member, `is a name that ${message}`]));
      if (node.properties && name in node.properties) found.push(...errors(file, node.properties[name], value[name], member));
      else if (node.additionalProperties === false) found.push([member, 'is a member the schema does not state']);
      else if (isObject(node.additionalProperties)) found.push(...errors(file, node.additionalProperties, value[name], member));
    }
  }
  for (const member of node.allOf ?? []) found.push(...errors(file, member, value, path));
  if ('anyOf' in node && !node.anyOf.some((member) => errors(file, member, value, path).length === 0)) say('is none of the anyOf');
  if ('oneOf' in node) {
    const matched = node.oneOf.filter((member) => errors(file, member, value, path).length === 0).length;
    if (matched !== 1) say(matched === 0 ? 'is none of the oneOf' : `is ${matched} of the oneOf, where it must be one`);
  }
  if ('not' in node && errors(file, node.not, value, path).length === 0) say('is what the not refuses');
  return found;
}

// The tables: each <area>/<stem>.json in specs/src that has its schema.
const tables = readdirSync(SPECS)
  .filter((area) => !['components', 'paths'].includes(area) && statSync(join(SPECS, area)).isDirectory())
  .flatMap((area) => readdirSync(join(SPECS, area)).filter((name) => /^[a-z][a-z0-9-]*\.json$/.test(name)).map((name) => `${area}/${name}`))
  .filter(hasSchema)
  .sort();

// CaseTable is open, for the tables that extend it to close, and leaves what
// a case is to each of them, but for its `why`.
for (const [name, property] of Object.entries(load(CASE_TABLE).properties)) {
  if (name !== 'cases') derivable(CASE_TABLE, `/properties/${escape(name)}`, property);
}
derivable(CASE_TABLE, '/properties/cases/items/properties/why', load(CASE_TABLE).properties.cases.items.properties.why);
for (const table of tables) {
  const file = schemaOf(table);
  const schema = load(file);
  tableSchema(table, file, schema);
  const value = load(table);
  for (const [pointer, message] of errors(file, schema, value, '')) fail(table, pointer, message);
  if (isCaseTable(table)) for (const [field, implementation, first] of restated(value)) fail(table, `/${field}/${escape(implementation)}`, `is already in ${first}`);
}

if (failures.length) {
  for (const failure of failures) console.error(`✗ ${failure}`);
  process.exit(1);
}
console.log(`✓ ${tables.length} tables are what their schemas say, and each schema is one a type can be derived from`);
