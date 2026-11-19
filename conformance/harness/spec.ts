/**
 * The specs, as the suite reads them — the gateway's API, and the Archivist's
 * HTTP surface its stand-in Archivist is held to: operations, their declared
 * responses, the limits, and validators compiled from the component schemas.
 * Everything a case asserts about shape comes from here — nothing is restated.
 */
import { readFileSync } from 'node:fs';
import { Ajv2020, type ValidateFunction } from 'ajv/dist/2020.js';
import addFormatsModule from 'ajv-formats';
import { inject } from 'vitest';
import {
  busRegistry,
  clientTiming as timingTable,
  identifiersKinds,
  kbIdentityCases,
  principalsCases,
  sdkTelemetryTelemetry,
  serviceEnvironmentVariables,
  serviceTelemetryTelemetry,
  type BusRegistry,
  type IdentifiersKinds,
  type KbIdentityCases,
  type PrincipalsCases,
  type SdkTelemetryTelemetry,
  type ServiceTelemetryTelemetry,
} from './generated/protocol/protocol';

// ajv-formats is CommonJS; its default export arrives wrapped under ESM.
const addFormats = (addFormatsModule as unknown as { default?: typeof addFormatsModule }).default ?? addFormatsModule;

type Json = null | boolean | number | string | Json[] | { [key: string]: Json };
type JsonObject = { [key: string]: Json };

export const METHODS = ['get', 'put', 'post', 'delete', 'patch'] as const;
export type Method = (typeof METHODS)[number];

export interface Operation {
  method: Method;
  /** OpenAPI form: `/resources/{id}`. */
  path: string;
  op: JsonObject;
}

const isObject = (v: unknown): v is JsonObject => typeof v === 'object' && v !== null && !Array.isArray(v);

const SCHEMA_BASE = 'https://spec.local/schemas/';

/** `#/components/schemas/X` → an absolute id, so a schema compiles on its own. */
function absolutize(node: Json): void {
  if (Array.isArray(node)) {
    node.forEach(absolutize);
    return;
  }
  if (!isObject(node)) return;
  const ref = node['$ref'];
  if (typeof ref === 'string' && ref.startsWith('#/components/schemas/')) {
    node['$ref'] = SCHEMA_BASE + ref.slice('#/components/schemas/'.length);
  }
  Object.values(node).forEach(absolutize);
}

export class Spec {
  readonly doc: JsonObject;
  private readonly ajv: Ajv2020;
  private readonly compiled = new Map<string, ValidateFunction>();

  constructor(path: string) {
    this.doc = JSON.parse(readFileSync(path, 'utf8')) as JsonObject;
    this.ajv = new Ajv2020({ allErrors: true, strict: false });
    addFormats(this.ajv);
    const components = this.doc['components'];
    const schemas = isObject(components) && isObject(components['schemas']) ? components['schemas'] : {};
    for (const [name, schema] of Object.entries(schemas)) {
      const copy = structuredClone(schema);
      absolutize(copy);
      this.ajv.addSchema(copy as object, SCHEMA_BASE + name);
    }
  }

  operations(): Operation[] {
    const out: Operation[] = [];
    const paths = this.doc['paths'];
    if (!isObject(paths)) return out;
    for (const [path, item] of Object.entries(paths)) {
      if (!isObject(item)) continue;
      for (const method of METHODS) {
        const op = item[method];
        if (isObject(op)) out.push({ method, path, op });
      }
    }
    return out;
  }

  operation(method: Method, path: string): JsonObject {
    const found = this.operations().find((o) => o.method === method && o.path === path);
    if (!found) throw new Error(`the spec declares no ${method.toUpperCase()} ${path}`);
    return found.op;
  }

  /** Follow a `#/components/...` reference inside the bundle. */
  deref(node: Json | undefined): Json | undefined {
    let current = node;
    while (isObject(current) && typeof current['$ref'] === 'string' && current['$ref'].startsWith('#/')) {
      let target: Json | undefined = this.doc;
      for (const segment of current['$ref'].slice(2).split('/')) {
        target = isObject(target) ? target[segment] : undefined;
      }
      current = target;
    }
    return current;
  }

  /** The declared response for a status, or undefined when it is not declared. */
  response(method: Method, path: string, status: number): JsonObject | undefined {
    const responses = this.operation(method, path)['responses'];
    if (!isObject(responses)) return undefined;
    const declared = this.deref(responses[String(status)]);
    return isObject(declared) ? declared : undefined;
  }

  /** A validator for a schema node as it appears in the bundle. */
  validator(schema: Json): ValidateFunction {
    const key = JSON.stringify(schema);
    let validate = this.compiled.get(key);
    if (!validate) {
      const copy = structuredClone(schema);
      absolutize(copy);
      validate = this.ajv.compile(copy as object);
      this.compiled.set(key, validate);
    }
    return validate;
  }

  /** The validator of a named component schema. */
  component(name: string): ValidateFunction {
    const validate = this.ajv.getSchema(SCHEMA_BASE + name);
    if (!validate) throw new Error(`the spec has no component schema ${name}`);
    return validate;
  }

  /** A component schema's own document, for reading its keywords (`maxItems`, patterns). */
  schema(name: string): JsonObject {
    const components = this.doc['components'];
    const schemas = isObject(components) ? components['schemas'] : undefined;
    const schema = isObject(schemas) ? schemas[name] : undefined;
    if (!isObject(schema)) throw new Error(`the spec has no component schema ${name}`);
    return schema;
  }

  /** An operation's `x-xynolog-limits`, every value a positive integer. */
  limits(method: Method, path: string): Record<string, number> {
    const limits = this.operation(method, path)['x-xynolog-limits'];
    const out: Record<string, number> = {};
    if (isObject(limits)) for (const [k, v] of Object.entries(limits)) if (typeof v === 'number') out[k] = v;
    return out;
  }

  /** One of the document's own `x-xynolog-limits`, which hold for every connection. */
  documentLimit(key: string): number {
    const limits = this.doc['x-xynolog-limits'];
    const value = isObject(limits) ? limits[key] : undefined;
    if (typeof value !== 'number') throw new Error(`the spec states no x-xynolog-limits.${key}`);
    return value;
  }

  /** One of a security scheme's `x-xynolog-limits`. */
  schemeLimit(scheme: string, key: string): number {
    const components = this.doc['components'];
    const schemes = isObject(components) ? components['securitySchemes'] : undefined;
    const declared = isObject(schemes) ? schemes[scheme] : undefined;
    const limits = isObject(declared) ? declared['x-xynolog-limits'] : undefined;
    const value = isObject(limits) ? limits[key] : undefined;
    if (typeof value !== 'number') throw new Error(`the spec's security scheme ${scheme} states no x-xynolog-limits.${key}`);
    return value;
  }

  /** A limit on a principal: its baseline, and a coefficient per role (`'unlimited'`, or the baseline's shape). */
  principalLimit<C>(method: Method, path: string, key: string): { baseline: C; roles: Record<string, C | 'unlimited'> } {
    const limit = (this.operation(method, path)['x-xynolog-limits'] as Record<string, unknown> | undefined)?.[key];
    if (!isObject(limit) || !('baseline' in limit)) throw new Error(`${method.toUpperCase()} ${path} states no principal limit ${key}`);
    return { baseline: limit['baseline'] as C, roles: (limit['roles'] ?? {}) as Record<string, C | 'unlimited'> };
  }
}

let cached: Spec | undefined;
/** The gateway's API. */
export function spec(): Spec {
  cached ??= new Spec(inject('specPath'));
  return cached;
}

let archivistCached: Spec | undefined;
/** The Archivist's HTTP surface. */
export function archivistSpec(): Spec {
  archivistCached ??= new Spec(inject('archivistSpecPath'));
  return archivistCached;
}

export function errorsOf(validate: ValidateFunction): string {
  return (validate.errors ?? []).map((e) => `${e.instancePath || '/'} ${e.message ?? ''}`).join('; ');
}

// ── The tables, as Protocol's reader writes them (generated/protocol) ─────

export function operationFor(request: string): BusRegistry.OperationsItem {
  const op = busRegistry.operations.find((o) => o.request === request);
  if (!op) throw new Error(`the registry declares no operation ${request}`);
  return op;
}

/** A Xynolog service, as the telemetry table names it. */
export type TelemetryService = ServiceTelemetryTelemetry.ServicesItem;

/** The names of the environment variables `service` reads (service-environment/variables.json). */
export function serviceEnvironment(service: TelemetryService): string[] {
  const names = serviceEnvironmentVariables.variables.filter((row) => row.services.includes(service)).map((row) => row.name);
  if (names.length === 0) throw new Error(`service-environment/variables.json lists no variables for the ${service}`);
  return names;
}

export type TelemetryAttribute = ServiceTelemetryTelemetry.AttributesItem;
type Span = ServiceTelemetryTelemetry.SpansItem | SdkTelemetryTelemetry.SpansItem;
type Metric = ServiceTelemetryTelemetry.MetricsItem | SdkTelemetryTelemetry.MetricsItem;
/** A row of either table. An SDK's arrives with its traffic: the table states no `when`, because there is no other. */
export type TelemetryRow = Span | Metric;

export interface Telemetry {
  spans: Span[];
  metrics: Metric[];
}

const SERVICE_TABLE = 'service-telemetry/telemetry.json';
const SDK_TABLE = 'sdk-telemetry/telemetry.json';

/**
 * For each service that reaches the bus through an SDK, what of the SDK
 * table it exports: each entry a transport, for all of its rows, or the name
 * of one row, for a service that uses part of a transport. Each names
 * something of the SDK table.
 */
const through: ServiceTelemetryTelemetry.T['sdk'] = (() => {
  const shared = [...sdkTelemetryTelemetry.spans, ...sdkTelemetryTelemetry.metrics];
  for (const [name, used] of Object.entries(serviceTelemetryTelemetry.sdk)) {
    for (const entry of used ?? []) {
      if (!shared.some((r) => r.transport === entry || r.name === entry)) throw new Error(`${SERVICE_TABLE}: ${name} names ${entry} of the SDK table, which is neither a transport ${SDK_TABLE} lists rows for nor one of its rows`);
    }
  }
  return serviceTelemetryTelemetry.sdk;
})();

/**
 * The telemetry `service` exports: the rows of the service table that list
 * it, and, when it reaches the bus through an SDK, the SDK table's rows for
 * the transports it uses and the rows it names.
 */
export function telemetry(service: TelemetryService): Telemetry {
  const used: string[] = through[service] ?? [];
  const shared = <R extends SdkTelemetryTelemetry.SpansItem | SdkTelemetryTelemetry.MetricsItem>(rows: R[]): R[] => rows.filter((r) => used.includes(r.transport) || used.includes(r.name));
  return {
    spans: [...serviceTelemetryTelemetry.spans.filter((r) => r.services.includes(service)), ...shared(sdkTelemetryTelemetry.spans)],
    metrics: [...serviceTelemetryTelemetry.metrics.filter((r) => r.services.includes(service)), ...shared(sdkTelemetryTelemetry.metrics)],
  };
}

/** The keys of the resource `service` exports every span and metric under: exactly these (the service table's `resource`). */
export function resourceKeys(service: TelemetryService): string[] {
  return serviceTelemetryTelemetry.resource.filter((r) => r.services.includes(service)).map((r) => r.key);
}

/**
 * The telemetry an SDK's transports export (sdk-telemetry/telemetry.json):
 * every row, or, for a client that uses one transport alone, that
 * transport's rows.
 */
export function sdkTelemetry(transport?: SdkTelemetryTelemetry.Transport): Telemetry {
  if (transport === undefined) return sdkTelemetryTelemetry;
  const rows = { spans: sdkTelemetryTelemetry.spans.filter((r) => r.transport === transport), metrics: sdkTelemetryTelemetry.metrics.filter((r) => r.transport === transport) };
  if (rows.spans.length + rows.metrics.length === 0) throw new Error(`${SDK_TABLE} lists no row for the transport ${transport}`);
  return rows;
}

/** What an exported span name looks like for a row: `{channel}` stands for any channel, `{jobType}` for any job type. */
export function spanPattern(row: TelemetryRow): RegExp {
  const parts = row.name.split(/\{[A-Za-z]+\}/).map((p) => p.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'));
  return new RegExp(`^${parts.join('.+')}$`);
}

/** A span's exported name, from the row the spec lists: `spanName('bus.dispatch:{channel}', { channel })`. */
export function spanName(template: string, fill: Record<string, string> = {}): string {
  if (![...serviceTelemetryTelemetry.spans, ...sdkTelemetryTelemetry.spans].some((r) => r.name === template)) throw new Error(`the spec lists no span ${template}`);
  return template.replace(/\{([A-Za-z]+)\}/g, (_, key: string) => {
    const value = fill[key];
    if (value === undefined) throw new Error(`span ${template} needs a value for {${key}}`);
    return value;
  });
}

/** A metric's name, as the spec lists it. */
export function metricName(name: string): string {
  if (![...serviceTelemetryTelemetry.metrics, ...sdkTelemetryTelemetry.metrics].some((r) => r.name === name)) throw new Error(`the spec lists no metric ${name}`);
  return name;
}

/** The strings a kind of id takes and the ones it refuses, each with why (identifiers/kinds.json). */
export function identifiers(kind: string): IdentifiersKinds.KindsItem {
  const found = identifiersKinds.kinds.find((k) => k.schema === kind);
  if (!found) throw new Error(`identifiers/kinds.json names no kind ${kind}`);
  return found;
}

type PrincipalCase = PrincipalsCases.PeopleItem;

/** A form of principals/cases.json, as a function of a domain and a subject: each part after the domain percent-encoded as the table states, encodeURIComponent's encoding. */
const named = (form: string) => (domain: string, subject: string) => form.replace('{domain}', domain).replace('{subject}', encodeURIComponent(subject));
const personDid = named(principalsCases.person);
const agentDid = named(principalsCases.agent);

/** Each case of principals/cases.json, checked against its form when the harness loads. */
for (const [kind, form, cases] of [
  ['person', personDid, principalsCases.people],
  ['agent', agentDid, principalsCases.agents],
] as const) {
  for (const c of cases) {
    if (form(c.domain, c.subject) !== c.did) throw new Error(`principals/cases.json: "${c.why}" — the ${kind} form gives ${form(c.domain, c.subject)}, the case says ${c.did}`);
  }
}

/** How the gateway names its principals under `domain` (principals/cases.json): a person's DID and an agent's, from the table's forms. */
export function principals(domain: string): { personDid(subject: string): string; agentDid(subject: string): string } {
  return { personDid: (subject) => personDid(domain, subject), agentDid: (subject) => agentDid(domain, subject) };
}

/**
 * The table's cases under `domain`: people, and agents, with the DID, address
 * and name the gateway must give each. The suite's knowledge base has both.
 */
export function principalCases(domain: string): { people: PrincipalCase[]; agents: PrincipalCase[] } {
  const people = principalsCases.people.filter((c) => c.domain === domain);
  const agents = principalsCases.agents.filter((c) => c.domain === domain);
  if (people.length === 0 || agents.length === 0) throw new Error(`principals/cases.json needs person and agent cases under ${domain}, the suite's knowledge base`);
  return { people, agents };
}

/** The case of kb-identity/cases.json the suite's knowledge base is. */
export const SUITE_KB = 'the shape xynolog init writes';

type Declared = 'name' | 'domain' | 'did' | 'resource';
/** A knowledge base's declared identity: a case of kb-identity/cases.json that states each of these. */
export type KbIdentity = { [K in Declared]: NonNullable<KbIdentityCases.CasesItem[K]> };

/** A knowledge base's identity, from the shared case table (kb-identity/cases.json): the case `why` names, which must declare one. */
export function kbIdentity(why: string): KbIdentity {
  const chosen = kbIdentityCases.cases.find((c) => c.why === why);
  if (!chosen) throw new Error(`kb-identity/cases.json has no "${why}" case`);
  const { name, domain, did, resource } = chosen;
  if (name === null || domain === null || did === null || resource === null) throw new Error(`kb-identity/cases.json's "${why}" case declares no identity: its name, domain, did and resource are not all stated`);
  return { name, domain, did, resource };
}

/** A duration of the timing every Xynolog client keeps (client/timing.json), by its name: milliseconds. */
export function clientTiming(name: string): number {
  const value = timingTable.timing.find((t) => t.name === name)?.value;
  if (typeof value !== 'number' || !name.endsWith('Ms')) throw new Error(`client/timing.json states no duration ${name}`);
  return value;
}
