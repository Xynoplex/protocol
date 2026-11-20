/**
 * Small protocol-derived helpers used by the conformance reader itself.
 * These are not an implementation dependency: the types come from the
 * bundled OpenAPI document, and the bus subject comes from the job-storage
 * specification.
 */
import { generateKeyPair, exportJWK, SignJWT } from 'jose';
import type { CryptoKey, JWK, JWTPayload } from 'jose';
import type { components } from './generated/openapi-types';
import { jobsStorage, type ServiceSchemas } from './generated/protocol/protocol';

export type { components, ServiceSchemas };
export type Annotation = components['schemas']['Annotation'];
export type ResourceDescriptor = components['schemas']['ResourceDescriptor'];
export type ResourceId = string & { readonly __brand: 'ResourceId' };
export type AnnotationId = string & { readonly __brand: 'AnnotationId' };

export const resourceId = (value: string): ResourceId => value as ResourceId;
export const annotationId = (value: string): AnnotationId => value as AnnotationId;
export const kbDid = (domain: string): string => `did:web:${domain}`;

export const JOBS_STREAM_SUBJECTS = [`${jobsStorage.stream.subjectRoot}.>`] as const;

interface IssuerKey {
  kid: string;
  privateKey: CryptoKey;
  jwk: JWK;
}

export interface TokenOptions {
  kid?: string;
  privateKey?: CryptoKey;
  issuer?: string;
  audience?: string;
  expiresIn?: string | Date;
  claims?: JWTPayload;
}

export interface FixtureIssuer {
  readonly issuer: string;
  readonly audience: string;
  readonly discoveryFetches: number;
  readonly jwksFetches: number;
  readonly discoveryUrl: string;
  readonly jwksUrl: string;
  discoveryDocument(): { issuer: string; jwks_uri: string };
  jwks(): { keys: JWK[] };
  addKey(kid: string): Promise<void>;
  token(options?: TokenOptions): Promise<string>;
  unpublishedKey(): Promise<CryptoKey>;
}

async function rsaKey(kid: string): Promise<IssuerKey> {
  const { privateKey, publicKey } = await generateKeyPair('RS256');
  const jwk = { ...(await exportJWK(publicKey)), kid, alg: 'RS256', use: 'sig' };
  return { kid, privateKey, jwk };
}

/** An in-process OIDC fixture issuer for conformance suites. */
export async function fixtureIssuer(
  origin: string,
  options: { audience: string; advertisedIssuer?: string },
): Promise<FixtureIssuer> {
  const keys: IssuerKey[] = [await rsaKey('k1')];
  const advertisedIssuer = options.advertisedIssuer ?? origin;
  let discoveryFetches = 0;
  let jwksFetches = 0;
  return {
    issuer: origin,
    audience: options.audience,
    discoveryUrl: `${origin}/.well-known/openid-configuration`,
    jwksUrl: `${origin}/keys`,
    discoveryDocument() {
      discoveryFetches++;
      return { issuer: advertisedIssuer, jwks_uri: `${origin}/keys` };
    },
    jwks() {
      jwksFetches++;
      return { keys: keys.map((key) => key.jwk) };
    },
    get discoveryFetches() {
      return discoveryFetches;
    },
    get jwksFetches() {
      return jwksFetches;
    },
    async addKey(kid) {
      keys.push(await rsaKey(kid));
    },
    async token(tokenOptions = {}) {
      const newest = keys[keys.length - 1]!;
      const kid = tokenOptions.kid ?? newest.kid;
      const published = keys.find((key) => key.kid === kid);
      const privateKey = tokenOptions.privateKey ?? published?.privateKey;
      if (!privateKey) throw new Error(`fixture issuer publishes no key '${kid}' and none was supplied`);
      return new SignJWT({ sub: 'user-1', ...tokenOptions.claims })
        .setProtectedHeader({ alg: 'RS256', kid })
        .setIssuer(tokenOptions.issuer ?? origin)
        .setAudience(tokenOptions.audience ?? options.audience)
        .setIssuedAt()
        .setExpirationTime(tokenOptions.expiresIn ?? '10m')
        .sign(privateKey);
    },
    async unpublishedKey() {
      return (await generateKeyPair('RS256')).privateKey;
    },
  };
}
