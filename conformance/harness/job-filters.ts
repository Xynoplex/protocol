/**
 * Claims' filters, as the cases name the jobs a claim takes: each a partial
 * job description (`JobFilter`).
 */
import type { components } from './protocol-core';
import { spec } from './spec';

type JobFilter = components['schemas']['JobFilter'];
type Motivation = components['schemas']['MarkJobParams']['motivation'];

/** A claim's filter for `mark` jobs of one motivation. */
export const marks = (motivation: Motivation): JobFilter => ({ jobType: 'mark', params: { motivation } });

/** A claim's filter for `yield` jobs. */
export const YIELDS: JobFilter = { jobType: 'yield' };

/** The motivations a `mark` job has: the ones the spec tells MarkJobParams apart by. */
function markMotivations(): Motivation[] {
  const params = spec().doc['components'] as { schemas: { MarkJobParams: { discriminator: { mapping: Record<string, string> } } } };
  return Object.keys(params.schemas.MarkJobParams.discriminator.mapping) as Motivation[];
}

/** Filters that between them take every job: one per motivation, and `yield`. */
export function everyJob(): JobFilter[] {
  return [...markMotivations().map(marks), YIELDS];
}
