/**
 * What a row is held to, read from the tables themselves: a value is
 * outside a row's values unless it is one of them as it was carried, so the
 * boolean `true` an SDK carries as `content.stream` is listed and the string
 * `"true"` is not.
 */
import { expect, it } from 'vitest';
import { sdkTelemetry, telemetry } from './spec';
import { outsideTheRow } from './telemetry';

it("holds a value to the type its row lists it as: content.get's content.stream is the boolean true", () => {
  const row = sdkTelemetry('content').spans.find((r) => r.name === 'content.get');
  if (!row) throw new Error('the SDK table lists no content.get span');
  expect(outsideTheRow('content.get', row, new Map([['content.stream', new Set([true])]]))).toEqual([]);
  expect(outsideTheRow('content.get', row, new Map([['content.stream', new Set(['true'])]]))).toEqual(['content.get carries content.stream="true", outside true']);
});

it("holds a metric's value the same way: the worker's detection.reroll is a boolean", () => {
  const row = telemetry('worker').metrics.find((r) => r.name.endsWith('xynolog.detection.calls'));
  if (!row) throw new Error('the service table lists no xynolog.detection.calls metric for the worker');
  expect(outsideTheRow(row.name, row, new Map([['detection.reroll', new Set([true, false])]]))).toEqual([]);
  expect(outsideTheRow(row.name, row, new Map([['detection.reroll', new Set(['false'])]]))).toEqual([`${row.name} carries detection.reroll="false", outside true, false`]);
});
