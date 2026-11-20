# Gateway conformance suite

A black-box suite for the gateway. It starts gateway processes, talks to them
over HTTP, and checks what they answer against the protocol as `specs/`
states it, the OpenAPI document and the bus registry, and against
[contract/TRANSPORT-HTTP.md](../../../contract/TRANSPORT-HTTP.md) for the
semantics a schema cannot hold.

The suite imports nothing from a gateway. What names one is `gateway` in the
run's [implementations file](../../README.md#implementation-manifest).
Xynolog's names the Rust gateway's binary, built in the consumer repository.

## The world around a gateway

The harness plays the world around a gateway
([harness/world.ml](../harness/world.ml)):

- the trusted issuer ([harness/issuer.ml](../harness/issuer.ml)), which
  serves discovery, keys and the client-credentials grant, and signs with the
  protocol-local fixture issuer;
- a stand-in Archivist
  ([harness/archivist_standin.ml](../harness/archivist_standin.ml)), held to
  the Archivist's spec. It answers a repeated upload by its principal and
  key, as the Archivist does;
- a broker ([harness/nats.ml](../harness/nats.ml)), a real `nats-server` from
  `PATH`. A case can make it require credentials, take it down and bring it
  back;
- a proxy ([harness/proxy.ml](../harness/proxy.ml)) that can stall a
  connection to the broker;
- an OTLP receiver ([harness/otlp.ml](../harness/otlp.ml)).

**Every case runs on each signal plane**, in-process and NATS. The
exceptions are the cases that need a broker and the ones that do not depend
on the plane:

- the cases that need a broker are replicas, a broker outage, and what the
  broker holds;
- the cases that do not depend on the plane are the slow heartbeat, reply
  retention, the connection wait, key rotation, the key fetch, and knowledge
  bases configured another way.

Each file's cases share a world on each plane, made by its first case.

**Every response and every stream is held to its declaration**, and a
violation fails the case whatever it was about:

- **Responses.** A reply's status must be declared for its route, and its
  body and headers must match the declaration
  ([harness/call.ml](../harness/call.ml)). No error body may carry a stack
  frame, a source path or a secret's name.
- **Streams.** Every stream must come as `text/event-stream`. Every message
  on it must be a `BusStreamMessage`, with the id its frame calls for
  ([harness/bus_stream.ml](../harness/bus_stream.ml), held to
  [`bus/event-id-cases.json`](../../../specs/src/bus/event-id-cases.json) by
  a census gate).
- **What the gateway asks the Archivist.** Every request must be an
  operation the Archivist's spec declares, with the parameters that
  operation requires, each within its schema.
- **What the stand-in answers.** Every reply the stand-in Archivist gives
  must match its declaration.

**A gateway is given its settings as the protocol's configuration
document**,
[`GatewayConfig`](../../../specs/src/gateway/GatewayConfig.json), at the
path its `--config` flag names. The document is held to its schema before
the gateway starts. The harness refuses to start a gateway with an
environment variable that
[`service-environment/variables.json`](../../../specs/src/service-environment/variables.json)
does not list for it and the document does not name. That way no case can
lean on a variable the spec does not state.

## What it checks

- **Derived from the spec** ([`spec_derived.ml`](spec_derived.ml)). A route
  added to the spec is covered without a new case. The suite checks:
  - every declared operation, probed without a credential, with another
    scheme before its body is read, and with a bad credential, with the
    challenge exact;
  - every public operation, answered as declared and without a challenge;
  - every JSON request body, refused when it does not validate;
  - every request limit, enforced at its bound and not below it;
  - an undeclared path, or an undeclared method on a declared one, answered
    404 with an `ErrorResponse`.
- **Hand-written**, each case citing the text it checks:
  - credentials, under the bearer and media-token schemes
    ([`tokens.ml`](tokens.ml)), and the person and agent cases of
    [`principals/cases.json`](../../../specs/src/principals/cases.json),
    each named exactly by a running gateway;
  - the issuer's key set: the deadline on a fetch, and one fetch per
    cooldown when a fetch fails ([`issuer.ml`](issuer.ml));
  - how long the Archivist is waited for
    ([`archivist_waits.ml`](archivist_waits.ml)): a read's answer from its
    first send, each next part of an answer, and an upload's answer from its
    last byte. An upload the Archivist recorded and never answered is
    answered, when it is sent again under its key, with the resource it made;
  - an emit or an upload sent again under its `Idempotency-Key`
    ([`keys.ml`](keys.ml)). It is answered as the first was and carried out
    once. It is refused 422 with another body, and waited for while the
    first is in hand. A key is its principal's. One is refused 400 without a
    key or with one out of bounds;
  - content and uploads ([`content.ml`](content.ml)):
    - the JSON-LD description;
    - a resource's anchored text, whole when it is larger than the largest
      bus message;
    - a path that names what is not a resource's id;
    - a read the Archivist closes its connection on without answering,
      which is sent once more and not again, as an upload never is;
  - emitting: stamping, claims, profiles, unanswerable requests, and the
    ids a payload and a scope may carry ([`emit.ml`](emit.ml));
  - the stream: delivery, reply routing, replay and every gap reason,
    recovery, presence and the heartbeat ([`stream.ml`](stream.ml));
  - the bounds:
    - on one connection ([`bounds.ml`](bounds.ml));
    - on one principal, by role
      ([`principal_limits.ml`](principal_limits.ml));
    - on the refused requests one holder may have, and on those that name
      no holder ([`refusals.ml`](refusals.ml));
    - on one process ([`capacity.ml`](capacity.ml));
  - replicas sharing a broker ([`replicas.ml`](replicas.ml)), a broker
    outage ([`outage.ml`](outage.ml)), and what the gateway leaves on the
    broker it shares with the dispatcher ([`broker.ml`](broker.ml));
  - boot refusals, broker credentials among them, and the signing key ring
    ([`boot.ml`](boot.ml));
  - the headers on every response, as
    [`gateway/response-headers.json`](../../../specs/src/gateway/response-headers.json)
    states them, and the routes that describe the gateway and the knowledge
    base ([`edge.ml`](edge.ml)).
- **Telemetry** ([`observability.ml`](observability.ml)). Each plane exports
  to a receiver of its own, which reads OTLP/HTTP as JSON or protobuf, as a
  collector does. The last case holds everything received to the rows of
  [`service-telemetry/telemetry.json`](../../../specs/src/service-telemetry/telemetry.json)
  that list the gateway, in both directions:
  - every span and metric is a listed row of the listed kind, and carries
    listed attributes;
  - every row the cases' traffic can produce has arrived;
  - all of it is exported under exactly the keys the table's `resource`
    lists for the gateway, with `xynolog.kb` the DID of the knowledge base
    its document names.
- **The environment** ([`environment.ml`](environment.ml)): what each
  variable `service-environment/variables.json` lists changes, and the
  document's `logFormat`.

## Running it

The implementation and its drivers are built in their owning repository and
named in an implementation manifest. This suite does not build consumer
code. `nats-server` must be on `PATH`. From the repository's root:

```bash
dune build --root conformance/ocaml
```

```bash
./conformance/ocaml/_build/default/bin/main.exe gateway --implementations /absolute/path/to/implementations.json
```

The suite runs four workers unless `-j` says otherwise.
