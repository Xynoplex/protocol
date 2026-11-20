# Dispatcher conformance suite

A black-box suite for the dispatcher. It starts dispatcher processes and
meets them only as the rest of Xynolog does: on the bus, through a real
gateway, and at the health port. It checks what they do against the job
protocol as [contract/JOBS.md](../../../contract/JOBS.md) states it, and
against the schemas `specs/` gives every channel.

The suite imports nothing from a dispatcher. What names one is `dispatcher`
in the run's [implementations file](../../README.md#implementation-manifest).
Xynolog's names the Rust dispatcher's binary, built in the consumer
repository.

## The world around a dispatcher

Each file runs its cases in a world of its own
([harness/dispatcher_world.ml](../harness/dispatcher_world.ml)):

- the trusted issuer ([harness/issuer.ml](../harness/issuer.ml)), which
  grants the dispatcher's service account;
- a gateway on the in-process plane, the dispatcher's only route to the bus;
- a JetStream broker for the queue ([harness/nats.ml](../harness/nats.ml)),
  fresh for every world, so the clocks a file sets are the ones the broker's
  objects are made with;
- the Archivist's Browser, answering the two vocabulary reads `job:create`
  makes over the bus, or absent, for the cases that need it gone;
- the dispatcher ([harness/dispatcher.ml](../harness/dispatcher.ml)),
  started with `--config` and a
  [`DispatcherConfig`](../../../specs/src/dispatcher/DispatcherConfig.json)
  document held to its schema, its clocks shrunk where a case needs them to
  act within a second. The harness refuses to start it with an environment
  variable that
  [`service-environment/variables.json`](../../../specs/src/service-environment/variables.json)
  does not list for it and the document does not name.

Cases play people, workers and sidecars. They emit through the gateway and
read the replies off their own streams. The operations they ask are those
the registry says the dispatcher answers. Every frame those streams carry
must be what the registry says its channel carries, and a case fails
otherwise, whatever it was about. After every case the world cancels
whatever is still pending, because a claim names jobs by what they are, not
by id, and would otherwise hand one case's job to the next.

Every text a case holds is Protocol's: the dispatcher's refusals and the
error its sweep records from
[`dispatcher/messages.json`](../../../specs/src/dispatcher/messages.json),
the refusals to start from
[`service-environment/boot-refusals.json`](../../../specs/src/service-environment/boot-refusals.json),
the gateway's `peer-unavailable` failure from
[`gateway/messages.json`](../../../specs/src/gateway/messages.json), and each
job type's retry budget from
[`jobs/budgets.json`](../../../specs/src/jobs/budgets.json).

## What it checks

- **A job's life** ([`lifecycle.ml`](lifecycle.ml)): admitted, announced,
  claimed and recorded, completed, and read back, end to end.
- **Admission** ([`create.ml`](create.ml)): what the gateway refuses at its
  door because it is not a job description (a parameter a job does not
  take, a value a job needs left out or empty), every check `job:create`
  makes and its refusal, the resource a job is recorded under, the two
  reads, and a refusal that carries `peer-unavailable` when the Archivist is
  gone.
- **Announcements** ([`announce.ml`](announce.ml)): a pending job is
  announced with its description less its input, and with nothing the
  dispatcher added to it.
- **Claims** ([`claim.ml`](claim.ml)): only a worker may claim; a claim
  takes a job that matches one of its filters, each a partial job
  description, and is atomic under simultaneous claims; the record a claim
  hands out; `job:assign`.
- **Concluding an attempt** ([`conclude.ml`](conclude.ml)), **progress and
  checkpoints** ([`progress.ml`](progress.ml)), **cancellation**
  ([`cancel.ml`](cancel.ml)) and **status** ([`status.ml`](status.ml)): each
  transition, its merge rules, and no effect on a job not in the state it
  needs.
- **The clocks** ([`clocks.ml`](clocks.ml)): the re-announce tick, the
  dead-worker sweep and the error it records, retention and the progress
  throttle.
- **Restarts and the broker** ([`restart.ml`](restart.ml)): the queue
  outlives its dispatcher, and the dispatcher rides out its broker
  restarting, but not a change of credentials.
- **Boot** ([`boot.ml`](boot.ml)): every refusal to start, in the table's
  words and its line, the health answer, and a clean stop.
- **Environment** ([`environment.ml`](environment.ml)): what each variable
  `service-environment/variables.json` lists for the dispatcher changes, and
  that a reply continues its request's trace.
- **Telemetry** ([`observability.ml`](observability.ml)): the `bus.recv` and
  `bus.emit` spans a job's frames make, the queue's size by every status the
  spec gives a job, and, last, everything exported held, in both directions,
  to the rows of
  [`service-telemetry/telemetry.json`](../../../specs/src/service-telemetry/telemetry.json)
  that list the dispatcher and the rows of
  [`sdk-telemetry/telemetry.json`](../../../specs/src/sdk-telemetry/telemetry.json)
  for the SDK transport it reaches the bus through, and to the table's
  `resource`: exported under exactly the keys it lists for the dispatcher,
  with `xynolog.kb` the DID of the knowledge base its document names. Its
  cases run solo, in order, on one world.

Behaviour JOBS.md lists as a known defect is not pinned by any case. A case
the dispatcher this suite is held to is known to fail says why, and fails
the run once it passes.

## Running it

The implementation and its drivers are built in their owning repository and
named in an implementation manifest. This suite does not build consumer
code. `nats-server` must be on `PATH`. From the repository's root:

```bash
dune build --root conformance/ocaml
```

```bash
./conformance/ocaml/_build/default/bin/main.exe dispatcher --implementations /absolute/path/to/implementations.json
```

The suite runs four workers unless `-j` says otherwise.
