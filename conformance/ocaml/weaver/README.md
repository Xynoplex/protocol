# Weaver conformance suite

A black-box suite for the Weaver: the process a weaver image runs. It starts
the Weaver from a command and meets it only where the rest of Xynolog does: in
the graph it writes, on the bus, and at its health port. It checks what the
Weaver does against [contract/WEAVER.md](../../../contract/WEAVER.md).

The suite imports nothing from a Weaver, and nothing a Weaver is built on.
What names one is its entry in `weaverServices` in the run's
[implementations file](../../README.md#implementation-manifest). Xynolog's
names the Weaver in Python, run by the interpreter of its project's locked
environment, built in the consumer repository. Another implementation joins
by adding its entry.

A Weaver is given its settings as the protocol's configuration document,
[`WeaverConfig`](../../../specs/src/weaver/WeaverConfig.json), at the path
its `--config` flag names, written into a home of the world's own
([harness/services.ml](../harness/services.ml)). The document says where the
Archivist listens and where the Weaver keeps its cursors: a temporary
directory inside that home. A Weaver stopped and started again finds what it
kept; one that is replaced starts with nothing. The harness refuses to start
it with an environment variable that
[`service-environment/variables.json`](../../../specs/src/service-environment/variables.json)
does not list for it.

The suite reads the graph with a reader of its own
([harness/graph_reader.ml](../harness/graph_reader.ml)): its own Cypher, on a
connection of its own, read over Bolt and over Neo4j's Query API, which must
agree. Every label, key, lookup, relationship and the stub's marker in that
Cypher is read from
[`graph/schema.json`](../../../specs/src/graph/schema.json), the shape every
store keeps the graph in. Each row is read by the harness's own reading of one
([harness/graph_codec.ml](../harness/graph_codec.ml)), held to
[`graph/codec-cases.json`](../../../specs/src/graph/codec-cases.json), the
case table the Weaver's codec is held to; `harness-test` runs that gate. So
the suite judges a Weaver by something other than the Weaver's own code.

That reader writes nothing. Two things the suite does in the database
itself, on the same connection, while no Weaver runs: it empties it, and it
takes one annotation out of it behind a Weaver's back.

## The world around a Weaver

Each file runs its cases in a world of its own
([harness/weaver_world.ml](../harness/weaver_world.ml)):

- an Archivist world ([harness/archivist_world.ml](../harness/archivist_world.ml)):
  a knowledge base, a real Archivist that records and answers, a real
  gateway, and the trusted issuer, which grants the Weaver's service account;
- a real Neo4j, cleared when the world is made;
- the Weaver, started with its settings and its environment, and waited for
  until its first sweep of the record is done and its reconcile has ended.

**Nothing is recorded but through the Archivist.** A case uploads as a
person, asks for annotations as a person, and commits a job's work as a
worker; the Weaver is given the events the record gives it, and the record a
case compares the graph with is the Archivist's own view. No case states a
view. A check of the graph against the record is only a check when the
record's side comes from the record.

**The run is told where its Neo4j is,** in `NEO4J_HOST` and `NEO4J_PASSWORD`,
and starts none. Without them it fails, and says so; it is not skipped.
**It clears the database it is given,** so it is given one of its own. The
graph is in the database
[`graph/database-cases.json`](../../../specs/src/graph/database-cases.json)
names for the suite's knowledge base (`kb-identity/cases.json`'s case `the
shape xynolog init writes`), which a Neo4j somebody else runs must already
hold, so the Neo4j is started with it as its default database, Neo4j's
`initial.dbms.default_database`. Its Bolt port, 7687, and its HTTP port,
7474, must both be reachable.

**One world at a time.** The files share one database, which a rebuild of
the whole graph clears, so the suite runs one worker, and a file's cases in
their order.

## What it checks

- **The graph it builds** ([`record.ml`](record.ml)): a person's highlight, a
  reference bound to another resource and tagged, and work a person asked a
  worker for, each held to what the graph is to hold of the Archivist's view;
  who each is by, the software that made it, and when it was changed; a
  resource's date and attribution; a resource followed through archiving, its
  entity types and an annotation's removal; how far it says it has taken each
  resource's stream; and that its own reconcile finds nothing to repair in a
  graph it has just built.
- **Bringing the graph back** ([`repair.ml`](repair.ml)): what was recorded
  while it was down, with its cursors and with nothing kept; what was lost
  from the graph behind its back, and a graph that was cleaned whole; a
  rebuild of one resource, which leaves another resource's link to it alone;
  and a rebuild of everything.
- **As a process** ([`boot.ml`](boot.ml)): what its health reports; a clean
  stop; and every refusal to start: no service account, no password for the
  graph database, a graph that lives in one process's memory, settings that
  name another knowledge base than the one at its gateway (named both by
  their DIDs), a gateway that does not answer.
- **What it exports** ([`telemetry.ml`](telemetry.ml)): a Weaver told where
  to export, held to the telemetry table in both directions: every row the
  table lists for it arrives, under its name, and nothing arrives that the
  table does not list; and all of it under the resource the table lists for
  it, with `xynolog.kb` the DID of the knowledge base its document names.

A case the Weaver this suite is held to is known to fail says why, and fails
the run once it passes.

## Running it

The implementation is built in its owning repository and named in an
implementation manifest. This suite does not build consumer code. From the
repository's root:

```bash
dune build --root conformance/ocaml
```

```bash
NEO4J_HOST=localhost NEO4J_PASSWORD=a-password ./conformance/ocaml/_build/default/bin/main.exe weaver --implementations /absolute/path/to/implementations.json
```
