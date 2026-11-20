# Librarian conformance suite

A black-box suite for the Librarian: the process a librarian image runs. It
starts the Librarian from a command and meets it only where the rest of
Xynolog does: on the bus, at its providers, and at its health port. It checks
what the Librarian does against
[contract/LIBRARIAN.md](../../../contract/LIBRARIAN.md).

The suite imports nothing from a Librarian. What names one is its entry in
`librarianServices` in the run's
[implementations file](../../README.md#implementation-manifest). Xynolog's names
the Librarian in Python (the consumer repository), run by the interpreter of
its project's locked environment. The Weaver and the Smelter it starts are the
file's `python` entries in `weaverServices` and `smelterServices`.

A Librarian writes nothing, so the suite reads no store to judge it. What a
Librarian reads, the graph and the vector index, is written here by the two
services that write it for an operator: a Weaver and a Smelter, which the
suite starts beside it. The suite does two things in the databases itself
([harness/librarian_world.ml](../harness/librarian_world.ml)): it empties the
graph database, in Cypher, and it clears the index, by its own calls to
Qdrant. It knows what `weave:applied` must reach from the events the
Archivist stored of each resource, read where
[`archivist/layout.json`](../../../specs/src/archivist/layout.json) says they
are.

## The world around a Librarian

Each file runs its cases in a world of its own
([harness/librarian_world.ml](../harness/librarian_world.ml)), made by the
file's first case and closed when a case of another file runs:

- an Archivist world ([harness/archivist_world.ml](../harness/archivist_world.ml)):
  a knowledge base, a real Archivist that records, answers and writes the
  views, a real gateway, and the trusted issuer, which grants each of the
  three services an account of its own;
- a real Neo4j and a real Qdrant, both cleared when the world is made and
  when it is closed. The Smelter and the Librarian each present the Qdrant a
  token of their own, minted from the Qdrant's key as
  [`specs/src/vectors/token-cases.json`](../../../specs/src/vectors/token-cases.json)
  states, and the suite's own calls present the key;
- a Weaver, which writes the graph, and a Smelter, which writes the index,
  each started as its own suite starts it
  ([harness/services.ml](../harness/services.ml));
- a stand-in for the model of each of the Librarian's two actors, the Matcher
  and the Gatherer ([harness/ollama.ml](../harness/ollama.ml)): a case scripts
  what each answers next, holds a generation until it releases it, or has
  each answer by what it was asked, and reads the prompts each was sent;
- two stand-ins for the embedding provider
  ([harness/ollama_embed.ml](../harness/ollama_embed.ml)), one the Librarian
  asks and one the Smelter asks, so what the Librarian asked is read apart
  from what the Smelter asked. Both give the same text the same vector;
- the Librarian, started with its document and its environment, and waited
  for until its health answers and it answers on the bus.

**Nothing is recorded but through the Archivist, and nothing is put in a
store but by its writer.** A case uploads a resource and asks for an
annotation as a person; the Weaver and the Smelter are given the events the
record gives them; and the case waits for what each signals on the bus,
`weave:applied` and `smelt:settled`, as a client hears them. What the
Librarian is then asked about is what the real writers wrote. A store that
lags is staged by stopping its writer before the case records.

**A case asks as a person who is signed in,** and reads the one reply to its
request. The operations it asks are those the bus registry says a Librarian
answers (`answeredBy`). Every frame its stream carries is held to what the
registry says its channel carries, and the reply it reads is held to its
channel's schema again where it is read.

**A model is asked nothing a case did not script.** A generation no case
scripted fails the case, whatever it was about, and so does a request a
stand-in does not serve.

**Each service's health is held to its schema**
([`LibrarianHealth`](../../../specs/src/librarian/LibrarianHealth.json),
[`WeaverHealth`](../../../specs/src/weaver/WeaverHealth.json),
[`SmelterHealth`](../../../specs/src/smelter/SmelterHealth.json)), and each
environment to what
[`service-environment/variables.json`](../../../specs/src/service-environment/variables.json)
lists for the service.

**The run is told where its databases are,** in `NEO4J_HOST`,
`NEO4J_PASSWORD`, `QDRANT_HOST` and `QDRANT_API_KEY`, the API key its Qdrant
was started with, and starts neither. The Qdrant is run with its JWT access
control on, as the launcher runs one. Without the four it fails, and names
the one that is missing; it is not skipped. **It clears the graph database
and the index it is given,** so it is given ones of its own. The images the
launcher starts for an operator are the ones to run:

```bash
docker run -d --name librarian-suite-neo4j -p 7687:7687 -e NEO4J_AUTH=neo4j/a-password -e NEO4J_initial_dbms_default__database="$(suite-database)" "$(consumer-owned source)"
docker run -d --name librarian-suite-qdrant -p 6333:6333 -e QDRANT__SERVICE__API_KEY=a-key -e QDRANT__SERVICE__JWT_RBAC=true "$(consumer-owned source)"
```

The graph is in the database
[`graph/database-cases.json`](../../../specs/src/graph/database-cases.json)
names for the suite's knowledge base (`kb-identity/cases.json`'s case `the
shape xynolog init writes`), which a Neo4j somebody else runs must already
hold, so the Neo4j is started with it as its default database, Neo4j's
`initial.dbms.default_database`.

**One world at a time.** The files share the two databases, and the health
ports of a Weaver and a Smelter are constants, so the suite runs one worker,
and its files one after another.

**A Librarian is given its settings as the protocol's configuration
document,**
[`LibrarianConfig`](../../../specs/src/librarian/LibrarianConfig.json),
at the path its `--config` flag names, written into a home directory of the
world's own. It states no state volume: a Librarian mounts none, and reads
what the world's Archivist records of each resource, and the profiles of the
principals it names, on the bus. It states the health port, a free port the
harness chooses for each world. It states the two numbers a deployment may
leave to their defaults too: how long a gather waits for a resource's
vectors, and the floor of the semantic fallback. The Librarian's environment
holds its service account, the graph database's password, its token for the
vector index and `PATH`, and nothing else. A case that takes a setting away,
or changes one, takes it out of the document or changes it there; a
Librarian that must refuse to start is given its document as the case
changed it, unchecked.

One read is carried to the real Archivist by the harness. A Librarian reads a
resource's anchored text from the gateway, and the gateway of an Archivist
world asks the harness's stand-in Archivist; for every resource a case makes,
the stand-in answers what the real Archivist answers when it is asked.

## What it checks

- **As a process** ([`boot.ml`](boot.ml)): what its health reports; a clean
  stop; and every refusal to start: no knowledge base, no gateway address, no
  issuer, no address for the Archivist, no graph database, no vector
  database, no embedding provider, no model for an actor, a graph or a vector
  index that lives in one process's memory, a settle bound that is no time at
  all or does not end under the stall of a held job
  ([`client/timing.json`](../../../specs/src/client/timing.json)'s
  `heldJobStallMs`), half a service account, no password for the graph
  database, no token for the vector index, settings that name another
  knowledge base than the one at its gateway, and a gateway that does not
  answer. Each refusal must name the member of the document, or the
  variable, that is missing or refused, and the one of another knowledge
  base names both by their DIDs.
- **Finding things** ([`search.ml`](search.ml)): a reference search's
  candidates from each of its four sources, with a score and a reason, and
  its own resource left out, with its model's scoring and without, when the
  model or the embedding provider fails, and its limit; a text search
  answered by name with previews and by meaning with the passage that
  matched, a later page, blank text, its filters by name and by meaning, the
  floor a hit by meaning must reach, a PDF's empty preview, and the empty page
  of a Librarian whose token reads another knowledge base's index and none of
  its own; what refers to a resource, and the failure for a resource nobody
  made; the names of the persons a page and a candidate mention; and the
  limits each actor reports of its own engine, with a provider that does not
  answer left out.
- **Gathering** ([`gather.ml`](gather.ml)): around an annotation, the
  annotation and its resource, the text selected with what is before and
  after it, a linked resource with its text and summary, and without the
  summary when its model fails on it, the knowledge graph, entity types and
  their frequencies, the sentence its model writes, similar passages, each
  option, and each failure; around a resource, its text and its neighbours',
  a summary and suggestions when asked, with the texts and without them,
  similar passages by its own vectors, and each failure; an annotation's
  summary; and that gathers for one resource are answered one at a time.
- **When a writer is behind** ([`barriers.ml`](barriers.ml)): with the
  Weaver or the Smelter stopped by the suite, a gather that waits for the
  graph and one that fails saying the graph is behind; a search answered from
  a view for a resource the graph does not hold yet; a gather that waits for
  the Smelter's decision, one answered at once for a resource the Smelter
  skips, one answered at once, by a Librarian that has heard no decision, for
  an indexed resource that nothing is like and for an image, and one answered
  without similar passages when the settle bound passes; and a Librarian that
  answers as before after it is started again.
- **What it exports** ([`telemetry.ml`](telemetry.ml)): a Librarian told
  where to export, held to the telemetry table in both directions: every row
  the table lists for it arrives, under its name, and nothing arrives that
  the table does not list; and all of it under the resource the table lists
  for it, with `xynolog.kb` the DID of the knowledge base its document names.
  Its traffic is a gather whose model answers and reports its tokens, one
  whose model fails, one around a bound reference whose model fails on the
  linked resource's summary and on the relationship sentence, one that reads
  a PDF's text, one the Smelter does not decide in time, and one for a
  resource the graph does not come to hold: every way a request of a model
  ends, and everything a gather can go without.

## Running it

The implementation and its drivers are built in their owning repository and
named in an implementation manifest. This suite does not build consumer code.
From the repository's root, with the four store variables set:

```bash
dune build --root conformance/ocaml
```

```bash
./conformance/ocaml/_build/default/bin/main.exe librarian --implementations /absolute/path/to/implementations.json
```

Review the setup requirements above before running: the suite clears the
Neo4j and the Qdrant it is given. Use dedicated test instances.
