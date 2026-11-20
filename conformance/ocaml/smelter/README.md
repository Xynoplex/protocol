# Smelter conformance suite

A black-box suite for the Smelter: the process a smelter image runs. It starts
the Smelter from a command and meets it only where the rest of Xynolog does: in
the vector index and the anchored-text store it writes, on the bus, at its
embedding provider and at its health port. It checks what the Smelter does
against [contract/SMELTER.md](../../../contract/SMELTER.md).

The suite imports nothing from a Smelter, and nothing a Smelter is built on.
What names one is its entry in `smelterServices` in the run's
[implementations file](../../README.md#implementation-manifest). Xynolog's
names the Smelter in Python, run by the interpreter of its project's locked
environment, built in the consumer repository. Another implementation joins
by adding its entry.

A Smelter is given its settings as the protocol's configuration document,
[`SmelterConfig`](../../../specs/src/smelter/SmelterConfig.json), at the path
its `--config` flag names, written into a home of the process's own
([harness/services.ml](../harness/services.ml)). The harness refuses to
start it with an environment variable that
[`service-environment/variables.json`](../../../specs/src/service-environment/variables.json)
does not list for it.

The Smelter in Python reads a scanned page with the `tesseract` program,
which must be on `PATH`, with its English data.

The index is read and changed by the suite's own calls to Qdrant's REST
interface ([harness/qdrant.ml](../harness/qdrant.ml)), and never through the
store a Smelter writes with. What a point holds is stated by
[`vectors/payload-cases.json`](../../../specs/src/vectors/payload-cases.json);
the collections' words, the payload members a point is found by and the
arithmetic of a point's id are read from that table, and a knowledge base's
collections are named from its DID as the table states. Every request the
suite makes presents the Qdrant's key, and the Smelter is handed a token
minted from it, whose claims are what
[`vectors/token-cases.json`](../../../specs/src/vectors/token-cases.json)
states.

Every text a case holds is Protocol's: what a Smelter logs and the words of
a refused rebuild from
[`smelter/messages.json`](../../../specs/src/smelter/messages.json), the
members of its health from
[`SmelterHealth`](../../../specs/src/smelter/SmelterHealth.json) and of a
reconcile's summary from
[`SmelterReconcileSummary`](../../../specs/src/smelter/SmelterReconcileSummary.json),
and how its provider is asked from
[`embedding/requests.json`](../../../specs/src/embedding/requests.json).

## The world around a Smelter

Each file runs its cases in a world of its own
([harness/smelter_world.ml](../harness/smelter_world.ml)):

- an Archivist world ([harness/archivist_world.ml](../harness/archivist_world.ml)):
  a knowledge base, a real Archivist that records and answers and serves a
  resource's bytes, a real gateway, and the trusted issuer, which grants the
  Smelter's service account;
- a real Qdrant, every collection of which is cleared when the world is made
  and again when it is closed;
- a stand-in for its embedding provider
  ([harness/ollama_embed.ml](../harness/ollama_embed.ml)): an Ollama's
  `POST /api/embed`. A vector is worked out from its text alone, so the same
  text has the same vector and two texts have two. It records every request
  it is sent, and a case can have it answer 404, as for a model not fetched
  yet, answer any other failing status, or hold a request until the case
  releases it;
- the anchored-text store, which the Archivist keeps: the Smelter states its
  stamp and stores its entries on the Archivist's surface and mounts none of
  it, and the suite reads the Archivist's files where
  [`archivist/layout.json`](../../../specs/src/archivist/layout.json) files
  them;
- the Smelter, started with its settings and its environment, which holds
  its service account and its token for the index, its cursors kept in a
  file of its home, and waited for until its start has swept the record.

**Nothing is recorded but through the Archivist.** A case uploads, updates,
archives, tags and annotates as a person, so the Smelter is given the events
the record gives it and reads the bytes the Archivist serves. What it then
says, a case reads off the bus: a stream of `smelt:settled` is open before
any Smelter is, and every decision it carries is held to its schema.

**The run is told where its Qdrant is,** in `QDRANT_HOST` (its REST port,
6333), **and the API key it was started with,** in `QDRANT_API_KEY`, and
starts none. The Qdrant is run with its JWT access control on, as the
launcher runs one. Without either variable the run fails, and names the one
that is missing, and so does a Qdrant that refuses the key or asks for none;
it is not skipped. **It clears every collection of the
database it is given,** so it is given one of its own:

```bash
docker run -d --name smelter-suite-qdrant -p 6333:6333 -e QDRANT__SERVICE__API_KEY=a-key -e QDRANT__SERVICE__JWT_RBAC=true "$(consumer-owned source)"
```

**One world at a time.** The files share one database, and every Smelter is
told the same health port, so the suite runs one worker, and a file's cases
in their order. For the same reason a case stops the world's Smelter before
it starts one that must refuse to start.

**The index is cleared only while no Smelter runs.** A Smelter makes its
collections when it connects, and asks its provider, each time it makes
them, how wide the model's vectors are. One whose collections were deleted
under it fails its next write. So a case stops the Smelter, clears the
index, and starts it again; and the requests that measure the model are
among those the provider records for that start. A case that wants only
what came after some point marks the point (`Ollama_embed.mark`) and asks
for what was sent since.

**Each start is of a container made again.** A world's `start` takes away
the cursors an earlier start kept, as every `xynolog start` does, so a
Smelter starts every resource the record holds by a reconcile, which runs
after health answers; `start` waits until the start has swept the record. A
case about what happens while the reconcile runs starts the Smelter to
`` `Serving `` only, and waits on `reconciled`.

## What it checks

- **As a process** ([`boot.ml`](boot.ml)): what its health reports, of a
  reconcile with nothing to mend, and after events and a reconcile; that an
  event recorded while its start's reconcile runs is applied then; a clean
  stop; every refusal to start, without either half of its service account,
  without its token for the index, and without each setting it is told; and
  that a provider answering 404 as it starts is asked again.
- **As the record changes** ([`index.ml`](index.ml)): what a recorded text
  becomes, by the table's points and the provider's vectors; a long text's
  several points; an update, an archive and an unarchive; an annotation
  added and removed, one that quotes nothing, and one made on an archived
  resource; entity types rewritten with nothing embedded; an image and a
  blank text skipped, by reason, and logged; and that bytes it cannot read,
  or text it cannot get vectors for, change nothing and stop nothing.
- **Given a PDF** ([`pdf.ml`](pdf.ml)): that a PDF with text of its own is
  indexed from it; that what deriving came to is stored under the checksum
  of the bytes, a decline too, and is what the Archivist then serves; that a
  resource is indexed though its entry cannot be written; and the rebuild
  command, naming a resource and naming none: what it derives, that it
  embeds nothing and deletes nothing, and what it answers when it could not
  do every resource.
- **Starting after things changed** ([`reconcile.ml`](reconcile.ml)): an
  index and an anchored-text store that were cleared, points taken out,
  bytes and entity types changed, an entry lost, a resource archived,
  annotations added and removed, and an annotation's body given a tag, each
  while the Smelter was stopped; what its reconcile then mends, what it asks
  of its provider, and what it reports. A text it remembers skipping is not
  read again, and an index a Smelter of another cut made is made again and
  filled whole.
- **Beside another knowledge base** ([`kb_index.ml`](kb_index.ml)): that the
  suite names a knowledge base's collections as the payload table states,
  and mints each service's token as the token table states; that a Smelter
  whose Qdrant holds another knowledge base's index neither reads nor purges
  nor writes it, and fills its own; and that one whose settings name another
  knowledge base than the one at its gateway refuses to start, naming both,
  and makes no index of the one they name.
- **Asking its provider** ([`provider.ml`](provider.ml)): a batch cut into
  slices of as many texts as `embedding/requests.json` says, each text given
  the vector that came back for it; one request at a time; a provider that
  says it is busy, which is asked again; and a request that fails, which
  leaves its resource with no points, and holds up no other resource, until
  the Smelter next starts.
- **What it exports** ([`telemetry.ml`](telemetry.ml)): told where to
  export, a Smelter exports under its own name every row
  [`service-telemetry/telemetry.json`](../../../specs/src/service-telemetry/telemetry.json)
  lists for it that its traffic can make, and nothing the table does not
  list, all of it under the resource the table lists for it, with
  `xynolog.kb` the DID of the knowledge base its document names; and its
  gauge of the index comes to say the number of points the index holds.

A case the Smelter this suite is held to is known to fail says why, and
fails the run once it passes.

## Running it

The implementation and its drivers are built in their owning repository and
named in an implementation manifest. This suite does not build consumer
code. From the repository's root:

```bash
dune build --root conformance/ocaml
```

```bash
QDRANT_HOST=localhost QDRANT_API_KEY=a-key ./conformance/ocaml/_build/default/bin/main.exe smelter --implementations /absolute/path/to/implementations.json
```
