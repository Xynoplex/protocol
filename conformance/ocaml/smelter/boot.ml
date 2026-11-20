(* How a Smelter starts, answers for itself and stops
   (SMELTER.md § Settings, § Boot, § Health, § Stopping): what its health
   reports, what it refuses to start without, that what is recorded while its
   start's reconcile runs is applied then, and that it stops cleanly. With
   it, the names of what a reconcile reports (§ Reconcile), and what a
   Smelter does when its provider has not fetched its model yet (§ The
   embedding provider).

   A case that must see a Smelter refuse stops the world's, since both are
   given one health port; a case that needs the world's starts it again. *)

open Xynolog_conformance
open Support

(* Each setting SMELTER.md § Settings says a Smelter refuses to start
   without: the member of its document taken away, and what its refusal
   names. *)
let required =
  [
    ( "the knowledge base it serves",
      [ "kb" ],
      {|Missing required property: kb\b|} );
    ("the gateway's URL", [ "gatewayUrl" ], "gatewayUrl");
    ("the issuer", [ "identity"; "issuer" ], "issuer");
    ("the Archivist's address", [ "archivist" ], "archivist");
    ( "the vector database's host",
      [ "vectors"; "host" ],
      "Missing required property: host" );
    ( "the variable its token for the vector database is in",
      [ "vectors"; "tokenEnv" ],
      "Missing required property: tokenEnv" );
    ("the embedding provider's kind", [ "embedding"; "type" ], "embedding");
    ( "the embedding provider's model",
      [ "embedding"; "model" ],
      "Missing required property: model" );
    ( "the embedding provider's address",
      [ "embedding"; "baseUrl" ],
      "Missing required property: baseUrl" );
    ("how many PDFs it works on at once", [ "pdfsAtOnce" ], "pdfsAtOnce");
  ]

(* Why images bb38209 fails a case of [required]: a member its document
   lacks is named without the member it is in. *)
let known_without_parent =
  [
    ( [ "embedding"; "type" ],
      "images bb38209 says \"Missing required property: type\" of a document \
       without embedding.type, naming the member without embedding, which \
       holds it" );
  ]

let health_of_itself (w : W.t) =
  started w;
  let health = W.health w in
  let document = w.base.document in
  Check.equal "the members of its health"
    (strs (members_of "SmelterHealth"))
    (strs (List.sort compare (Yojson.Safe.Util.keys health)));
  Check.equal "its status" (str "ok") (Check.at health [ "status" ]);
  Check.equal "the knowledge base it serves" (str w.kb.world.kb.did)
    (Check.at health [ "kb" ]);
  (* Its start has swept the record, which it lists once its stream is open. *)
  Check.equal "its stream" (str "open") (Check.at health [ "stream" ]);
  Check.equal "how it follows the record"
    (`Assoc
       [
         ("sweep", str "done");
         ("stopped", `Int 0);
         ("refused", `Int 0);
         ("streamDrops", `Int 0);
       ])
    (Check.at health [ "following" ]);
  Check.equal "the events it applied" (`Int 0)
    (Check.at health [ "eventsProcessed" ]);
  (* The stamp is the provider's kind and its model, and the two numbers a
     text is cut by: what this world's Smelter is started with. *)
  Check.equal "its index's stamp"
    (`Assoc
       [
         ( "embedding",
           `Assoc
             [
               ("type", Check.at document [ "embedding"; "type" ]);
               ("model", Check.at document [ "embedding"; "model" ]);
             ] );
         ("chunking", Check.at document [ "chunking" ]);
       ])
    (Check.at health [ "index"; "stamp" ]);
  (* The knowledge base holds no resource yet: none was without a cursor,
     and no reconcile was begun. *)
  Check.equal "its reconcile"
    (`Assoc [ ("phase", str "pending") ])
    (Check.at health [ "reconcile" ])

let twelve_counts (w : W.t) =
  started w;
  let id =
    W.created w
      "A Smelter that starts on an index that holds this has nothing to mend.\n"
      plain
  in
  ignore (W.settled w id : Yojson.Safe.t);
  (* Started again, its start has no cursor for the resource: a reconcile,
     which finds the index as the record says. *)
  W.restart w;
  Check.equal "its reconcile"
    (`Assoc
       [
         ("phase", str "done");
         ( "summary",
           only
             [ ("resourcesEligible", count 1); ("resourcesIndexed", count 1) ]
         );
       ])
    (Check.at (W.health w) [ "reconcile" ]);
  (* Archived, it is no resource a later case's reconcile counts. *)
  W.archived w id

let stops (w : W.t) =
  started w;
  Check.equal "its exit code" (`Int 0)
    (match W.stop w with Some code -> `Int code | None -> `Null);
  W.start w;
  (* Started again as it was, it finds the index it made, and keeps it. *)
  Check.equal "whether its start made the index anew" (`Bool false)
    (Check.at (W.health w) [ "index"; "madeAnew" ])

(* A Smelter launched as [launch] refused to start: not with 0, and its
   output says [said]. *)
let refuses (w : W.t) launch said =
  stopped w;
  let code, output = W.refused w launch in
  Check.that "it exits with other than 0" (code <> Some 0);
  Check.that
    (Printf.sprintf "its output matches /%s/:\n%s" said output)
    (Check.matches_re said output)

let without_variable variable (w : W.t) =
  refuses w (W.with_variable w variable None) variable

let without_setting path said (w : W.t) = refuses w (W.changed w path None) said

let not_fetched (w : W.t) =
  (* A Smelter asks its provider how wide the model's vectors are when it
     makes its collections, so it is started on an index that has none. *)
  stopped w;
  W.clear_index w;
  let before = Ollama_embed.mark w.provider in
  Ollama_embed.script w.provider [ Ollama_embed.not_fetched ];
  W.start w;
  match Ollama_embed.since w.provider before with
  | first :: second :: _ ->
      Check.equal "the first request's status" (`Int 404)
        (match first.status with Some s -> `Int s | None -> `Null);
      Check.equal "the second request's status" (`Int 200)
        (match second.status with Some s -> `Int s | None -> `Null);
      Check.equal "the second request's body" first.body second.body;
      Check.equal "its status" (str "ok") (Check.at (W.health w) [ "status" ])
  | asked ->
      failwith
        (Printf.sprintf "the provider was asked %d time(s), and not twice"
           (List.length asked))

let events_counted (w : W.t) =
  started w;
  let id = W.created w "A Smelter counts each event it applies.\n" plain in
  ignore (W.settled w id : Yojson.Safe.t);
  Check.equal "the events it applied" (`Int 1)
    (`Int (W.events_processed (W.applied w 1)));
  (* Started again on an index that was cleared, its reconcile indexes the
     resource: that is work, and no event. *)
  ignore (W.stop w : int option);
  W.clear_index w;
  W.start w;
  let health = W.health w in
  Check.like "its reconcile"
    (`Assoc
       [
         ("phase", str "done");
         ("summary", `Assoc [ ("resourcesEmbedded", count 1) ]);
       ])
    (Check.at health [ "reconcile" ]);
  Check.equal "the events it applied" (`Int 0)
    (Check.at health [ "eventsProcessed" ]);
  (* An index that was cleared states nothing: this start made it. *)
  Check.equal "whether its start made the index anew" (`Bool true)
    (Check.at health [ "index"; "madeAnew" ])

let reconcile_at_work (w : W.t) =
  started w;
  let waited_for = "What its reconcile is kept waiting for." in
  let indexed =
    W.created w
      "Indexed before it stopped, and archived while its reconcile ran.\n" plain
  in
  let missing = W.created w (waited_for ^ "\n") plain in
  ignore (W.settled w indexed : Yojson.Safe.t);
  ignore (W.settled w missing : Yojson.Safe.t);
  (* It is started with one resource's points gone from the index, and its
     provider holds the request that would bring them back. *)
  ignore (W.stop w : int option);
  Qdrant.delete_points_of w.index "resources" missing;
  w.provider.choose <-
    Some
      (fun (request : Ollama_embed.recorded) ->
        if
          List.exists
            (fun text -> Re.execp (Re.compile (Re.str waited_for)) text)
            request.texts
        then Some Ollama_embed.Hold
        else None);
  W.start w ~until:`Serving;
  Run.eventually "its reconcile to ask for the vectors it is kept waiting for"
    30.0 (fun () -> if Ollama_embed.held w.provider = 1 then Some () else None);
  let meanwhile = W.health w in
  Check.equal "its status and its reconcile's phase meanwhile"
    (strs [ "ok"; "running" ])
    (`List [ Check.at meanwhile [ "status" ]; W.phase meanwhile ]);
  (* An event recorded now is applied now, and not when the reconcile has
     ended. *)
  W.archived w indexed;
  Check.equal "its reconcile's phase once the event is applied" (str "running")
    (W.phase (W.applied w 1));
  Ollama_embed.release w.provider;
  Check.like "its reconcile"
    (`Assoc
       [
         ("phase", str "done");
         ("summary", `Assoc [ ("resourcesEmbedded", count 1) ]);
       ])
    (Check.at (W.reconciled w) [ "reconcile" ])

let tests implementations =
  let cases = in_world implementations "boot" in
  let category = [ "a Smelter, as a process" ] in
  cases ~category
    [
      Case.case
        "answers at its health port for itself: for the knowledge base it \
         serves, for its stream, for how it follows the record, for the events \
         it applied, for its reconcile, and for what it holds its index to"
        health_of_itself;
      Case.case
        "reports the twelve counts of a reconcile that had nothing to mend"
        twelve_counts;
      Case.case
        "stops when it is told to, and says nothing is wrong by its exit" stops;
    ]
  @ cases
      ~category:(category @ [ "started without something it needs" ])
      (List.map
         (fun variable ->
           Case.case
             (Printf.sprintf
                "refuses to start without %s, half of its service account, and \
                 names it"
                variable)
             (without_variable variable))
         [ "XYNOLOG_OIDC_CLIENT_ID"; "XYNOLOG_OIDC_CLIENT_SECRET" ]
      @ [
          Case.case
            "refuses to start without its token for the index, and names the \
             variable its settings name"
            (without_variable Qdrant.token_variable);
        ]
      @ List.map
          (fun (what, path, said) ->
            Case.case
              ?known:(List.assoc_opt path known_without_parent)
              (Printf.sprintf
                 "refuses to start without %s, and says which is missing" what)
              (without_setting path said))
          required)
  @ cases ~category
      [
        Case.case
          "asks its embedding provider again when it answers 404 as the \
           Smelter starts: the model is taken to be on its way"
          not_fetched;
        Case.case
          "counts the events it has applied since it started, and not the work \
           of its reconcile"
          events_counted;
        Case.case
          "answers at its health port while its reconcile is still at work, \
           and applies what is recorded meanwhile"
          reconcile_at_work;
      ]
