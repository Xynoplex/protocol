(* reconcile.test.ts: what a Smelter finds and mends when it starts
   (SMELTER.md § Reconcile). Each case stops the Smelter, changes the index,
   the record or the anchored-text store behind its back, starts it again,
   and reads what the two stores then hold, what its embedding provider was
   asked, and what its health reports of the pass. With it, that the work of
   a reconcile is no event (§ Health), and that an index another cut made is
   made again (§ Boot).

   The work a start plans is stated, difference by difference, in
   specs/src/smelter/reconcile-cases.json, which each implementation runs
   through its own planner. These cases hold that the work is then done. *)

open Xynolog_conformance
open Support

(* A PDF a case has recorded: its resource, and the checksum of its bytes,
   which its anchored text is stored under. *)
type recorded_pdf = { id : string; checksum : string }

(* Every PDF the cases so far have recorded. None is changed or archived
   afterwards, so each has points of its bytes, and one entry in the
   anchored-text store. *)
let pdfs : recorded_pdf list ref = ref []

let indexed what decided =
  Check.like what (`Assoc [ ("outcome", str "indexed") ]) decided

(* Record a resource of plain text, and wait until the Smelter has indexed
   it: its id. *)
let indexed_text (w : W.t) name text =
  let id = W.created w text plain ~name in
  indexed "the decision" (W.settled w id);
  id

(* Record a PDF of one page that draws [lines], and wait until the Smelter
   has indexed it. *)
let indexed_pdf (w : W.t) name lines =
  let bytes = Pdf.of_pages [ lines ] in
  let checksum = Archivist_world.sha256 bytes in
  let id = W.created w bytes pdf ~name in
  Check.like "the decision"
    (`Assoc [ ("outcome", str "indexed"); ("contentChecksum", str checksum) ])
    (W.settled w id);
  { id; checksum }

(* Record an annotation that quotes [exact], and wait until the index holds
   its point: its id. A Smelter signals nothing of an annotation. *)
let indexed_annotation (w : W.t) resource_id exact =
  let id = W.annotated w resource_id exact in
  ignore
    (Run.eventually ("the index to hold the point of the annotation " ^ id) 20.0
       (fun () -> Qdrant.annotation_point w.index id)
      : Yojson.Safe.t);
  id

(* An annotation's point, with its vector. The case fails here when the
   index holds none. *)
let point_of_annotation (w : W.t) annotation_id =
  match Qdrant.annotation_point ~vectors:true w.index annotation_id with
  | Some point -> point
  | None ->
      failwith ("the index holds no point of the annotation " ^ annotation_id)

let held (w : W.t) id = Qdrant.points_of ~vectors:true w.index "resources" id

(* The texts of each request the provider was given since [mark]. *)
let requests (w : W.t) mark =
  List.map
    (fun (r : Ollama_embed.recorded) -> r.texts)
    (Ollama_embed.since w.provider mark)

let asked (w : W.t) mark = List.concat (requests w mark)

let nothing_asked (w : W.t) mark =
  Check.equal "the requests the provider was given" (`List [])
    (`List (List.map strs (requests w mark)))

let asked_only (w : W.t) mark texts =
  Check.equal "the requests the provider was given"
    (`List (List.map strs texts))
    (`List (List.map strs (requests w mark)))

let entity_types_of point =
  Check.at (payload point) [ Qdrant.payload_member "resources" "entityTypes" ]

let annotation_entity_types point =
  Check.at (payload point) [ Qdrant.payload_member "annotations" "entityTypes" ]

let cleared_index (w : W.t) =
  let texts =
    [
      "The first of three texts is quoted by an annotation, in these words and \
       no others.";
      "The second of three texts is quoted by nothing.";
      "The third of three texts is the last of them.";
    ]
  in
  let ids =
    List.mapi
      (fun at text ->
        indexed_text w (Printf.sprintf "Cleared, text %d" (at + 1)) text)
      texts
  in
  (* A resource whose media type has no text is of the catalog, and is none
     of the resources a Smelter indexes. *)
  let image = W.created w png "image/png" ~name:"Cleared, a picture" in
  ignore (W.settled w image : Yojson.Safe.t);
  let quote = "in these words and no others" in
  let annotation = indexed_annotation w (List.hd ids) quote in
  let held_before = List.map (held w) ids in
  let held_of_annotation = point_of_annotation w annotation in
  ignore (W.stop w : int option);
  W.clear_index w;
  let before = Ollama_embed.mark w.provider in
  W.start w;
  (* Nothing was recorded before this case: the three texts are every
     resource of the catalog that has text, and the annotation is every
     annotation. *)
  expect_only (summary_of w)
    [
      ("resourcesEmbedded", count 3);
      ("annotationsEmbedded", count 1);
      ("resourcesEligible", count 3);
      ("resourcesIndexed", count 3);
    ];
  List.iter2
    (fun id was -> same "a text's points" (held w id) was)
    ids held_before;
  same "the annotation's point"
    [ point_of_annotation w annotation ]
    [ held_of_annotation ];
  Check.equal "how many points the resources have" (`Int 3)
    (`Int (Qdrant.count w.index "resources"));
  Check.equal "how many points the annotations have" (`Int 1)
    (`Int (Qdrant.count w.index "annotations"));
  (* The provider is asked for each text once, and for the first text before
     the words quoted from it: a resource is indexed before an annotation on
     it. What else it is asked, to learn how wide its vectors are, is not
     this case's. *)
  let asked = asked w before in
  let wanted = texts @ [ quote ] in
  Check.equal "the texts the provider was asked for"
    (strs (List.sort compare wanted))
    (strs (List.sort compare (List.filter (fun t -> List.mem t wanted) asked)));
  let index_of text =
    let rec find at = function
      | [] -> failwith ("the provider was not asked for " ^ text)
      | t :: rest -> if t = text then at else find (at + 1) rest
    in
    find 0 asked
  in
  Check.that "the first text is asked for before the words quoted from it"
    (index_of (List.hd texts) < index_of quote)

let points_taken_out (w : W.t) =
  let text =
    "The points of this text are taken out of the index while the Smelter is \
     stopped."
  in
  let emptied = indexed_text w "Emptied" text in
  let other =
    indexed_text w "Beside the emptied"
      "The points of this text are left where they are."
  in
  let held_emptied = held w emptied in
  let held_other = held w other in
  ignore (W.stop w : int option);
  Qdrant.delete_points_of w.index "resources" emptied;
  Check.equal "the emptied resource's points" (`List [])
    (`List (Qdrant.points_of w.index "resources" emptied));
  let before = Ollama_embed.mark w.provider in
  W.start w;
  expect_only (summary_of w) [ ("resourcesEmbedded", count 1) ];
  (* The index was there when it started, so it asks nothing to learn how
     wide a vector is: its one request is for the text whose points were
     gone. *)
  asked_only w before [ [ text ] ];
  same "the emptied resource's points" (held w emptied) held_emptied;
  same "the other resource's points" (held w other) held_other

let bytes_changed (w : W.t) =
  let id =
    indexed_text w "Changed"
      "What this resource said before the Smelter stopped."
  in
  let changed =
    "What this resource says now, which was written while the Smelter was \
     stopped."
  in
  ignore (W.stop w : int option);
  let checksum = W.updated w id changed in
  let before = Ollama_embed.mark w.provider in
  W.start w;
  expect_only (summary_of w) [ ("resourcesEmbedded", count 1) ];
  asked_only w before [ [ changed ] ];
  let points = held w id in
  let named = Qdrant.payload_member "resources" in
  Check.like "its points' payloads"
    (`List
       [
         `Assoc
           [
             (named "text", str changed); (named "contentChecksum", str checksum);
           ];
       ])
    (`List (List.map payload points));
  Check.that "its points have the new text's vector"
    (List.for_all (fun point -> embeds point changed) points);
  (* It says what it decided of the new bytes, as it does of any bytes it
     indexes. *)
  Check.like "the second decision"
    (`Assoc [ ("outcome", str "indexed"); ("contentChecksum", str checksum) ])
    (W.settled w ~nth:2 id)

let given_entity_type (w : W.t) =
  let id =
    indexed_text w "Given an entity type"
      "A text that is given an entity type while the Smelter is stopped."
  in
  let held_before = held w id in
  Check.equal "its points' entity types"
    (`List [ `List [] ])
    (`List (List.map entity_types_of held_before));
  ignore (W.stop w : int option);
  W.tagged w id "Treaty";
  let before = Ollama_embed.mark w.provider in
  W.start w;
  expect_only (summary_of w) [ ("resourcesRestamped", count 1) ];
  nothing_asked w before;
  (* The same points, with the same vectors: only the entity types they
     carry are other than they were. *)
  same "its points" (held w id)
    (List.map (with_entity_types [ "Treaty" ]) held_before)

let anchored_text_lost (w : W.t) =
  let pdf =
    indexed_pdf w "Lost its anchored text"
      [
        "A page whose text is read from the page itself.";
        "Where each run of it is on the page is stored beside it.";
      ]
  in
  pdfs := !pdfs @ [ pdf ];
  let entry =
    match W.anchored_entry w pdf.checksum with
    | Some entry -> entry
    | None -> failwith "the anchored-text store holds no entry of the PDF"
  in
  let held_before = held w pdf.id in
  ignore (W.stop w : int option);
  W.lose_anchored_entry w pdf.checksum;
  let before = Ollama_embed.mark w.provider in
  W.start w;
  expect_only (summary_of w) [ ("resourcesReanchored", count 1) ];
  Check.equal "the store's entry" entry
    (match W.anchored_entry w pdf.checksum with Some e -> e | None -> `Null);
  nothing_asked w before;
  same "its points" (held w pdf.id) held_before;
  (* Its points were not made again, so nothing is decided of it a second
     time. *)
  W.unsettled w ~nth:2 pdf.id

let archived_while_stopped (w : W.t) =
  let archived =
    indexed_text w "Archived"
      "A text that is archived while the Smelter is stopped, with what is \
       quoted from it."
  in
  let on_archived = indexed_annotation w archived "what is quoted from it" in
  let kept =
    indexed_text w "Beside the archived"
      "A text that stays, with the words an annotation quotes from it."
  in
  let on_kept = indexed_annotation w kept "the words an annotation quotes" in
  let held_kept = held w kept in
  let held_on_kept = point_of_annotation w on_kept in
  (* A start that finds nothing to mend reports what the knowledge base
     holds before the resource is archived. *)
  W.restart w;
  let was = summary_of w in
  expect_only was [];
  ignore (W.stop w : int option);
  W.archived w archived;
  W.start w;
  let summary = summary_of w in
  expect_only summary
    [
      ("resourceVectorsDeleted", count 1); ("annotationVectorsDeleted", count 1);
    ];
  (* An archived resource is none of the catalog's: one fewer of its
     resources has text, and one fewer has points. *)
  List.iter
    (fun name ->
      Check.equal name
        (`Int (summary_count was name - 1))
        (`Int (summary_count summary name)))
    [ "resourcesEligible"; "resourcesIndexed" ];
  Check.equal "the archived resource's points" (`List [])
    (`List (Qdrant.points_of w.index "resources" archived));
  Check.equal "the points of the annotations on it" (`List [])
    (`List (Qdrant.points_of w.index "annotations" archived));
  Check.that "the annotation on it has no point"
    (Qdrant.annotation_point w.index on_archived = None);
  same "the kept resource's points" (held w kept) held_kept;
  same "the point of the annotation on the kept"
    [ point_of_annotation w on_kept ]
    [ held_on_kept ]

let body_tagged_while_stopped (w : W.t) =
  let quote = "words whose annotation is given an entity type" in
  let id =
    indexed_text w "Tagged in its body while stopped"
      (Printf.sprintf "A text with %s while the Smelter is stopped." quote)
  in
  let annotation = indexed_annotation w id quote in
  let held_before = point_of_annotation w annotation in
  Check.equal "its point's entity types" (`List [])
    (annotation_entity_types held_before);
  ignore (W.stop w : int option);
  W.body_tagged w id annotation [ "Treaty" ];
  let before = Ollama_embed.mark w.provider in
  W.start w;
  expect_only (summary_of w) [ ("annotationsRestamped", count 1) ];
  nothing_asked w before;
  (* The same point, with the same vector: only the entity types it carries
     are other than they were. *)
  let point = point_of_annotation w annotation in
  same "its point" [ with_entity_types [] point ] [ held_before ];
  Check.equal "its point's entity types" (strs [ "Treaty" ])
    (annotation_entity_types point)

let annotated_while_stopped (w : W.t) =
  let id =
    indexed_text w "Annotated while stopped"
      "A text with words quoted before the Smelter stopped, and other words \
       quoted after."
  in
  let removed =
    indexed_annotation w id "words quoted before the Smelter stopped"
  in
  let quote = "other words quoted after" in
  ignore (W.stop w : int option);
  let added = W.annotated w id quote in
  W.unannotated w id removed;
  let before = Ollama_embed.mark w.provider in
  W.start w;
  expect_only (summary_of w)
    [ ("annotationsEmbedded", count 1); ("annotationVectorsDeleted", count 1) ];
  asked_only w before [ [ quote ] ];
  let point = point_of_annotation w added in
  let named = Qdrant.payload_member "annotations" in
  Check.like "its point's payload"
    (`Assoc
       [
         (named "annotationId", str added);
         (named "resourceId", str id);
         (named "text", str quote);
       ])
    (payload point);
  Check.that "its point has its quotation's vector" (embeds point quote);
  Check.that "the removed annotation has no point"
    (Qdrant.annotation_point w.index removed = None)

let store_cleared (w : W.t) =
  pdfs :=
    !pdfs
    @ [
        indexed_pdf w "Cleared, a first document"
          [
            "The first of two documents recorded before the store was cleared.";
          ];
        indexed_pdf w "Cleared, a second document"
          [
            "The second of two documents recorded before the store was cleared.";
          ];
      ];
  let files = W.anchored_files w in
  let entry_of { checksum; _ } =
    match W.anchored_entry w checksum with
    | Some e -> e
    | None -> failwith ("the anchored-text store holds no entry of " ^ checksum)
  in
  let entries = List.map entry_of !pdfs in
  let held_before = List.map (fun { id; _ } -> held w id) !pdfs in
  ignore (W.stop w : int option);
  W.clear_anchored_text w;
  Check.equal "the store's files" (`List []) (strs (W.anchored_files w));
  let before = Ollama_embed.mark w.provider in
  W.start w;
  (* Every PDF still has points of its bytes, so none is indexed again: each
     has its anchored text derived again, and that is all the pass does. *)
  expect_only (summary_of w)
    [ ("resourcesReanchored", count (List.length !pdfs)) ];
  nothing_asked w before;
  Check.equal "the store's entries" (`List entries)
    (`List (List.map entry_of !pdfs));
  Check.equal "the store's files" (strs files) (strs (W.anchored_files w));
  List.iter2
    (fun { id; _ } was -> same "a PDF's points" (held w id) was)
    !pdfs held_before

let reconcile_no_event (w : W.t) =
  let id =
    indexed_text w "Indexed again, and not counted"
      "A text whose points are made again by a reconcile, which is no event."
  in
  ignore (W.stop w : int option);
  Qdrant.delete_points_of w.index "resources" id;
  W.start w;
  expect_only (summary_of w) [ ("resourcesEmbedded", count 1) ];
  Check.equal "the events it applied" (`Int 0)
    (`Int (W.events_processed (W.health w)));
  (* What is recorded next is an event, and is counted. *)
  ignore
    (indexed_text w "Recorded after, and counted"
       "A text recorded after the Smelter started, which is an event."
      : string);
  Check.equal "the events it applied" (`Int 1)
    (`Int (W.events_processed (W.applied w 1)))

let remembers_skipping (w : W.t) =
  let id = W.created w " \n\t\n" plain ~name:"Nothing but white space" in
  Check.like "the decision"
    (`Assoc [ ("outcome", str "skipped"); ("reason", str "empty") ])
    (W.settled w id);
  Check.equal "its points" (`List [])
    (`List (Qdrant.points_of w.index "resources" id));
  ignore (W.stop w : int option);
  let before = Ollama_embed.mark w.provider in
  W.start w;
  (* Its media type has text and it has no points, and still it is not set
     out to be indexed: nothing is skipped again, and nothing is signalled
     of it again. *)
  expect_only (summary_of w) [];
  nothing_asked w before;
  Check.equal "how many decisions of it were signalled" (`Int 1)
    (`Int (List.length (W.settled_signals ~resource_id:id w)))

let other_cut (w : W.t) =
  let text =
    "A text that is indexed, and indexed again by a Smelter that cuts a text \
     by other numbers."
  in
  let id = indexed_text w "Cut by other numbers" text in
  let quote = "indexed again by a Smelter" in
  let annotation = indexed_annotation w id quote in
  let held_before = held w id in
  let held_of_annotation = point_of_annotation w annotation in
  let document = w.base.document in
  let cut = `Assoc [ ("chunkSize", `Int 256); ("overlap", `Int 32) ] in
  ignore (W.stop w : int option);
  let before = Ollama_embed.mark w.provider in
  W.start w ~launch:(W.changed w [ "chunking" ] (Some cut));
  (* The index said another cut made it, and the Smelter's health says it
     made the index again, and by what. *)
  Check.equal "its index"
    (`Assoc
       [
         ( "stamp",
           `Assoc
             [
               ( "embedding",
                 `Assoc
                   [
                     ("type", Check.at document [ "embedding"; "type" ]);
                     ("model", Check.at document [ "embedding"; "model" ]);
                   ] );
               ("chunking", cut);
             ] );
         ("madeAnew", `Bool true);
       ])
    (Check.at (W.health w) [ "index" ]);
  (* Every resource whose media type has text is set out to be indexed
     again, and each is indexed or skipped. *)
  let summary = summary_of w in
  let skipped =
    List.fold_left
      (fun sum (_, n) -> sum + Yojson.Safe.Util.to_int n)
      0
      (Yojson.Safe.Util.to_assoc (Check.at summary [ "resourcesSkipped" ]))
  in
  Check.equal "how many it indexed or skipped"
    (`Int (summary_count summary "resourcesEligible"))
    (`Int (summary_count summary "resourcesEmbedded" + skipped));
  Check.equal "how many were unavailable, and how many failed"
    (`List [ `Int 0; `Int 0 ])
    (`List
       [
         `Int (summary_count summary "resourcesUnavailable");
         `Int (summary_count summary "resourcesFailed");
       ]);
  Check.that "it embedded annotations"
    (summary_count summary "annotationsEmbedded" > 0);
  let asked_now = asked w before in
  Check.that "the provider was asked for the text" (List.mem text asked_now);
  Check.that "the provider was asked for the quotation"
    (List.mem quote asked_now);
  (* A text this short is one chunk by either cut: its point is as it was,
     and so is the annotation's. *)
  same "its points" (held w id) held_before;
  same "the annotation's point"
    [ point_of_annotation w annotation ]
    [ held_of_annotation ];
  (* Started as it was before, it finds an index the other cut made, and
     makes it again: the index is as this world's Smelter makes one. *)
  ignore (W.stop w : int option);
  let again = Ollama_embed.mark w.provider in
  W.start w;
  Check.that "the provider was asked for the text again"
    (List.mem text (asked w again));
  Check.equal "how many resources it embedded"
    (`Int (summary_count summary "resourcesEmbedded"))
    (`Int (summary_count (summary_of w) "resourcesEmbedded"))

let tests implementations =
  in_world implementations "reconcile"
    ~category:
      [ "a Smelter, starting after what it keeps was changed behind it" ]
    [
      Case.case "fills again, whole, an index that was cleared" cleared_index;
      Case.case
        "indexes again a resource whose points were taken out behind it, and \
         touches no other"
        points_taken_out;
      Case.case
        "indexes again, under the new checksum, a resource whose bytes changed \
         while it was stopped"
        bytes_changed;
      Case.case
        "rewrites the entity types on the points of a resource that was given \
         one while it was stopped, and embeds nothing"
        given_entity_type;
      Case.case
        "derives again the anchored text of a PDF whose entry was lost, and \
         neither embeds nor touches its points"
        anchored_text_lost;
      Case.case
        "takes out the points of a resource that was archived while it was \
         stopped, and of the annotations on it"
        archived_while_stopped;
      Case.case
        "rewrites the entity types on the point of an annotation whose body \
         was given a tag while it was stopped, and embeds nothing"
        body_tagged_while_stopped;
      Case.case
        "indexes an annotation added while it was stopped, and takes out the \
         point of one removed"
        annotated_while_stopped;
      Case.case
        "fills again an anchored-text store that was cleared, deriving again \
         the anchored text of every PDF"
        store_cleared;
      Case.case
        "counts none of the work of its reconcile among the events it has \
         applied"
        reconcile_no_event;
      Case.case
        "does not read again, or decide again, a text it remembers skipping"
        remembers_skipping;
      Case.case
        "makes its index again, and indexes everything again, when it is \
         started to cut a text by other numbers"
        other_cut;
    ]
