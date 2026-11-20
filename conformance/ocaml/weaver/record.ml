(* The graph a Weaver builds from the record (WEAVER.md
   § The fold, § What the graph holds, § Reconcile): what each resource and
   annotation is in it, read back by the suite's own reader, and whether the
   Weaver's own check of the graph against the record finds anything to
   repair in a graph it has just built.

   Nothing here states a view: the record is the real Archivist's, and the
   graph is held to what that Archivist's view holds. *)

open Xynolog_conformance
open Support

let says_what_the_record_says (w : W.t) =
  let linked_to = uploaded w "Linked to" in
  let id = uploaded w "Annotated" in
  let grace = W.person w "grace" in
  let worker = Archivist_world.worker w.kb "model" in
  (* A person's highlight; a reference she then binds to another resource
     and tags; and work she asked a worker for, which the worker sends and
     the record says she asked for. *)
  let marked = highlighted grace id "quick brown" in
  let reference = referenced grace id "fox" in
  bound grace id reference [ tag "Animal"; link linked_to ];
  let worked = whole id "lazy dog" in
  delegated w grace worker id worked;
  W.caught_up w [ id; linked_to ];

  (* Every annotation in the graph is what the graph is to hold of the one
     the record's view holds. *)
  let viewed = viewed w id in
  Check.equal "the annotations the record's view holds"
    (`List
       (List.map str (List.sort compare [ marked; reference; id_of worked ])))
    (`List (List.map str (ids viewed)));
  Check.equal "the annotations in the graph"
    (`List (by_id (List.map Graph_codec.intended viewed)))
    (`List (by_id (G.annotations_on w.graph id)));

  (* By the one who asked, not the one who sent it; with the software that
     made it. *)
  let delegated_in_graph =
    match G.annotation w.graph (id_of worked) with
    | Some annotation -> annotation
    | None -> failwith "the graph holds no delegated annotation"
  in
  Check.matching "who the delegated annotation is by"
    [ ("@type", str "Person"); ("@id", str grace.did) ]
    (Check.at delegated_in_graph [ "creator" ]);
  Check.matching "the software that made it"
    [ ("@type", str "Software"); ("@id", str worker.did) ]
    (Check.at delegated_in_graph [ "generator" ]);
  Check.equal "who the record says sent it" (str worker.did)
    (Check.at
       (List.find
          (fun event ->
            Protocol.at event [ "payload"; "annotation"; "id" ]
            = Some (str (id_of worked)))
          (W.recorded w id "mark:added"))
       [ "userId" ]);

  (* A link and a tag added after the annotation was made are read back,
     dated when the record says the body changed. *)
  let bound_in_graph =
    match G.annotation w.graph reference with
    | Some annotation -> annotation
    | None -> failwith "the graph holds no reference"
  in
  Check.equal "the reference's body"
    (`List [ tag "Animal"; link linked_to ])
    (Check.at bound_in_graph [ "body" ]);
  Check.equal "when its body changed"
    (Check.at (List.hd (W.recorded w id "mark:body-updated")) [ "timestamp" ])
    (Check.at bound_in_graph [ "modified" ]);
  Check.equal "what references the resource linked to"
    (`List [ str reference ])
    (`List (List.map str (ids (G.references_to w.graph linked_to))));

  (* The link is a connection of the resource it is on, to the resource it
     is to; and a resource is found by its name. *)
  let links = G.links_from w.graph id in
  Check.equal "where its links go"
    (`List [ str linked_to ])
    (`List (List.map (fun (to_, _) -> str (G.id_of to_)) links));
  Check.equal "what links there"
    (`List [ str reference ])
    (`List (List.map str (snd (List.hd links))));
  Check.that "a resource is found by its name"
    (List.mem linked_to (G.resources_named w.graph "Linked to"));

  (* The resource is dated and attributed as its event records. *)
  let created = List.hd (W.recorded w id "yield:created") in
  (match G.resource w.graph id with
  | None -> failwith "the graph holds no resource it was given"
  | Some resource ->
      Check.matching "the resource in the graph"
        [
          ("name", str "Annotated");
          ("archived", `Bool false);
          ("dateCreated", Check.at created [ "timestamp" ]);
          ("wasAttributedTo", Check.at created [ "payload"; "wasAttributedTo" ]);
        ]
        resource);

  (* Reconcile runs when a Weaver starts. In a graph this Weaver built, it
     finds nothing to repair. *)
  W.restart w;
  let health = W.health w in
  Check.matching "how it follows the record"
    [ ("sweep", str "done"); ("stopped", `Int 0); ("refused", `Int 0) ]
    (Check.at health [ "following" ]);
  Check.equal "how its reconcile ended" (str "done")
    (Check.at health [ "reconcile"; "phase" ]);
  Check.matching "what its reconcile found"
    [
      ("divergent", `Int 0);
      ("healed", `Int 0);
      ("healFailures", `Int 0);
      ("orphaned", `Int 0);
    ]
    (summary health)

let ask_of (who : Archivist_world.client) operation members =
  ignore (Archivist_world.ask who operation (`Assoc members) : Yojson.Safe.t)

let strs items = `List (List.map str items)

let follows_archiving_and_types (w : W.t) =
  let id = uploaded w "Followed" in
  let grace = W.person w "grace" in
  let kept = highlighted grace id "quick" in
  let removed = highlighted grace id "lazy dog" in
  ask_of grace "mark:delete"
    [ ("resourceId", str id); ("annotationId", str removed) ];
  (* An entity type joins the knowledge base's list before a resource is
     given it: the Archivist refuses one that has not. *)
  ask_of grace "frame:add-entity-type" [ ("tag", str "Fable") ];
  ask_of grace "frame:add-entity-type" [ ("tag", str "Pangram") ];
  ask_of grace "mark:update-entity-types"
    [
      ("resourceId", str id);
      ("currentEntityTypes", strs []);
      ("updatedEntityTypes", strs [ "Fable"; "Pangram" ]);
    ];
  ask_of grace "mark:update-entity-types"
    [
      ("resourceId", str id);
      ("currentEntityTypes", strs [ "Fable"; "Pangram" ]);
      ("updatedEntityTypes", strs [ "Pangram" ]);
    ];
  ask_of grace "mark:archive" [ ("resourceId", str id) ];
  W.caught_up w [ id ];
  Check.equal "the annotations left on it"
    (`List [ str kept ])
    (`List (List.map str (ids (G.annotations_on w.graph id))));
  (match G.resource w.graph id with
  | None -> failwith "the graph holds no resource it was given"
  | Some resource ->
      Check.matching "the resource, archived"
        [ ("archived", `Bool true); ("entityTypes", strs [ "Pangram" ]) ]
        resource);
  Check.that "one resource has the entity type Pangram"
    (List.mem ("Pangram", 1) (G.entity_type_counts w.graph));
  ask_of grace "mark:unarchive" [ ("resourceId", str id) ];
  W.caught_up w [ id ];
  match G.resource w.graph id with
  | None -> failwith "the graph holds no resource it was given"
  | Some resource ->
      Check.equal "whether it is archived" (`Bool false)
        (Check.at resource [ "archived" ])

let says_how_far_it_has_got (w : W.t) =
  let id = uploaded w "Signalled" in
  let grace = W.person w "grace" in
  List.iter
    (fun exact -> ignore (highlighted grace id exact : string))
    [ "quick"; "brown"; "fox" ];
  W.caught_up w [ id ];
  let signalled = List.map snd (W.applied_signals ~resource_id:id w) in
  Check.equal "how far it last said it took the stream"
    (`Int (W.last w id))
    (`Int (List.nth signalled (List.length signalled - 1)));
  Check.that "it says so in order" (signalled = List.sort compare signalled);
  Check.that "it says each once"
    (List.length (List.sort_uniq compare signalled) = List.length signalled)

let tests implementations =
  in_world implementations "record"
    ~category:[ "the graph a Weaver builds" ]
    [
      Case.case
        "says what the record says, and is one its own reconcile finds nothing \
         wrong with"
        says_what_the_record_says;
      Case.case
        "follows a resource through archiving, its entity types and an \
         annotation's removal"
        follows_archiving_and_types;
      Case.case
        "says how far it has taken each resource's stream, in order, to its \
         last event, and never past what the record holds"
        says_how_far_it_has_got;
    ]
