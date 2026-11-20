(* How a Weaver brings the graph back in line with the
   record (WEAVER.md § What it follows, § What it has applied, § Reconcile,
   § Rebuild): what it missed while it was down, what was lost from the
   graph behind its back, and what it is asked to rebuild. *)

open Xynolog_conformance
open Support

let following health = Check.at health [ "following" ]
let phase health = Check.at health [ "reconcile"; "phase" ]

(* Its reconcile ended done, and found [found] of what its summary counts. *)
let reconciled health found =
  Check.equal "how its reconcile ended" (str "done") (phase health);
  Check.matching "what its reconcile found"
    (List.map (fun (name, n) -> (name, `Int n)) found)
    (summary health)

let followed health =
  Check.matching "how it follows the record"
    [ ("sweep", str "done"); ("stopped", `Int 0); ("refused", `Int 0) ]
    (following health)

let applies_what_it_missed (w : W.t) =
  let before = uploaded w "Before it stopped" in
  W.caught_up w [ before ];
  Check.that "it exits with 0" (W.stop w = Some 0);
  let during = uploaded w "While it was down" in
  let grace = W.person w "grace" in
  let marked = highlighted grace during "quick brown" in
  ignore (highlighted grace before "fox" : string);
  W.start w;
  let health = W.health w in
  (* Its cursors say how far it had got: its first sweep takes what was
     recorded since. *)
  followed health;
  reconciled health [ ("divergent", 0) ];
  as_the_record_says w during;
  as_the_record_says w before;
  Check.that "the graph holds what was marked while it was down"
    (G.annotation w.graph marked <> None)

let replays_the_whole_record (w : W.t) =
  let id = uploaded w "Replayed whole" in
  let grace = W.person w "grace" in
  ignore (highlighted grace id "lazy dog" : string);
  W.caught_up w [ id ];
  let held = by_id (G.annotations_on w.graph id) in
  W.replace w;
  (* A Weaver made again has no cursors: its first sweep takes every log
     from its first event. *)
  let health = W.health w in
  followed health;
  reconciled health [ ("divergent", 0) ];
  Check.equal "the annotations in the graph" (`List held)
    (`List (by_id (G.annotations_on w.graph id)))

let rebuilds_what_was_lost (w : W.t) =
  let id = uploaded w "Damaged" in
  let grace = W.person w "grace" in
  let lost = highlighted grace id "quick" in
  W.caught_up w [ id ];
  let created annotation_id =
    match G.annotation w.graph annotation_id with
    | Some annotation -> Check.at annotation [ "created" ]
    | None -> failwith ("the graph holds no annotation " ^ annotation_id)
  in
  let made = created lost in
  ignore (W.stop w : int option);
  W.lose w lost;
  W.start w;
  let health = W.health w in
  (* Its cursor says the event was taken, so its sweep takes nothing:
     reconcile is what finds it. *)
  Check.matching "how it follows the record"
    [ ("sweep", str "done"); ("stopped", `Int 0) ]
    (following health);
  reconciled health
    [ ("divergent", 1); ("healed", 1); ("healFailures", 0); ("orphaned", 0) ];
  as_the_record_says w id;
  (* Rebuilt from the record, it is dated as the record dates it, not when
     it was rebuilt. *)
  Check.equal "when the rebuilt annotation was made" made (created lost)

let rebuilds_a_cleaned_graph (w : W.t) =
  let linked_to = uploaded w "Cleaned, linked to" in
  let id = uploaded w "Cleaned" in
  let grace = W.person w "grace" in
  let reference = referenced grace id "fox" in
  bound grace id reference [ link linked_to ];
  W.caught_up w [ id; linked_to ];
  ignore (W.stop w : int option);
  W.clean w;
  W.start w;
  let health = W.health w in
  Check.equal "how its reconcile ended" (str "done") (phase health);
  Check.matching "what its reconcile found"
    [ ("healFailures", `Int 0); ("orphaned", `Int 0) ]
    (summary health);
  Check.equal "it healed every divergent resource"
    (`Int (count health "divergent"))
    (`Int (count health "healed"));
  Check.that "it found at least two divergent" (count health "divergent" >= 2);
  as_the_record_says w id;
  as_the_record_says w linked_to;
  Check.equal "what references the resource linked to"
    (`List [ str reference ])
    (`List (List.map str (ids (G.references_to w.graph linked_to))))

(* The reply a rebuild must have: answered on the operation's result. *)
let rebuilt (answer : Archivist_world.answer) =
  let op = Protocol.operation_for "weave:rebuild" in
  Check.that
    (Printf.sprintf "the rebuild is answered on %s, and was on %s: %s" op.result
       answer.channel
       (Yojson.Safe.to_string answer.payload))
    (answer.ok && answer.channel = op.result)

let rebuilds_one_resource (w : W.t) =
  let linked_to = uploaded w "Rebuilt, linked to" in
  let id = uploaded w "Links to the rebuilt" in
  let grace = W.person w "grace" in
  let reference = referenced grace id "fox" in
  bound grace id reference [ link linked_to ];
  let own = highlighted grace linked_to "quick" in
  W.caught_up w [ id; linked_to ];
  rebuilt (W.rebuild ~resource_id:linked_to w);
  as_the_record_says w linked_to;
  as_the_record_says w id;
  Check.equal "the annotations on the rebuilt resource"
    (`List [ str own ])
    (`List (List.map str (ids (G.annotations_on w.graph linked_to))));
  Check.equal "what references the rebuilt resource"
    (`List [ str reference ])
    (`List (List.map str (ids (G.references_to w.graph linked_to))))

let rebuilds_everything (w : W.t) =
  let id = uploaded w "Rebuilt with all" in
  let grace = W.person w "grace" in
  ignore (highlighted grace id "brown" : string);
  W.caught_up w [ id ];
  rebuilt (W.rebuild w);
  as_the_record_says w id

let tests implementations =
  in_world implementations "repair"
    ~category:[ "a Weaver, bringing the graph back" ]
    [
      Case.case
        "applies what was recorded while it was down, when it starts again"
        applies_what_it_missed;
      Case.case
        "with nothing kept, applies the whole record again and ends with the \
         same graph"
        replays_the_whole_record;
      Case.case
        "rebuilds from the record what was lost from the graph behind its \
         back, when it next starts"
        rebuilds_what_was_lost;
      Case.case "rebuilds a whole graph that was cleaned, when it next starts"
        rebuilds_a_cleaned_graph;
      Case.case
        "rebuilds one resource when asked, and leaves another resource's link \
         to it alone"
        rebuilds_one_resource;
      Case.case
        "rebuilds the whole graph when asked, and ends with what the record \
         says"
        rebuilds_everything;
    ]
