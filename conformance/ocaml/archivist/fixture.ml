(* A tree written before (ARCHIVIST.md § The record on disk): the knowledge
   base in the fixture was written by the Archivist's first implementation,
   in TypeScript, and is kept as it was left, save that its profiles are
   under their present names (principal:profiled, profiles.json). An
   Archivist serves it, rebuilds from its log the very views it was left
   with, and goes on from it. *)

open Xynolog_conformance
open Support
open Case

(* From the repository's root, where the harness runs. *)
let fixture = "conformance/ocaml/archivist/fixtures/typescript-tree"

(* The fixture's knowledge base, copied over the world's tree before the
   Archivist first boots. *)
let copied (dirs : A.dirs) =
  List.iter
    (fun (name, bytes) -> A.write_file (dirs.root // name) bytes)
    (A.files_under (fixture // "kb"))

let shared = "75cac8fed07641da9a9423a65108a2ce"
let copy = "84660af034bb49b781d3904fde93f562"

(* The lines of the stream of [id] as the fixture left them: its first
   file, where the world's tree keeps it. *)
let left_lines (w : A.t) id =
  List.filter
    (fun line -> line <> "")
    (String.split_on_char '\n'
       (A.read_file
          (fixture // "kb"
          // A.in_tree w (A.stream_dir w id)
          // A.first_event_file ())))

let in_order =
  "the tree is judged as it was left before the last case goes on from it"

let rebuilt (w : A.t) =
  let left = A.files_under (fixture // "state") in
  Check.equal "the files left" (`Int 7) (`Int (List.length left));
  let rebuilt = A.files_under w.dirs.state_home in
  let names files = strs (sorted (List.map fst files)) in
  Check.equal "the files rebuilt" (names left) (names rebuilt);
  List.iter
    (fun (name, bytes) ->
      Check.equal name (str bytes) (str (List.assoc name rebuilt)))
    left

let served (w : A.t) =
  let reader = A.person w "reader" in
  let listed =
    Check.list
      (A.ask reader "browse:resources-requested" (`Assoc []))
      [ "resources" ]
  in
  Check.equal "the resources"
    (`List
       (sorted
          [
            `List [ str shared; str "Shared"; `Bool false ];
            `List [ str copy; str "A copy"; `Bool true ];
          ]))
    (`List
       (sorted
          (List.map
             (fun r ->
               `List
                 [
                   Check.at r [ "@id" ];
                   Check.at r [ "name" ];
                   Check.at r [ "archived" ];
                 ])
             listed)));

  let described = A.ask reader "browse:resource-requested" (on shared) in
  Check.equal "its entity types"
    (`List [ str "Event" ])
    (Check.at described [ "resource"; "entityTypes" ]);
  Check.that "its dateModified is a string"
    (match Check.at described [ "resource"; "dateModified" ] with
    | `String _ -> true
    | _ -> false);
  Check.equal "its lastSequence"
    (Check.at (view w shared) [ "lastSequence" ])
    (Check.at described [ "lastSequence" ]);
  Check.equal "its annotations" (`Int 1)
    (Check.at (Option.get (A.annotations w shared)) [ "total" ]);

  let replay =
    A.http w ~route:"/events/{resourceId}" "GET"
      (Printf.sprintf "/events/%s?fromSequence=1" shared)
  in
  Check.equal "its replay"
    (`List (List.map Yojson.Safe.from_string (left_lines w shared)))
    (Check.at (Option.get replay.json) [ "events" ]);

  let content =
    A.http w ~route:"/resources/{id}/content" "GET"
      (A.resource_path shared "/content")
  in
  Check.equal "its content"
    (str (A.read_file (fixture // "kb" // "shared" // "résumé 📚.md")))
    (str content.text);
  Check.that "the entity types hold Élan"
    (List.mem (str "Élan")
       (Check.list
          (A.ask reader "browse:entity-types-requested" (`Assoc []))
          [ "entityTypes" ]))

let gone_on_from (w : A.t) =
  let before = List.length (A.stored w shared) in
  Check.equal "Person seeded" (`Int 1)
    (`Int
       (List.length
          (List.filter
             (fun e ->
               of_type "frame:entity-type-added" e
               && Check.at e [ "payload"; "entityType" ] = str "Person")
             (A.stored w A.system))));

  let editor = A.person w "editor" in
  ignore (A.ask editor "mark:archive" (on shared) : Yojson.Safe.t);
  let events = A.stored w shared in
  Check.equal "its events" (`Int (before + 1)) (`Int (List.length events));
  let latest = last "its events" events in
  Check.equal "the last's type" (str "mark:archived")
    (Check.at latest [ "type" ]);
  Check.equal "the last's sequence number"
    (`Int (before + 1))
    (Check.at latest [ "metadata"; "sequenceNumber" ]);
  let view = view w shared in
  Check.equal "its view's lastSequence"
    (`Int (before + 1))
    (Check.at view [ "lastSequence" ]);
  Check.equal "its view's archived" (`Bool true)
    (Check.at view [ "resource"; "archived" ]);
  (* What was there is as it was. *)
  Check.equal "the lines left"
    (strs (left_lines w shared))
    (strs (List.filteri (fun at _ -> at < before) (A.stream_lines w shared)))

let tests implementations =
  with_archivist implementations ~git_sync:false ~before:copied
    "a tree written before"
    [
      case ~solo:in_order
        "is rebuilt from its log into the views it was left with, byte for byte"
        rebuilt;
      case ~solo:in_order "is served as it was recorded" served;
      case ~solo:in_order
        "is gone on from: each stream numbered on from where it was left, and \
         nothing seeded twice"
        gone_on_from;
    ]
