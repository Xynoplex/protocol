(* A tree written before (ARCHIVIST.md § The record on disk): the knowledge
   base in the fixture was written by the Archivist's first implementation,
   in TypeScript, and is kept as it was left, save that its profiles are
   under their present names (principal:profiled, profiles.json). An
   Archivist serves it, rebuilds from its log the very views it was left
   with, and goes on from it. *)

open Xynolog_conformance
open Support
open Case

let ( // ) = Filename.concat

(* From the repository's root, where the harness runs. *)
let fixture = "conformance/ocaml/archivist/fixtures/typescript-tree"

(* Every file under [dir], by its path from there, with its bytes. *)
let rec files ?(from = "") dir =
  List.concat_map
    (fun entry ->
      let path = dir // entry in
      let name = if from = "" then entry else from ^ "/" ^ entry in
      if Sys.is_directory path then files ~from:name path
      else [ (name, A.read_file path) ])
    (List.sort compare (Array.to_list (Sys.readdir dir)))

(* The fixture's knowledge base, copied over the world's tree before the
   Archivist first boots. *)
let copied (dirs : A.dirs) =
  List.iter
    (fun (name, bytes) ->
      let path = dirs.root // name in
      A.make_dirs (Filename.dirname path);
      Out_channel.with_open_bin path (fun out -> output_string out bytes))
    (files (fixture // "kb"))

let shared = "75cac8fed07641da9a9423a65108a2ce"
let copy = "84660af034bb49b781d3904fde93f562"

(* The lines of the stream of [id] as the fixture left them: its first
   file, where the world's tree keeps it. *)
let left_lines (w : A.t) id =
  let root = w.dirs.root ^ "/" in
  let dir = A.stream_dir w id in
  let from_root =
    String.sub dir (String.length root) (String.length dir - String.length root)
  in
  List.filter
    (fun line -> line <> "")
    (String.split_on_char '\n'
       (A.read_file
          (fixture // "kb" // from_root
          // A.event_file_name (A.layout ()).event_file.first)))

let in_order =
  "the tree is judged as it was left before the last case goes on from it"

let rebuilt (w : A.t) =
  let left = files (fixture // "state") in
  Check.equal "the files left" (`Int 7) (`Int (List.length left));
  let rebuilt = A.state_files w in
  let names files =
    `List (List.sort compare (List.map (fun (name, _) -> str name) files))
  in
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
  let sorted items = List.sort compare items in
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

  let described =
    A.ask reader "browse:resource-requested"
      (`Assoc [ ("resourceId", str shared) ])
  in
  Check.equal "its entity types"
    (`List [ str "Event" ])
    (Check.at described [ "resource"; "entityTypes" ]);
  Check.that "its dateModified is a string"
    (match Check.at described [ "resource"; "dateModified" ] with
    | `String _ -> true
    | _ -> false);
  Check.equal "its lastSequence"
    (Check.at (Option.get (A.view w shared)) [ "lastSequence" ])
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
               Check.at e [ "type" ] = str "frame:entity-type-added"
               && Check.at e [ "payload"; "entityType" ] = str "Person")
             (A.stored w A.system))));

  let editor = A.person w "editor" in
  ignore
    (A.ask editor "mark:archive" (`Assoc [ ("resourceId", str shared) ])
      : Yojson.Safe.t);
  let events = A.stored w shared in
  Check.equal "its events" (`Int (before + 1)) (`Int (List.length events));
  let last = List.hd (List.rev events) in
  Check.equal "the last's type" (str "mark:archived") (Check.at last [ "type" ]);
  Check.equal "the last's sequence number"
    (`Int (before + 1))
    (Check.at last [ "metadata"; "sequenceNumber" ]);
  let view = Option.get (A.view w shared) in
  Check.equal "its view's lastSequence"
    (`Int (before + 1))
    (Check.at view [ "lastSequence" ]);
  Check.equal "its view's archived" (`Bool true)
    (Check.at view [ "resource"; "archived" ]);
  (* What was there is as it was. *)
  Check.equal "the lines left"
    (`List (List.map str (left_lines w shared)))
    (`List
       (List.map str
          (List.filteri (fun at _ -> at < before) (A.stream_lines w shared))))

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
