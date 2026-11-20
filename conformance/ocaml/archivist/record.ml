(* The record across restarts (ARCHIVIST.md § The record on disk, § Boot and
   shutdown): what survives, what is rebuilt, what is reaped, and how a log
   written before is read. *)

open Xynolog_conformance
open Support
open Case

let text = "The record survives the process that keeps it.\n"
let first_file () = A.event_file_name (A.layout ()).event_file.first

let present what = function
  | Some json -> json
  | None -> failwith (what ^ " is absent")

(* A resource ada uploaded as [name], and the annotation grace made on it. *)
let annotated (w : A.t) name =
  let id =
    A.created w
      (World.person_did w.world "ada")
      [
        ("name", name);
        ( "storageUri",
          Printf.sprintf "file://survives/%s.md"
            (String.map (function ' ' -> '-' | c -> c) name) );
      ]
      text
  in
  let grace = A.person w "grace" in
  let made =
    A.ask grace "mark:create-request"
      (`Assoc [ ("resourceId", str id); ("request", highlight id "record" 4) ])
  in
  (id, A.string_member "annotationId" made)

let on id = `Assoc [ ("resourceId", str id) ]

let numbered_events events =
  `List
    (List.map
       (fun e ->
         `List
           [
             Check.at e [ "type" ]; Check.at e [ "metadata"; "sequenceNumber" ];
           ])
       events)

let numbered_as pairs =
  `List (List.map (fun (type_, n) -> `List [ str type_; `Int n ]) pairs)

let shard_table _ =
  List.iter
    (fun (c : Xynolog_protocol.Tables.Archivist_shard_cases.cases_item) ->
      Check.equal c.key (str c.shard) (str (A.shard_of c.key)))
    (Protocol.tables ()).archivist_shard_cases.cases

let numbered w =
  let id, _ = annotated w "numbered" in
  let editor () = A.person w "editor" in
  A.restart w;
  ignore (A.ask (editor ()) "mark:archive" (on id) : Yojson.Safe.t);
  A.crash_and_restart w;
  ignore (A.ask (editor ()) "mark:unarchive" (on id) : Yojson.Safe.t);
  Check.equal "the stream's events, numbered"
    (numbered_as
       [
         ("yield:created", 1);
         ("mark:added", 2);
         ("mark:archived", 3);
         ("mark:unarchived", 4);
       ])
    (numbered_events (A.stored w id));
  Check.equal "the stream's files"
    (`List [ str (first_file ()) ])
    (`List (List.map str (A.stream_files w id)));
  let view = present "the view" (A.view w id) in
  Check.equal "the view's last sequence" (`Int 4)
    (Check.at view [ "lastSequence" ]);
  Check.equal "whether the view's resource is archived" (`Bool false)
    (Check.at view [ "resource"; "archived" ]);
  Check.equal "the view's annotations' version" (`Int 4)
    (Check.at view [ "annotations"; "version" ]);
  (* The system stream too: the vocabulary is seeded once, and numbered on. *)
  let curator = A.person w "curator" in
  ignore
    (A.ask curator "frame:add-entity-type" (`Assoc [ ("tag", str "Restarted") ])
      : Yojson.Safe.t);
  let added =
    List.filter
      (fun e -> Protocol.at e [ "type" ] = Some (str "frame:entity-type-added"))
      (A.stored w A.system)
  in
  let of_type e = Protocol.at e [ "payload"; "entityType" ] in
  List.iter
    (fun entity_type ->
      Check.equal
        (Printf.sprintf "the times %s was added" entity_type)
        (`Int 1)
        (`Int
           (List.length
              (List.filter (fun e -> of_type e = Some (str entity_type)) added))))
    (Protocol.tables ()).archivist_default_entity_types.entity_types;
  Check.equal "the entity type added last" (str "Restarted")
    (present "an added entity type"
       (of_type (List.nth added (List.length added - 1))))

let projections w =
  List.map
    (fun (file : A.Layout.files_item) ->
      (file.name, present file.name (A.projection w file.name)))
    (A.layout ()).system_projections.files

let rebuilt (w : A.t) =
  let id, annotation_id = annotated w "rebuilt" in
  let curator = A.person w "curator" in
  ignore
    (A.ask curator "frame:add-tag-schema"
       (`Assoc
          [
            ( "schema",
              `Assoc
                [
                  ("id", str "rebuilt");
                  ("name", str "Rebuilt");
                  ("description", str "Survives");
                  ("domain", str "general");
                  ("tags", `List []);
                ] );
          ])
      : Yojson.Safe.t);
  A.emit curator "principal:profile" (`Assoc [ ("name", str "The Curator") ]);
  ignore
    (A.fact curator "principal:profiled" ~matches:(fun e ->
         Protocol.at e [ "payload"; "name" ] = Some (str "The Curator"))
      : Yojson.Safe.t);
  let uri = "file://survives/rebuilt.md" in
  let view = present "the view" (A.view w id) in
  let before = projections w in
  let entry = present "the storage-uri entry" (A.storage_uri_entry w uri) in
  Check.equal "the storage-uri entry"
    (`Assoc [ ("uri", str uri); ("resourceId", str id) ])
    entry;
  ignore (Archivist.stop w.archivist : int option);
  Array.iter
    (fun entry -> Gateway.remove (Filename.concat w.dirs.state_home entry))
    (Sys.readdir w.dirs.state_home);
  A.restart w;
  Check.equal "the view, rebuilt" view (present "the view" (A.view w id));
  List.iter2
    (fun (name, was) (_, is) -> Check.equal (name ^ ", rebuilt") was is)
    before (projections w);
  Check.equal "the storage-uri entry, rebuilt" entry
    (present "the storage-uri entry" (A.storage_uri_entry w uri));
  let reader = A.person w "reader" in
  Check.equal "the annotation's id" (str annotation_id)
    (Check.at
       (A.ask reader "browse:annotation-requested"
          (`Assoc
             [ ("resourceId", str id); ("annotationId", str annotation_id) ]))
       [ "annotation"; "id" ]);
  (* A rebuild publishes nothing: no event of a resource reaches a stream
     opened after it. *)
  Check.equal "the facts of a resource the reader's stream carried" (`List [])
    (`List
       (List.filter
          (fun e -> Protocol.at e [ "resourceId" ] <> None)
          (A.facts reader)))

(* With the Archivist stopped, rename the resource [id]'s view to [name],
   and put a copy of it beside it as the view of [orphan_id], which has no
   stream: the orphan's path. *)
let tampered (w : A.t) id ~name ~orphan_id =
  let good = A.read_file (A.view_path w id) in
  let orphan = A.view_path w orphan_id in
  ignore (Archivist.stop w.archivist : int option);
  Out_channel.with_open_bin (A.view_path w id) (fun out ->
      output_string out
        (Yojson.Safe.pretty_to_string
           (Edit.set_at [ "resource"; "name" ] (str name)
              (Yojson.Safe.from_string good))));
  A.make_dirs (Filename.dirname orphan);
  Out_channel.with_open_bin orphan (fun out ->
      output_string out
        (Re.replace_string (Re.compile (Re.str id)) ~by:orphan_id good));
  orphan

let repeated pair = String.concat "" (List.init 16 (fun _ -> pair))

let view_name w id =
  Check.at (present "the view" (A.view w id)) [ "resource"; "name" ]

let corrected w =
  let id, _ = annotated w "corrected" in
  let orphan = tampered w id ~name:"tampered with" ~orphan_id:(repeated "0f") in
  A.restart w;
  Check.equal "the view's name" (str "corrected") (view_name w id);
  Check.that "the orphan's view is reaped" (not (Sys.file_exists orphan))

let skipped w =
  let id, _ = annotated w "skipped" in
  let orphan = tampered w id ~name:"left as found" ~orphan_id:(repeated "0e") in
  A.restart ~change:(Edit.set_at [ "skipRebuild" ] (`Bool true)) w;
  let reader = A.person w "reader" in
  Check.equal "the resource's name, as found" (str "left as found")
    (Check.at
       (A.ask reader "browse:resource-requested" (on id))
       [ "resource"; "name" ]);
  Check.that "the orphan's view is kept" (Sys.file_exists orphan);
  A.restart ~change:(Edit.set_at [ "skipRebuild" ] (`Bool false)) w;
  Check.equal "the view's name, rebuilt" (str "skipped") (view_name w id);
  Check.that "the orphan's view is reaped" (not (Sys.file_exists orphan))

(* The lines of the Archivist's stdout that log the message [row], each one
   JSON object, as service-telemetry/log-lines.json says. *)
let logged_lines (w : A.t) (row : M.logged_item) =
  let message_key =
    match
      (Protocol.tables ()).service_telemetry_log_lines.logger.json.first
    with
    | [ _; message ] -> message
    | _ -> failwith "the json line names other than a level and a message first"
  in
  List.filter
    (fun line -> Protocol.at line [ message_key ] = Some (str row.message))
    (List.map
       (fun line ->
         match Yojson.Safe.from_string line with
         | `Assoc _ as json -> json
         | _ -> failwith ("a line that is not one JSON object: " ^ line)
         | exception Yojson.Json_error why ->
             failwith
               (Printf.sprintf "a line that is not JSON (%s): %s" why line))
       (Dynarray.to_list w.archivist.process.stdout))

let as_found (w : A.t) =
  let id, _ = annotated w "as found" in
  let file = Filename.concat (A.stream_dir w id) (first_file ()) in
  (* JSON, and no event: it states no `type` of its own, and what it holds
     under `event` is not read. *)
  let untyped =
    `Assoc
      [
        ( "event",
          `Assoc
            [
              ("type", str "mark:archived");
              ("resourceId", str id);
              ("userId", str (World.person_did w.world "ada"));
              ("version", `Int 1);
              ("payload", `Assoc []);
              ("id", str "6f1e2d3c-4b5a-4c6d-8e7f-0a1b2c3d4e5f");
              ("timestamp", str "2026-02-02T02:02:02.002Z");
            ] );
        ("metadata", `Assoc [ ("sequenceNumber", `Int 9) ]);
      ]
  in
  ignore (Archivist.stop w.archivist : int option);
  Out_channel.with_open_gen [ Open_append; Open_binary ] 0o644 file (fun out ->
      output_string out
        ("\nthis line is not JSON\n" ^ Yojson.Safe.to_string untyped ^ "\n"));
  A.restart w;
  let view = present "the view" (A.view w id) in
  Check.equal "the view's last sequence" (`Int 2)
    (Check.at view [ "lastSequence" ]);
  Check.equal "whether the view's resource is archived" (`Bool false)
    (Check.at view [ "resource"; "archived" ]);
  let replay =
    A.http w ~route:"/events/{resourceId}" "GET"
      ("/events/" ^ Protocol.uri_component id ^ "?fromSequence=1")
  in
  Check.equal "the events replayed"
    (numbered_as [ ("yield:created", 1); ("mark:added", 2) ])
    (numbered_events
       (Check.list (present "the replay" replay.json) [ "events" ]));
  let row = logged "not-an-event" in
  let level_key =
    match
      (Protocol.tables ()).service_telemetry_log_lines.logger.json.first
    with
    | level :: _ -> level
    | [] -> failwith "the json line names no first members"
  in
  let lines = logged_lines w row in
  Check.equal
    (Printf.sprintf "the lines logging %S" row.message)
    (`Int 2)
    (`Int (List.length lines));
  List.iter2
    (fun line number ->
      Check.equal "the line's level"
        (str (M.log_level_value row.level))
        (Check.at line [ level_key ]);
      List.iter
        (fun (field, form) ->
          Check.equal
            (Printf.sprintf "the line's %s" field)
            (str
               (filled form [ ("path", file); ("line", string_of_int number) ]))
            (Check.at line [ field ]))
        row.fields)
    lines [ 4; 5 ];
  (* The next event follows the last one read, whatever number a skipped
     line carries. *)
  ignore (A.ask (A.person w "editor") "mark:archive" (on id) : Yojson.Safe.t);
  let last =
    List.nth (A.stream_lines w id) (List.length (A.stream_lines w id) - 1)
  in
  Check.matching "the event appended next"
    [
      ("type", str "mark:archived");
      ("metadata", `Assoc [ ("sequenceNumber", `Int 3) ]);
    ]
    (Yojson.Safe.from_string last)

let tokened w =
  let id, _ = annotated w "tokened" in
  let cloner = A.person w "cloner" in
  let token =
    A.string_member "token" (A.ask cloner "yield:clone-token-requested" (on id))
  in
  let with_token = `Assoc [ ("token", str token) ] in
  Check.equal "the clone's source" (str id)
    (Check.at
       (A.ask cloner "yield:clone-resource-requested" with_token)
       [ "sourceResource"; "@id" ]);
  A.restart w;
  Check.equal "the refusal of the token, after a restart"
    (str (refusal "invalid-token" []))
    (str
       (A.refused (A.person w "cloner") "yield:clone-resource-requested"
          with_token))

let tests implementations =
  with_archivist implementations "the record, across restarts"
    [
      case "agrees with the shard table" shard_table;
      case
        "numbers a stream on from where it stopped, after a stop and after a \
         crash"
        numbered;
      case
        "rebuilds every view and projection from the log when the state volume \
         is emptied"
        rebuilt;
      case
        "rebuilds a view that is wrong, and reaps a view whose resource has no \
         stream"
        corrected;
      case
        "serves the views it finds, rebuilding and reaping nothing, when told \
         to skip the rebuild"
        skipped;
      case
        "reads a log as it finds it: blank lines skipped, and a line that is \
         not an event logged and skipped"
        as_found;
      case "keeps a clone token only as long as the process" tokened;
    ]
