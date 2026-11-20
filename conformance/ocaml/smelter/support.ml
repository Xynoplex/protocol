(* What the smelter files share: a file's world, the points and the health
   a case reads, the texts a Smelter's output and refusals are held to, and
   the documents a case records. *)

open Xynolog_conformance
module W = Smelter_world

let str s = `String s
let strs items = `List (List.map str items)

(* [cases] against one Smelter world, under [category]: the file's, made by
   its first case. *)
let in_world implementations file =
  let made =
    Suite.world ("smelter/" ^ file)
      ~make:(fun () -> W.create implementations)
      ~close:W.close
  in
  fun ~category cases ->
    List.map
      (fun (c : W.t Case.t) ->
        Case.test ~category c (fun () -> W.end_case made c.run))
      cases

(* The world's Smelter, started when a case before stopped it. *)
let started (w : W.t) = if w.smelter = None then W.start w

(* The world's Smelter, stopped when it runs: a Smelter that must refuse
   would answer on its health port. *)
let stopped (w : W.t) = if w.smelter <> None then ignore (W.stop w : int option)

(* ── The index ──────────────────────────────────────────────────────── *)

(* The media type the plain texts are recorded as, and so the one their
   points state their bytes were read as. *)
let plain = "text/plain"
let pdf = "application/pdf"

(* The eight bytes a PNG begins with, and no more: nothing reads them. *)
let png = "\x89PNG\r\n\x1a\n"
let payload = Qdrant.payload

(* A point's id and its payload, whole: what the table states of a point,
   beside its vector. *)
let stated point =
  `Assoc
    [
      ("id", Check.at point [ "id" ]); ("payload", Check.at point [ "payload" ]);
    ]

let stated_all points = `List (List.map stated points)

(* A point's vector, which it was read with. *)
let vector_of point =
  match Protocol.at point [ "vector" ] with
  | Some (`List numbers) -> List.map Yojson.Safe.Util.to_number numbers
  | _ ->
      failwith
        (Check.shown (Check.at point [ "id" ]) ^ " was read without its vector")

(* Whether a point's vector is the one the stand-in provider gives [text]:
   that text was embedded, and its vector landed on this point. *)
let embeds point text =
  Ollama_embed.same_embedding (vector_of point) (Ollama_embed.embedding_of text)

(* Hold [now] to be the points [before] were: their ids and payloads the
   same, and their vectors the same to the precision an index stores one
   in. *)
let same what now before =
  Check.equal what (stated_all before) (stated_all now);
  Check.that (what ^ ": their vectors")
    (List.for_all2
       (fun n b -> Ollama_embed.same_embedding (vector_of n) (vector_of b))
       now before)

(* A point with its payload's entity types set to [entity_types]. *)
let with_entity_types entity_types point =
  match point with
  | `Assoc members ->
      `Assoc
        (List.map
           (fun (k, v) ->
             if k = "payload" then
               (k, Edit.set_at [ "entityTypes" ] (strs entity_types) v)
             else (k, v))
           members)
  | other -> other

let payload_text point =
  Yojson.Safe.Util.to_string
    (Check.at (payload point) [ Qdrant.payload_member "resources" "text" ])

(* ── The health ─────────────────────────────────────────────────────── *)

let schema name =
  match Protocol.json_of ("smelter/" ^ name ^ ".json") with
  | Some json -> json
  | None -> failwith ("specs/src/smelter has no " ^ name)

(* The members a schema states, sorted. *)
let members_of name =
  List.sort compare
    (Yojson.Safe.Util.keys (Check.at (schema name) [ "properties" ]))

(* A reconcile summary that did nothing: every count SmelterReconcileSummary
   states 0, and none skipped, with [work] over it. *)
let only work =
  `Assoc
    (List.map
       (fun name ->
         match List.assoc_opt name work with
         | Some value -> (name, value)
         | None -> (
             match
               Check.at
                 (schema "SmelterReconcileSummary")
                 [ "properties"; name; "type" ]
             with
             | `String "object" -> (name, `Assoc [])
             | _ -> (name, `Int 0)))
       (members_of "SmelterReconcileSummary"))

let count n = `Int n

let summary_count summary name =
  Yojson.Safe.Util.to_int (Check.at summary [ name ])

(* What the reconcile of the Smelter's last start came to. One that did not
   end well has no summary, and fails the case here. *)
let summary_of w =
  let reconcile = Check.at (W.health w) [ "reconcile" ] in
  match
    (Protocol.at reconcile [ "phase" ], Protocol.at reconcile [ "summary" ])
  with
  | Some (`String "done"), Some summary -> summary
  | _ ->
      failwith
        ("the Smelter's reconcile did not end well: " ^ Check.shown reconcile)

(* Hold a reconcile to have done [work] and no other. Its two counts of what
   the knowledge base holds are no work, and are left as it reports them:
   the cases of a file share one knowledge base, so a case holds those two
   only where it knows them, or as it changes them. *)
let expect_only summary work =
  let catalog = [ "resourcesEligible"; "resourcesIndexed" ] in
  Check.equal "the reconcile's summary"
    (only
       (List.map (fun name -> (name, Check.at summary [ name ])) catalog @ work))
    summary

(* ── What a Smelter says ────────────────────────────────────────────── *)

module M = Xynolog_protocol.Tables.Smelter_messages

let messages () = (Protocol.tables ()).smelter_messages

(* The message [name] smelter/messages.json says a Smelter logs. *)
let logged name =
  match
    List.find_opt
      (fun (r : M.logged_item) -> r.name = name)
      (messages ()).logged
  with
  | Some row -> row
  | None -> failwith ("smelter/messages.json states no logged message " ^ name)

(* The refusal [name] of smelter/messages.json, filled. *)
let refusal name fill =
  match
    List.find_opt
      (fun (r : M.refusals_item) -> r.name = name)
      (messages ()).refusals
  with
  | Some row -> Protocol.fill_template row.message fill
  | None -> failwith ("smelter/messages.json states no refusal " ^ name)

(* The facts of a line of a Smelter's output that logs [row]'s message: the
   JSON object written after it. None for a line that logs another. *)
let facts_of (row : M.logged_item) line =
  match Re.exec_opt (Re.compile (Re.str row.message)) line with
  | None -> None
  | Some found -> (
      let after = Re.Group.stop found 0 in
      match String.index_from_opt line after '{' with
      | None -> Some (`Assoc [])
      | Some brace -> (
          match
            Yojson.Safe.from_string
              (String.sub line brace (String.length line - brace))
          with
          | `Assoc _ as facts -> Some facts
          | _ -> None
          | exception Yojson.Json_error _ -> None))

(* Whether [line] is at [row]'s level: the bracketed level a line names
   begins with the level's name, in any case. *)
let at_level (row : M.logged_item) line =
  Re.execp
    (Re.compile
       (Re.seq
          [
            Re.char '[';
            Re.no_case (Re.str (M.log_level_value row.level));
            Re.rep (Re.compl [ Re.char ']' ]);
            Re.char ']';
          ]))
    line

(* Wait for a line of [lines ()] that logs [name] at its level, each of its
   fields as [fill] fills it and each fact it carries there: that line's
   facts. *)
let logged_line ?(timeout = 10.0) lines name fill =
  let row = logged name in
  let wanted =
    List.map
      (fun (field, form) -> (field, Protocol.fill_template form fill))
      row.fields
  in
  Run.eventually
    (Printf.sprintf "a line of its output that logs %S with %s" row.message
       (String.concat ", " (List.map (fun (k, v) -> k ^ "=" ^ v) wanted)))
    timeout
    (fun () ->
      List.find_map
        (fun line ->
          match facts_of row line with
          | Some facts
            when at_level row line
                 && List.for_all
                      (fun (field, value) ->
                        match Protocol.at facts [ field ] with
                        | Some (`String s) -> s = value
                        | Some (`Int n) -> string_of_int n = value
                        | _ -> false)
                      wanted
                 && List.for_all
                      (fun fact -> Protocol.at facts [ fact ] <> None)
                      row.carries ->
              Some facts
          | _ -> None)
        (lines ()))

(* ── What a case records ────────────────────────────────────────────── *)

(* Bytes that are no PDF, to be recorded as one: a Smelter declines them as
   corrupt. *)
let not_a_pdf = "not a pdf at all"

(* A decision of [checksum] for [resource_id]: [outcome], and the reason
   when one is given. *)
let decision ?reason resource_id checksum outcome =
  `Assoc
    ([
       ("resourceId", str resource_id);
       ("contentChecksum", str checksum);
       ("outcome", str outcome);
     ]
    @ match reason with Some r -> [ ("reason", str r) ] | None -> [])

(* The point the table states for one chunk of a plain text, which was not
   read from pixels: it has no mark of that, and no other member. Each
   member's name is the table's. *)
let chunk_point resource_id index text checksum entity_types =
  let named name = Qdrant.payload_member "resources" name in
  `Assoc
    [
      ("id", str (Qdrant.qdrant_id (Printf.sprintf "%s-%d" resource_id index)));
      ( "payload",
        `Assoc
          [
            (named "resourceId", str resource_id);
            (named "chunkIndex", `Int index);
            (named "text", str text);
            (named "contentChecksum", str checksum);
            (named "mediaType", str plain);
            (named "entityTypes", strs entity_types);
          ] );
    ]

(* The point the table states for an annotation that quotes [text]. *)
let annotation_point annotation_id resource_id motivation entity_types text =
  let named name = Qdrant.payload_member "annotations" name in
  `Assoc
    [
      ("id", str (Qdrant.qdrant_id annotation_id));
      ( "payload",
        `Assoc
          [
            (named "annotationId", str annotation_id);
            (named "resourceId", str resource_id);
            (named "motivation", str motivation);
            (named "entityTypes", strs entity_types);
            (named "text", str text);
          ] );
    ]

(* The ids of the annotations [points] are of. *)
let annotation_ids points =
  `List
    (List.map
       (fun point ->
         Check.at (payload point)
           [ Qdrant.payload_member "annotations" "annotationId" ])
       points)

let mentions needle text = Re.execp (Re.compile (Re.str needle)) text
