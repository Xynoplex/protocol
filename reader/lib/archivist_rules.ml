(* The Archivist is specified: what it keeps and how it is started is stated
   for a second implementation to read instead of the code.

   - Each file the record keeps has a schema, Archivist's own (archivist/) or
     a component, and so does each part of its configuration.
   - ArchivistConfig defaults nothing, and leaves optional only a job's or an
     actor's engine, which is absent when it has none.
   - The engines' job types are exactly JobType, and their mark motivations
     exactly Motivation.
   - The anchored-text entry's provenance is ExtractedText's, and its decline
     classes are ExtractionDeclined's.
   - Each refusal of archivist/messages.json is of operations the registry
     says the Archivist answers, and each route's error is of an operation
     of the Archivist's API. Its refusal of an entity type not registered
     is the dispatcher's, word for word (dispatcher/messages.json).
   - contract/ARCHIVIST.md names each record schema but the anchored-text
     entries, ArchivistConfig, each table of archivist/, every persisted
     event of the bus and every route of Archivist's HTTP surface. That one
     reads outside specs/src, so the repository's check holds it, not
     [Spec.load]. *)

let record_schemas =
  [
    "ResourceView";
    "ResourceAnnotations";
    "EntityTypesProjection";
    "TagSchemasProjection";
    "ProfilesProjection";
    "StorageUriEntry";
    "AnchoredTextEntry";
    "AnchoredTextExtractedEntry";
    "AnchoredTextDeclinedEntry";
  ]

let config_schemas = [ "ArchivistConfig"; "ArchivistEngines"; "Engine" ]
let protocol = "contract/ARCHIVIST.md"
let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token

let resolve ids file target =
  fst (Decode.resolve ids { target; file; pointer = "" })

let at pointer json = Decode.at_pointer pointer json
let text = function Some (`String s) -> Some s | _ -> None

let texts = function
  | Some (`List items) ->
      List.filter_map (function `String s -> Some s | _ -> None) items
  | _ -> []

let keys = function Some (`Assoc members) -> List.map fst members | _ -> []
let shown words = "[" ^ String.concat "," words ^ "]"

(* [json], read from [file], with each [$ref] naming the file it resolves to:
   two schemas in different directories that name one schema say the
   same. *)
let rec anchored ids file (json : Yojson.Safe.t) : Yojson.Safe.t =
  match json with
  | `Assoc members ->
      `Assoc
        (List.map
           (function
             | "$ref", `String target ->
                 ("$ref", `String (resolve ids file target))
             | key, value -> (key, anchored ids file value))
           members)
  | `List items -> `List (List.map (anchored ids file) items)
  | value -> value

let refusals ~ids ~(read_json : string -> Yojson.Safe.t option) =
  let found = ref [] in
  let refuse file pointer fmt =
    Printf.ksprintf
      (fun message -> found := { Decode.file; pointer; message } :: !found)
      fmt
  in
  let file_of name =
    List.find_opt
      (fun file -> Option.is_some (read_json file))
      [ "archivist/" ^ name ^ ".json"; Places.components ^ name ^ ".json" ]
  in
  let schema name =
    Option.bind (file_of name) (fun file ->
        Option.map (fun json -> (file, json)) (read_json file))
  in
  List.iter
    (fun name ->
      if Option.is_none (file_of name) then
        refuse
          ("archivist/" ^ name ^ ".json")
          ""
          "%s has no schema in specs/src/archivist or \
           specs/src/components/schemas"
          name)
    (record_schemas @ config_schemas);
  let enum name =
    Option.fold ~none:[]
      ~some:(fun (_, json) -> texts (at "/enum" json))
      (schema name)
  in
  (match (schema "ArchivistConfig", schema "ArchivistEngines") with
  | Some (config_file, config), Some (engines_file, engines) ->
      let rec everything_stated file pointer path ~optional_allowed node =
        if Option.is_some (at "/default" node) then
          refuse file (pointer / "default") "ArchivistConfig defaults %s" path;
        if text (at "/type" node) = Some "object" then
          let required = texts (at "/required" node) in
          List.iter
            (fun name ->
              let property_at = if path = "" then name else path ^ "." ^ name in
              if (not (List.mem name required)) && not optional_allowed then
                refuse file
                  (pointer / "properties" / name)
                  "ArchivistConfig leaves %s optional" property_at;
              let property = Option.get (at ("" / "properties" / name) node) in
              let file, pointer, property =
                match text (at "/$ref" property) with
                | Some ref when resolve ids file ref = engines_file ->
                    (engines_file, "", engines)
                | _ -> (file, pointer / "properties" / name, property)
              in
              everything_stated file pointer property_at
                ~optional_allowed:
                  (List.mem property_at
                     [ "engines.jobs"; "engines.jobs.mark"; "engines.actors" ])
                property)
            (keys (at "/properties" node))
      in
      everything_stated config_file "" "" ~optional_allowed:false config;
      let engine_job_types = keys (at "/properties/jobs/properties" engines) in
      let job_types = enum "JobType" in
      if List.sort compare engine_job_types <> List.sort compare job_types then
        refuse engines_file "/properties/jobs"
          "the engines' job types are %s, JobType is %s"
          (shown engine_job_types) (shown job_types);
      let engine_motivations =
        keys (at "/properties/jobs/properties/mark/properties" engines)
      in
      let motivations = enum "Motivation" in
      if List.sort compare engine_motivations <> List.sort compare motivations
      then
        refuse engines_file "/properties/jobs/properties/mark"
          "the engines' mark motivations are %s, Motivation is %s"
          (shown engine_motivations) (shown motivations)
  | _ -> ());
  (match
     ( schema "AnchoredTextEntry",
       schema "ExtractedText",
       schema "ExtractionDeclined" )
   with
  | ( Some (entry_file, entry),
      Some (text_file, extracted_text),
      Some (declined_file, declined_text) ) -> (
      let member index =
        Option.bind
          (text (at ("/oneOf" / string_of_int index / "$ref") entry))
          (fun ref ->
            let file = resolve ids entry_file ref in
            Option.map
              (fun json -> (file, anchored ids file json))
              (read_json file))
      in
      match (member 0, member 1) with
      | Some (extracted_file, extracted), Some (declined_file', declined) ->
          let provenance =
            match
              at "/allOf/1/properties" (anchored ids text_file extracted_text)
            with
            | Some (`Assoc properties) ->
                List.filter (fun (name, _) -> name <> "kind") properties
            | _ -> []
          in
          List.iter
            (fun (name, property) ->
              if at ("" / "properties" / name) extracted <> Some property then
                refuse extracted_file
                  ("" / "properties" / name)
                  "AnchoredTextEntry's %s is not ExtractedText's" name)
            provenance;
          if
            at "/properties/declined" declined
            <> at "/properties/declined"
                 (anchored ids declined_file declined_text)
          then
            refuse declined_file' "/properties/declined"
              "AnchoredTextEntry's declined is not ExtractionDeclined's"
      | _ ->
          refuse entry_file "/oneOf"
            "AnchoredTextEntry is not an extracted entry or a declined one")
  | _ -> ());
  let messages = "archivist/messages.json" in
  let rows section =
    match Option.bind (read_json messages) (at section) with
    | Some (`List rows) -> rows
    | _ -> []
  in
  let answered_by_archivist =
    match Option.bind (read_json "bus/registry.json") (at "/operations") with
    | Some (`List operations) ->
        List.filter_map
          (fun op ->
            match text (at "/answeredBy" op) with
            | Some "archivist" -> text (at "/request" op)
            | _ -> None)
          operations
    | _ -> []
  in
  List.iteri
    (fun i row ->
      List.iteri
        (fun j op ->
          if not (List.mem op answered_by_archivist) then
            refuse messages
              ("/refusals" / string_of_int i / "operations" / string_of_int j)
              "%s is no operation the registry says the Archivist answers" op)
        (texts (at "/operations" row)))
    (rows "/refusals");
  let api = "archivist/openapi.json" in
  let operation_of route meth =
    Option.bind (read_json api) (fun openapi ->
        Option.bind
          (text (at ("/paths" / route / "$ref") openapi))
          (fun ref ->
            Option.bind
              (read_json (resolve ids api ref))
              (at ("" / String.lowercase_ascii meth))))
  in
  List.iteri
    (fun i row ->
      match (text (at "/method" row), text (at "/route" row)) with
      | Some meth, Some route when Option.is_none (operation_of route meth) ->
          refuse messages
            ("/http" / string_of_int i / "route")
            "%s %s is no operation of the Archivist's API" meth route
      | _ -> ())
    (rows "/http");
  let named_message file section name =
    Option.bind (read_json file) (fun table ->
        match at section table with
        | Some (`List rows) ->
            List.find_map
              (fun row ->
                if text (at "/name" row) = Some name then
                  text (at "/message" row)
                else None)
              rows
        | _ -> None)
  in
  (match
     ( named_message messages "/refusals" "entity-type-not-registered",
       named_message "dispatcher/messages.json" "/refusals"
         "entity-type-not-registered" )
   with
  | Some archivist, Some dispatcher when archivist <> dispatcher ->
      refuse messages "/refusals"
        "the Archivist refuses an entity type not registered in %S, and the \
         dispatcher in %S"
        archivist dispatcher
  | Some _, Some _ -> ()
  | _ ->
      refuse messages "/refusals"
        "the Archivist's and the dispatcher's refusals of an entity type not \
         registered are not both stated (entity-type-not-registered)");
  List.rev !found

(* contract/ARCHIVIST.md, under the repository's [root], names what the
   Archivist keeps and answers. *)
let contract ~root (registry : Tables.Bus_registry.t) =
  let path = Filename.concat root protocol in
  let refuse message = { Decode.file = protocol; pointer = ""; message } in
  if not (Sys.file_exists path) then [ refuse "is missing" ]
  else
    let prose = In_channel.with_open_bin path In_channel.input_all in
    let contains term = Re.execp (Re.compile (Re.str term)) prose in
    let named what term =
      if contains term then []
      else [ refuse (Printf.sprintf "does not name %s %s" what term) ]
    in
    let anchored_entry name =
      String.starts_with ~prefix:"AnchoredText" name
      && String.ends_with ~suffix:"Entry" name
      && name <> "AnchoredTextEntry"
    in
    let persisted =
      List.filter_map
        (fun (c : Tables.Bus_registry.channels_item) ->
          match c with Stored_event { channel; _ } -> Some channel | _ -> None)
        registry.channels
    in
    let routes =
      match
        Decode.at_pointer "/paths"
          (Yojson.Safe.from_file
             (Filename.concat root "specs/src/archivist/openapi.json"))
      with
      | Some (`Assoc paths) -> List.map fst paths
      | _ -> []
    in
    List.concat_map (named "the schema")
      (List.filter (fun name -> not (anchored_entry name)) record_schemas
      @ [ "ArchivistConfig" ])
    @ List.concat_map
        (fun table -> named "the table" (Filename.basename table))
        (List.filter
           (String.starts_with ~prefix:"archivist/")
           (Places.tables (Filename.concat root "specs/src")))
    @ (if persisted = [] then
         [
           refuse
             "is read against a bus registry that names no persisted event: \
              the check has lost what it reads";
         ]
       else [])
    @ List.concat_map
        (fun channel -> named "the persisted event" ("`" ^ channel ^ "`"))
        persisted
    @ List.concat_map
        (fun route -> named "the route" ("`" ^ route ^ "`"))
        routes
