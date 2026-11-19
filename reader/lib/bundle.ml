(* Each OpenAPI document of specs/src as one file: what the conformance
   harness, a service and a client read without following a file. Every
   file a [$ref] names, by its [$id] or by its path ([Decode.resolve]), is
   followed. A file the document's [paths] names is a path item, written in
   its place. Any other is a component, and a reference names it there: by
   the kind and name the document lists it under [components], or, unlisted,
   by its stem, as a schema when a schema names it and otherwise as the kind
   its directory is named for ([schemas] or [responses]). A description
   written beside a reference is kept: beside a component's reference, and
   over the description of a path item written in its place. A
   discriminator's mapping names its schemas as a reference does. A
   component carries no [$schema] or [$id]: the bundle names it within
   itself.

   The files are read as JSON, not through the model, so a bundle keeps
   every member Protocol writes.

   The services' bundle, specs/services.json, is of no document: its
   components are each service's schema (Places.service_schemas), by its
   stem, which the conformance harness holds a service's values to. *)

exception Refused of Decode.error

let refuse file pointer fmt =
  Printf.ksprintf
    (fun message -> raise (Refused { Decode.file; pointer; message }))
    fmt

(* Each document, and the bundle written of it under the repository's root. *)
let documents =
  [
    ("openapi.json", "specs/openapi.json");
    ("archivist/openapi.json", "specs/archivist.openapi.json");
  ]

type st = {
  specs : string;
  ids : Decode.ids;
  path_items : string list;  (** The files the document's [paths] names. *)
  named : (string, string * string) Hashtbl.t;
      (** Each file that is a component, by its file: its kind and name. *)
  files : (string * string, string) Hashtbl.t;
      (** Each component's file, by its kind and name. *)
  components : (string * string, Yojson.Safe.t) Hashtbl.t;
}

let parse st file =
  match Decode.parse_file ~root:st.specs file with
  | Ok json -> json
  | Error error -> raise (Refused error)

let name st file ((kind, n) as named) =
  (match Hashtbl.find_opt st.named file with
  | Some (k, m) when (k, m) <> named ->
      refuse file "" "is named both %s/%s and %s/%s" k m kind n
  | _ -> ());
  (match Hashtbl.find_opt st.files named with
  | Some other when other <> file ->
      refuse file "" "is named %s/%s, which names %s too" kind n other
  | _ -> ());
  Hashtbl.replace st.named file named;
  Hashtbl.replace st.files named file

let stem file = Filename.remove_extension (Filename.basename file)

(* The component a file is when the document lists none, by its stem: a
   schema, when a schema names it, as a [$ref] in a schema names only
   schemas; otherwise what its directory, of schemas or of responses, holds. *)
let unlisted ~in_schema file =
  if in_schema then Some ("schemas", stem file)
  else
    match Filename.basename (Filename.dirname file) with
    | ("schemas" | "responses") as kind -> Some (kind, stem file)
    | _ -> None

let target_file st file target =
  if String.starts_with ~prefix:"#" target then
    refuse file "" "%s refers within its file: specs/src refers by file" target;
  match Decode.resolve st.ids { Decode.target; file; pointer = "" } with
  | target_file, "" -> target_file
  | _ ->
      refuse file "" "%s points into a file: specs/src refers to whole files"
        target

(* A schema's file without what makes it one, its [$schema] and its [$id]: a
   bundle names its components within itself, and an [$id] in it would
   rebase every reference beneath. *)
let unidentified = function
  | `Assoc members ->
      `Assoc (List.filter (fun (k, _) -> k <> "$schema" && k <> "$id") members)
  | json -> json

let rec load st ~in_schema file =
  resolve st ~in_schema file (unidentified (parse st file))

(* [json], written in [file], with every reference resolved; [in_schema]
   when it is a schema's. *)
and resolve st ~in_schema file (json : Yojson.Safe.t) : Yojson.Safe.t =
  match json with
  | `List items -> `List (List.map (resolve st ~in_schema file) items)
  | `Assoc members -> (
      match List.assoc_opt "$ref" members with
      | Some (`String target) -> reference st ~in_schema file target members
      | Some _ -> refuse file "" "states a $ref that is no text"
      | None ->
          `Assoc
            (List.map
               (function
                 | "discriminator", `Assoc discriminator
                   when List.mem_assoc "propertyName" discriminator ->
                     ("discriminator", `Assoc (mapped st file discriminator))
                 | key, value -> (key, resolve st ~in_schema file value))
               members))
  | other -> other

(* A discriminator's members, its mapping's files named as the schemas they
   are. *)
and mapped st file discriminator =
  List.map
    (function
      | "mapping", `Assoc mapping ->
          ( "mapping",
            `Assoc
              (List.map
                 (function
                   | value, `String target ->
                       let kind, n = component st ~in_schema:true file target in
                       (value, `String ("#/components/" ^ kind ^ "/" ^ n))
                   | value, _ ->
                       refuse file "" "maps %s to what is no text" value)
                 mapping) )
      | member -> member)
    discriminator

(* The component [target], named in [file], is: its kind and name. An
   unlisted one is written once, when first named. What a schema names is a
   schema. *)
and component st ~in_schema file target =
  let target_file = target_file st file target in
  match Hashtbl.find_opt st.named target_file with
  | Some (("schemas", _) as named) -> named
  | Some ((kind, n) as named) ->
      if in_schema then
        refuse file "" "names %s as a schema, and it is %s/%s" target kind n;
      named
  | None -> (
      match unlisted ~in_schema target_file with
      | Some ((kind, _) as named) ->
          name st target_file named;
          Hashtbl.replace st.components named
            (load st ~in_schema:(kind = "schemas") target_file);
          named
      | None ->
          refuse file ""
            "names %s, which is no path item, no component the document lists, \
             and in no directory of components"
            target)

and reference st ~in_schema file target members =
  let beside = List.remove_assoc "$ref" members in
  List.iter
    (fun (key, _) ->
      if key <> "description" then
        refuse file "" "states %s beside its reference to %s" key target)
    beside;
  let target_file = target_file st file target in
  if List.mem target_file st.path_items then
    match (load st ~in_schema:false target_file, beside) with
    | written, [] -> written
    | `Assoc written, [ said ] ->
        `Assoc (said :: List.remove_assoc "description" written)
    | _ -> refuse target_file "" "is no object, and a description is beside it"
  else
    let kind, n = component st ~in_schema file target in
    `Assoc (("$ref", `String ("#/components/" ^ kind ^ "/" ^ n)) :: beside)

let object_ file what = function
  | `Assoc members -> members
  | _ -> refuse file "" "%s is no object" what

(* The document [members], written in [file] under [specs], bundled. *)
let bundle ~specs ~ids file members =
  let st =
    {
      specs;
      ids;
      path_items = [];
      named = Hashtbl.create 64;
      files = Hashtbl.create 64;
      components = Hashtbl.create 512;
    }
  in
  match
    let members = members st in
    let path_items =
      match List.assoc_opt "paths" members with
      | Some paths ->
          List.filter_map
            (function
              | _, `Assoc item -> (
                  match List.assoc_opt "$ref" item with
                  | Some (`String target) -> Some (target_file st file target)
                  | _ -> None)
              | _ -> None)
            (object_ file "paths" paths)
      | None -> []
    in
    let st = { st with path_items } in
    let declared =
      match List.assoc_opt "components" members with
      | Some kinds ->
          List.map
            (fun (kind, entries) ->
              (kind, object_ file ("components/" ^ kind) entries))
            (object_ file "components" kinds)
      | None -> []
    in
    (* A component the document lists by its file, as its file. *)
    let listed = function
      | `Assoc [ ("$ref", `String target) ] -> Some (target_file st file target)
      | _ -> None
    in
    let each f =
      List.iter
        (fun (kind, entries) ->
          List.iter (fun (n, entry) -> f kind n entry) entries)
        declared
    in
    each (fun kind n entry ->
        name st
          (Option.value (listed entry)
             ~default:(file ^ "#/components/" ^ kind ^ "/" ^ n))
          (kind, n));
    each (fun kind n entry ->
        Hashtbl.replace st.components (kind, n)
          (match listed entry with
          | Some target -> load st ~in_schema:(kind = "schemas") target
          | None -> resolve st ~in_schema:(kind = "schemas") file entry));
    let resolved =
      List.map
        (fun (key, value) ->
          if key = "components" then (key, value)
          else (key, resolve st ~in_schema:false file value))
        members
    in
    let entries =
      List.sort compare
        (Hashtbl.fold
           (fun (kind, n) json all -> (kind, n, json) :: all)
           st.components [])
    in
    let components =
      `Assoc
        (List.map
           (fun kind ->
             ( kind,
               `Assoc
                 (List.filter_map
                    (fun (k, n, json) ->
                      if k = kind then Some (n, json) else None)
                    entries) ))
           (List.sort_uniq String.compare
              (List.map (fun (k, _, _) -> k) entries)))
    in
    `Assoc
      (if List.mem_assoc "components" members then
         List.map
           (fun (key, value) ->
             if key = "components" then (key, components) else (key, value))
           resolved
       else resolved @ [ ("components", components) ])
  with
  | bundled -> Ok bundled
  | exception Refused error -> Error error

(* The document whose root is [file] under [specs], bundled. *)
let document ~specs ~ids file =
  bundle ~specs ~ids file (fun st ->
      object_ file "the document" (parse st file))

(* The services' schemas, [Places.service_schemas], as one file: each a
   component schema named as its file. A service's schema is read by its
   name there, as the conformance harness reads it. *)
let services_bundle = "specs/services.json"

let services ~specs ~ids =
  bundle ~specs ~ids "services.json" (fun _ ->
      [
        ( "components",
          `Assoc
            [
              ( "schemas",
                `Assoc
                  (List.map
                     (fun file ->
                       ( stem file,
                         `Assoc
                           [
                             ( "$ref",
                               `String (Decode.base ^ Filename.basename file) );
                           ] ))
                     (Places.service_schemas specs)) );
            ] );
      ])

(* Each document's bundle, and the services', written under the repository's
   [root]. *)
let write ~root ~ids =
  let specs = Filename.concat root Repository.specs in
  List.concat_map
    (fun (source, bundled, json) ->
      match json with
      | Error error -> [ error ]
      | Ok json -> (
          match Repository.canonical json with
          | Ok text ->
              Out_channel.with_open_bin (Filename.concat root bundled)
                (fun out -> Out_channel.output_string out text);
              []
          | Error what ->
              [
                {
                  Decode.file = source;
                  pointer = "";
                  message =
                    Printf.sprintf
                      "bundles to what holds %s, which cannot be spelled as \
                       JSON.stringify does"
                      what;
                };
              ]))
    (List.map
       (fun (source, bundled) -> (source, bundled, document ~specs ~ids source))
       documents
    @ [ ("services.json", services_bundle, services ~specs ~ids) ])
