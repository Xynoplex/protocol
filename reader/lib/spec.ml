(* The pinned Protocol's specs/src, every file of it accounted for and every
   JSON file read into the model. A file is one of:
   - the two OpenAPI documents (specs/README.md: "the gateway and Archivist
     OpenAPI documents");
   - a path item, which a document's "paths" names;
   - a response, which a document's components or an operation names;
   - a table, each of which the model names here and types in Tables;
   - a fixture, which smelter/extraction-cases.json names;
   - a README.md;
   - a JSON Schema: every other JSON file.
   Any other file is refused, as is a file the model names that Protocol does
   not have, and every "$ref" must name a file of the kind its place calls
   for. *)

let ( let+ ) result f = Result.map f result

let ( and+ ) a b =
  match (a, b) with
  | Ok a, Ok b -> Ok (a, b)
  | Error e, Ok _ | Ok _, Error e -> Error e
  | Error a, Error b -> Error (a @ b)

let ( let* ) = Result.bind

let all results =
  List.fold_right
    (fun result rest ->
      let+ value = result and+ values = rest in
      value :: values)
    results (Ok [])

type t = {
  gateway : Openapi.document;
  archivist : Openapi.document;
  path_items : (string * Openapi.path_item) list;
  responses : (string * Openapi.response) list;
  schemas : (string * Schema.t) list;
  tables : Tables.t;
  table_files : string list;
}

type kind = Document | Path_item | Response | Table | Schema_file

let kind_name = function
  | Document -> "an OpenAPI document"
  | Path_item -> "a path item"
  | Response -> "a response"
  | Table -> "a table"
  | Schema_file -> "a JSON Schema"

let refusal file pointer message = { Decode.file; pointer; message }

let refused_at (reference : Decode.reference) message =
  refusal reference.file reference.pointer message

let rec walk root relative =
  let path = Filename.concat root relative in
  if Sys.is_directory path then
    Sys.readdir path |> Array.to_list |> List.sort compare
    |> List.concat_map (fun name ->
        walk root (if relative = "" then name else relative ^ "/" ^ name))
  else [ relative ]

let operations (item : Openapi.path_item) =
  List.filter_map Fun.id [ item.get; item.put; item.post ]

let operation_responses item =
  List.concat_map
    (fun (operation : Openapi.operation) ->
      List.filter_map
        (function
          | _, Openapi.Ref reference -> Some reference
          | _, Openapi.Inline _ -> None)
        operation.responses)
    (operations item)

let component_responses (document : Openapi.document) =
  match document.components.responses with
  | None -> []
  | Some responses -> List.map snd responses

(* The files the references name that Protocol has; a reference naming one it
   does not have is refused where it is written. *)
let named_files files references =
  List.sort_uniq compare (List.map (fun r -> fst (Decode.resolve r)) references)
  |> List.filter (fun file -> List.mem file files)

let same (a : Decode.reference) (b : Decode.reference) =
  a.file = b.file && a.pointer = b.pointer

let load root =
  let files = walk root "" in
  let decoded = Hashtbl.create 1024 in
  let references = ref [] in
  let decode kind file decoder =
    if not (List.mem file files) then
      Error
        [ refusal file "" "the model reads this file, and Protocol has none" ]
    else
      match Decode.parse_file ~root file with
      | Error error -> Error [ error ]
      | Ok json -> (
          match Decode.run ~file json decoder with
          | Error error -> Error [ error ]
          | Ok (value, found) ->
              Hashtbl.replace decoded file (kind, json);
              references := found @ !references;
              Ok value)
  in
  let named kind decoder file =
    let+ value = decode kind file decoder in
    (file, value)
  in
  (* The directory smelter/extraction-cases.json's "fixtures" names, from the
     repository's root, within specs/src; and the fixtures its cases name. *)
  let fixtures_of (extraction : Tables.Smelter_extraction_cases.t) =
    let prefix = "specs/src/" in
    let directory = extraction.fixtures in
    if not (String.starts_with ~prefix directory) then
      Error
        [
          refusal "smelter/extraction-cases.json" "/fixtures"
            "names a directory outside specs/src";
        ]
    else
      let within =
        String.sub directory (String.length prefix)
          (String.length directory - String.length prefix)
      in
      Ok
        ( within,
          List.filter_map
            (fun (case : Tables.Smelter_extraction_cases.cases_item) ->
              match case with
              | Read { fixture; _ } | File_declined { fixture; _ } ->
                  Some (within ^ "/" ^ fixture)
              | Bytes_declined _ -> None)
            extraction.cases )
  in
  let* gateway, archivist, tables =
    let+ gateway = decode Document "openapi.json" Openapi.document
    and+ archivist = decode Document "archivist/openapi.json" Openapi.document
    and+ tables =
      Tables.load { table = (fun file decoder -> decode Table file decoder) }
    in
    (gateway, archivist, tables)
  in
  let documents = [ gateway; archivist ] in
  let* path_items =
    documents
    |> List.concat_map (fun (document : Openapi.document) ->
        List.map snd document.paths)
    |> named_files files
    |> List.map (named Path_item Openapi.path_item)
    |> all
  in
  let response_references =
    List.concat_map component_responses documents
    @ List.concat_map (fun (_, item) -> operation_responses item) path_items
  in
  let* responses =
    named_files files response_references
    |> List.map (named Response Openapi.response)
    |> all
  in
  let* fixture_directory, fixtures =
    fixtures_of tables.smelter_extraction_cases
  in
  let is_fixture file =
    String.starts_with ~prefix:(fixture_directory ^ "/") file
  in
  let is_readme file = Filename.basename file = "README.md" in
  let schemas =
    files
    |> List.filter (fun file ->
        Filename.check_suffix file ".json"
        && (not (Hashtbl.mem decoded file))
        && not (is_fixture file))
    |> List.map (named Schema_file Schema.decode)
    |> all
  in
  let census =
    List.filter_map
      (fun file ->
        if Hashtbl.mem decoded file || is_readme file || is_fixture file then
          None
        else Some (refusal file "" "a file the model does not know"))
      files
    @ List.filter_map
        (fun fixture ->
          if List.mem fixture files then None
          else
            Some
              (refusal "smelter/extraction-cases.json" "/cases"
                 (Printf.sprintf
                    "names the fixture %s, which Protocol does not have" fixture)))
        fixtures
  in
  let whole kind (reference : Decode.reference) =
    let target, fragment = Decode.resolve reference in
    match Hashtbl.find_opt decoded target with
    | Some (found, _) when found = kind && fragment = "" -> None
    | Some (found, _) ->
        Some
          (refused_at reference
             (Printf.sprintf "names %s, %s where %s belongs" reference.target
                (kind_name found) (kind_name kind)))
    | None ->
        Some
          (refused_at reference
             (Printf.sprintf "names %s, which Protocol does not have"
                reference.target))
  in
  let schema (reference : Decode.reference) =
    let target, fragment = Decode.resolve reference in
    match Hashtbl.find_opt decoded target with
    | Some (Schema_file, _) when fragment = "" -> None
    | Some (((Schema_file | Table) as found), json) -> (
        match Decode.at_pointer fragment json with
        | None ->
            Some
              (refused_at reference
                 (Printf.sprintf "names %s, which is nothing in %s"
                    reference.target (kind_name found)))
        | Some node -> (
            match
              Decode.run ~file:(target ^ "#" ^ fragment) node Schema.decode
            with
            | Ok _ -> None
            | Error error ->
                Some
                  (refused_at reference
                     (Printf.sprintf "names %s, which is not a JSON Schema: %s"
                        reference.target
                        (Decode.error_to_string error)))))
    | Some (found, _) ->
        Some
          (refused_at reference
             (Printf.sprintf "names %s, %s where a JSON Schema belongs"
                reference.target (kind_name found)))
    | None ->
        Some
          (refused_at reference
             (Printf.sprintf "names %s, which Protocol does not have"
                reference.target))
  in
  let path_references =
    List.concat_map
      (fun (document : Openapi.document) -> List.map snd document.paths)
      documents
  in
  let schema_file_references =
    List.concat_map
      (fun (document : Openapi.document) ->
        List.map snd document.components.schemas)
      documents
  in
  let placed = path_references @ response_references @ schema_file_references in
  let unresolved =
    List.filter_map (whole Path_item) path_references
    @ List.filter_map (whole Response) response_references
    @ List.filter_map (whole Schema_file) schema_file_references
    @ List.filter_map schema
        (List.filter
           (fun reference -> not (List.exists (same reference) placed))
           (List.rev !references))
  in
  (* D1's generic check: every table has its schema, every schema that is
     a table's has its table, and each table is what its schema says. *)
  let tables_here = Places.tables root in
  let read_json file =
    match Hashtbl.find_opt decoded file with
    | Some (_, json) -> Some json
    | None when Sys.file_exists (Filename.concat root file) ->
        Result.to_option (Decode.parse_file ~root file)
    | None -> None
  in
  let schema_cache = Hashtbl.create 256 in
  let schema_at ({ file; pointer } : Validate.place) =
    match Hashtbl.find_opt schema_cache (file, pointer) with
    | Some found -> found
    | None ->
        let found =
          Option.bind (read_json file) (fun json ->
              Option.bind (Decode.at_pointer pointer json) (fun node ->
                  Result.to_option
                    (Result.map fst (Decode.run ~file node Schema.decode))))
        in
        Hashtbl.replace schema_cache (file, pointer) found;
        found
  in
  let stray_case_tables =
    List.filter_map
      (fun file ->
        if Places.is_case_table file && not (List.mem file tables_here) then
          Some
            (refusal file ""
               "is a case table, and a table is at the top of an area")
        else None)
      files
  in
  let names_case_table file (node : Yojson.Safe.t option) =
    match node with
    | Some (`Assoc [ ("$ref", `String target) ]) ->
        fst (Decode.resolve { target; file; pointer = "" }) = Places.case_table
    | _ -> false
  in
  let orphan_schemas =
    List.filter_map
      (fun file ->
        match String.split_on_char '/' file with
        | [ area; name ]
          when List.mem area (Places.areas root)
               && Filename.check_suffix name ".json"
               && name.[0] >= 'A'
               && name.[0] <= 'Z'
               && not
                    (List.exists
                       (fun table -> Places.schema_of table = file)
                       tables_here) -> (
            match read_json file with
            | Some json ->
                let top =
                  (match Decode.at_pointer "/allOf" json with
                    | Some (`List parts) -> List.map Option.some parts
                    | _ -> [])
                  @ [ Decode.at_pointer "/properties/$comment" json ]
                in
                if List.exists (names_case_table file) top then
                  Some
                    (refusal file ""
                       "is a table's schema, and no table beside it is named \
                        for it")
                else None
            | None -> None)
        | _ -> None)
      files
  in
  let validation =
    List.concat_map
      (fun table ->
        let schema_file = Places.schema_of table in
        match
          (read_json table, schema_at { file = schema_file; pointer = "" })
        with
        | _, None ->
            [
              refusal table ""
                (Printf.sprintf "has no schema, which would be %s" schema_file);
            ]
        | None, _ -> []
        | Some json, Some schema ->
            List.map
              (fun (pointer, message) -> refusal table pointer message)
              (Validate.errors schema_at schema json ""))
      tables_here
  in
  let census = census @ stray_case_tables @ orphan_schemas @ validation in
  match (schemas, census @ unresolved) with
  | Ok schemas, [] ->
      let table_files =
        Hashtbl.fold
          (fun file (kind, _) files ->
            if kind = Table then file :: files else files)
          decoded []
        |> List.sort compare
      in
      Ok
        {
          gateway;
          archivist;
          path_items;
          responses;
          schemas;
          tables;
          table_files;
        }
  | Ok _, errors -> Error errors
  (* A file that is no JSON Schema leaves the references to it unresolved, so
     its refusal is given in their place. *)
  | Error errors, _ -> Error (errors @ census)
