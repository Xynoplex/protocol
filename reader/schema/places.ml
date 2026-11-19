(* Where Protocol's tables are, and where each one's schema is. A table is a
   JSON file at the top of an area of specs/src, named in kebab case, but an
   area's openapi.json, which is its API document; components/ and paths/
   are the gateway document's, and hold none. A case table is a table whose
   name ends "cases.json" (specs/README.md, "Case tables"). Both CaseTable.json
   and JsonSchema.json are at specs/src's top.

   A table's schema is beside it, named by the area and the file's stem in
   PascalCase, so jobs/retry-cases.json's is jobs/JobsRetryCases.json. That
   name is its model's too: the module Jobs_retry_cases. *)

let case_table = "CaseTable.json"

(* The gateway's public schemas (specs/README.md, "Layout"): its document
   lists every file of components/schemas and the client's discovery
   document's, which discovery/ holds. A service's own schemas are in its
   area. *)
let components = "components/schemas/"
let discovery = "discovery/"
let is_component file = String.starts_with ~prefix:components file

(* What a table member that holds a JSON Schema names: such a member is one,
   read as Schema reads any. *)
let json_schema = "JsonSchema.json"
let is_case_table table = String.ends_with ~suffix:"cases.json" table

let is_kebab name =
  name <> ""
  && (match name.[0] with 'a' .. 'z' -> true | _ -> false)
  && String.for_all
       (function 'a' .. 'z' | '0' .. '9' | '-' -> true | _ -> false)
       name

let stem name = Filename.remove_extension name

let is_table name =
  Filename.check_suffix name ".json"
  && name <> "openapi.json"
  && is_kebab (stem name)

let sorted_entries directory =
  Sys.readdir directory |> Array.to_list |> List.sort compare

let areas root =
  sorted_entries root
  |> List.filter (fun area ->
      area <> "components" && area <> "paths"
      && Sys.is_directory (Filename.concat root area))

(* Every table of specs/src at [root], as a path from it, in order. *)
let tables root =
  List.concat_map
    (fun area ->
      sorted_entries (Filename.concat root area)
      |> List.filter is_table
      |> List.map (fun name -> area ^ "/" ^ name))
    (areas root)

let area_and_stem table =
  match String.split_on_char '/' table with
  | [ area; name ] when is_kebab area && is_table name -> (area, stem name)
  | _ -> invalid_arg (table ^ " is not <area>/<stem>.json, where a table is")

let pascal words =
  String.split_on_char '-' words
  |> List.map String.capitalize_ascii
  |> String.concat ""

(* The schema of [table], from specs/src. *)
let schema_of table =
  let area, stem = area_and_stem table in
  area ^ "/" ^ pascal area ^ pascal stem ^ ".json"

(* The areas whose schemas are a service's own, such as its configuration
   document and its health answer (specs/README.md, "Layout"): each
   service's, and the stores' and the embedding model's. *)
let services =
  [
    "archivist";
    "dispatcher";
    "embedding";
    "gateway";
    "graph";
    "librarian";
    "smelter";
    "vectors";
    "weaver";
    "worker";
  ]

(* The services' schemas at [root], as paths from it, in order: each a JSON
   file at the top of a service's area, named in PascalCase, that is no
   table's schema. A service's area that is no directory holds none. *)
let service_schemas root =
  let tables_schemas = List.map schema_of (tables root) in
  List.concat_map
    (fun area ->
      let directory = Filename.concat root area in
      if not (Sys.file_exists directory && Sys.is_directory directory) then []
      else
        sorted_entries directory
        |> List.filter (fun name ->
            Filename.check_suffix name ".json"
            && match name.[0] with 'A' .. 'Z' -> true | _ -> false)
        |> List.map (fun name -> area ^ "/" ^ name)
        |> List.filter (fun file -> not (List.mem file tables_schemas)))
    services

(* The name of [table]'s model, as a value: jobs_retry_cases. Its module is
   that name capitalized. *)
let model_of table =
  let area, stem = area_and_stem table in
  String.map (function '-' -> '_' | c -> c) (area ^ "_" ^ stem)

(* Every JSON file under [root], as a path from it. *)
let rec json_files root relative =
  let path = Filename.concat root relative in
  if Sys.is_directory path then
    List.concat_map
      (fun name ->
        json_files root (if relative = "" then name else relative ^ "/" ^ name))
      (sorted_entries path)
  else if Filename.check_suffix relative ".json" then [ relative ]
  else []

(* Each schema's [$id], as the JSON files under [root] state them at their
   roots ([Decode.ids]). Which files state one, and what, the reader holds. *)
let ids root =
  Decode.ids
    (List.filter_map
       (fun file ->
         match Decode.parse_file ~root file with
         | Ok (`Assoc members) -> (
             match List.assoc_opt "$id" members with
             | Some (`String id) -> Some (id, file)
             | _ -> None)
         | _ -> None)
       (json_files root ""))
