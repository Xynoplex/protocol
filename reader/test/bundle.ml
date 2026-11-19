(* bundle.exe SPECS: Bundle bundles every OpenAPI document of Protocol's
   specs/src, each to one that names nothing outside itself; a small specs
   tree, its schemas named by their [$id]s, bundles as Redocly bundles the
   same tree named by path, with no [$schema] or [$id] in it; and the
   bundler refuses a tree it would bundle otherwise. *)

let write specs (file, json) =
  let path = Filename.concat specs file in
  let rec directory d =
    if not (Sys.file_exists d) then (
      directory (Filename.dirname d);
      Sys.mkdir d 0o755)
  in
  directory (Filename.dirname path);
  Out_channel.with_open_bin path (fun out ->
      Out_channel.output_string out (Yojson.Safe.to_string json))

let id name = Decode.base ^ name

(* A schema's file: [members], stating its dialect and its [$id]. *)
let identified name members =
  `Assoc
    (("$schema", `String Decode.dialect)
    :: ("$id", `String (id name))
    :: members)

let ref_ ?description target =
  `Assoc
    (("$ref", `String target)
    :: Option.fold ~none:[]
         ~some:(fun d -> [ ("description", `String d) ])
         description)

(* A document with a path item, a listed schema that names an unlisted one
   beside its directory and one in a service's area, and a response its
   directory makes a component. *)
let tree =
  [
    ( "openapi.json",
      `Assoc
        [
          ("openapi", `String "3.1.1");
          ("paths", `Assoc [ ("/a", ref_ "paths/a.json") ]);
          ( "components",
            `Assoc
              [
                ("schemas", `Assoc [ ("Listed", ref_ (id "Listed.json")) ]);
                ( "securitySchemes",
                  `Assoc [ ("bearer", `Assoc [ ("type", `String "http") ]) ] );
              ] );
        ] );
    ( "paths/a.json",
      `Assoc
        [
          ( "get",
            `Assoc
              [
                ( "responses",
                  `Assoc
                    [ ("200", ref_ "../components/responses/Answered.json") ] );
              ] );
        ] );
    ( "components/responses/Answered.json",
      `Assoc
        [
          ("description", `String "answered");
          ( "content",
            `Assoc
              [
                ( "application/json",
                  `Assoc [ ("schema", ref_ (id "Listed.json")) ] );
              ] );
        ] );
    ( "components/schemas/Listed.json",
      identified "Listed.json"
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ("beside", ref_ ~description:"said here" (id "Beside.json"));
                ("area", ref_ (id "InArea.json"));
              ] );
          ( "discriminator",
            `Assoc
              [
                ("propertyName", `String "kind");
                ("mapping", `Assoc [ ("b", `String (id "Beside.json")) ]);
              ] );
        ] );
    ( "components/schemas/Beside.json",
      identified "Beside.json" [ ("type", `String "string") ] );
    ( "area/InArea.json",
      identified "InArea.json" [ ("type", `String "integer") ] );
  ]

let components kind n = `String ("#/components/" ^ kind ^ "/" ^ n)

let bundled =
  `Assoc
    [
      ("openapi", `String "3.1.1");
      ( "paths",
        `Assoc
          [
            ( "/a",
              `Assoc
                [
                  ( "get",
                    `Assoc
                      [
                        ( "responses",
                          `Assoc
                            [
                              ( "200",
                                `Assoc
                                  [
                                    ("$ref", components "responses" "Answered");
                                  ] );
                            ] );
                      ] );
                ] );
          ] );
      ( "components",
        `Assoc
          [
            ( "responses",
              `Assoc
                [
                  ( "Answered",
                    `Assoc
                      [
                        ("description", `String "answered");
                        ( "content",
                          `Assoc
                            [
                              ( "application/json",
                                `Assoc
                                  [
                                    ( "schema",
                                      `Assoc
                                        [
                                          ("$ref", components "schemas" "Listed");
                                        ] );
                                  ] );
                            ] );
                      ] );
                ] );
            ( "schemas",
              `Assoc
                [
                  ("Beside", `Assoc [ ("type", `String "string") ]);
                  ("InArea", `Assoc [ ("type", `String "integer") ]);
                  ( "Listed",
                    `Assoc
                      [
                        ("type", `String "object");
                        ( "properties",
                          `Assoc
                            [
                              ( "beside",
                                `Assoc
                                  [
                                    ("$ref", components "schemas" "Beside");
                                    ("description", `String "said here");
                                  ] );
                              ( "area",
                                `Assoc
                                  [ ("$ref", components "schemas" "InArea") ] );
                            ] );
                        ( "discriminator",
                          `Assoc
                            [
                              ("propertyName", `String "kind");
                              ( "mapping",
                                `Assoc [ ("b", components "schemas" "Beside") ]
                              );
                            ] );
                      ] );
                ] );
            ( "securitySchemes",
              `Assoc [ ("bearer", `Assoc [ ("type", `String "http") ]) ] );
          ] );
    ]

(* The tree, with [changes] written over it, bundled. *)
let bundle changes =
  let specs = Filename.temp_dir "bundle" "" in
  List.iter (write specs) tree;
  List.iter (write specs) changes;
  Bundle.document ~specs ~ids:(Result.get_ok (Places.ids specs)) "openapi.json"

let rec sorted : Yojson.Safe.t -> Yojson.Safe.t = function
  | `Assoc members ->
      `Assoc
        (List.sort compare (List.map (fun (k, v) -> (k, sorted v)) members))
  | `List items -> `List (List.map sorted items)
  | v -> v

let failures = ref 0

let fail fmt =
  Printf.ksprintf
    (fun message ->
      incr failures;
      prerr_endline message)
    fmt

let refused ~why changes ~file expected =
  match bundle changes with
  | Ok _ -> fail "A tree that %s was bundled" why
  | Error { Decode.file = f; message; _ } when f = file && message = expected ->
      ()
  | Error error ->
      fail "A tree that %s was refused with\n  %s\nnot\n  %s: %s" why
        (Decode.error_to_string error)
        file expected

(* Every reference in [json], and every discriminator's mapping, names a
   component [json] holds. *)
let names_within document json =
  let rec walk (j : Yojson.Safe.t) =
    match j with
    | `Assoc members ->
        List.iter
          (function
            | "$ref", `String target -> holds target
            | "mapping", `Assoc mapping
              when List.mem_assoc "propertyName" members ->
                List.iter
                  (function
                    | _, `String target -> holds target
                    | value, _ -> fail "%s maps %s to no text" document value)
                  mapping
            | _, v -> walk v)
          members
    | `List items -> List.iter walk items
    | _ -> ()
  and holds target =
    if not (String.starts_with ~prefix:"#/components/" target) then
      fail "%s names %s, outside itself" document target
    else
      match
        Decode.at_pointer (String.sub target 1 (String.length target - 1)) json
      with
      | Some _ -> ()
      | None -> fail "%s names %s, which it does not hold" document target
  in
  walk json

let () =
  let specs =
    match Sys.argv with
    | [| _; specs |] -> specs
    | _ ->
        prerr_endline "usage: bundle.exe SPECS";
        exit 2
  in
  let rec documents dir =
    List.concat_map
      (fun name ->
        let path = if dir = "" then name else dir ^ "/" ^ name in
        if Sys.is_directory (Filename.concat specs path) then documents path
        else if name = "openapi.json" then [ path ]
        else [])
      (List.sort compare
         (Array.to_list (Sys.readdir (Filename.concat specs dir))))
  in
  if
    List.sort compare (documents "")
    <> List.sort compare (List.map fst Bundle.documents)
  then
    fail "specs/src's OpenAPI documents are %s, and Bundle bundles %s"
      (String.concat ", " (documents ""))
      (String.concat ", " (List.map fst Bundle.documents));
  List.iter
    (fun (document, _) ->
      match
        Bundle.document ~specs ~ids:(Result.get_ok (Places.ids specs)) document
      with
      | Ok json -> names_within document json
      | Error error ->
          fail "%s is refused: %s" document (Decode.error_to_string error))
    Bundle.documents;
  (match bundle [] with
  | Ok json when sorted json = sorted bundled -> ()
  | Ok json ->
      fail "The tree bundles to\n%s" (Yojson.Safe.pretty_to_string json)
  | Error error -> fail "The tree is refused: %s" (Decode.error_to_string error));
  refused ~why:"refers within a file"
    [ ("paths/a.json", `Assoc [ ("get", ref_ "#/x") ]) ]
    ~file:"paths/a.json" "#/x refers within its file: specs/src refers by file";
  refused ~why:"points into a file"
    [ ("paths/a.json", `Assoc [ ("get", ref_ "../area/InArea.json#/type") ]) ]
    ~file:"paths/a.json"
    "../area/InArea.json#/type points into a file: specs/src refers to whole \
     files";
  refused ~why:"constrains a reference"
    [
      ( "components/schemas/Beside.json",
        identified "Beside.json"
          [
            ( "items",
              `Assoc
                [
                  ("$ref", `String "../../area/InArea.json"); ("maximum", `Int 1);
                ] );
          ] );
    ]
    ~file:"components/schemas/Beside.json"
    "states maximum beside its reference to ../../area/InArea.json";
  refused ~why:"names a service's schema that no schema names"
    [
      ("area/Unnamed.json", `Assoc [ ("type", `String "null") ]);
      ( "components/responses/Answered.json",
        `Assoc
          [
            ( "content",
              `Assoc
                [
                  ( "application/json",
                    `Assoc [ ("schema", ref_ "../../area/Unnamed.json") ] );
                ] );
          ] );
    ]
    ~file:"components/responses/Answered.json"
    "names ../../area/Unnamed.json, which is no path item, no component the \
     document lists, and in no directory of components";
  refused ~why:"gives two schemas one name"
    [
      ("area/Beside.json", `Assoc [ ("type", `String "null") ]);
      ( "components/schemas/Beside.json",
        identified "Beside.json" [ ("items", ref_ "../../area/Beside.json") ] );
    ]
    ~file:"area/Beside.json"
    "is named schemas/Beside, which names components/schemas/Beside.json too";
  refused ~why:"lists a schema under two names"
    [
      ( "openapi.json",
        `Assoc
          [
            ("openapi", `String "3.1.1");
            ("paths", `Assoc [ ("/a", ref_ "paths/a.json") ]);
            ( "components",
              `Assoc
                [
                  ( "schemas",
                    `Assoc
                      [
                        ("Listed", ref_ "components/schemas/Listed.json");
                        ("Again", ref_ "components/schemas/Listed.json");
                      ] );
                ] );
          ] );
    ]
    ~file:"components/schemas/Listed.json"
    "is named both schemas/Listed and schemas/Again";
  refused ~why:"maps a discriminator's value to a response"
    [
      ( "openapi.json",
        `Assoc
          [
            ("openapi", `String "3.1.1");
            ("paths", `Assoc [ ("/a", ref_ "paths/a.json") ]);
            ( "components",
              `Assoc
                [
                  ( "responses",
                    `Assoc
                      [
                        ("Answered", ref_ "components/responses/Answered.json");
                      ] );
                  ( "schemas",
                    `Assoc [ ("Listed", ref_ "components/schemas/Listed.json") ]
                  );
                ] );
          ] );
      ( "components/schemas/Beside.json",
        identified "Beside.json"
          [
            ( "discriminator",
              `Assoc
                [
                  ("propertyName", `String "kind");
                  ( "mapping",
                    `Assoc [ ("a", `String "../responses/Answered.json") ] );
                ] );
          ] );
    ]
    ~file:"components/schemas/Beside.json"
    "names ../responses/Answered.json as a schema, and it is responses/Answered";
  if !failures > 0 then exit 1;
  print_endline
    "Bundle bundles every OpenAPI document of specs/src, each to one that \
     names nothing outside itself, a tree named by $id bundles as Redocly \
     bundles it named by path, and the bundler refuses a reference within a \
     file or into one, a constrained reference, a schema it cannot name, one \
     name for two files, two names for one, and a schema that names a response"
