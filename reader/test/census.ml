(* census.exe SPECS: the model reads every file of Protocol's specs/src, and
   refuses a copy of it that has what the model does not know, lacks what it
   names, or has a table that is not what its schema says. *)

let rec copy source target =
  if Sys.is_directory source then (
    Sys.mkdir target 0o755;
    Array.iter
      (fun name ->
        copy (Filename.concat source name) (Filename.concat target name))
      (Sys.readdir source))
  else
    Out_channel.with_open_bin target (fun out ->
        Out_channel.output_string out
          (In_channel.with_open_bin source In_channel.input_all))

let write path text =
  Out_channel.with_open_bin path (fun out -> Out_channel.output_string out text)

let messages = function
  | Ok _ -> []
  | Error errors -> List.map Decode.error_to_string errors

(* A copy of the specs, changed, must be refused with this refusal among
   those it gives. *)
let refused specs ~why change expected =
  let copied = Filename.concat (Filename.temp_dir "census" "") "src" in
  copy specs copied;
  change copied;
  let found = messages (Spec.load copied) in
  if not (List.mem expected found) then (
    Printf.eprintf "A copy that %s was not refused with:\n  %s\nbut with:\n" why
      expected;
    List.iter (Printf.eprintf "  %s\n") found;
    exit 1)

let rewrite path change =
  write path (Yojson.Safe.to_string (change (Yojson.Safe.from_file path)))

let members f = function
  | `Assoc members -> `Assoc (f members)
  | _ -> failwith "not an object"

(* Every "$ref" in [json] that names [from] names [onto] instead. *)
let rec repoint ~from ~onto = function
  | `Assoc members ->
      `Assoc
        (List.map
           (function
             | "$ref", `String target when target = from ->
                 ("$ref", `String onto)
             | name, value -> (name, repoint ~from ~onto value))
           members)
  | `List values -> `List (List.map (repoint ~from ~onto) values)
  | value -> value

(* The gateway's document's listing, changed. *)
let gateway_listing copied change =
  rewrite
    (Filename.concat copied "openapi.json")
    (members
       (List.map (function
         | "components", components ->
             ( "components",
               members
                 (List.map (function
                   | "schemas", `Assoc schemas ->
                       ("schemas", `Assoc (change schemas))
                   | member -> member))
                 components )
         | member -> member)))

let list_in_gateway copied name target =
  gateway_listing copied (fun schemas ->
      schemas @ [ (name, `Assoc [ ("$ref", `String target) ]) ])

let () =
  let specs =
    match Sys.argv with
    | [| _; specs |] -> specs
    | _ ->
        prerr_endline "usage: census.exe SPECS";
        exit 2
  in
  (match Spec.load specs with
  | Ok _ -> ()
  | Error errors ->
      List.iter (fun e -> prerr_endline (Decode.error_to_string e)) errors;
      Printf.eprintf "Protocol has %d refusals\n" (List.length errors);
      exit 1);
  refused specs ~why:"has a file the model does not know"
    (fun copied -> write (Filename.concat copied "retry/notes.txt") "")
    "retry/notes.txt: /: a file the model does not know";
  refused specs ~why:"lacks a table the model names"
    (fun copied -> Sys.remove (Filename.concat copied "retry/cases.json"))
    "retry/cases.json: /: the model reads this file, and Protocol has none";
  refused specs ~why:"has a reference naming nothing"
    (fun copied ->
      write
        (Filename.concat copied "components/schemas/Dangling.json")
        {|{"$ref":"Nowhere.json"}|})
    "components/schemas/Dangling.json: /$ref: names Nowhere.json, which \
     Protocol does not have";
  refused specs ~why:"has a schema with a keyword the model does not know"
    (fun copied ->
      write
        (Filename.concat copied "components/schemas/Unknown.json")
        {|{"type":"string","contentEncoding":"base64"}|})
    "components/schemas/Unknown.json: /contentEncoding: an unknown member";
  refused specs ~why:"lacks a path item a document names"
    (fun copied -> Sys.remove (Filename.concat copied "paths/api_health.json"))
    "openapi.json: /paths/~1api~1health/$ref: names paths/api_health.json, \
     which Protocol does not have";
  refused specs ~why:"lacks a table's schema"
    (fun copied -> Sys.remove (Filename.concat copied "retry/RetryCases.json"))
    "retry/cases.json: /: has no schema, which would be retry/RetryCases.json";
  refused specs ~why:"has a table's schema with no table"
    (fun copied ->
      write
        (Filename.concat copied "retry/RetryLost.json")
        {|{"allOf":[{"$ref":"../CaseTable.json"}]}|})
    "retry/RetryLost.json: /: is a table's schema, and no table beside it is \
     named for it";
  refused specs ~why:"has a case table off the top of an area"
    (fun copied ->
      write (Filename.concat copied "components/stray-cases.json") "{}")
    "components/stray-cases.json: /: is a case table, and a table is at the \
     top of an area";
  refused specs ~why:"has a table value its schema refuses"
    (fun copied ->
      rewrite
        (Filename.concat copied "jobs/retry-cases.json")
        (members
           (List.map (function
             | "cases", `List (`Assoc case :: rest) ->
                 ( "cases",
                   `List
                     (`Assoc
                        (List.map
                           (function
                             | "retryCount", _ -> ("retryCount", `Int (-1))
                             | member -> member)
                           case)
                     :: rest) )
             | member -> member))))
    "jobs/retry-cases.json: /cases/0/retryCount: is less than 0";
  refused specs
    ~why:"has a component a route uses that the gateway does not list"
    (fun copied -> gateway_listing copied (List.remove_assoc "HealthResponse"))
    "components/schemas/HealthResponse.json: /: is a component schema, and the \
     gateway's document does not list it";
  refused specs ~why:"has a service's schema listed by the gateway"
    (fun copied ->
      list_in_gateway copied "WorkerConfig" "worker/WorkerConfig.json")
    "openapi.json: /components/schemas/WorkerConfig/$ref: lists \
     worker/WorkerConfig.json, which is neither a component schema nor \
     discovery's: the gateway's document lists the public schemas alone";
  refused specs ~why:"has a gateway route that uses a service's schema"
    (fun copied ->
      rewrite
        (Filename.concat copied "paths/api_health.json")
        (repoint ~from:"../components/schemas/HealthResponse.json"
           ~onto:"../dispatcher/DispatcherHealth.json"))
    "paths/api_health.json: \
     /get/responses/200/content/application~1json/schema/$ref: names \
     ../dispatcher/DispatcherHealth.json, outside components/schemas: what a \
     gateway route uses is a component schema";
  refused specs ~why:"has a component schema that refers into a service's area"
    (fun copied ->
      write
        (Filename.concat copied "components/schemas/Stray.json")
        {|{"$ref":"../../worker/WorkerConfig.json"}|};
      list_in_gateway copied "Stray" "components/schemas/Stray.json")
    "components/schemas/Stray.json: /$ref: names \
     ../../worker/WorkerConfig.json, outside components/schemas: a component \
     schema refers to component schemas alone";
  refused specs ~why:"has Archivist's own schema put back among the components"
    (fun copied ->
      let at = Filename.concat copied in
      Sys.rename
        (at "archivist/ArchivistHealth.json")
        (at "components/schemas/ArchivistHealth.json");
      rewrite
        (at "archivist/paths/health.json")
        (repoint ~from:"../ArchivistHealth.json"
           ~onto:"../../components/schemas/ArchivistHealth.json");
      rewrite
        (at "archivist/openapi.json")
        (repoint ~from:"./ArchivistHealth.json"
           ~onto:"../components/schemas/ArchivistHealth.json");
      list_in_gateway copied "ArchivistHealth"
        "components/schemas/ArchivistHealth.json")
    "components/schemas/ArchivistHealth.json: /: is reached by Archivist's \
     document alone, not by a gateway route or the bus: it is Archivist's own, \
     so its place is archivist/";
  print_endline
    "The model reads every file of Protocol, and refuses a copy with a stray \
     file, a missing table, a dangling reference, an unknown keyword, a \
     missing path item, a table without its schema, a schema without its \
     table, a case table off an area's top, a value its schema refuses, a \
     component the gateway does not list, a service's schema it lists, a \
     gateway route or a component that refers into a service's area, or \
     Archivist's own schema among the components"
