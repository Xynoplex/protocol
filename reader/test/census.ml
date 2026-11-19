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

(* [json] with the value at [place], a pointer's tokens, changed. *)
let rec update place change (json : Yojson.Safe.t) =
  match (place, json) with
  | [], value -> change value
  | token :: rest, `Assoc members ->
      `Assoc
        (List.map
           (fun (key, value) ->
             if key = token then (key, update rest change value)
             else (key, value))
           members)
  | token :: rest, `List items ->
      `List
        (List.mapi
           (fun index item ->
             if string_of_int index = token then update rest change item
             else item)
           items)
  | _ -> failwith "no such place"

(* A copy with the value at [place] in [file] changed, refused with
   [expected] at that file. *)
let changed specs ~why file place change expected =
  refused specs ~why
    (fun copied ->
      rewrite
        (Filename.concat copied file)
        (update
           (if place = "" then [] else String.split_on_char '/' place)
           change))
    (file ^ ": " ^ expected)

let surface specs ~why place change expected =
  changed specs ~why "client/surface.json" place change expected

(* [text] with each character outside the Basic Multilingual Plane written as
   JSON's escape of its UTF-16 surrogates: the same value, spelled in ASCII. *)
let escape_astral text =
  let out = Buffer.create (String.length text) in
  let rec from i =
    if i < String.length text then (
      let decoded = String.get_utf_8_uchar text i in
      let length = Uchar.utf_decode_length decoded in
      let code = Uchar.to_int (Uchar.utf_decode_uchar decoded) in
      if code > 0xFFFF then
        let v = code - 0x10000 in
        Printf.bprintf out "\\u%04x\\u%04x"
          (0xD800 lor (v lsr 10))
          (0xDC00 lor (v land 0x3FF))
      else Buffer.add_string out (String.sub text i length);
      from (i + length))
  in
  from 0;
  Buffer.contents out

let () =
  let specs =
    match Sys.argv with
    | [| _; specs |] -> specs
    | _ ->
        prerr_endline "usage: census.exe SPECS";
        exit 2
  in
  let spec =
    match Spec.load specs with
    | Ok spec -> spec
    | Error errors ->
        List.iter (fun e -> prerr_endline (Decode.error_to_string e)) errors;
        Printf.eprintf "Protocol has %d refusals\n" (List.length errors);
        exit 1
  in
  (* A table's value is of the type its schema states, through a reference
     into another table's schema too: content.get's content.stream, which
     sdk-telemetry states through service-telemetry's attribute, is the
     boolean true. *)
  let content_get =
    List.find
      (fun (span : Tables.Sdk_telemetry_telemetry.spans_item) ->
        span.name = "content.get")
      spec.tables.sdk_telemetry_telemetry.spans
  in
  (match
     List.find_map
       (fun (attribute :
              Tables.Sdk_telemetry_telemetry
              .service_telemetry_telemetry_attributes_item) ->
         if attribute.key = "content.stream" then Some attribute.values
         else None)
       content_get.attributes
   with
  | Some (Some [ Boolean true ]) -> ()
  | _ ->
      prerr_endline
        "sdk-telemetry/telemetry.json: content.get's content.stream is not the \
         boolean true";
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
  surface specs ~why:"gives a method an argument it does not take"
    "namespaces/8/methods/4/cases/0/args"
    (members (fun args -> args @ [ ("jobID", `String "job-1") ]))
    "/namespaces/8/methods/4/cases/0/args/jobID: is no parameter of job.status";
  surface specs ~why:"lacks an argument a method requires"
    "namespaces/2/methods/1/cases/0/args"
    (members (List.remove_assoc "annotationId"))
    "/namespaces/2/methods/1/cases/0/args: lacks annotationId, which \
     mark.delete requires";
  surface specs ~why:"gives an argument its parameter's schema refuses"
    "namespaces/7/methods/4/cases/0/args/annotationId"
    (fun _ -> `Int 5)
    "/namespaces/7/methods/4/cases/0/args/annotationId: is none of the anyOf";
  surface specs ~why:"misspells a member of an object argument"
    "namespaces/5/methods/0/cases/1/args/options"
    (members
       (List.map (function
         | "limit", value -> ("limmit", value)
         | member -> member)))
    "/namespaces/5/methods/0/cases/1/args/options/limmit: is a member the \
     schema does not state";
  surface specs ~why:"gives an argument below its scalar's minimum"
    "namespaces/6/methods/1/cases/1/args/stallDeadlineMs"
    (fun _ -> `Int (-1))
    "/namespaces/6/methods/1/cases/1/args/stallDeadlineMs: is less than 0";
  surface specs ~why:"names a fixture the table has not"
    "namespaces/5/methods/0/cases/0/args/context"
    (fun _ -> `Assoc [ ("$fixture", `String "nowhere") ])
    "/namespaces/5/methods/0/cases/0/args/context: names the fixture nowhere, \
     which the table has not";
  surface specs ~why:"gives a callback as an argument"
    "namespaces/8/methods/5/cases/0/args"
    (members (fun args -> args @ [ ("onStatus", `Assoc []) ]))
    "/namespaces/8/methods/5/cases/0/args/onStatus: is a callback, which a \
     case cannot give";
  surface specs ~why:"has a method take a parameter twice"
    "namespaces/8/methods/4/parameters"
    (function
      | `List [ parameter ] -> `List [ parameter; parameter ]
      | _ -> failwith "job.status takes one parameter")
    "/namespaces/8/methods/4/parameters/1: names jobId, which job.status takes \
     already";
  let bounds = "bus/reply-bounds.json" in
  changed specs ~why:"names a reply of no operation" bounds "replies"
    (members (fun replies ->
         replies
         @ [
             ( "nowhere:requested",
               `Assoc [ ("response", `Assoc [ ("grows", `String "always") ]) ]
             );
           ]))
    "/replies/nowhere:requested: names nowhere:requested, which is no \
     operation of the registry";
  refused specs ~why:"leaves a reply's unbounded list unnamed"
    (fun copied ->
      rewrite
        (Filename.concat copied bounds)
        (update [ "replies" ]
           (members (List.remove_assoc "browse:entity-types-requested"))))
    "components/schemas/GetEntityTypesResponse.json: /properties/entityTypes: \
     is response.entityTypes of browse:entity-types-result, a list with no \
     bound that bus/reply-bounds.json does not name: bound it in its schema, \
     or say what bounds it";
  refused specs ~why:"names a list its schema bounds"
    (fun copied ->
      rewrite
        (Filename.concat copied "components/schemas/GetEntityTypesResponse.json")
        (update
           [ "properties"; "entityTypes" ]
           (members (fun schema -> schema @ [ ("maxItems", `Int 64) ]))))
    "bus/reply-bounds.json: \
     /replies/browse:entity-types-requested/response.entityTypes: names \
     response.entityTypes of browse:entity-types-result, which its schema \
     bounds";
  changed specs ~why:"names a list a reply does not carry" bounds
    "replies/browse:entity-types-requested"
    (members (fun entries ->
         entries
         @ [ ("response.nothing", `Assoc [ ("grows", `String "always") ]) ]))
    "/replies/browse:entity-types-requested/response.nothing: names \
     response.nothing of browse:entity-types-result, which it does not carry";
  changed specs ~why:"bounds a list by no member of its request" bounds
    "replies/match:search-requested/response/by"
    (fun _ -> `String "nowhere")
    "/replies/match:search-requested/response/by: is nowhere, which is no \
     member of match:search-requested";
  changed specs ~why:"bounds a list by a member that bounds nothing" bounds
    "replies/match:search-requested/response/by"
    (fun _ -> `String "useSemanticScoring")
    "/replies/match:search-requested/response/by: is useSemanticScoring of \
     match:search-requested, which is neither an array nor an integer with a \
     maximum";
  changed specs ~why:"has a description of an offset leave its unit out"
    "components/schemas/TextPositionSelector.json"
    "properties/start/description"
    (fun _ -> `String "Where the selection starts.")
    "/properties/start: carries an offset or a length of text, and its \
     description does not say that it counts code points";
  changed specs ~why:"loses a member that carries an offset"
    "client/types/TextSpan.json" "allOf/0/properties"
    (members (List.remove_assoc "end"))
    "/allOf/0/properties/end: is listed as carrying an offset or a length of \
     text, and is not there";
  changed specs ~why:"has a new description speak of characters"
    "components/schemas/UnitCursor.json" "properties/size/description"
    (fun _ -> `String "How many characters the piece was cut at.")
    "/properties/size: speaks of an offset or of characters and does not say \
     \"code points\": \"How many characters the piece was cut at.\"";
  refused specs ~why:"spells a counting table's astral characters in ASCII"
    (fun copied ->
      let path = Filename.concat copied "text/offset-cases.json" in
      write path
        (escape_astral (In_channel.with_open_bin path In_channel.input_all)))
    "text/offset-cases.json: /: counts offsets or lengths of text and has no \
     character outside the Basic Multilingual Plane to tell code points from \
     UTF-16 code units or bytes";
  changed specs ~why:"lets a cancellation name more than a job"
    "components/schemas/JobCancelRequest.json" "properties"
    (members (fun m ->
         m @ [ ("reason", `Assoc [ ("type", `String "string") ]) ]))
    "/: is not exactly one required jobId, a JobId: a cancellation names one \
     job, and selects no other way";
  changed specs ~why:"tells filters apart by another property"
    "components/schemas/JobFilter.json" "discriminator/propertyName"
    (fun _ -> `String "kind")
    "/discriminator: JobFilter is not told apart by jobType";
  changed specs ~why:"maps a job description to no yield job"
    "components/schemas/JobCreateCommand.json" "discriminator/mapping"
    (members (List.remove_assoc "yield"))
    "/discriminator/mapping: JobCreateCommand is told apart by [mark], JobType \
     is [mark,yield]";
  changed specs ~why:"has a filter select the other job type"
    "components/schemas/MarkJobFilter.json" "properties/jobType/enum"
    (fun _ -> `List [ `String "yield" ])
    "/properties/jobType: JobFilter selects ./MarkJobFilter.json by jobType \
     mark, whose jobType is [yield]";
  changed specs ~why:"opens a motivation's parameters"
    "components/schemas/HighlightingJobParams.json" ""
    (members (List.remove_assoc "additionalProperties"))
    "/: a highlighting job's parameters are open: a parameter it does not take \
     would be accepted";
  changed specs ~why:"announces a mark job with other parameters"
    "components/schemas/MarkJobQueuedEvent.json" "properties/params/$ref"
    (fun _ -> `String "./GenerationJobParams.json")
    "/properties/params: a mark job is announced with \
     ./GenerationJobParams.json, and created with ./MarkJobParams.json";
  changed specs ~why:"states a yield job's parameters in three parts"
    "components/schemas/GenerationJobParams.json" "allOf"
    (function
      | `List parts -> `List (parts @ [ `Assoc [ ("type", `String "object") ] ])
      | _ -> failwith "no allOf")
    "/allOf: a yield job's parameters are not stated as what is asked for and \
     its input";
  changed specs ~why:"gives a yield job an input beside its context"
    "components/schemas/GenerationJobParams.json" "allOf/1/properties"
    (members (fun m -> m @ [ ("more", `Assoc [ ("type", `String "string") ]) ]))
    "/allOf/1: a yield job's input is [context,more], and an announcement \
     leaves out exactly its context";
  changed specs ~why:"opens a filter" "components/schemas/YieldJobFilter.json"
    ""
    (members (List.remove_assoc "additionalProperties"))
    "/: a yield filter is open";
  changed specs ~why:"has a filter name what a job does not have"
    "components/schemas/YieldJobFilter.json" "properties"
    (members (fun m ->
         m @ [ ("nowhere", `Assoc [ ("type", `String "string") ]) ]))
    "/properties/nowhere: a yield filter names nowhere, which a yield job \
     description does not have";
  changed specs ~why:"lets a mark filter leave out its motivation"
    "components/schemas/MarkJobFilter.json" "properties/params/required"
    (fun _ -> `List [])
    "/properties/params: a mark filter does not always state its motivation";
  changed specs ~why:"gives JobResult a discriminant"
    "components/schemas/JobResult.json" ""
    (members (fun m ->
         m @ [ ("discriminator", `Assoc [ ("propertyName", `String "kind") ]) ]))
    "/discriminator: JobResult has a discriminant of its own";
  changed specs ~why:"opens a result" "components/schemas/JobDeclined.json" ""
    (members (List.remove_assoc "additionalProperties"))
    "/: ./JobDeclined.json is open: another result would decode as it";
  changed specs ~why:"has a result state a kind"
    "components/schemas/JobDeclined.json" "properties"
    (members (fun m -> m @ [ ("kind", `Assoc [ ("type", `String "string") ]) ]))
    "/properties/kind: ./JobDeclined.json states a kind";
  refused specs ~why:"has two results require one member"
    (fun copied ->
      rewrite (Filename.concat copied "components/schemas/JobDeclined.json")
        (fun json ->
          json
          |> update [ "properties" ]
               (members (fun m ->
                    m @ [ ("found", `Assoc [ ("type", `String "integer") ]) ]))
          |> update [ "required" ] (function
            | `List r -> `List (r @ [ `String "found" ])
            | _ -> failwith "no required")))
    "components/schemas/JobDeclined.json: /required: ./JobDeclined.json and \
     ./MarkJobDone.json both require found";
  changed specs ~why:"has JobResult hold a result no verb reports"
    "components/schemas/JobResult.json" "oneOf"
    (function
      | `List rs ->
          `List (rs @ [ `Assoc [ ("$ref", `String "./JobPending.json") ] ])
      | _ -> failwith "no oneOf")
    "/oneOf: the verbs report \
     [./JobDeclined.json,./MarkJobDone.json,./YieldJobDone.json], JobResult is \
     [./MarkJobDone.json,./YieldJobDone.json,./JobDeclined.json,./JobPending.json]";
  changed specs ~why:"names a storage record that is no schema"
    "jobs/storage.json" "bucket/record"
    (fun _ -> `String "Nowhere")
    "/bucket/record: the storage layout's record Nowhere is not a schema";
  changed specs ~why:"selects a job by a status it does not state"
    "components/schemas/JobPending.json" "properties/status/enum"
    (fun _ -> `List [ `String "running" ])
    "/properties/status: Job's discriminator selects ./JobPending.json by \
     status pending, whose status is [running]";
  changed specs ~why:"has the dispatcher's document default a field"
    "dispatcher/DispatcherConfig.json" "properties/port"
    (members (fun m -> m @ [ ("default", `Int 8080) ]))
    "/properties/port/default: DispatcherConfig defaults port";
  changed specs ~why:"has a worker's document leave a field optional"
    "worker/WorkerConfig.json" "required"
    (function
      | `List r -> `List (List.filter (( <> ) (`String "port")) r)
      | _ -> failwith "no required")
    "/properties/port: WorkerConfig leaves port optional";
  refused specs ~why:"lacks a schema of what the Archivist keeps"
    (fun copied ->
      Sys.remove
        (Filename.concat copied "components/schemas/ProfilesProjection.json"))
    "archivist/ProfilesProjection.json: /: ProfilesProjection has no schema in \
     specs/src/archivist or specs/src/components/schemas";
  changed specs ~why:"has ArchivistConfig default a field"
    "archivist/ArchivistConfig.json" "properties/port"
    (members (fun m -> m @ [ ("default", `Int 9000) ]))
    "/properties/port/default: ArchivistConfig defaults port";
  changed specs ~why:"has ArchivistConfig leave a field optional"
    "archivist/ArchivistConfig.json" "required"
    (function
      | `List r -> `List (List.filter (( <> ) (`String "root")) r)
      | _ -> failwith "no required")
    "/properties/root: ArchivistConfig leaves root optional";
  changed specs ~why:"gives the Archivist no yield engine"
    "components/schemas/ArchivistEngines.json" "properties/jobs/properties"
    (members (List.remove_assoc "yield"))
    "/properties/jobs: the engines' job types are [mark], JobType is \
     [mark,yield]";
  changed specs ~why:"gives the Archivist no tagging engine"
    "components/schemas/ArchivistEngines.json"
    "properties/jobs/properties/mark/properties"
    (members (List.remove_assoc "tagging"))
    "/properties/jobs/properties/mark: the engines' mark motivations are \
     [highlighting,commenting,assessing,linking], Motivation is \
     [assessing,commenting,highlighting,linking,tagging]";
  changed specs ~why:"restates an extraction's method otherwise"
    "archivist/AnchoredTextExtractedEntry.json" "properties/method"
    (fun _ -> `Assoc [ ("type", `String "string") ])
    "/properties/method: AnchoredTextEntry's method is not ExtractedText's";
  changed specs ~why:"restates the decline classes otherwise"
    "archivist/AnchoredTextDeclinedEntry.json" "properties/declined"
    (fun _ -> `Assoc [ ("type", `String "string") ])
    "/properties/declined: AnchoredTextEntry's declined is not \
     ExtractionDeclined's";
  print_endline
    "The model reads every file of Protocol, and refuses a copy with a stray \
     file, a missing table, a dangling reference, an unknown keyword, a \
     missing path item, a table without its schema, a schema without its \
     table, a case table off an area's top, a value its schema refuses, a \
     component the gateway does not list, a service's schema it lists, a \
     gateway route or a component that refers into a service's area, \
     Archivist's own schema among the components, a client surface case whose \
     arguments are not its method's parameters, a reply's list neither bounded \
     nor named, an offset whose unit is unsaid, a job protocol that disagrees \
     with itself, or an Archivist stated short; and it reads each value as the \
     type its schema states"
