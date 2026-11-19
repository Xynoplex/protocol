(* Protocol, as the harness reads it: the reader's model of specs/src, and
   what the harness asks of it. Every shape a case holds anything to, every
   table it reads and every name it uses of the spec's comes from here, by
   way of [Xynolog_protocol]: nothing is restated.

   A harness program runs from the repository's root, where specs/src is. *)

open Xynolog_protocol

let root = "specs/src"

let spec =
  lazy
    (if not (Sys.file_exists root && Sys.is_directory root) then
       failwith
         "the harness runs from the repository's root, and there is no \
          specs/src here";
     match Spec.load root with
     | Ok spec -> spec
     | Error refusals ->
         failwith
           (String.concat "\n"
              ("the reader refuses specs/src:"
              :: List.map Decode.error_to_string refusals)))

let spec () = Lazy.force spec

(* The value at a path of members within [json], when every step is an
   object holding the next. *)
let rec at json = function
  | [] -> Some json
  | name :: rest -> (
      match json with
      | `Assoc members ->
          Option.bind (List.assoc_opt name members) (fun value -> at value rest)
      | _ -> None)

let tables () = (spec ()).tables

(* ── Schemas ─────────────────────────────────────────────────────────── *)

let files = Hashtbl.create 64

let json_of file =
  match Hashtbl.find_opt files file with
  | Some json -> json
  | None ->
      let json =
        match Decode.parse_file ~root file with
        | Ok json -> Some json
        | Error _ -> None
      in
      Hashtbl.replace files file json;
      json

let schemas = Hashtbl.create 256

(* The schema at a place of specs/src, as [Spec.load] reads one. *)
let schema_at ({ file; pointer } : Validate.place) =
  match Hashtbl.find_opt schemas (file, pointer) with
  | Some found -> found
  | None ->
      let found =
        Option.bind (json_of file) (fun json ->
            Option.bind (Decode.at_pointer pointer json) (fun node ->
                Result.to_option
                  (Result.map fst (Decode.run ~file node Schema.decode))))
      in
      Hashtbl.replace schemas (file, pointer) found;
      found

let place_of (reference : Decode.reference) =
  let file, pointer = Decode.resolve reference in
  { Validate.file; pointer }

let schema_of reference =
  match schema_at (place_of reference) with
  | Some schema -> schema
  | None -> failwith ("specs/src names no schema at " ^ reference.target)

(* How [value] is not what [schema] says: each refusal as the pointer into
   the value and why. Empty when it is. *)
let refusals schema value =
  List.map
    (fun (pointer, why) -> (if pointer = "" then "/" else pointer) ^ " " ^ why)
    (Validate.errors schema_at schema value "")

(* A component schema of the gateway's document, by its name. *)
let component name =
  match List.assoc_opt name (spec ()).gateway.components.schemas with
  | Some reference -> schema_of reference
  | None -> failwith ("the gateway's document has no component schema " ^ name)

let against name value = refusals (component name) value

(* A service's own schema (Places.service_schemas): its configuration
   document, its health, by its name. *)
let service_schema name =
  match
    List.find_opt
      (fun file -> Filename.basename file = name ^ ".json")
      (spec ()).services
  with
  | Some file -> (
      match schema_at { file; pointer = "" } with
      | Some schema -> schema
      | None -> failwith (file ^ " is no schema"))
  | None -> failwith ("specs/src has no service schema " ^ name)

(* ── Operations ──────────────────────────────────────────────────────── *)

type document = Gateway | Archivist

let document = function
  | Gateway -> (spec ()).gateway
  | Archivist -> (spec ()).archivist

let find_operation which meth path : Openapi.operation option =
  Option.bind (List.assoc_opt path (document which).paths) (fun reference ->
      let item : Openapi.path_item =
        List.assoc (fst (Decode.resolve reference)) (spec ()).path_items
      in
      match meth with
      | "GET" -> item.get
      | "PUT" -> item.put
      | "POST" -> item.post
      | _ -> None)

let operation which meth path =
  match find_operation which meth path with
  | Some operation -> operation
  | None -> failwith (Printf.sprintf "the document declares no %s %s" meth path)

(* [path], which [which] must declare an operation of [meth] on. *)
let path which meth path =
  ignore (operation which meth path : Openapi.operation);
  path

(* The declared response of an operation for a status, or None. *)
let response which meth path status : Openapi.response option =
  match
    List.assoc_opt (string_of_int status) (operation which meth path).responses
  with
  | None -> None
  | Some (Openapi.Inline response) -> Some response
  | Some (Openapi.Ref reference) ->
      List.assoc_opt (fst (Decode.resolve reference)) (spec ()).responses

(* One of an operation's x-xynolog-limits that is a count. *)
let limit which meth path key =
  match (operation which meth path).limits with
  | Some limits -> (
      match List.assoc_opt key limits with
      | Some (Openapi.Count n) -> n
      | _ -> failwith (Printf.sprintf "%s %s states no count %s" meth path key))
  | None -> failwith (Printf.sprintf "%s %s states no limits" meth path)

(* The template of a document's path that [concrete] fills, with an
   operation of [meth]: the template and its parameters' values. *)
let route which meth concrete =
  let parts path = String.split_on_char '/' path in
  List.find_map
    (fun (template, _) ->
      let rec matched acc = function
        | [], [] -> Some (List.rev acc)
        | a :: rest, b :: others when String.length a > 2 && a.[0] = '{' ->
            matched ((a, Uri.pct_decode b) :: acc) (rest, others)
        | a :: rest, b :: others when a = b -> matched acc (rest, others)
        | _ -> None
      in
      match matched [] (parts template, parts concrete) with
      | Some values when find_operation which meth template <> None ->
          Some (template, values)
      | _ -> None)
    (document which).paths

(* ── The bus registry ─────────────────────────────────────────────────── *)

module Registry = Tables.Bus_registry

let operation_for request =
  match
    List.find_opt
      (fun (op : Registry.operations_item) -> op.request = request)
      (tables ()).bus_registry.operations
  with
  | Some op -> op
  | None -> failwith ("the registry declares no operation " ^ request)

type carried = Schema of string | Envelope of string | Stored_event | Nothing

let carried channel =
  List.find_map
    (fun (row : Registry.channels_item) ->
      match row with
      | Schema { channel = c; schema; _ } when c = channel ->
          Some (Schema schema)
      | Envelope { channel = c; schema; _ } when c = channel ->
          Some (Envelope schema)
      | Stored_event { channel = c; _ } when c = channel -> Some Stored_event
      | (Void { channel = c; _ } | Empty { channel = c; _ }) when c = channel ->
          Some Nothing
      | _ -> None)
    (tables ()).bus_registry.channels

(* The persisted channels: the facts. *)
let facts () =
  List.filter_map
    (fun (row : Registry.channels_item) ->
      match row with Stored_event { channel; _ } -> Some channel | _ -> None)
    (tables ()).bus_registry.channels

(* A payload without the gateway's stamps (EVENT-BUS.md), which are the
   bus's and not the payload's. *)
let unstamped = function
  | `Assoc members ->
      `Assoc
        (List.filter
           (fun (key, _) -> not (String.length key > 0 && key.[0] = '_'))
           members)
  | other -> other

(* What the registry's schema for [channel] says against [payload]: None
   when the payload is what the channel carries. An envelope carries its
   schema under [response]. *)
let against_channel channel payload =
  let said schema value =
    match against schema value with
    | [] -> None
    | refusals ->
        Some
          (Printf.sprintf "not a %s: %s" schema (String.concat "; " refusals))
  in
  match carried channel with
  | None -> Some "the registry does not declare the channel"
  | Some (Schema schema) -> said schema payload
  | Some (Envelope schema) ->
      said schema (Option.value ~default:`Null (at payload [ "response" ]))
  | Some (Stored_event | Nothing) ->
      Some "the registry names no schema for the channel"

(* ── Telemetry ────────────────────────────────────────────────────────── *)

module Service_rows = Tables.Service_telemetry_telemetry
module Sdk_rows = Tables.Sdk_telemetry_telemetry

type service = Service_rows.services_item
type value = String of string | Boolean of bool
type attribute = { key : string; only : bool; values : value list option }

type span_row = {
  name : string;
  kind : int;  (** OTLP's number for the kind the row names. *)
  span_expected : bool;
  span_attributes : attribute list;
}

type metric_row = {
  metric : string;
  instrument : string;  (** As the receiver reports what a metric came as. *)
  metric_expected : bool;
  metric_attributes : attribute list;
}

let otlp_kind : Service_rows.kind -> int = function
  | Internal -> 1
  | Server -> 2
  | Client -> 3
  | Producer -> 4
  | Consumer -> 5

let sdk_kind : Sdk_rows.service_telemetry_telemetry_kind -> int = function
  | Internal -> 1
  | Server -> 2
  | Client -> 3
  | Producer -> 4
  | Consumer -> 5

let instrument_word : Service_rows.instrument -> string = function
  | Counter -> "counter"
  | Up_down_counter -> "up-down counter"
  | Gauge -> "gauge"
  | Histogram -> "histogram"

let sdk_instrument_word :
    Sdk_rows.service_telemetry_telemetry_instrument -> string = function
  | Counter -> "counter"
  | Up_down_counter -> "up-down counter"
  | Gauge -> "gauge"
  | Histogram -> "histogram"

let attributes (rows : Service_rows.attributes_item list) =
  List.map
    (fun (a : Service_rows.attributes_item) ->
      {
        key = a.key;
        only = a.only <> None;
        values =
          Option.map
            (List.map (function
              | Service_rows.String s -> String s
              | Boolean b -> Boolean b))
            a.values;
      })
    rows

let sdk_attributes
    (rows : Sdk_rows.service_telemetry_telemetry_attributes_item list) =
  List.map
    (fun (a : Sdk_rows.service_telemetry_telemetry_attributes_item) ->
      {
        key = a.key;
        only = a.only <> None;
        values =
          Option.map
            (List.map (function
              | Sdk_rows.String s -> String s
              | Boolean b -> Boolean b))
            a.values;
      })
    rows

let transport_word : Sdk_rows.transport -> string = function
  | Bus -> "bus"
  | Content -> "content"

(* What a row names of the SDK table, which a service names it by: a
   transport for all its rows, or one row. *)
let names_sdk_row used (transport, name) =
  List.mem (transport_word transport) used || List.mem name used

(* The rows a service exports where it runs on no plane: its rows of the
   service table, and the SDK table's rows it names; each marked expected
   when its cases must make it export it. *)
let telemetry (service : service) =
  let service_table = (tables ()).service_telemetry_telemetry in
  let sdk_table = (tables ()).sdk_telemetry_telemetry in
  let used =
    Option.value ~default:[] (List.assoc_opt service service_table.sdk)
  in
  List.iter
    (fun entry ->
      if
        not
          (List.exists
             (fun (r : Sdk_rows.spans_item) ->
               names_sdk_row [ entry ] (r.transport, r.name))
             sdk_table.spans
          || List.exists
               (fun (r : Sdk_rows.metrics_item) ->
                 names_sdk_row [ entry ] (r.transport, r.name))
               sdk_table.metrics)
      then
        failwith
          ("service-telemetry/telemetry.json names " ^ entry
         ^ " of the SDK table, which is neither a transport nor a row of it"))
    used;
  let expected (when_ : Service_rows.when_) planes =
    (when_ = Export || when_ = Traffic) && planes = None
  in
  let spans =
    List.filter_map
      (fun (r : Service_rows.spans_item) ->
        if List.mem service r.services then
          Some
            {
              name = r.name;
              kind = otlp_kind r.kind;
              span_expected = expected r.when_ None;
              span_attributes = attributes r.attributes;
            }
        else None)
      service_table.spans
    @ List.filter_map
        (fun (r : Sdk_rows.spans_item) ->
          if names_sdk_row used (r.transport, r.name) then
            Some
              {
                name = r.name;
                kind = sdk_kind r.kind;
                span_expected = true;
                span_attributes = sdk_attributes r.attributes;
              }
          else None)
        sdk_table.spans
  in
  let metrics =
    List.filter_map
      (fun (r : Service_rows.metrics_item) ->
        if List.mem service r.services then
          Some
            {
              metric = r.name;
              instrument = instrument_word r.instrument;
              metric_expected = expected r.when_ r.planes;
              metric_attributes = attributes r.attributes;
            }
        else None)
      service_table.metrics
    @ List.filter_map
        (fun (r : Sdk_rows.metrics_item) ->
          if names_sdk_row used (r.transport, r.name) then
            Some
              {
                metric = r.name;
                instrument = sdk_instrument_word r.instrument;
                metric_expected = true;
                metric_attributes = sdk_attributes r.attributes;
              }
          else None)
        sdk_table.metrics
  in
  (spans, metrics)

(* The keys of the resource a service exports under: exactly these. *)
let resource_keys service =
  List.filter_map
    (fun (r : Service_rows.resource_item) ->
      if List.mem service r.services then Some r.key else None)
    (tables ()).service_telemetry_telemetry.resource

(* What an exported span's name looks like for a row: each {placeholder}
   stands for any text. *)
let span_pattern name =
  let marked =
    Re.replace_string (Re.Pcre.regexp "\\{[A-Za-z]+\\}") ~by:"\000" name
  in
  let pieces = String.split_on_char '\000' marked in
  Re.compile
    (Re.seq
       (Re.bos
        :: List.concat
             (List.mapi
                (fun at piece ->
                  if at = 0 then [ Re.str piece ]
                  else [ Re.rep1 Re.any; Re.str piece ])
                pieces)
       @ [ Re.eos ]))

let all_span_names () =
  List.map
    (fun (r : Service_rows.spans_item) -> r.name)
    (tables ()).service_telemetry_telemetry.spans
  @ List.map
      (fun (r : Sdk_rows.spans_item) -> r.name)
      (tables ()).sdk_telemetry_telemetry.spans

(* A span's exported name, from the row the spec lists, its placeholders
   filled. *)
let span_name template fill =
  if not (List.mem template (all_span_names ())) then
    failwith ("the spec lists no span " ^ template);
  Re.replace
    (Re.compile (Re.Pcre.re "\\{([A-Za-z]+)\\}"))
    ~f:(fun group ->
      let key = Re.Group.get group 1 in
      match List.assoc_opt key fill with
      | Some value -> value
      | None -> failwith (Printf.sprintf "span %s needs {%s}" template key))
    template

let metric_name name =
  if
    not
      (List.exists
         (fun (r : Service_rows.metrics_item) -> r.name = name)
         (tables ()).service_telemetry_telemetry.metrics
      || List.exists
           (fun (r : Sdk_rows.metrics_item) -> r.name = name)
           (tables ()).sdk_telemetry_telemetry.metrics)
  then failwith ("the spec lists no metric " ^ name);
  name

(* The variables a service reads (service-environment/variables.json). *)
let service_environment
    (service :
      Tables.Service_environment_variables
      .service_telemetry_telemetry_services_item) =
  match
    List.filter_map
      (fun (row : Tables.Service_environment_variables.variables_item) ->
        if List.mem service row.services then Some row.name else None)
      (tables ()).service_environment_variables.variables
  with
  | [] ->
      failwith
        "service-environment/variables.json lists no variables for the service"
  | names -> names

(* ── Knowledge bases and principals ───────────────────────────────────── *)

type kb = { name : string; domain : string; did : string; resource : string }

(* The case of kb-identity/cases.json the suite's knowledge base is. *)
let suite_kb = "the shape xynolog init writes"

let kb_identity why =
  match
    List.find_opt
      (fun (c : Tables.Kb_identity_cases.cases_item) -> c.why = why)
      (tables ()).kb_identity_cases.cases
  with
  | Some
      {
        name = Some name;
        domain = Some domain;
        did = Some did;
        resource = Some resource;
        _;
      } ->
      { name; domain; did; resource }
  | Some _ ->
      failwith ("kb-identity/cases.json's \"" ^ why ^ "\" declares no identity")
  | None -> failwith ("kb-identity/cases.json has no \"" ^ why ^ "\" case")

(* encodeURIComponent's encoding, which principals/cases.json states its
   forms in, and its cases hold. *)
let uri_component text =
  let kept = function
    | 'A' .. 'Z'
    | 'a' .. 'z'
    | '0' .. '9'
    | '-' | '_' | '.' | '!' | '~' | '*' | '\'' | '(' | ')' ->
        true
    | _ -> false
  in
  String.concat ""
    (List.map
       (fun c ->
         if kept c then String.make 1 c
         else Printf.sprintf "%%%02X" (Char.code c))
       (List.of_seq (String.to_seq text)))

let filled form domain subject =
  let replace pattern by text =
    Re.replace_string (Re.compile (Re.str pattern)) ~by ~all:false text
  in
  replace "{subject}" (uri_component subject) (replace "{domain}" domain form)

let principals_checked =
  lazy
    (let table = (tables ()).principals_cases in
     List.iter
       (fun (kind, form, cases) ->
         List.iter
           (fun (c : Tables.Principals_cases.people_item) ->
             let made = filled form c.domain c.subject in
             if made <> c.did then
               failwith
                 (Printf.sprintf
                    "principals/cases.json: \"%s\": the %s form gives %s, the \
                     case says %s"
                    c.why kind made c.did))
           cases)
       [
         ("person", table.person, table.people);
         ("agent", table.agent, table.agents);
       ];
     table)

let person_did domain subject =
  filled (Lazy.force principals_checked).person domain subject

let agent_did domain subject =
  filled (Lazy.force principals_checked).agent domain subject

(* ── Roles ────────────────────────────────────────────────────────────── *)

(* A name the bearerAuth scheme's description declares, in backticks: the
   roles and the claim that marks an agent's account. *)
let declared word =
  match
    List.assoc_opt "bearerAuth" (spec ()).gateway.components.security_schemes
  with
  | Some (Openapi.Http { description = Some description; _ })
    when Re.execp (Re.compile (Re.str ("`" ^ word ^ "`"))) description ->
      word
  | _ -> failwith ("the bearerAuth scheme no longer declares " ^ word)

let service_role () = declared "xynolog-service"
let kind_claim () = declared "kind"
let agent_kind () = declared "agent"
