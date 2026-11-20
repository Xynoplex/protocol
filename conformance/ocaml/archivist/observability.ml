(* The telemetry the Archivist exports: the spans and metrics
   specs/src/service-telemetry/telemetry.json lists for it, exported over
   OTLP to the endpoint the standard OpenTelemetry environment names. Its
   last case holds everything the receiver got to the table, in both
   directions; so its cases run solo, in order, on one world. *)

open Xynolog_conformance
open Support
open Case

(* The span kinds the cases name, as OTLP numbers them. *)
let producer = Protocol.otlp_kind Producer
let consumer = Protocol.otlp_kind Consumer

let in_order =
  "the file's last case is judged on everything its cases made the Archivist \
   export"

type shared = { aw : A.t; otlp : Otlp.t }

let make (implementations : Implementations.t) () =
  let otlp = Otlp.start () in
  let aw =
    A.create
      ~staging:[ ("flushMs", 100); ("maxWaitMs", 500) ]
      ~env:
        [
          (* Read when the world starts, after the receiver is listening. *)
          ("OTEL_EXPORTER_OTLP_ENDPOINT", otlp.endpoint);
          ("OTEL_METRIC_EXPORT_INTERVAL", "500");
          ("OTEL_BSP_SCHEDULE_DELAY", "100");
        ]
      ~gateway:implementations.gateway ~command:implementations.archivist ()
  in
  { aw; otlp }

let close { aw; otlp } =
  A.close aw;
  Otlp.close otlp

(* The values the table lists for the attribute [key] of the Archivist's
   metric [name]. *)
let listed_values name key =
  let _, metrics = Protocol.telemetry Archivist in
  match
    List.find_opt (fun (r : Protocol.metric_row) -> r.metric = name) metrics
  with
  | None -> failwith ("the table lists no " ^ name ^ " for the Archivist")
  | Some row -> (
      match
        List.find_opt
          (fun (a : Protocol.attribute) -> a.key = key)
          row.metric_attributes
      with
      | Some { values = Some values; _ } ->
          List.map
            (function
              | Protocol.String s -> s
              | Protocol.Boolean _ ->
                  failwith (Printf.sprintf "%s's %s lists a flag" name key))
            values
      | _ -> failwith (Printf.sprintf "%s lists no values of %s" name key))

(* Whether the metric [name] has carried [value] of its attribute [key]. *)
let carried (otlp : Otlp.t) name key value =
  match Otlp.metric otlp name with
  | None -> false
  | Some m -> (
      match Dynarray.find_opt (fun (k, _) -> k = key) m.attributes with
      | None -> false
      | Some (_, seen) -> Dynarray.exists (fun v -> v = Otlp.Text value) seen)

let bus_spans { aw = w; otlp } =
  let owner = A.person w "owner" in
  let uri = "file://observed/a.md" in
  let id =
    A.created w owner.did
      [ ("name", "Observed"); ("storageUri", uri) ]
      "observed\n"
  in
  ignore
    (A.ask owner "mark:archive"
       (`Assoc [ ("resourceId", str id); ("storageUri", str uri) ])
      : Yojson.Safe.t);
  ignore (A.ask owner "browse:kb-requested" (`Assoc []) : Yojson.Safe.t);
  let service = Protocol.default_service_name Archivist in
  List.iter
    (fun (template, channel, kind) ->
      let name = Protocol.span_name template [ ("channel", channel) ] in
      let s =
        Run.eventually (Printf.sprintf "a %s span" name) 10.0 (fun () ->
            Dynarray.find_opt (fun (s : Otlp.span) -> s.name = name) otlp.spans)
      in
      Check.that
        (Printf.sprintf "%s is of kind %d, not %d" name s.kind kind)
        (s.kind = kind);
      Check.that
        (Printf.sprintf "%s carries bus.channel %s" name channel)
        (List.assoc_opt "bus.channel" s.attributes = Some (Otlp.Text channel));
      Check.that
        (Printf.sprintf "%s is exported under %s" name service)
        (List.assoc_opt "service.name" s.resource = Some (Otlp.Text service)))
    [
      ("bus.recv:{channel}", "mark:archive", consumer);
      ("bus.recv:{channel}", "browse:kb-requested", consumer);
      ( "bus.emit:{channel}",
        (Protocol.operation_for "mark:archive").result,
        producer );
      ("bus.emit:{channel}", "yield:created", producer);
      ("bus.emit:{channel}", "mark:archived", producer);
    ]

let fact_pump { otlp; _ } =
  let depth = Protocol.metric_name "xynolog.archivist.fact_pump.depth" in
  Run.eventually "a depth of zero" 10.0 (fun () ->
      match Otlp.metric otlp depth with
      | Some m
        when Dynarray.length m.values > 0 && Dynarray.get_last m.values = 0.0 ->
          Some ()
      | _ -> None)

let git_duration { otlp; _ } =
  let duration = Protocol.metric_name "xynolog.git.duration" in
  let commands = listed_values duration "git.command" in
  Run.eventually
    ("a measured " ^ String.concat ", " commands)
    15.0
    (fun () ->
      if List.for_all (carried otlp duration "git.command") commands then
        Some ()
      else None)

let staging_failures { aw = w; otlp } =
  let owner = A.person w "owner" in
  let failures = Protocol.metric_name "xynolog.git.staging.failures" in
  let reason = "index-lock" in
  Check.that
    (Printf.sprintf "the table lists %s as a reason of %s" reason failures)
    (List.mem reason (listed_values failures "reason"));
  let release = A.hold_git_index w in
  Fun.protect ~finally:release (fun () ->
      ignore
        (A.created w owner.did
           [ ("name", "Unstaged"); ("storageUri", "file://observed/b.md") ]
           "unstaged\n"
          : string);
      Run.eventually "a counted degradation" 20.0 (fun () ->
          if carried otlp failures "reason" reason then Some () else None))

(* Last, and judged on everything the cases above made the Archivist
   export: run it with them, never alone. *)
let exactly_the_table { aw; otlp } =
  Check.equal "what the Archivist exported outside the table" (`List [])
    (`List
       (List.map
          (fun f -> `String f)
          (Telemetry.outside_the_table otlp Archivist (Some aw.world.kb.did))))

let tests implementations =
  let shared =
    Suite.world "archivist/what the Archivist exports"
      ~make:(make implementations) ~close
  in
  List.map
    (fun c ->
      Case.test ~category:[ "what the Archivist exports" ] c (fun () ->
          let s = shared () in
          ended s.aw (fun () -> c.run s)))
    [
      case ~solo:in_order
        "each frame it receives is a bus.recv consumer span, and each it sends \
         a bus.emit producer span, under the Archivist's name"
        bus_spans;
      case ~solo:in_order
        "xynolog.archivist.fact_pump.depth returns to zero once what was \
         appended is published"
        fact_pump;
      case ~solo:in_order
        "xynolog.git.duration measures each git command, by command"
        git_duration;
      case ~solo:in_order
        "xynolog.git.staging.failures counts a batch that could not be staged, \
         by why"
        staging_failures;
      case ~solo:in_order
        "exports exactly the telemetry the spec lists: every row it can, and \
         nothing else"
        exactly_the_table;
    ]
