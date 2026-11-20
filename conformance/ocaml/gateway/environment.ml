(* What each variable in specs/src/service-environment/variables.json
   changes, seen from outside, and what the document's logFormat changes.
   Each case runs a gateway of its own with the environment it is about.
   JWT_SECRET, XYNOLOG_OIDC_CLIENT_ID and XYNOLOG_OIDC_CLIENT_SECRET are
   boot.ml's, as is `--config`, and observability.ml exports to
   OTEL_EXPORTER_OTLP_ENDPOINT throughout. *)

open Xynolog_conformance
open Support
open Case

let dispatch () =
  Protocol.span_name "bus.dispatch:{channel}" [ ("channel", "beckon:focus") ]

let contains text needle = Re.execp (Re.compile (Re.str needle)) text

(* A gateway with [env] (and, when given, a changed document), exporting to
   its own receiver. *)
let with_gateway implementations ?document env run =
  let otlp = Otlp.start () in
  Fun.protect ~finally:(fun () -> Otlp.close otlp) @@ fun () ->
  let w =
    World.create ~command:implementations.Implementations.gateway ?document
      ~env:(env otlp) In_process
  in
  Fun.protect ~finally:(fun () -> World.close w) @@ fun () ->
  World.end_case
    ~account:(fun () ->
      "gateway output:\n"
      ^ String.concat "\n" (Process.output w.gateway.process))
    ~settle:(fun () -> World.drain w)
    (fun () -> run w otlp)

let emit (w : World.t) =
  status "the emit" 202
    (World.emit w
       (World.person w "environment")
       (`Assoc
          [
            ("channel", `String "beckon:focus");
            ("payload", `Assoc [ ("resourceId", `String "res-1") ]);
          ]))

let in_output (w : World.t) text =
  List.exists
    (fun line -> contains line text)
    (Process.output w.gateway.process)

let dispatched (otlp : Otlp.t) =
  let name = dispatch () in
  Dynarray.find_opt (fun (s : Otlp.span) -> s.name = name) otlp.spans

let names_received (otlp : Otlp.t) =
  `Assoc
    [
      ( "spans",
        `List
          (List.map
             (fun (s : Otlp.span) -> `String s.name)
             (Dynarray.to_list otlp.spans)) );
      ( "metrics",
        `List
          (List.map (fun (n, _) -> `String n) (Dynarray.to_list otlp.metrics))
      );
    ]

let nothing_received = `Assoc [ ("spans", `List []); ("metrics", `List []) ]

let service_name implementations () =
  List.iter
    (fun (name, expected) ->
      with_gateway implementations
        (fun otlp ->
          [
            ("OTEL_EXPORTER_OTLP_ENDPOINT", Some otlp.endpoint);
            ("OTEL_BSP_SCHEDULE_DELAY", Some "100");
            ("OTEL_SERVICE_NAME", name);
          ])
        (fun w otlp ->
          emit w;
          let span =
            Run.eventually (dispatch ()) 10.0 (fun () -> dispatched otlp)
          in
          Check.that
            (Printf.sprintf "the span's service is %s" expected)
            (List.assoc_opt "service.name" span.resource
            = Some (Otlp.Text expected))))
    [
      (None, Protocol.default_service_name Gateway);
      (Some "conformance-renamed", "conformance-renamed");
    ]

let sdk_disabled implementations () =
  with_gateway implementations
    (fun otlp ->
      [
        ("OTEL_EXPORTER_OTLP_ENDPOINT", Some otlp.endpoint);
        ("OTEL_BSP_SCHEDULE_DELAY", Some "100");
        ("OTEL_METRIC_EXPORT_INTERVAL", Some "500");
        ("OTEL_SDK_DISABLED", Some "true");
      ])
    (fun w otlp ->
      emit w;
      Run.sleep 2.0;
      Check.equal "what the receiver got" nothing_received (names_received otlp))

let console_spans implementations () =
  with_gateway implementations
    (fun _ ->
      [
        ("OTEL_CONSOLE_EXPORTER", Some "true");
        ("OTEL_BSP_SCHEDULE_DELAY", Some "100");
      ])
    (fun w _ ->
      emit w;
      Run.eventually
        (dispatch () ^ " in the output")
        10.0
        (fun () -> if in_output w (dispatch ()) then Some () else None))

let console_metrics implementations () =
  with_gateway implementations
    (fun otlp ->
      [
        ("OTEL_EXPORTER_OTLP_ENDPOINT", Some otlp.endpoint);
        ("OTEL_BSP_SCHEDULE_DELAY", Some "100");
        ("OTEL_METRIC_EXPORT_INTERVAL", Some "500");
        ("OTEL_METRICS_EXPORTER", Some "console");
      ])
    (fun w otlp ->
      emit w;
      ignore
        (Run.eventually (dispatch ()) 10.0 (fun () -> dispatched otlp)
          : Otlp.span);
      let emits = Protocol.metric_name "xynolog.bus.emit" in
      Run.eventually (emits ^ " in the output") 10.0 (fun () ->
          if in_output w emits then Some () else None);
      Check.equal "the metrics the receiver got" (`List [])
        (`List
           (List.map (fun (n, _) -> `String n) (Dynarray.to_list otlp.metrics))))

let metric_interval implementations () =
  with_gateway implementations
    (fun otlp ->
      [
        ("OTEL_EXPORTER_OTLP_ENDPOINT", Some otlp.endpoint);
        ("OTEL_METRIC_EXPORT_INTERVAL", Some "500");
      ])
    (fun _ otlp ->
      Run.eventually "a metric export within 3 s" 3.0 (fun () ->
          if Dynarray.length otlp.metrics > 0 then Some () else None))

let schedule_delay implementations () =
  with_gateway implementations
    (fun otlp ->
      [
        ("OTEL_EXPORTER_OTLP_ENDPOINT", Some otlp.endpoint);
        ("OTEL_BSP_SCHEDULE_DELAY", Some "100");
      ])
    (fun w otlp ->
      emit w;
      ignore
        (Run.eventually
           (dispatch () ^ " within 2 s")
           2.0
           (fun () -> dispatched otlp)
          : Otlp.span))

let bus_log implementations () =
  List.iter
    (fun on ->
      with_gateway implementations
        (fun _ -> [ ("XYNOLOG_BUS_LOG", if on then Some "1" else None) ])
        (fun w _ ->
          emit w;
          if on then
            let line = Telemetry.bus_line "EMIT" "beckon:focus" in
            Run.eventually (Printf.sprintf "a %s line" line) 5.0 (fun () ->
                if in_output w line then Some () else None)
          else (
            Run.sleep 0.5;
            Check.equal "the bus log's lines" (`List [])
              (`List
                 (List.filter_map
                    (fun line ->
                      if contains line (Telemetry.bus_line_start ()) then
                        Some (`String line)
                      else None)
                    (Process.output w.gateway.process))))))
    [ false; true ]

let supervised implementations () =
  Telemetry.with_supervisor_events ~name:"gateway" ~lives:3 @@ fun events ->
  with_gateway implementations
    (fun otlp ->
      [
        ("OTEL_EXPORTER_OTLP_ENDPOINT", Some otlp.endpoint);
        ("OTEL_METRIC_EXPORT_INTERVAL", Some "500");
        ("SUPERVISE_EVENTS", Some events);
        ("SUPERVISE_NAME", Some "gateway");
      ])
    (fun _ otlp ->
      let restarts = Protocol.metric_name "xynolog.process.restarts" in
      let values =
        Run.eventually restarts 10.0 (fun () ->
            Option.bind (Otlp.metric otlp restarts) (fun m ->
                if Dynarray.length m.values > 0 then Some m.values else None))
      in
      Check.equal "the restarts last reported" (`Float 2.0)
        (`Float (Dynarray.get_last values)))

(* Every line the gateway wrote to stdout, once it has written any. *)
let stdout_lines (w : World.t) =
  Run.eventually "log lines on stdout" 5.0 (fun () ->
      if Dynarray.length w.gateway.process.stdout > 0 then
        Some (Dynarray.to_list w.gateway.process.stdout)
      else None)

let parsed line =
  match Yojson.Safe.from_string line with
  | `Assoc _ as json -> json
  | _ -> failwith ("a line that is not one JSON object: " ^ line)
  | exception Yojson.Json_error why ->
      failwith (Printf.sprintf "a line that is not JSON (%s): %s" why line)

let text_member json name =
  match Yojson.Safe.Util.member name json with
  | `String text -> Some text
  | _ -> None

let json_format implementations () =
  let json = (Protocol.tables ()).service_telemetry_log_lines.logger.json in
  let trace_key, span_key =
    match json.trace with
    | [ trace_key; span_key ] -> (trace_key, span_key)
    | _ -> failwith "the json line names other than a trace's and a span's id"
  in
  with_gateway implementations
    ~document:(fun d ->
      World.with_member "logLevel" (`String "debug")
        (World.with_member "logFormat" (`String "json") d))
    (fun otlp -> [ ("OTEL_EXPORTER_OTLP_ENDPOINT", Some otlp.endpoint) ])
    (fun w _ ->
      let trace_id = Fresh.hex 16 in
      let traceparent = Printf.sprintf "00-%s-%s-01" trace_id (Fresh.hex 8) in
      status "the traced emit" 202
        (Call.call ~token:(World.person w "traced")
           ~json:
             (`Assoc
                [
                  ("channel", `String "beckon:focus");
                  ("payload", `Assoc [ ("resourceId", `String "res-1") ]);
                ])
           ~headers:
             [
               ("traceparent", traceparent); ("idempotency-key", Fresh.uuid ());
             ]
           (World.origin w) "POST"
           (Protocol.path Gateway "POST" "/bus/emit"));
      let traced =
        Run.eventually ("a line carrying trace " ^ trace_id) 5.0 (fun () ->
            Dynarray.find_opt
              (fun line -> contains line trace_id)
              w.gateway.process.stdout)
      in
      let line = parsed traced in
      Check.that
        (Printf.sprintf "the traced line's %s is the trace's: %s" trace_key
           traced)
        (text_member line trace_key = Some trace_id);
      Check.that
        (Printf.sprintf "the traced line's %s is 16 hex digits: %s" span_key
           traced)
        (Option.fold ~none:false
           ~some:(Check.matches_re "^[0-9a-f]{16}$")
           (text_member line span_key));
      List.iter
        (fun text ->
          let line = parsed text in
          List.iter
            (fun key ->
              Check.that
                (Printf.sprintf "the line's %s is text: %s" key text)
                (text_member line key <> None))
            json.first)
        (stdout_lines w))

(* The simple format's template up to its message, as a pattern a line must
   start with: [{timestamp}] the moment as YYYY-MM-DD hh:mm:ss, [{LEVEL}] an
   upper-case word. *)
let simple_start () =
  Re.compile
    (Re.seq
       (Re.bos
       :: List.map
            (function
              | `Text text -> Re.str text
              | `Delim found -> (
                  match Re.Group.get found 1 with
                  | "timestamp" ->
                      Re.Perl.re "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}"
                  | "LEVEL" -> Re.rep1 (Re.rg 'A' 'Z')
                  | other ->
                      failwith
                        ("the simple format's {" ^ other ^ "} is unknown here")))
            (Re.split_full
               (Re.compile (Re.Perl.re "\\{([A-Za-z]+)\\}"))
               (Telemetry.before "{message}"
                  (Protocol.tables ()).service_telemetry_log_lines.logger.simple))
       ))

let simple_format implementations () =
  with_gateway implementations
    ~document:(fun d ->
      World.with_member "logLevel" (`String "info")
        (World.with_member "logFormat" (`String "simple") d))
    (fun _ -> [])
    (fun w _ ->
      let start = simple_start () in
      List.iter
        (fun line ->
          Check.that
            (Printf.sprintf "a line in the simple format: %s" line)
            (Re.execp start line))
        (stdout_lines w))

let tests implementations =
  let case category name body =
    Testo.create ~category:[ category ] ~max_duration:120.0 name
      (body implementations)
  in
  let environment = case "the gateway's environment"
  and log_format = case "the document's logFormat" in
  [
    environment
      "OTEL_SERVICE_NAME names the service its spans are exported under; \
       xynolog-gateway when unset"
      service_name;
    environment
      "OTEL_SDK_DISABLED=true exports nothing, though an endpoint is set"
      sdk_disabled;
    environment
      "OTEL_CONSOLE_EXPORTER=true, with no endpoint, writes spans to the \
       gateway's output"
      console_spans;
    environment
      "OTEL_METRICS_EXPORTER=console writes metrics to the output, and spans \
       still reach the endpoint"
      console_metrics;
    environment
      "OTEL_METRIC_EXPORT_INTERVAL sets how often metrics are exported (30 s \
       when unset)"
      metric_interval;
    environment
      "OTEL_BSP_SCHEDULE_DELAY sets how long a finished span waits to be \
       exported (5 s when unset)"
      schedule_delay;
    environment
      "XYNOLOG_BUS_LOG writes a line per frame the gateway accepts; unset, it \
       writes none"
      bus_log;
    environment
      "SUPERVISE_EVENTS and SUPERVISE_NAME report xynolog.process.restarts: \
       the lives the supervisor recorded, less one"
      supervised;
    log_format
      "json writes each line as one JSON object, and a line logged during a \
       traced request carries its trace_id and span_id"
      json_format;
    log_format "simple writes <timestamp> [<LEVEL>] <message>" simple_format;
  ]
