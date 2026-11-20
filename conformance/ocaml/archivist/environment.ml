(* What each variable specs/src/service-environment/variables.json lists for
   the Archivist changes, seen from outside. Each case runs an Archivist
   world of its own with the environment it is about, and makes the
   Archivist answer one request: a browse:kb-requested. Its service account,
   XYNOLOG_OIDC_CLIENT_ID and XYNOLOG_OIDC_CLIENT_SECRET, is boot.ml's.

   A span is told by what it carries rather than by name: its kind, and the
   channel it is about. *)

open Xynolog_conformance
open Support
open Case

let request = "browse:kb-requested"
let result () = (Protocol.operation_for request).result

(* An Archivist world with [env] (a variable with a value is set, one with
   none left unset), exporting to its own receiver. *)
let with_env implementations env run =
  let otlp = Otlp.start () in
  Fun.protect ~finally:(fun () -> Otlp.close otlp) @@ fun () ->
  own_world implementations ~git_sync:false
    ~env:
      (List.filter_map
         (fun (name, value) -> Option.map (fun v -> (name, v)) value)
         (env otlp))
    (fun w -> run w otlp)

(* Make the Archivist answer one request. *)
let ask w =
  let answer = A.request (A.sidecar w "environment") request (`Assoc []) in
  Check.equal "the reply's channel" (str (result ())) (str answer.channel)

let about channel kind (s : Otlp.span) =
  s.kind = kind
  && List.assoc_opt "bus.channel" s.attributes = Some (Otlp.Text channel)

let span what (otlp : Otlp.t) timeout matches =
  Run.eventually what timeout (fun () -> Dynarray.find_opt matches otlp.spans)

let received ?(timeout = 10.0) otlp =
  span
    (Printf.sprintf "the %s it received" request)
    otlp timeout (about request consumer)

let sent ?(timeout = 10.0) otlp =
  span
    (Printf.sprintf "the %s it sent" (result ()))
    otlp timeout
    (about (result ()) producer)

let in_output w text =
  List.exists
    (fun line -> Re.execp (Re.compile (Re.str text)) line)
    (A.output w)

let metric_names (otlp : Otlp.t) =
  `List (List.map (fun (n, _) -> `String n) (Dynarray.to_list otlp.metrics))

let endpoint (otlp : Otlp.t) =
  ("OTEL_EXPORTER_OTLP_ENDPOINT", Some otlp.endpoint)

let service_name implementations () =
  List.iter
    (fun (name, expected) ->
      with_env implementations
        (fun otlp ->
          [
            endpoint otlp;
            ("OTEL_BSP_SCHEDULE_DELAY", Some "100");
            ("OTEL_SERVICE_NAME", name);
          ])
        (fun w otlp ->
          ask w;
          let s = received otlp in
          Check.that
            (Printf.sprintf "the span's service is %s" expected)
            (List.assoc_opt "service.name" s.resource
            = Some (Otlp.Text expected))))
    [
      (None, Protocol.default_service_name Archivist);
      (Some "conformance-renamed", "conformance-renamed");
    ]

let reply_traced implementations () =
  with_env implementations
    (fun otlp -> [ endpoint otlp; ("OTEL_BSP_SCHEDULE_DELAY", Some "100") ])
    (fun w otlp ->
      ask w;
      let reply = sent otlp in
      let answered =
        span (Printf.sprintf "the %s it answered" request) otlp 10.0 (fun s ->
            about request consumer s && s.trace_id = reply.trace_id)
      in
      Check.equal "the reply's trace" (str answered.trace_id)
        (str reply.trace_id))

let sdk_disabled implementations () =
  with_env implementations
    (fun otlp ->
      [
        endpoint otlp;
        ("OTEL_BSP_SCHEDULE_DELAY", Some "100");
        ("OTEL_METRIC_EXPORT_INTERVAL", Some "500");
        ("OTEL_SDK_DISABLED", Some "true");
      ])
    (fun w otlp ->
      ask w;
      Run.sleep 2.0;
      Check.equal "the spans it exported" (`Int 0)
        (`Int (Dynarray.length otlp.spans));
      Check.equal "the metrics it exported" (`List []) (metric_names otlp))

let console_spans implementations () =
  with_env implementations
    (fun _ ->
      [
        ("OTEL_CONSOLE_EXPORTER", Some "true");
        ("OTEL_BSP_SCHEDULE_DELAY", Some "100");
      ])
    (fun w _ ->
      ask w;
      Run.eventually (Printf.sprintf "a span about %s in the output" request)
        10.0 (fun () ->
          if
            List.exists
              (fun line ->
                Re.execp (Re.compile (Re.str request)) line
                && not (String.starts_with ~prefix:"{" line))
              (A.output w)
          then Some ()
          else None))

let console_metrics implementations () =
  with_env implementations
    (fun otlp ->
      [
        endpoint otlp;
        ("OTEL_BSP_SCHEDULE_DELAY", Some "100");
        ("OTEL_METRIC_EXPORT_INTERVAL", Some "500");
        ("OTEL_METRICS_EXPORTER", Some "console");
      ])
    (fun w otlp ->
      ask w;
      ignore (received otlp : Otlp.span);
      let started = Protocol.metric_name "xynolog.process.start_time" in
      Run.eventually (started ^ " in the output") 10.0 (fun () ->
          if in_output w started then Some () else None);
      Check.equal "the metrics the receiver got" (`List []) (metric_names otlp))

let metric_interval implementations () =
  with_env implementations
    (fun otlp -> [ endpoint otlp; ("OTEL_METRIC_EXPORT_INTERVAL", Some "500") ])
    (fun _ otlp ->
      Run.eventually "a metric export within 3 s" 3.0 (fun () ->
          if Dynarray.length otlp.metrics > 0 then Some () else None))

let schedule_delay implementations () =
  with_env implementations
    (fun otlp -> [ endpoint otlp; ("OTEL_BSP_SCHEDULE_DELAY", Some "100") ])
    (fun w otlp ->
      ask w;
      ignore (sent ~timeout:2.0 otlp : Otlp.span))

let bus_log implementations () =
  List.iter
    (fun on ->
      with_env implementations
        (fun _ -> [ ("XYNOLOG_BUS_LOG", if on then Some "1" else None) ])
        (fun w _ ->
          ask w;
          if on then
            List.iter
              (fun line ->
                Run.eventually (Printf.sprintf "a %s line" line) 5.0 (fun () ->
                    if in_output w line then Some () else None))
              [
                Telemetry.bus_line "RECV" request;
                Telemetry.bus_line "EMIT" (result ());
              ]
          else (
            Run.sleep 0.5;
            Check.equal "the bus log's lines" (`List [])
              (`List
                 (List.filter_map
                    (fun line ->
                      if
                        Re.execp
                          (Re.compile (Re.str (Telemetry.bus_line_start ())))
                          line
                      then Some (`String line)
                      else None)
                    (A.output w))))))
    [ false; true ]

let supervised implementations () =
  Telemetry.with_supervisor_events ~name:"archivist" ~lives:3 @@ fun events ->
  with_env implementations
    (fun otlp ->
      [
        endpoint otlp;
        ("OTEL_METRIC_EXPORT_INTERVAL", Some "500");
        ("SUPERVISE_EVENTS", Some events);
        ("SUPERVISE_NAME", Some "archivist");
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

let tests implementations =
  List.map
    (fun c ->
      Case.test ~category:[ "the Archivist's environment" ] c (fun () ->
          c.run ()))
    [
      case
        "OTEL_SERVICE_NAME names the service its spans are exported under; \
         xynolog-archivist when unset"
        (service_name implementations);
      case
        "with OTEL_EXPORTER_OTLP_ENDPOINT, a reply is sent in the trace of the \
         request it answers"
        (reply_traced implementations);
      case "OTEL_SDK_DISABLED=true exports nothing, though an endpoint is set"
        (sdk_disabled implementations);
      case
        "OTEL_CONSOLE_EXPORTER=true, with no endpoint, writes its spans to the \
         Archivist's output"
        (console_spans implementations);
      case
        "OTEL_METRICS_EXPORTER=console writes metrics to the output, and spans \
         still reach the endpoint"
        (console_metrics implementations);
      case
        "OTEL_METRIC_EXPORT_INTERVAL sets how often metrics are exported (30 s \
         when unset)"
        (metric_interval implementations);
      case
        "OTEL_BSP_SCHEDULE_DELAY sets how long a finished span waits to be \
         exported (5 s when unset)"
        (schedule_delay implementations);
      case
        "XYNOLOG_BUS_LOG writes a line per frame the Archivist receives and \
         sends; unset, it writes none"
        (bus_log implementations);
      case
        "SUPERVISE_EVENTS and SUPERVISE_NAME report xynolog.process.restarts: \
         the lives the supervisor recorded, less one"
        (supervised implementations);
    ]
