(* The telemetry the gateway exports, which dashboards and alerts are built
   on: the spans and metrics specs/src/service-telemetry/telemetry.json lists
   for it, exported over OTLP to the endpoint the standard OpenTelemetry
   environment names, and the trace context carried onto bus frames
   (contract/EVENT-BUS.md § Trace context). Each plane exports to a receiver
   of its own, and its last case holds everything that receiver got to the
   table, in both directions; so each plane's cases run solo, in order, on
   one world. *)

open Xynolog_conformance
open Support
open Case

(* The span kinds the cases name, as OTLP numbers them. *)
let server = Protocol.otlp_kind Server
let client = Protocol.otlp_kind Client
let producer = Protocol.otlp_kind Producer

let in_order =
  "the plane's last case is judged on everything its cases made the gateway \
   export"

type shared = { world : World.t; otlp : Otlp.t }

let make implementations plane =
  let otlp = Otlp.start () in
  let world =
    World.create ~command:implementations.Implementations.gateway
      ~env:
        [
          (* Read when the world starts, after the receiver is listening. *)
          ("OTEL_EXPORTER_OTLP_ENDPOINT", Some otlp.endpoint);
          ("OTEL_METRIC_EXPORT_INTERVAL", Some "500");
          ("OTEL_BSP_SCHEDULE_DELAY", Some "100");
        ]
      plane
  in
  { world; otlp }

let close { world; otlp } =
  World.close world;
  Otlp.close otlp

type traceparent = { trace_id : string; span_id : string; header : string }

let traceparent () =
  let trace_id = Fresh.hex 16 and span_id = Fresh.hex 8 in
  { trace_id; span_id; header = Printf.sprintf "00-%s-%s-01" trace_id span_id }

let text value = Some (Otlp.Text value)

(* The first span [matches] accepts, once it has arrived. *)
let span ?(timeout = 10.0) what (otlp : Otlp.t) matches =
  Run.eventually what timeout (fun () -> Dynarray.find_opt matches otlp.spans)

let named name traced (s : Otlp.span) =
  s.name = name
  && match traced with Some t -> s.trace_id = t.trace_id | None -> true

let kind what expected (s : Otlp.span) =
  Check.that
    (Printf.sprintf "%s is of kind %d, not %d" what s.kind expected)
    (s.kind = expected)

let attribute key (s : Otlp.span) = List.assoc_opt key s.attributes

let traced_emit { world = w; otlp } =
  let watcher =
    World.subscribe w (World.person w "watcher")
      (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ())
         ~global:[ "beckon:focus" ] ())
  in
  let t = traceparent () in
  let marker = Fresh.uuid () in
  let reply =
    Call.call ~token:(World.person w "tracer")
      ~json:
        (`Assoc
           [
             ("channel", `String "beckon:focus");
             ( "payload",
               `Assoc
                 [
                   ("annotationId", `String marker);
                   ("resourceId", `String "res-1");
                 ] );
           ])
      ~headers:[ ("traceparent", t.header); ("idempotency-key", Fresh.uuid ()) ]
      (World.origin w) "POST"
      (Protocol.path Gateway "POST" "/bus/emit")
  in
  status "the traced emit" 202 reply;
  let dispatch =
    Protocol.span_name "bus.dispatch:{channel}" [ ("channel", "beckon:focus") ]
  in
  let found = span "the bus.dispatch span" otlp (named dispatch (Some t)) in
  kind dispatch server found;
  Check.that "the span's parent is the caller's span"
    (found.parent_span_id = Some t.span_id);
  Check.that "the span carries its channel"
    (attribute "bus.channel" found = text "beckon:focus");
  Check.that "the span's service is the gateway's"
    (List.assoc_opt "service.name" found.resource
    = text (Protocol.default_service_name Gateway));
  let frame =
    Bus_stream.frame watcher "beckon:focus" ~matches:(fun f ->
        Protocol.at f.payload [ "annotationId" ] = Some (`String marker))
  in
  let carried =
    match
      Protocol.at frame.payload
        [ (Protocol.tables ()).bus_registry.trace.field; "traceparent" ]
    with
    | Some (`String carried) -> carried
    | _ -> ""
  in
  Check.equal "the trace the frame carries on" (`String t.trace_id)
    (`String
       (match String.split_on_char '-' carried with
       | _ :: trace :: _ -> trace
       | _ -> ""))

let request = "browse:resource-requested"

let reply_delivered { world = w; otlp } =
  let result = (Protocol.operation_for request).result in
  ignore
    (World.responder w [ request ] (fun _ -> Some (result, described ()))
      : World.responder);
  let alice = World.person w "alice" in
  let client_id = Fresh.uuid () in
  let stream =
    World.subscribe w alice
      (Bus_stream.subscribe_body ~client_id ~global:[ result ] ())
  in
  let correlation_id = Fresh.uuid () in
  ignore
    (World.emit w alice
       (`Assoc
          [
            ("channel", `String request);
            ("payload", `Assoc [ ("resourceId", `String "r") ]);
            ("correlationId", `String correlation_id);
            ("clientId", `String client_id);
          ])
      : Call.reply);
  ignore
    (Bus_stream.frame stream result ~matches:(fun f ->
         f.correlation_id = Some correlation_id)
      : Bus_stream.frame);
  let deliver =
    Protocol.span_name "sse.deliver:{channel}" [ ("channel", result) ]
  in
  kind deliver producer
    (span "the sse.deliver span" otlp (fun s ->
         s.name = deliver && attribute "bus.cid" s = text correlation_id))

let content_spans { world = w; otlp } =
  let token = World.person w "reader" in
  let id = "res-" ^ Fresh.uuid () in
  Hashtbl.replace w.archivist.resources id
    { storage_uri = "file://" ^ id; media_type = "text/plain" };
  Hashtbl.replace w.archivist.content ("file://" ^ id) "bytes";
  let get ?(path = "/resources/{id}") t =
    Call.call ~token
      ~headers:[ ("traceparent", t.header) ]
      (World.origin w) "GET"
      (Protocol.path_of Gateway "GET" path [ ("id", id) ])
  in
  let traced_span name t =
    let name = Protocol.span_name name [] in
    span name otlp (named name (Some t))
  in
  let t = traceparent () in
  ignore (get t : Call.reply);
  kind "content.get.server" server (traced_span "content.get.server" t);
  let read = traced_span "archivist.content.get" t in
  kind "archivist.content.get" client read;
  Check.that "the client span names the Archivist"
    (attribute "peer.service" read = text "archivist");
  let put = traceparent () in
  let content_type, body =
    Call.multipart
      [
        Call.Field ("name", "traced");
        Call.Field ("format", "text/plain");
        Call.Field
          ("storageUri", Printf.sprintf "file://uploads/%s.txt" (Fresh.uuid ()));
        Call.File
          {
            name = "file";
            filename = "traced.txt";
            media = "text/plain";
            bytes = "traced bytes";
          };
      ]
  in
  let uploaded =
    Call.call ~token ~body
      ~headers:
        [
          ("content-type", content_type);
          ("traceparent", put.header);
          ("idempotency-key", Fresh.uuid ());
        ]
      (World.origin w) "POST"
      (Protocol.path Gateway "POST" "/resources")
  in
  status "the traced upload" 202 uploaded;
  kind "content.put.server" server (traced_span "content.put.server" put);
  kind "archivist.resources.record" client
    (traced_span "archivist.resources.record" put);
  Hashtbl.replace w.archivist.descriptions id
    (`Assoc
       [
         ( "resource",
           `Assoc
             [
               ("@context", `String "https://schema.org/");
               ("@id", `String id);
               ("name", `String "traced");
               ("representations", `List []);
             ] );
         ("annotations", `List []);
       ]);
  let describe = traceparent () in
  status "the description" 200 (get ~path:"/resources/{id}/jsonld" describe);
  kind "archivist.resources.describe" client
    (traced_span "archivist.resources.describe" describe);
  Hashtbl.replace w.archivist.annotations id (fun () ->
      Some (`Assoc [ ("annotations", `List []); ("total", `Int 0) ]));
  let listed = traceparent () in
  status "the annotations" 200 (get ~path:"/resources/{id}/annotations" listed);
  kind "archivist.resources.annotations" client
    (traced_span "archivist.resources.annotations" listed);
  Hashtbl.replace w.archivist.anchored_texts id (fun () ->
      `Assoc [ ("kind", `String "no-map") ]);
  let anchored = traceparent () in
  status "the anchored text" 200
    (get ~path:"/resources/{id}/anchored-text" anchored);
  let anchored_span =
    traced_span "archivist.resources.anchored_text" anchored
  in
  kind "archivist.resources.anchored_text" client anchored_span;
  Check.that "the anchored-text span names the Archivist"
    (attribute "peer.service" anchored_span = text "archivist");
  let scope = "res-" ^ Fresh.uuid () in
  Hashtbl.replace w.archivist.events scope
    [ Archivist_standin.stored_event "mark:added" scope 2 ];
  let replay =
    World.open_stream w token
      (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ())
         ~scoped:
           [
             Bus_stream.scope
               ~last_event_id:(Printf.sprintf "p-%s-1" scope)
               scope [ "mark:added" ];
           ]
         ())
  in
  ignore
    (Bus_stream.next replay "the first ping" (fun m -> m.event = "ping")
      : Bus_stream.message);
  let replay_span = Protocol.span_name "archivist.events.replay" [] in
  kind replay_span client (span replay_span otlp (named replay_span None))

let emit_focus (w : World.t) token =
  World.emit w token
    (`Assoc
       [
         ("channel", `String "beckon:focus");
         ("payload", `Assoc [ ("resourceId", `String "res-1") ]);
       ])

(* An emit's bucket for a principal holding no role the spec exempts. *)
let baseline_burst () =
  match
    Option.bind (Protocol.operation Gateway "POST" "/bus/emit").limits
      (fun limits -> List.assoc_opt "emitsPerPrincipal" limits)
  with
  | Some (Limits rate) -> (
      match List.assoc_opt "baseline" rate with
      | Some (Limits baseline) -> (
          match List.assoc_opt "burst" baseline with
          | Some (Count burst) -> burst
          | _ -> failwith "emitsPerPrincipal's baseline states no burst")
      | _ -> failwith "emitsPerPrincipal states no baseline")
  | _ -> failwith "POST /bus/emit states no emitsPerPrincipal"

let metrics_exported { world = w; otlp } =
  (* Something to count for each: an emit, a subscriber, a resume gap, a
     reply withheld from a subscriber that did not ask for it. *)
  let token = World.person w "metered" in
  ignore (emit_focus w token : Call.reply);
  let gap =
    World.open_stream w token
      (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ())
         ~scoped:
           [
             Bus_stream.scope ~last_event_id:"garbage" "res-metered"
               [ "mark:added" ];
           ]
         ())
  in
  ignore
    (Bus_stream.next gap "the first ping" (fun m -> m.event = "ping")
      : Bus_stream.message);
  let result = (Protocol.operation_for request).result in
  let client_id = Fresh.uuid () and correlation_id = Fresh.uuid () in
  let owner =
    World.subscribe w token
      (Bus_stream.subscribe_body ~client_id ~global:[ result ] ())
  in
  ignore
    (World.subscribe w
       (World.person w "not-the-owner")
       (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ()) ~global:[ result ]
          ())
      : Bus_stream.t);
  ignore
    (World.emit w token
       (`Assoc
          [
            ("channel", `String request);
            ("payload", `Assoc [ ("resourceId", `String "r") ]);
            ("correlationId", `String correlation_id);
            ("clientId", `String client_id);
          ])
      : Call.reply);
  let participant, _ = World.agent w "metered" in
  ignore
    (World.emit w participant
       (`Assoc
          [
            ("channel", `String result);
            ("payload", described ());
            ("correlationId", `String correlation_id);
          ])
      : Call.reply);
  ignore
    (Bus_stream.frame owner result ~matches:(fun f ->
         f.correlation_id = Some correlation_id)
      : Bus_stream.frame);
  (* A request nothing answers: counted where the plane can count
     subscribers. *)
  ignore
    (World.emit w token
       (`Assoc
          [
            ("channel", `String "browse:kb-requested");
            ("payload", `Assoc []);
            ("correlationId", `String (Fresh.uuid ()));
            ("clientId", `String (Fresh.uuid ()));
          ])
      : Call.reply);
  (* A burst past one principal's emit bucket: a refusal, counted by its
     reason. *)
  let burst = baseline_burst () in
  let hasty = World.person w "hasty" in
  Run.eventually "an emit refused past its bucket" 15.0 (fun () ->
      if
        List.exists
          (fun (r : Call.reply) -> r.status = 429)
          (Eio.Fiber.List.map
             (fun () -> emit_focus w hasty)
             (List.init burst ignore))
      then Some ()
      else None);
  (* A request refused 401, for each reason one can be: no token, a token
     the gateway did not sign and the issuer did not, a media token that is
     none. *)
  let me = Protocol.path Gateway "GET" "/api/users/me" in
  status "no token" 401 (Call.call (World.origin w) "GET" me);
  status "a token that is none" 401
    (Call.call ~token:"not-a-token" (World.origin w) "GET" me);
  status "a media token that is none" 401
    (Call.call (World.origin w) "GET"
       (Protocol.path_of Gateway "GET" "/api/resources/{id}"
          [ ("id", "res-metered") ]
       ^ "?token=not-a-media-token"));
  let unauthenticated =
    Protocol.metric_name "xynolog.gateway.unauthenticated"
  in
  let reason = "unauthenticated.reason" in
  let _, metric_rows = Protocol.telemetry ~plane:w.plane Gateway in
  let listed =
    match
      List.find_opt
        (fun (r : Protocol.metric_row) -> r.metric = unauthenticated)
        metric_rows
    with
    | Some row -> (
        match
          List.find_opt
            (fun (a : Protocol.attribute) -> a.key = reason)
            row.metric_attributes
        with
        | Some { values = Some values; _ } ->
            List.map
              (function
                | Protocol.String s -> s | Boolean b -> string_of_bool b)
              values
        | _ -> failwith (unauthenticated ^ " lists no values of " ^ reason))
    | None -> failwith ("the gateway's table has no " ^ unauthenticated)
  in
  let reasons =
    Run.eventually "every reason a request is refused 401 for" 15.0 (fun () ->
        Option.bind (Otlp.metric otlp unauthenticated) (fun metric ->
            match
              Dynarray.find_opt (fun (k, _) -> k = reason) metric.attributes
            with
            | Some (_, values) when Dynarray.length values = List.length listed
              ->
                Some
                  (List.map
                     (function
                       | Otlp.Text s -> s
                       | _ -> failwith (reason ^ " came as other than text"))
                     (Dynarray.to_list values))
            | _ -> None))
  in
  Check.equal "the reasons a request was refused 401 for"
    (`List (List.map (fun s -> `String s) (List.sort compare listed)))
    (`List (List.map (fun s -> `String s) (List.sort compare reasons)));
  (* Every metric the gateway's traffic exports, here: its table's rows but
     those it exports unasked, and the SDK table's rows it names. *)
  let exported_unasked =
    List.filter_map
      (fun (row : Protocol.Service_rows.metrics_item) ->
        if
          List.mem Protocol.Service_rows.Gateway row.services
          && row.when_ = Export
        then Some row.name
        else None)
      (Protocol.tables ()).service_telemetry_telemetry.metrics
  in
  List.iter
    (fun (row : Protocol.metric_row) ->
      let seen =
        Run.eventually (Printf.sprintf "the %s metric" row.metric) 15.0
          (fun () -> Otlp.metric otlp (Protocol.metric_name row.metric))
      in
      List.iter
        (fun (a : Protocol.attribute) ->
          if not a.only then
            Run.eventually (Printf.sprintf "%s carrying %s" row.metric a.key)
              15.0 (fun () ->
                if Dynarray.exists (fun (k, _) -> k = a.key) seen.attributes
                then Some ()
                else None))
        row.metric_attributes)
    (List.filter
       (fun (row : Protocol.metric_row) ->
         row.metric_expected && not (List.mem row.metric exported_unasked))
       metric_rows)

(* Last in its plane, and judged on everything this plane's cases made the
   gateway export: run it with them, never alone. *)
let exactly_the_table { world = w; otlp } =
  Check.equal "what the gateway exported outside the table" (`List [])
    (`List
       (List.map
          (fun f -> `String f)
          (Telemetry.outside_the_table ~plane:w.plane otlp Gateway
             (Some w.kb.did))))

let tests implementations =
  each_plane ~make:(make implementations)
    ~world:(fun { world; _ } -> world)
    ~close "what the gateway exports"
    [
      case ~solo:in_order
        "a traced emit is a bus.dispatch server span in the caller's trace, \
         and its frames carry the trace on"
        traced_emit;
      case ~solo:in_order
        "a reply delivered to its requester is an sse.deliver producer span"
        reply_delivered;
      case ~solo:in_order
        "content reads and writes, descriptions, anchored texts and replay \
         reads, are server spans with an archivist client span beneath"
        content_spans;
      case ~solo:in_order
        "exports the bus and subscriber metrics, with the attributes \
         dashboards group them by"
        metrics_exported;
      case ~solo:in_order
        "exports exactly the telemetry the spec lists: every row it can, and \
         nothing else"
        exactly_the_table;
    ]
