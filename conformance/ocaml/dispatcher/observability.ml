(* The telemetry the dispatcher exports: the spans and metrics
   specs/src/service-telemetry/telemetry.json lists for it, exported over
   OTLP to the endpoint the standard OpenTelemetry environment names. Its
   last case holds everything the receiver got to the table, in both
   directions; so its cases run solo, in order, on one world. *)

open Xynolog_conformance
open Support
open Case

(* OTLP's span kinds. *)
let producer = 4
let consumer = 5

let in_order =
  "the file's last case is judged on everything its cases made the dispatcher \
   export"

type shared = { dw : D.t; otlp : Otlp.t }

let make (implementations : Implementations.t) () =
  let otlp = Otlp.start () in
  let dw =
    D.start ~gateway:implementations.gateway ~command:implementations.dispatcher
      ~env:
        [
          (* Read when the world starts, after the receiver is listening. *)
          ("OTEL_EXPORTER_OTLP_ENDPOINT", otlp.endpoint);
          ("OTEL_METRIC_EXPORT_INTERVAL", "500");
          ("OTEL_BSP_SCHEDULE_DELAY", "100");
        ]
      ()
  in
  { dw; otlp }

let close { dw; otlp } =
  D.close dw;
  Otlp.close otlp

let bus_spans { dw = w; otlp } =
  let r = D.running w in
  D.report_progress r.worker r.ref (`Int 50);
  D.complete
    ~result:(`Assoc [ ("found", `Int 0); ("persisted", `Int 0) ])
    r.worker r.ref;
  ignore
    (D.until_status r.creator
       (D.text r.job [ "metadata"; "id" ])
       "the job to complete" "complete"
      : Yojson.Safe.t);
  let cancelled =
    D.created ~resource_id:(D.resource_id ()) r.creator "mark"
      (`Assoc [ ("motivation", str "highlighting") ])
  in
  Check.that "the cancellation is answered"
    (D.cancel_request r.creator (`Assoc [ ("jobId", str cancelled) ])).ok;
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
        (Printf.sprintf "%s is exported under %s" name
           (Protocol.default_service_name Dispatcher))
        (List.assoc_opt "service.name" s.resource
        = Some (Otlp.Text (Protocol.default_service_name Dispatcher))))
    [
      ("bus.recv:{channel}", "job:create", consumer);
      ("bus.recv:{channel}", "job:claim", consumer);
      ("bus.recv:{channel}", "job:complete", consumer);
      ("bus.emit:{channel}", "job:created", producer);
      ("bus.emit:{channel}", "job:queued", producer);
      ("bus.emit:{channel}", "job:claimed", producer);
    ]

let queue_size { otlp; _ } =
  let statuses =
    match
      Option.bind (Protocol.json_of "components/schemas/JobStatusResponse.json")
        (fun schema -> Protocol.at schema [ "properties"; "status"; "enum" ])
    with
    | Some (`List statuses) -> statuses
    | _ -> failwith "JobStatusResponse states no status enum"
  in
  let name = Protocol.metric_name "xynolog.job.queue.size" in
  let _, metrics = Protocol.telemetry Dispatcher in
  let row =
    match
      List.find_opt (fun (r : Protocol.metric_row) -> r.metric = name) metrics
    with
    | Some row -> row
    | None -> failwith ("the table lists no " ^ name ^ " for the dispatcher")
  in
  Check.equal "the statuses the row gives job.status" (`List statuses)
    (match
       List.find_opt
         (fun (a : Protocol.attribute) -> a.key = "job.status")
         row.metric_attributes
     with
    | Some { values = Some values; _ } ->
        `List
          (List.map
             (function
               | Protocol.String s -> `String s | Protocol.Boolean b -> `Bool b)
             values)
    | _ -> `Null);
  Run.eventually "a queue size for every status" 10.0 (fun () ->
      match Otlp.metric otlp name with
      | None -> None
      | Some m -> (
          match
            Dynarray.find_opt (fun (k, _) -> k = "job.status") m.attributes
          with
          | None -> None
          | Some (_, seen) ->
              if
                List.for_all
                  (function
                    | `String s ->
                        Dynarray.exists (fun v -> v = Otlp.Text s) seen
                    | _ -> false)
                  statuses
              then Some ()
              else None))

(* Last, and judged on everything the cases above made the dispatcher
   export: run it with them, never alone. *)
let exactly_the_table { dw; otlp } =
  Check.equal "what the dispatcher exported outside the table" (`List [])
    (`List
       (List.map
          (fun f -> `String f)
          (Telemetry.outside_the_table otlp Dispatcher (Some dw.world.kb.did))))

let tests implementations =
  let shared =
    Suite.world "dispatcher/what the dispatcher exports"
      ~make:(make implementations) ~close
  in
  List.map
    (fun c ->
      Case.test ~category:[ "what the dispatcher exports" ] c (fun () ->
          let s = shared () in
          ended s.dw (fun () -> c.run s)))
    [
      case ~solo:in_order
        "each frame it receives is a bus.recv consumer span, and each it sends \
         a bus.emit producer span, under the dispatcher's name"
        bus_spans;
      case ~solo:in_order
        "xynolog.job.queue.size reports the queue by each status the spec \
         gives a job"
        queue_size;
      case ~solo:in_order
        "exports exactly the telemetry the spec lists: every row it can, and \
         nothing else"
        exactly_the_table;
    ]
