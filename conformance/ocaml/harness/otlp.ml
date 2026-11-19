(* An OTLP/HTTP receiver: collects the spans a service exports, each with
   the attributes of the resource it was exported under, and for each
   metric the instruments it arrived as, the attributes and values its data
   points carried and the resources it arrived under, so a case can hold the
   service to the telemetry the spec lists (Telemetry). It reads either
   encoding OTLP/HTTP has, by Content-Type, as a collector does: protobuf,
   on the opentelemetry package's generated decoders, and JSON; and a body
   sent with Content-Encoding: gzip, on decompress. *)

module Proto = Opentelemetry_proto

(* An attribute's value as OTLP carried it. An int64 and a double are both
   a number; any other value (an array, a map, bytes) is [Unreadable]. *)
type value = Text of string | Flag of bool | Number of float | Unreadable

(* A resource's attributes, in key order, so two exports under one resource
   are equal. *)
type resource = (string * value) list

type span = {
  name : string;
  kind : int;
  resource : resource;
  attributes : (string * value) list;
  trace_id : string;
  parent_span_id : string option;
}

type metric = {
  instruments : string Dynarray.t;
      (** counter, up-down counter, gauge, histogram *)
  attributes : (string * value Dynarray.t) Dynarray.t;
      (** Key → every value it carried. *)
  values : float Dynarray.t;
      (** Every value its sum and gauge points carried, in order. *)
  resources : resource Dynarray.t;  (** Each once, in arrival order. *)
}

type t = {
  endpoint : string;
  spans : span Dynarray.t;
  metrics : (string * metric) Dynarray.t;
  unreadable : string Dynarray.t;  (** Each body it could not read, and why. *)
  server : Serve.t;
}

(* ── One shape for both encodings ──────────────────────────────────── *)

type point = { point_attributes : (string * value) list; number : float option }
type exported = { metric_name : string; data : (string * point list) list }

let sorted resource = List.sort (fun (a, _) (b, _) -> compare a b) resource

let of_any : Proto.Common.any_value option -> value = function
  | Some (String_value s) -> Text s
  | Some (Bool_value b) -> Flag b
  | Some (Int_value n) -> Number (Int64.to_float n)
  | Some (Double_value f) -> Number f
  | Some (Array_value _ | Kvlist_value _ | Bytes_value _) | None -> Unreadable

let of_key_values (kvs : Proto.Common.key_value list) =
  List.map (fun (kv : Proto.Common.key_value) -> (kv.key, of_any kv.value)) kvs

let resource_of (resource : Proto.Resource.resource option) =
  sorted
    (match resource with Some r -> of_key_values r.attributes | None -> [])

let kind_number : Proto.Trace.span_span_kind -> int = function
  | Span_kind_unspecified -> 0
  | Span_kind_internal -> 1
  | Span_kind_server -> 2
  | Span_kind_client -> 3
  | Span_kind_producer -> 4
  | Span_kind_consumer -> 5

let hex bytes = Ohex.encode (Bytes.to_string bytes)

let spans_of_protobuf body =
  let request =
    Proto.Trace_service.decode_pb_export_trace_service_request
      (Pbrt.Decoder.of_string body)
  in
  List.concat_map
    (fun (rs : Proto.Trace.resource_spans) ->
      let resource = resource_of rs.resource in
      List.concat_map
        (fun (ss : Proto.Trace.scope_spans) ->
          List.map
            (fun (s : Proto.Trace.span) ->
              {
                name = s.name;
                kind = kind_number s.kind;
                resource;
                attributes = of_key_values s.attributes;
                trace_id = hex s.trace_id;
                parent_span_id =
                  (if Bytes.length s.parent_span_id = 0 then None
                   else Some (hex s.parent_span_id));
              })
            ss.spans)
        rs.scope_spans)
    request.resource_spans

let metrics_of_protobuf body =
  let request =
    Proto.Metrics_service.decode_pb_export_metrics_service_request
      (Pbrt.Decoder.of_string body)
  in
  let numbers (points : Proto.Metrics.number_data_point list) =
    List.map
      (fun (p : Proto.Metrics.number_data_point) ->
        {
          point_attributes = of_key_values p.attributes;
          number =
            (match p.value with
            | Some (As_int n) -> Some (Int64.to_float n)
            | Some (As_double f) -> Some f
            | None -> None);
        })
      points
  in
  List.concat_map
    (fun (rm : Proto.Metrics.resource_metrics) ->
      let resource = resource_of rm.resource in
      List.concat_map
        (fun (sm : Proto.Metrics.scope_metrics) ->
          List.map
            (fun (m : Proto.Metrics.metric) ->
              let data =
                match m.data with
                | Some (Sum s) ->
                    [
                      ( (if s.is_monotonic then "counter" else "up-down counter"),
                        numbers s.data_points );
                    ]
                | Some (Gauge g) -> [ ("gauge", numbers g.data_points) ]
                | Some (Histogram h) ->
                    [
                      ( "histogram",
                        List.map
                          (fun (p : Proto.Metrics.histogram_data_point) ->
                            {
                              point_attributes = of_key_values p.attributes;
                              number = None;
                            })
                          h.data_points );
                    ]
                | Some (Exponential_histogram _ | Summary _) | None -> []
              in
              (resource, { metric_name = m.name; data }))
            sm.metrics)
        rm.scope_metrics)
    request.resource_metrics

(* OTLP/JSON: the same messages, members in lowerCamelCase, an int64 as a
   string or a number. *)
let list json path =
  match Protocol.at json path with Some (`List items) -> items | _ -> []

let text json path =
  match Protocol.at json path with Some (`String s) -> Some s | _ -> None

let json_number = function
  | Some (`Int n) -> Some (float_of_int n)
  | Some (`Intlit s) | Some (`String s) -> float_of_string_opt s
  | Some (`Float f) -> Some f
  | _ -> None

let json_value json =
  match json with
  | `Assoc members -> (
      match
        List.find_map
          (fun (key, v) ->
            match (key, v) with
            | "stringValue", `String s -> Some (Text s)
            | "boolValue", `Bool b -> Some (Flag b)
            | ("intValue" | "doubleValue"), v ->
                Option.map (fun n -> Number n) (json_number (Some v))
            | _ -> None)
          members
      with
      | Some value -> value
      | None -> Unreadable)
  | _ -> Unreadable

let json_attributes kvs =
  List.filter_map
    (fun kv ->
      match text kv [ "key" ] with
      | Some key ->
          Some
            ( key,
              json_value
                (Option.value ~default:`Null (Protocol.at kv [ "value" ])) )
      | None -> None)
    kvs

let json_resource entry =
  sorted (json_attributes (list entry [ "resource"; "attributes" ]))

let spans_of_json body =
  List.concat_map
    (fun rs ->
      let resource = json_resource rs in
      List.concat_map
        (fun ss ->
          List.map
            (fun s ->
              {
                name = Option.value ~default:"" (text s [ "name" ]);
                kind =
                  (match Protocol.at s [ "kind" ] with
                  | Some (`Int k) -> k
                  | _ -> 0);
                resource;
                attributes = json_attributes (list s [ "attributes" ]);
                trace_id = Option.value ~default:"" (text s [ "traceId" ]);
                parent_span_id =
                  (match text s [ "parentSpanId" ] with
                  | Some "" | None -> None
                  | id -> id);
              })
            (list ss [ "spans" ]))
        (list rs [ "scopeSpans" ]))
    (list body [ "resourceSpans" ])

let metrics_of_json body =
  let points data numbered =
    List.map
      (fun p ->
        {
          point_attributes = json_attributes (list p [ "attributes" ]);
          number =
            (if numbered then
               match json_number (Protocol.at p [ "asInt" ]) with
               | Some n -> Some n
               | None -> json_number (Protocol.at p [ "asDouble" ])
             else None);
        })
      (list data [ "dataPoints" ])
  in
  List.concat_map
    (fun rm ->
      let resource = json_resource rm in
      List.concat_map
        (fun sm ->
          List.filter_map
            (fun m ->
              Option.map
                (fun metric_name ->
                  let data =
                    List.filter_map
                      (fun kind ->
                        match Protocol.at m [ kind ] with
                        | Some (`Assoc _ as data) ->
                            let instrument =
                              match kind with
                              | "sum" ->
                                  if
                                    Protocol.at data [ "isMonotonic" ]
                                    = Some (`Bool true)
                                  then "counter"
                                  else "up-down counter"
                              | other -> other
                            in
                            Some (instrument, points data (kind <> "histogram"))
                        | _ -> None)
                      [ "sum"; "gauge"; "histogram" ]
                  in
                  (resource, { metric_name; data }))
                (text m [ "name" ]))
            (list sm [ "metrics" ]))
        (list rm [ "scopeMetrics" ]))
    (list body [ "resourceMetrics" ])

(* ── The body ───────────────────────────────────────────────────────── *)

let gunzip compressed =
  let input = De.bigstring_create De.io_buffer_size
  and output = De.bigstring_create De.io_buffer_size in
  let plain = Buffer.create (4 * String.length compressed) in
  let read = ref 0 in
  let refill buffer =
    let length = min (String.length compressed - !read) De.io_buffer_size in
    for at = 0 to length - 1 do
      Bigarray.Array1.set buffer at compressed.[!read + at]
    done;
    read := !read + length;
    length
  in
  let flush buffer length =
    for at = 0 to length - 1 do
      Buffer.add_char plain (Bigarray.Array1.get buffer at)
    done
  in
  match Gz.Higher.uncompress ~refill ~flush input output with
  | Ok _ -> Buffer.contents plain
  | Error (`Msg why) -> failwith ("a body sent as gzip that is not: " ^ why)

let add_once dynarray item =
  if not (Dynarray.exists (( = ) item) dynarray) then
    Dynarray.add_last dynarray item

let metric_named metrics name =
  match Dynarray.find_opt (fun (n, _) -> n = name) metrics with
  | Some (_, metric) -> metric
  | None ->
      let metric =
        {
          instruments = Dynarray.create ();
          attributes = Dynarray.create ();
          values = Dynarray.create ();
          resources = Dynarray.create ();
        }
      in
      Dynarray.add_last metrics (name, metric);
      metric

let take_metric metrics (resource, exported) =
  let metric = metric_named metrics exported.metric_name in
  add_once metric.resources resource;
  List.iter
    (fun (instrument, points) ->
      add_once metric.instruments instrument;
      List.iter
        (fun point ->
          List.iter
            (fun (key, value) ->
              match
                Dynarray.find_opt (fun (k, _) -> k = key) metric.attributes
              with
              | Some (_, seen) -> add_once seen value
              | None ->
                  Dynarray.add_last metric.attributes
                    (key, Dynarray.of_list [ value ]))
            point.point_attributes;
          match point.number with
          | Some n when instrument <> "histogram" && Float.is_finite n ->
              Dynarray.add_last metric.values n
          | _ -> ())
        points)
    exported.data

let receive spans metrics (request : Serve.request) =
  let header name =
    Option.map String.lowercase_ascii (Http.Header.get request.headers name)
  in
  let media =
    Option.map
      (fun v -> String.trim (List.hd (String.split_on_char ';' v)))
      (header "content-type")
  in
  let body =
    if header "content-encoding" = Some "gzip" then gunzip request.body
    else request.body
  in
  let protobuf = media = Some "application/x-protobuf" in
  let json () =
    match Call.parsed body with
    | Some (`Assoc _ as json) -> json
    | _ -> failwith "not an OTLP body"
  in
  match request.path with
  | "/v1/traces" ->
      List.iter (Dynarray.add_last spans)
        (if protobuf then spans_of_protobuf body else spans_of_json (json ()))
  | "/v1/metrics" ->
      List.iter (take_metric metrics)
        (if protobuf then metrics_of_protobuf body
         else metrics_of_json (json ()))
  | path ->
      failwith ("an export to " ^ path ^ ", which OTLP/HTTP has no signal at")

let start () =
  let spans = Dynarray.create ()
  and metrics = Dynarray.create ()
  and unreadable = Dynarray.create () in
  let server =
    Serve.start (fun request ->
        (match receive spans metrics request with
        | () -> ()
        | exception failure ->
            Dynarray.add_last unreadable
              (Printf.sprintf "%s %s: %s" request.meth request.path
                 (Printexc.to_string failure)));
        Serve.Reply (Serve.json_reply 200 (`Assoc [])))
  in
  { endpoint = server.origin; spans; metrics; unreadable; server }

let metric t name =
  Option.map snd (Dynarray.find_opt (fun (n, _) -> n = name) t.metrics)

let close t = t.server.stop ()
