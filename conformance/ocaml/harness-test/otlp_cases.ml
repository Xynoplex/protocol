(* The OTLP receiver itself: it reads either encoding OTLP/HTTP has, and
   gzip; an export encoded as protobuf is read exactly as the same export
   encoded as OTLP/JSON; each span and metric reports the attributes of the
   resource it was exported under, as they were sent; and an attribute's
   value keeps the type it was carried as.

   The protobuf of an export is opentelemetry.proto's generated encoder's,
   which the receiver's generated decoder reads. Its JSON is the same
   package's generated JSON encoder's, with the two things that encoder
   writes otherwise than OTLP/JSON does put as OTLP/JSON writes them (an
   enum as its number, an id as hex rather than base64): so the receiver's
   own JSON reader is held to an encoding it did not write. *)

open Xynolog_conformance
open Expect
module Proto = Opentelemetry_proto

let gzip plain =
  let input = De.bigstring_create De.io_buffer_size
  and output = De.bigstring_create De.io_buffer_size in
  let compressed = Buffer.create 256 in
  let read = ref 0 in
  let refill buffer =
    let length = min (String.length plain - !read) De.io_buffer_size in
    for at = 0 to length - 1 do
      Bigarray.Array1.set buffer at plain.[!read + at]
    done;
    read := !read + length;
    length
  in
  let flush buffer length =
    for at = 0 to length - 1 do
      Buffer.add_char compressed (Bigarray.Array1.get buffer at)
    done
  in
  Gz.Higher.compress
    ~w:(De.Lz77.make_window ~bits:15)
    ~q:(De.Queue.create 0x1000) ~level:4 ~refill ~flush ()
    (Gz.Higher.configuration Gz.Unix (fun () -> 0l))
    input output;
  Buffer.contents compressed

let attribute key value = Proto.Common.make_key_value ~key ~value ()

let service =
  Proto.Resource.make_resource
    ~attributes:[ attribute "service.name" (String_value "standin") ]
    ()

let traces () =
  let span =
    Proto.Trace.make_span
      ~trace_id:(Bytes.of_string "0123456789abcdef")
      ~span_id:(Bytes.of_string "01234567")
      ~name:"bus.emit:gather:complete" ~kind:Span_kind_producer
      ~attributes:
        [
          attribute "bus.channel" (String_value "gather:complete");
          attribute "retry" (Bool_value false);
        ]
      ()
  in
  let request =
    Proto.Trace_service.make_export_trace_service_request
      ~resource_spans:
        [
          Proto.Trace.make_resource_spans ~resource:service
            ~scope_spans:[ Proto.Trace.make_scope_spans ~spans:[ span ] () ]
            ();
        ]
      ()
  in
  let encoder = Pbrt.Encoder.create () in
  Proto.Trace_service.encode_pb_export_trace_service_request request encoder;
  Pbrt.Encoder.to_string encoder

let metrics () =
  let point n outcome =
    Proto.Metrics.make_number_data_point ~value:(As_int n)
      ~attributes:[ attribute "inference.outcome" (String_value outcome) ]
      ()
  in
  let request =
    Proto.Metrics_service.make_export_metrics_service_request
      ~resource_metrics:
        [
          Proto.Metrics.make_resource_metrics ~resource:service
            ~scope_metrics:
              [
                Proto.Metrics.make_scope_metrics
                  ~metrics:
                    [
                      Proto.Metrics.make_metric ~name:"xynolog.inference.calls"
                        ~data:
                          (Sum
                             (Proto.Metrics.make_sum ~is_monotonic:true
                                ~data_points:
                                  [ point 2L "answered"; point 1L "failed" ]
                                ()))
                        ();
                    ]
                  ();
              ]
            ();
        ]
      ()
  in
  let encoder = Pbrt.Encoder.create () in
  Proto.Metrics_service.encode_pb_export_metrics_service_request request encoder;
  Pbrt.Encoder.to_string encoder

let exported (otlp : Otlp.t) path headers body =
  let reply = Call.call ~headers ~body otlp.endpoint "POST" path in
  check (Printf.sprintf "%s is answered 200" path) (reply.status = 200)

let reads_otlp () =
  let otlp = Otlp.start () in
  Fun.protect ~finally:(fun () -> Otlp.close otlp) @@ fun () ->
  let protobuf = ("content-type", "application/x-protobuf") in
  exported otlp "/v1/traces"
    [ protobuf; ("content-encoding", "gzip") ]
    (gzip (traces ()));
  exported otlp "/v1/metrics" [ protobuf ] (metrics ());
  exported otlp "/v1/traces"
    [ ("content-type", "application/json") ]
    {|{"resourceSpans":[{"resource":{"attributes":[{"key":"service.name","value":{"stringValue":"standin"}}]},
      "scopeSpans":[{"spans":[{"name":"gather.resource","kind":1,"traceId":"00","attributes":[{"key":"n","value":{"intValue":"7"}}]}]}]}]}|};
  check
    ("nothing is unreadable: "
    ^ String.concat "; " (Dynarray.to_list otlp.unreadable))
    (Dynarray.length otlp.unreadable = 0);
  match Dynarray.to_list otlp.spans with
  | [ gzipped; json ] -> (
      check "the gzipped protobuf span's name"
        (gzipped.name = "bus.emit:gather:complete");
      check "its kind is a producer's" (gzipped.kind = 4);
      check "its resource"
        (gzipped.resource = [ ("service.name", Otlp.Text "standin") ]);
      check "its attributes, each as it was carried"
        (gzipped.attributes
        = [
            ("bus.channel", Otlp.Text "gather:complete");
            ("retry", Otlp.Flag false);
          ]);
      check "its trace id, as hex"
        (gzipped.trace_id = Ohex.encode "0123456789abcdef");
      check "the JSON span's int64, carried as a string"
        (json.attributes = [ ("n", Otlp.Number 7.0) ]);
      match Otlp.metric otlp "xynolog.inference.calls" with
      | None -> failwith "the metric did not arrive"
      | Some metric ->
          check "a monotonic sum is a counter"
            (Dynarray.to_list metric.instruments = [ "counter" ]);
          check "its points' values"
            (Dynarray.to_list metric.values = [ 2.0; 1.0 ]);
          check "every outcome it carried"
            (List.map
               (fun (k, vs) -> (k, Dynarray.to_list vs))
               (Dynarray.to_list metric.attributes)
            = [ ("inference.outcome", [ Otlp.Text "answered"; Text "failed" ]) ]
            ))
  | spans ->
      failwith (Printf.sprintf "%d spans arrived, not 2" (List.length spans))

(* ── One export, in either encoding ─────────────────────────────────── *)

(* The enums of an export, by OTLP's number for each value. *)
let enum_numbers =
  [
    ( "kind",
      [
        "SPAN_KIND_UNSPECIFIED";
        "SPAN_KIND_INTERNAL";
        "SPAN_KIND_SERVER";
        "SPAN_KIND_CLIENT";
        "SPAN_KIND_PRODUCER";
        "SPAN_KIND_CONSUMER";
      ] );
    ( "aggregationTemporality",
      [
        "AGGREGATION_TEMPORALITY_UNSPECIFIED";
        "AGGREGATION_TEMPORALITY_DELTA";
        "AGGREGATION_TEMPORALITY_CUMULATIVE";
      ] );
  ]

let ids = [ "traceId"; "spanId"; "parentSpanId" ]

let index_of value values =
  let rec at i = function
    | [] -> failwith ("OTLP has no enum value " ^ value)
    | v :: _ when v = value -> i
    | _ :: rest -> at (i + 1) rest
  in
  at 0 values

(* The generated encoder's JSON, as OTLP/JSON writes it. *)
let rec as_otlp_json (json : Yojson.Basic.t) : Yojson.Safe.t =
  match json with
  | `Assoc members ->
      `Assoc
        (List.map
           (fun (key, value) ->
             match (List.assoc_opt key enum_numbers, value) with
             | Some values, `String name -> (key, `Int (index_of name values))
             | _ -> (
                 match value with
                 | `String encoded when List.mem key ids ->
                     (key, `String (Ohex.encode (Base64.decode_exn encoded)))
                 | value -> (key, as_otlp_json value)))
           members)
  | `List items -> `List (List.map as_otlp_json items)
  | (`Null | `Bool _ | `Int _ | `Float _ | `String _) as scalar ->
      (scalar :> Yojson.Safe.t)

let string value = Proto.Common.String_value value

let resource_of attributes =
  Proto.Resource.make_resource
    ~attributes:(List.map (fun (key, value) -> attribute key value) attributes)
    ()

(* A service's resource, naming a knowledge base whose domain holds %, ,
   and =, which a DID passes through intact; and another, naming none. *)
let service_attributes =
  [
    ("service.name", "otlp-receiver");
    ("service.version", "0.0.0");
    ("xynolog.kb", "did:web:example.org%3A8443:kb,a=b");
  ]

let another_attributes =
  [ ("service.name", "otlp-receiver-too"); ("service.version", "0.0.0") ]

let resource attributes =
  resource_of (List.map (fun (key, value) -> (key, string value)) attributes)

let received attributes =
  List.sort compare
    (List.map (fun (key, value) -> (key, Otlp.Text value)) attributes)

let spans_request resource spans =
  Proto.Trace_service.make_export_trace_service_request
    ~resource_spans:
      [
        Proto.Trace.make_resource_spans ~resource
          ~scope_spans:[ Proto.Trace.make_scope_spans ~spans () ]
          ();
      ]
    ()

let metrics_request resource metrics =
  Proto.Metrics_service.make_export_metrics_service_request
    ~resource_metrics:
      [
        Proto.Metrics.make_resource_metrics ~resource
          ~scope_metrics:[ Proto.Metrics.make_scope_metrics ~metrics () ]
          ();
      ]
    ()

(* A server span and the client span it is the parent of. *)
let two_spans () =
  let trace_id = Bytes.of_string "0123456789abcdef" in
  let parent = Bytes.of_string "parent01" in
  [
    Proto.Trace.make_span ~trace_id ~span_id:parent
      ~name:"bus.dispatch:beckon:focus" ~kind:Span_kind_server
      ~attributes:
        [
          attribute "bus.channel" (string "beckon:focus");
          attribute "count" (Int_value 7L);
          attribute "ratio" (Double_value 0.5);
          attribute "flag" (Bool_value true);
        ]
      ();
    Proto.Trace.make_span ~trace_id
      ~span_id:(Bytes.of_string "child001")
      ~parent_span_id:parent ~name:"archivist.content.get"
      ~kind:Span_kind_client
      ~attributes:[ attribute "peer.service" (string "archivist") ]
      ();
  ]

let point ?(attributes = []) value =
  Proto.Metrics.make_number_data_point ~value
    ~attributes:(List.map (fun (key, value) -> attribute key value) attributes)
    ()

(* One metric of each instrument: a counter, an up-down counter, a gauge
   and a histogram. *)
let four_metrics () =
  [
    Proto.Metrics.make_metric ~name:"xynolog.bus.emit"
      ~data:
        (Sum
           (Proto.Metrics.make_sum ~is_monotonic:true
              ~aggregation_temporality:Aggregation_temporality_cumulative
              ~data_points:
                [
                  point
                    ~attributes:[ ("bus.channel", string "beckon:focus") ]
                    (As_int 2L);
                ]
              ()))
      ();
    Proto.Metrics.make_metric ~name:"xynolog.sse.subscribers"
      ~data:
        (Sum
           (Proto.Metrics.make_sum ~is_monotonic:false
              ~aggregation_temporality:Aggregation_temporality_cumulative
              ~data_points:[ point (As_int (-1L)) ]
              ()))
      ();
    Proto.Metrics.make_metric ~name:"xynolog.runtime.heap"
      ~data:
        (Gauge
           (Proto.Metrics.make_gauge
              ~data_points:
                [
                  point
                    ~attributes:[ ("heap.stat", string "used") ]
                    (As_double 3.5);
                  point ~attributes:[ ("heap.stat", string "rss") ] (As_int 9L);
                ]
              ()))
      ();
    Proto.Metrics.make_metric ~name:"xynolog.inference.duration"
      ~data:
        (Histogram
           (Proto.Metrics.make_histogram
              ~aggregation_temporality:Aggregation_temporality_cumulative
              ~data_points:
                [
                  Proto.Metrics.make_histogram_data_point
                    ~attributes:
                      [ attribute "inference.outcome" (string "success") ]
                    ~count:1L ~sum:4.0 ~bucket_counts:[ 0L; 1L ]
                    ~explicit_bounds:[ 1.0 ] ();
                ]
              ()))
      ();
  ]

let protobuf_of encode request =
  let encoder = Pbrt.Encoder.create () in
  encode request encoder;
  Pbrt.Encoder.to_string encoder

(* Each encoding of an export: its content type, and its traces' and
   metrics' bodies. *)
let encodings =
  [
    ( "application/json",
      (fun request ->
        Yojson.Safe.to_string
          (as_otlp_json
             (Proto.Trace_service.encode_json_export_trace_service_request
                request))),
      fun request ->
        Yojson.Safe.to_string
          (as_otlp_json
             (Proto.Metrics_service.encode_json_export_metrics_service_request
                request)) );
    ( "application/x-protobuf",
      protobuf_of Proto.Trace_service.encode_pb_export_trace_service_request,
      protobuf_of Proto.Metrics_service.encode_pb_export_metrics_service_request
    );
  ]

let sent (otlp : Otlp.t) path content_type body =
  exported otlp path [ ("content-type", content_type) ] body

let with_receiver f =
  let otlp = Otlp.start () in
  Fun.protect ~finally:(fun () -> Otlp.close otlp) @@ fun () ->
  f otlp;
  check
    ("nothing is unreadable: "
    ^ String.concat "; " (Dynarray.to_list otlp.unreadable))
    (Dynarray.length otlp.unreadable = 0)

(* What a receiver got, as values a case can compare. *)
let spans_of (otlp : Otlp.t) = Dynarray.to_list otlp.spans

let metrics_of (otlp : Otlp.t) =
  List.map
    (fun (name, (metric : Otlp.metric)) ->
      ( name,
        ( Dynarray.to_list metric.instruments,
          List.map
            (fun (key, values) -> (key, Dynarray.to_list values))
            (Dynarray.to_list metric.attributes),
          Dynarray.to_list metric.values,
          Dynarray.to_list metric.resources ) ))
    (Dynarray.to_list otlp.metrics)

let read_alike () =
  let traces = spans_request (resource service_attributes) (two_spans ()) in
  let metrics =
    metrics_request (resource service_attributes) (four_metrics ())
  in
  match
    List.map
      (fun (content_type, traces_body, metrics_body) ->
        let got = ref ([], []) in
        with_receiver (fun otlp ->
            sent otlp "/v1/traces" content_type (traces_body traces);
            sent otlp "/v1/metrics" content_type (metrics_body metrics);
            got := (spans_of otlp, metrics_of otlp));
        !got)
      encodings
  with
  | [ (json_spans, json_metrics); (protobuf_spans, protobuf_metrics) ] ->
      check "the spans arrive by name"
        (List.sort compare (List.map (fun (s : Otlp.span) -> s.name) json_spans)
        = [ "archivist.content.get"; "bus.dispatch:beckon:focus" ]);
      check "the child names its parent"
        (List.exists
           (fun (s : Otlp.span) ->
             s.parent_span_id = Some (Ohex.encode "parent01"))
           json_spans);
      check "the protobuf spans are read as the JSON spans are"
        (protobuf_spans = json_spans);
      check "the metrics arrive by name"
        (List.sort compare (List.map fst json_metrics)
        = [
            "xynolog.bus.emit";
            "xynolog.inference.duration";
            "xynolog.runtime.heap";
            "xynolog.sse.subscribers";
          ]);
      check "the protobuf metrics are read as the JSON metrics are"
        (protobuf_metrics = json_metrics)
  | _ -> failwith "an export was read in other than two encodings"

let under_their_resources () =
  let traces = spans_request (resource service_attributes) (two_spans ()) in
  let one = metrics_request (resource service_attributes) (four_metrics ()) in
  let another =
    metrics_request (resource another_attributes) (four_metrics ())
  in
  List.iter
    (fun (content_type, traces_body, metrics_body) ->
      with_receiver (fun otlp ->
          sent otlp "/v1/traces" content_type (traces_body traces);
          sent otlp "/v1/metrics" content_type (metrics_body one);
          sent otlp "/v1/metrics" content_type (metrics_body one);
          sent otlp "/v1/metrics" content_type (metrics_body another);
          let spans = spans_of otlp in
          check (content_type ^ ": two spans") (List.length spans = 2);
          List.iter
            (fun (s : Otlp.span) ->
              check
                (Printf.sprintf "%s: %s under its resource" content_type s.name)
                (s.resource = received service_attributes))
            spans;
          let metrics = metrics_of otlp in
          check (content_type ^ ": four metrics") (List.length metrics = 4);
          List.iter
            (fun (name, (_, _, _, resources)) ->
              check
                (Printf.sprintf "%s: %s under each resource, once" content_type
                   name)
                (resources
                = [ received service_attributes; received another_attributes ]))
            metrics))
    encodings

let keeps_types () =
  let traces =
    spans_request
      (resource service_attributes)
      [
        Proto.Trace.make_span
          ~trace_id:(Bytes.of_string "0123456789abcdef")
          ~span_id:(Bytes.of_string "01234567")
          ~name:"content.get" ~kind:Span_kind_client
          ~attributes:[ attribute "content.stream" (Bool_value true) ]
          ();
      ]
  in
  let metrics =
    metrics_request
      (resource service_attributes)
      [
        Proto.Metrics.make_metric ~name:"xynolog.detection.calls"
          ~data:
            (Sum
               (Proto.Metrics.make_sum ~is_monotonic:true
                  ~data_points:
                    [
                      point
                        ~attributes:[ ("detection.reroll", Bool_value true) ]
                        (As_int 1L);
                      point
                        ~attributes:[ ("detection.reroll", string "true") ]
                        (As_int 1L);
                    ]
                  ()))
          ();
      ]
  in
  List.iter
    (fun (content_type, traces_body, metrics_body) ->
      with_receiver (fun otlp ->
          sent otlp "/v1/traces" content_type (traces_body traces);
          sent otlp "/v1/metrics" content_type (metrics_body metrics);
          check
            (content_type ^ ": the span's boolean is a boolean")
            (List.map (fun (s : Otlp.span) -> s.attributes) (spans_of otlp)
            = [ [ ("content.stream", Otlp.Flag true) ] ]);
          check
            (content_type ^ ": the metric's boolean is not its string")
            (List.assoc_opt "xynolog.detection.calls" (metrics_of otlp)
            |> Option.map (fun (_, attributes, _, _) -> attributes)
            = Some [ ("detection.reroll", [ Otlp.Flag true; Text "true" ]) ])))
    encodings

let tests =
  List.map
    (fun (name, case) -> Testo.create ~category:[ "otlp" ] name case)
    [
      ("reads an export as protobuf, gzipped or not, and as JSON", reads_otlp);
      ( "reads a protobuf export as it reads the same export sent as JSON",
        read_alike );
      ( "reports each span and metric with the attributes of the resource it \
         was exported under, and a metric under each resource once",
        under_their_resources );
      ( "keeps each attribute value of the type it was carried as, so a \
         boolean is not its string",
        keeps_types );
    ]
