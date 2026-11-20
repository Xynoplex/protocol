(* What the harness reads of Protocol and holds values to: the reader's
   Validate, over the schemas of specs/src, and a telemetry row's values,
   each of the type the table lists it as. *)

open Xynolog_conformance
open Expect

let address value =
  Protocol.refusals
    (Protocol.component Gateway "ServiceAddress")
    (Yojson.Safe.from_string value)

let accepts () =
  check "a host and a port is a ServiceAddress"
    (address {|{"host":"localhost","port":8080}|} = [])

let refuses_each () =
  let refusals = address {|{"host":"","port":70000,"colour":"red"}|} in
  check
    ("each refusal, at the pointer into the value: "
    ^ String.concat "; " refusals)
    (refusals
    = [
        "/host is shorter than 1 code points";
        "/port is more than 65535";
        "/colour is a member the schema does not state";
      ])

let code_points () =
  let refusals = address {|{"host":"😀","port":5000000000}|} in
  check
    ("one code point is long enough, and a port past 2^32 is an integer: "
    ^ String.concat "; " refusals)
    (refusals = [ "/port is more than 65535" ])

let names_none () =
  fails "a schema the document does not name" "no component schema NoSuchSchema"
    (fun () -> Protocol.component Gateway "NoSuchSchema")

(* A row's values, each as it was carried, so the boolean true is listed
   and the string "true" is not. *)
let content_stream () =
  let spans, _ = Protocol.sdk_telemetry () in
  let row =
    match
      List.find_opt
        (fun (r : Protocol.span_row) -> r.name = "content.get")
        spans
    with
    | Some row -> row
    | None -> failwith "the SDK table lists no content.get span"
  in
  let outside values =
    Telemetry.outside_the_row "content.get" row.span_attributes
      [ ("content.stream", values) ]
  in
  check "the boolean true is the row's" (outside [ Otlp.Flag true ] = []);
  check "the string \"true\" is not"
    (outside [ Otlp.Text "true" ]
    = [ {|content.get carries content.stream="true", outside true|} ])

let detection_reroll () =
  let _, metrics = Protocol.telemetry Worker in
  let row =
    match
      List.find_opt
        (fun (r : Protocol.metric_row) ->
          String.ends_with ~suffix:"xynolog.detection.calls" r.metric)
        metrics
    with
    | Some row -> row
    | None ->
        failwith
          "the service table lists no xynolog.detection.calls metric for the \
           worker"
  in
  let outside values =
    Telemetry.outside_the_row row.metric row.metric_attributes
      [ ("detection.reroll", values) ]
  in
  check "both booleans are the row's"
    (outside [ Otlp.Flag true; Flag false ] = []);
  check "the string \"false\" is not"
    (outside [ Otlp.Text "false" ]
    = [
        row.metric ^ {| carries detection.reroll="false", outside true, false|};
      ])

let tests =
  List.map
    (fun (category, name, case) ->
      Testo.create ~category:[ category ] name case)
    [
      ("validate", "accepts a value its schema holds", accepts);
      ( "validate",
        "refuses each thing its schema refuses, at the pointer into the value",
        refuses_each );
      ( "validate",
        "counts a string in code points, and reads a large integer as an \
         integer",
        code_points );
      ("validate", "fails for a schema the document does not name", names_none);
      ( "telemetry",
        "holds a value to the type its row lists it as: content.get's \
         content.stream is the boolean true",
        content_stream );
      ( "telemetry",
        "holds a metric's value the same way: the worker's detection.reroll is \
         a boolean",
        detection_reroll );
    ]
