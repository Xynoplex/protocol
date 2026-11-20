(* telemetry.test.ts: the telemetry a Smelter exports (SMELTER.md
   § Telemetry): everything specs/src/service-telemetry/telemetry.json lists
   for it, and the rows of specs/src/sdk-telemetry/telemetry.json it names
   there, exported over OTLP to the endpoint the standard OpenTelemetry
   environment names; and nothing else. And what its gauge of the index
   says: the number of points the index holds. *)

open Xynolog_conformance
open Support

(* One case, and judged before the Smelter is stopped: everything it
   exported for the traffic below, held to the table in both directions. *)
let exports_exactly_the_table otlp (w : W.t) =
  (* The world's Smelter was started with no endpoint, and exports nothing.
     This one is told where to export, and how often. *)
  stopped w;
  W.start w
    ~launch:
      {
        w.base with
        env =
          w.base.env
          @ [
              ("OTEL_EXPORTER_OTLP_ENDPOINT", otlp.Otlp.endpoint);
              ("OTEL_METRIC_EXPORT_INTERVAL", "500");
              ("OTEL_BSP_SCHEDULE_DELAY", "100");
            ];
      };
  (* Its traffic: what it asks of the record as it starts, an event it is
     delivered and applies, and the decision it then signals. *)
  let id = W.created w "A Smelter says what it has done.\n" plain in
  ignore (W.settled w id : Yojson.Safe.t);
  let signalled =
    Protocol.span_name "bus.emit:{channel}" [ ("channel", "smelt:settled") ]
  in
  let span =
    Run.eventually "the span of a decision it signalled" 15.0 (fun () ->
        Dynarray.find_opt (fun (s : Otlp.span) -> s.name = signalled) otlp.spans)
  in
  Check.that "the span is exported under the Smelter's name"
    (List.assoc_opt "service.name" span.resource
    = Some (Otlp.Text "xynolog-smelter"));
  (match Telemetry.outside_the_table otlp Smelter (Some w.kb.world.kb.did) with
  | [] -> ()
  | unlisted ->
      failwith (String.concat "\n" (unlisted @ [ Telemetry.received otlp ])));
  (* The index holds the resource's points now. A reading is up to an export
     interval old, so the gauge is waited for. *)
  let size = Protocol.metric_name "xynolog.vector.index.size" in
  let held =
    Run.eventually "the gauge to say how many points the index holds" 15.0
      (fun () ->
        let points =
          Qdrant.count w.index "resources" + Qdrant.count w.index "annotations"
        in
        match Otlp.metric otlp size with
        | Some seen
          when Dynarray.length seen.values > 0
               && Dynarray.get_last seen.values = float_of_int points ->
            Some points
        | _ -> None)
  in
  Check.that "the index holds points" (held > 0)

let tests implementations =
  let otlp = lazy (Otlp.start ()) in
  in_world implementations "telemetry"
    ~category:[ "what a Smelter exports" ]
    [
      Case.case
        "exports, under the Smelter's name, exactly the telemetry the spec \
         lists: every row it can, and nothing else" (fun w ->
          exports_exactly_the_table (Lazy.force otlp) w);
    ]
