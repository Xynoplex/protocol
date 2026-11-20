(* telemetry.test.ts: the telemetry a Weaver exports (WEAVER.md
   § Telemetry): everything specs/src/service-telemetry/telemetry.json lists
   for it, and the rows of specs/src/sdk-telemetry/telemetry.json it names
   there, exported over OTLP to the endpoint the standard OpenTelemetry
   environment names; and nothing else. *)

open Xynolog_conformance
open Support

(* One case, and judged before the Weaver is stopped: everything it exported
   for the traffic below, held to the table in both directions. *)
let exports_exactly_the_table otlp (w : W.t) =
  (* The world's Weaver was started with no endpoint, and exports nothing.
     This one is told where to export, and how often. *)
  ignore (W.stop w : int option);
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
  (* Its traffic: what it asks of the record as it starts, two events it is
     delivered and applies, and the marks it then signals. *)
  let id = uploaded w "Told of" in
  let grace = W.person w "grace" in
  ignore (highlighted grace id "quick" : string);
  W.caught_up w [ id ];
  let signalled =
    Protocol.span_name "bus.emit:{channel}" [ ("channel", "weave:applied") ]
  in
  let span =
    Run.eventually "the span of a mark it signalled" 15.0 (fun () ->
        Dynarray.find_opt (fun (s : Otlp.span) -> s.name = signalled) otlp.spans)
  in
  Check.that "the span is exported under the Weaver's name"
    (List.assoc_opt "service.name" span.resource
    = Some (Otlp.Text (Protocol.default_service_name Weaver)));
  match Telemetry.outside_the_table otlp Weaver (Some w.kb.world.kb.did) with
  | [] -> ()
  | unlisted ->
      failwith (String.concat "\n" (unlisted @ [ Telemetry.received otlp ]))

let tests implementations =
  let otlp = lazy (Otlp.start ()) in
  in_world implementations "telemetry"
    ~category:[ "what a Weaver exports" ]
    [
      Case.case
        "exports, under the Weaver's name, exactly the telemetry the spec \
         lists: every row it can, and nothing else" (fun w ->
          exports_exactly_the_table (Lazy.force otlp) w);
    ]
