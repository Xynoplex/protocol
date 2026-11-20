(* The worker suite (conformance/worker/README.md): every SDK that has a
   worker's surface, through its worker driver, put through every case of
   the worker corpus against a real gateway on each signal plane, with the
   suite playing the dispatcher. The case format and its runner are the SDK
   suite's (harness/sdk_case.ml).

   The gateway exports its telemetry, to a receiver nothing reads: a gateway
   carries a trace from the frame it is sent to the frame it delivers only
   when it exports, and a case holds a worker to the trace a reply brings
   it. *)

open Xynolog_conformance

type shared = { world : World.t; otlp : Otlp.t }

let make (implementations : Implementations.t) plane =
  let otlp = Otlp.start () in
  {
    world =
      World.create ~command:implementations.gateway
        ~env:[ ("OTEL_EXPORTER_OTLP_ENDPOINT", Some otlp.endpoint) ]
        plane;
    otlp;
  }

let close { world; otlp } =
  World.close world;
  Otlp.close otlp

let tests (implementations : Implementations.t) =
  let corpus = Sdk_case.corpus "conformance/worker/cases" in
  List.concat_map
    (fun (sdk, (drivers : Implementations.sdk)) ->
      match drivers.worker with
      | None -> []
      | Some command ->
          Sdk_case.tests ~make:(make implementations)
            ~world:(fun shared -> shared.world)
            ~close
            (Printf.sprintf "a worker written on the %s SDK" sdk)
            command corpus Worker)
    implementations.sdks

let suite : Suite.t = { name = "worker"; workers = 4; tests }
