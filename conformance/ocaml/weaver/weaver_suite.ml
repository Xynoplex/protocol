(* The weaver suite: one module for each file of cases, each a world of its
   own. *)

(* One world at a time: the files share one graph database, which a rebuild
   of the whole graph clears, and a Weaver's health port is its
   implementation's. A file's cases run in their order, in one process:
   some are judged on what the cases before them recorded. *)
let suite : Xynolog_conformance.Suite.t =
  {
    name = "weaver";
    workers = 1;
    tests =
      (fun implementations ->
        List.concat_map
          (fun tests -> tests implementations)
          [ Boot.tests; Record.tests; Repair.tests; Telemetry.tests ]);
  }
