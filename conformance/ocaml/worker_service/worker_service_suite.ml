(* The worker-service suite: one module for each file of cases, each a
   world of its own. A world's ports are the system's to pick, so files run
   side by side. *)

let suite : Xynolog_conformance.Suite.t =
  {
    name = "worker-service";
    workers = 4;
    tests =
      (fun implementations ->
        List.concat_map
          (fun tests -> tests implementations)
          [
            Boot.tests; Highlighting.tests; Prompts.tests; Knowledge_bases.tests;
          ]);
  }
