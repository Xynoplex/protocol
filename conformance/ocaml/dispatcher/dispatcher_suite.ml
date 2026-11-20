(* The dispatcher suite: one module for each file of cases, each its own
   worlds. *)

let suite : Xynolog_conformance.Suite.t =
  {
    name = "dispatcher";
    workers = 4;
    tests =
      (fun implementations ->
        List.concat_map
          (fun tests -> tests implementations)
          [ Lifecycle.tests; Boot.tests; Clocks.tests ]);
  }
