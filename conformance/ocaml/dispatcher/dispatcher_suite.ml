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
          [
            Announce.tests;
            Boot.tests;
            Cancel.tests;
            Claim.tests;
            Clocks.tests;
            Conclude.tests;
            Create.tests;
            Environment.tests;
            Lifecycle.tests;
            Observability.tests;
            Progress.tests;
            Restart.tests;
            Status.tests;
          ]);
  }
