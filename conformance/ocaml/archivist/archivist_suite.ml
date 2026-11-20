(* The archivist suite: one module for each file of cases, each its own
   worlds. *)

let suite : Xynolog_conformance.Suite.t =
  {
    name = "archivist";
    workers = 4;
    tests =
      (fun implementations ->
        List.concat_map
          (fun tests -> tests implementations)
          [
            Anchored_text.tests;
            Annotations.tests;
            Boot.tests;
            Browse.tests;
            Clone.tests;
            Environment.tests;
            Events.tests;
            Fixture.tests;
            Idempotency.tests;
            Jobs.tests;
            Observability.tests;
            Record.tests;
            Resources.tests;
            Staging.tests;
          ]);
  }
