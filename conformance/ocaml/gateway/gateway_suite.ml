(* The gateway suite: one module for each file of cases, each its own
   worlds. *)

let suite : Xynolog_conformance.Suite.t =
  {
    name = "gateway";
    workers = 4;
    tests =
      (fun implementations ->
        List.concat_map
          (fun tests -> tests implementations)
          [
            Broker.tests;
            Issuer.tests;
            Outage.tests;
            Capacity.tests;
            Content.tests;
            Stream.tests;
            Boot.tests;
            Bounds.tests;
            Replicas.tests;
          ]);
  }
