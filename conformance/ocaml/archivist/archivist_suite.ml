(* The archivist suite: one module for each file of cases, each its own
   worlds. *)

let suite : Xynolog_conformance.Suite.t =
  {
    name = "archivist";
    workers = 4;
    tests =
      (fun implementations ->
        List.concat_map (fun tests -> tests implementations) [ Boot.tests ]);
  }
