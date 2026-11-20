(* The smelter suite: one module for each file of cases, each a world of its
   own. *)

(* One world at a time: the files share one vector database, cleared when a
   file's world is made, and a Smelter's health port is a constant. A
   file's cases run in their order, in one process: some are judged on what
   the cases before them recorded. *)
let suite : Xynolog_conformance.Suite.t =
  {
    name = "smelter";
    workers = 1;
    tests =
      (fun implementations ->
        List.concat_map
          (fun tests -> tests implementations)
          [ Boot.tests; Index.tests ]);
  }
