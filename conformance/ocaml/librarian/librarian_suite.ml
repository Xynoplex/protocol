(* The librarian suite: boot, barriers, search, gather and telemetry. Each
   file is a world of its own. *)

(* One world at a time: the files share one graph database and one vector
   database, each cleared when a file's world is made, and the health ports
   of a Weaver and a Smelter are constants. *)
let suite : Xynolog_conformance.Suite.t =
  {
    name = "librarian";
    workers = 1;
    tests =
      (fun implementations ->
        List.concat_map
          (fun tests -> tests implementations)
          [
            Boot.tests;
            Barriers.tests;
            Search.tests;
            Gather.tests;
            Telemetry.tests;
          ]);
  }
