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
            Boot.tests;
            Highlighting.tests;
            Prompts.tests;
            Knowledge_bases.tests;
            Cancel.tests;
            On_anthropic.tests;
            Stop.tests;
            Yield.tests;
            Environment.tests;
            On_pdf.tests;
            Exported.tests;
            Linking.tests;
            Anchoring.tests;
            Tagging.tests;
            Resume.tests;
            Halved.tests;
            Chunks.tests;
            Code_points.tests;
            Started.tests;
            Failures.tests;
            Limits.tests;
            Declines.tests;
            Commenting.tests;
            Assessing.tests;
            Output.tests;
          ]);
  }
