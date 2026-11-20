(* tagging.test.ts: a mark job of motivation tagging, end to end
   (WORKER-SERVICE.md § Detection, § The five kinds of mark job): one unit
   for each category of the schema the dispatcher handed over with the job,
   each asked of the whole text. *)

open Xynolog_conformance
open Support
open Case

let model = (List.hd W.ollama_engines).model

(* The schema the job's schemaId names, as the dispatcher adds it to the job
   it hands over: the one worker/prompt-cases.json's tagging cases are
   asked with. *)
let schema () = Check.at (params "tagging-claim") [ "schema" ]

(* A tagging job of both the schema's categories, about [about]. *)
let tagging_job ?about (w : W.t) name =
  let schema = schema () in
  mark_job ?about w.own name
    (`Assoc
       [
         ("motivation", str "tagging");
         ("schemaId", Check.at schema [ "id" ]);
         ("categories", strs [ "Claim"; "Evidence" ]);
         ("schema", schema);
       ])

(* Where a tagging job stands as it begins [category], with [processed] of
   its two categories behind it. *)
let at category processed completed =
  [
    ("current", `Assoc [ ("kind", str "category"); ("value", str category) ]);
    ("processed", `Int processed);
    ("total", `Int 2);
    ("completedItems", `List completed);
  ]

let found_in category count =
  `Assoc [ ("value", str category); ("foundCount", `Int count) ]

let analyzing ?extra job percentage =
  report ?extra job percentage (code "analyzing-tags")

let creating job count =
  report job 60
    (code ~members:[ ("count", `Int count) ] "creating-tag-annotations")

let complete job count =
  report job 100
    (code
       ~members:[ ("count", `Int count); ("motivation", str "tagging") ]
       "complete-created")

let each_category (w : W.t) =
  let job = tagging_job w "tagging" in
  let resource_id = job.resource_id in
  Ollama.script w.ollama
    [
      answer
        (Yojson.Safe.to_string
           (`List [ `Assoc [ ("exact", str "London was wrong.") ] ]));
      answer "[]";
    ];
  let served = W.start w in
  let completion = settled served job in
  expect_generations w.ollama
    [ generation model "tagging-claim"; generation model "tagging-evidence" ];
  (* A category with nothing to commit commits nothing, and still says where
     it got to. *)
  expect_sequence served
    [
      "emit job:claim";
      "emit job:start";
      "emit browse:resource-requested";
      "GET /resources/" ^ resource_id;
      "emit mark:commit";
      "emit job:checkpoint";
      "emit job:checkpoint";
      "emit job:complete";
      "emit job:claim";
    ];
  Check.equal "what it committed"
    (`List
       [
         `Assoc
           [
             ("resourceId", str resource_id);
             ("jobId", str job.id);
             ( "annotations",
               `List
                 [
                   text_annotation (W.generator w.own) resource_id "tagging"
                     "htF9JvtYLflP_yBTSknGT" ~start:373 ~stop:390
                     ~prefix:
                       "The program was na\u{00ef}ve by later standards, and \
                        London ignored it. "
                     ~suffix:"\n"
                     ~body:
                       (* Two bodies: the category, and the schema it is a
                          category of. *)
                       (`List
                          [
                            `Assoc
                              [
                                ("type", str "TextualBody");
                                ("value", str "Claim");
                                ("purpose", str "tagging");
                                ("format", str "text/plain");
                                ("language", str "en");
                              ];
                            `Assoc
                              [
                                ("type", str "TextualBody");
                                ("value", str "argument");
                                ("purpose", str "classifying");
                                ("format", str "text/plain");
                              ];
                          ])
                     "London was wrong.";
                 ] );
           ];
       ])
    (committed served);
  (* A tagging job's units are its categories: each has a cursor of its own,
     and none is ever said to be finished. *)
  let next = code_points text in
  let claim =
    ("Claim", cursor ~next ~size:2621 ~found:1 ~emitted:1 ~errors:0)
  in
  Check.equal "its checkpoints"
    (`List
       [
         checkpoint job [ claim ];
         checkpoint job
           [
             claim;
             ("Evidence", cursor ~next ~size:2618 ~found:0 ~emitted:0 ~errors:0);
           ];
       ])
    (checkpoints served);
  expect_progress served job
    [
      report job 10 (code "loading");
      analyzing job 30;
      analyzing job 30 ~extra:(at "Claim" 0 []);
      creating job 1;
      analyzing job 45 ~extra:(at "Evidence" 1 [ found_in "Claim" 1 ]);
      (* Said after each category's answer, with what the job has made so
         far. *)
      creating job 1;
      complete job 1;
    ];
  Check.equal "its completion"
    (identified job
       [
         ( "result",
           `Assoc
             [
               ("found", `Int 1);
               ("persisted", `Int 1);
               ("byCategory", `Assoc [ ("Claim", `Int 1) ]);
             ] );
         ("durability", str "acknowledged");
       ])
    completion

let at_the_boundary (w : W.t) =
  w.ollama.show <- Ollama.Context middling_context_length;
  let job = tagging_job ~about:long_text w "tagging-pieces" in
  let resource_id = job.resource_id in
  (* The text is two pieces for each category. No answer names a marker of
     the overlap. *)
  let found marker =
    answer
      (Yojson.Safe.to_string
         (`List
            [ `Assoc [ ("exact", str (Printf.sprintf "marker %d" marker)) ] ]))
  in
  Ollama.script w.ollama [ found 103; found 702; found 205; found 801 ];
  let served = W.start w in
  let completion = settled served job in
  Check.equal "the generations its model was asked for" (`Int 4)
    (`Int (Dynarray.length w.ollama.generations));
  expect_sequence served
    [
      "emit job:claim";
      "emit job:start";
      "emit browse:resource-requested";
      "GET /resources/" ^ resource_id;
      "emit mark:commit";
      "emit job:checkpoint";
      "emit mark:commit";
      "emit job:checkpoint";
      "emit mark:commit";
      "emit job:checkpoint";
      "emit mark:commit";
      "emit job:checkpoint";
      "emit job:complete";
      "emit job:claim";
    ];
  Check.equal "the sizes of its batches"
    (`List [ `Int 1; `Int 1; `Int 1; `Int 1 ])
    (`List
       (List.map
          (fun (c : W.commit) -> `Int (List.length c.annotations))
          (Dynarray.to_list w.own.commits)));
  (* For each category the first piece ends where the fifth paragraph does,
     and the second starts 256 code points before that. *)
  let boundary = 2147 and length = code_points long_text in
  let claim next found =
    ("Claim", cursor ~next ~size:690 ~found ~emitted:found ~errors:0)
  and evidence next found =
    ("Evidence", cursor ~next ~size:687 ~found ~emitted:found ~errors:0)
  in
  Check.equal "its checkpoints"
    (`List
       [
         checkpoint job [ claim boundary 1 ];
         checkpoint job [ claim length 2 ];
         checkpoint job [ claim length 2; evidence boundary 1 ];
         checkpoint job [ claim length 2; evidence length 2 ];
       ])
    (checkpoints served);
  let claimed = [ found_in "Claim" 2 ] in
  (* The 30 points from 30 to 60 are shared among the categories, and a
     boundary stands in its category's share where the cursor stands in the
     text: 2147 of 3847 code points, 0.558 of a category. Of two categories
     that is 8.4 points for the first, and 23.4 for the second, which has the
     whole of the first before it. *)
  expect_progress served job
    [
      report job 10 (code "loading");
      analyzing job 30;
      (* Claim: begun, its first piece, the boundary, its second piece. The
         second is the text's last, so no boundary follows it. *)
      analyzing job 30 ~extra:(at "Claim" 0 []);
      creating job 1;
      analyzing job 38 ~extra:(at "Claim" 0 []);
      creating job 2;
      (* Evidence: the same four. *)
      analyzing job 45 ~extra:(at "Evidence" 1 claimed);
      creating job 3;
      analyzing job 53 ~extra:(at "Evidence" 1 claimed);
      creating job 4;
      complete job 4;
    ];
  Check.equal "its completion"
    (identified job
       [
         ( "result",
           `Assoc
             [
               ("found", `Int 4);
               ("persisted", `Int 4);
               ("byCategory", `Assoc [ ("Claim", `Int 2); ("Evidence", `Int 2) ]);
             ] );
         ("durability", str "acknowledged");
       ])
    completion

let tests implementations =
  in_world implementations "tagging" ~category:[ "a tagging job" ]
    [
      case
        "asks its model for each category in turn, and commits each tag with \
         its category and the schema it is of"
        each_category;
      case
        "says how far through a category it is at the boundary between two \
         pieces of the text"
        at_the_boundary;
    ]
