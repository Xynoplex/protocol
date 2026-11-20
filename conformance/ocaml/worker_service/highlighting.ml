(* highlighting.test.ts: a mark job of motivation highlighting, end to end
   (WORKER-SERVICE.md § What a job reads, § Detection, § The five kinds of
   mark job): what it reads, the one request it makes of its model, the
   annotations it commits, what it reports as it goes, and its
   completion. *)

open Xynolog_conformance
open Support
open Case

(* The size, in tokens, the job cuts its text at: what its unit's cursor
   states. *)
let chunk_size = 2641
let model = (List.hd W.ollama_engines).model

let checkpoint (job : W.job) ~found ~emitted ~errors =
  checkpoint job
    [
      ( "highlighting",
        cursor ~next:(code_points text) ~size:chunk_size ~found ~emitted ~errors
      );
    ]

let reports ?extra job ~count =
  [
    report ?extra job 10 (code "loading");
    report ?extra job 30 (code "analyzing");
    report ?extra job 60
      (code ~members:[ ("count", `Int count) ] "creating-annotations");
    report ?extra job 100
      (code
         ~members:[ ("count", `Int count); ("motivation", str "highlighting") ]
         "complete-created");
  ]

let end_to_end (w : W.t) =
  let job = case_job w.own "highlighting" in
  let resource_id = job.resource_id in
  Ollama.script w.ollama
    [
      answer
        (Yojson.Safe.to_string
           (`List
              [
                `Assoc
                  [
                    ("exact", str "the first program");
                    ("prefix", str "Ada Lovelace published ");
                    ("suffix", str " in 1843");
                  ];
                (* Three times in the text: what the model says is around it
                   tells which. *)
                `Assoc
                  [
                    ("exact", str "London");
                    ("prefix", str "standards, and ");
                    ("suffix", str " ignored it");
                  ];
                (* Nowhere in the text. *)
                `Assoc [ ("exact", str "a steam locomotive crossing the Alps") ];
              ]));
    ];
  let served = W.start w in
  let completion = settled served job in
  (* What it asked the gateway for, in order. The job's description is read
     on the bus, its bytes over HTTP. *)
  expect_sequence served
    [
      "emit job:claim";
      "emit job:start";
      "emit browse:resource-requested";
      "GET /resources/" ^ resource_id;
      "emit mark:commit";
      "emit job:checkpoint";
      "emit job:complete";
      "emit job:claim";
    ];
  Check.equal "its start"
    (`List [ identity job ])
    (`List (W.payloads served "job:start"));
  Check.equal "what it asked of the record"
    (`List [ `Assoc [ ("resourceId", str resource_id) ] ])
    (`List (W.payloads served "browse:resource-requested"));
  Check.equal "the anchored text it read" (`List [])
    (strs (W.anchored_text_reads w.own));
  (* What it asked its model: its limits once, and one generation over the
     whole text. *)
  Check.equal "what it asked of its model's limits"
    (`List [ `Assoc [ ("model", str model) ] ])
    (`List (Dynarray.to_list w.ollama.shows));
  expect_generations w.ollama [ generation model "highlighting" ];
  (* What it committed: one batch, for the job, of the two spans that are in
     the text. *)
  let generator = W.generator w.own in
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
                   text_annotation generator resource_id "highlighting"
                     "L44bEdl-gy0zz0oZ4FIbs" ~start:23 ~stop:40
                     ~prefix:"Ada Lovelace published "
                     ~suffix:
                       " in 1843 \u{2014} a method for computing Bernoulli \
                        numbers on the Analytical"
                     "the first program";
                   text_annotation generator resource_id "highlighting"
                     "KMvMOmT6HejPBEUDVZpfV" ~start:354 ~stop:360
                     ~prefix:
                       "claim for a century.\n\n\
                        The program was na\u{00ef}ve by later standards, and "
                     ~suffix:" ignored it. London was wrong.\n" "London";
                 ] );
           ];
       ])
    (committed served);
  Check.equal "who committed"
    (strs [ W.did w.own ])
    (`List
       (List.map
          (fun (c : W.commit) -> c.committed_by)
          (Dynarray.to_list w.own.commits)));
  (* Where that leaves its one unit, said once the batch is on the record. *)
  Check.equal "its checkpoint"
    (`List [ checkpoint job ~found:3 ~emitted:2 ~errors:1 ])
    (`List (W.payloads served "job:checkpoint"));
  expect_progress served job (reports job ~count:2);
  (* Three proposed, two recorded, one that could not be anchored. *)
  Check.equal "its completion"
    (identified job
       [
         ( "result",
           `Assoc
             [ ("found", `Int 3); ("persisted", `Int 2); ("errors", `Int 1) ] );
         ("durability", str "acknowledged");
       ])
    completion

let finds_nothing (w : W.t) =
  let job = mark_job w.own "highlighting-none" (params "highlighting") in
  Ollama.script w.ollama [ answer "[]" ];
  let served = W.start w in
  let completion = settled served job in
  expect_sequence served
    [
      "emit job:claim";
      "emit job:start";
      "emit browse:resource-requested";
      "GET /resources/" ^ job.resource_id;
      "emit job:checkpoint";
      "emit job:complete";
      "emit job:claim";
    ];
  Check.equal "what it committed" (`Int 0)
    (`Int (Dynarray.length w.own.commits));
  Check.equal "its checkpoint"
    (`List [ checkpoint job ~found:0 ~emitted:0 ~errors:0 ])
    (`List (W.payloads served "job:checkpoint"));
  expect_progress served job (reports job ~count:0);
  (* A job that committed nothing says nothing of how its commits were
     established. *)
  Check.equal "its completion"
    (identified job
       [ ("result", `Assoc [ ("found", `Int 0); ("persisted", `Int 0) ]) ])
    completion

let instructed (w : W.t) =
  let job = case_job w.own "highlighting-instructed" in
  Ollama.script w.ollama
    [
      answer
        (Yojson.Safe.to_string (`List [ `Assoc [ ("exact", str "1843") ] ]));
    ];
  let served = W.start w in
  let completion = settled served job in
  expect_generations w.ollama [ generation model "highlighting-instructed" ];
  (* What the job was asked with is echoed on every report. *)
  let extra =
    [
      ( "requestParams",
        `List
          [
            `Assoc
              [
                ("label", str "instructions");
                ("value", str "Highlight every date.");
              ];
            `Assoc [ ("label", str "density"); ("value", str "2") ];
          ] );
    ]
  in
  expect_progress served job (reports ~extra job ~count:1);
  Check.equal "what it committed"
    (`List
       [
         `List
           [
             text_annotation (W.generator w.own) job.resource_id "highlighting"
               "RcaAxfiOERnhmMQjc71iR" ~start:44 ~stop:48
               ~prefix:"Ada Lovelace published the first program in "
               ~suffix:
                 " \u{2014} a method for computing Bernoulli numbers on the \
                  Analytical Engine"
               "1843";
           ];
       ])
    (batches served);
  Check.equal "its completion"
    (identified job
       [
         ("result", `Assoc [ ("found", `Int 1); ("persisted", `Int 1) ]);
         ("durability", str "acknowledged");
       ])
    completion

let tests implementations =
  in_world implementations "highlighting" ~category:[ "a highlighting job" ]
    [
      case
        "reads the resource, asks its model once, commits the spans it can \
         anchor, and completes with its counts"
        end_to_end;
      case
        "commits nothing when its model finds nothing, and says so in its \
         counts"
        finds_nothing;
      case "asks as its instructions, density and source language say"
        instructed;
    ]
