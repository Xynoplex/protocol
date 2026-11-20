(* A mark job of motivation assessing, end to end
   (WORKER-SERVICE.md § Detection, § The five kinds of mark job): asked with
   a tone and no instructions, and answered with the same assessment twice
   and a blank one. *)

open Xynolog_conformance
open Support
open Case

let end_to_end (w : W.t) =
  let job = case_job w.own "assessing" in
  let resource_id = job.resource_id in
  let assessed =
    `Assoc
      [
        ("exact", str "nobody tested that claim for a century");
        ("assessment", str "The text gives no source for this.");
      ]
  in
  Ollama.script w.ollama
    [
      proposed
        [
          assessed;
          assessed;
          `Assoc [ ("exact", str "London was wrong."); ("assessment", str " ") ];
        ];
    ];
  let served = W.start w in
  let completion = settled served job in
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
  expect_generations w.ollama [ generation model "assessing" ];
  (* An assessment's body is one object, not a list: its text, in the
     language the job asked for, English when it asked for none. *)
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
                   text_annotation (W.generator w.own) resource_id "assessing"
                     "IR7o6S29Rwezi-MGHsfyL" ~start:267 ~stop:305
                     ~prefix:
                       "argued that the engine could manipulate symbols as \
                        well as numbers; "
                     ~suffix:
                       ".\n\n\
                        The program was na\u{00ef}ve by later standards, and \
                        London ignored it"
                     ~body:
                       (`Assoc
                          [
                            ("type", str "TextualBody");
                            ("value", str "The text gives no source for this.");
                            ("purpose", str "assessing");
                            ("format", str "text/plain");
                            ("language", str "en");
                          ])
                     "nobody tested that claim for a century";
                 ] );
           ];
       ])
    (committed served);
  Check.equal "its checkpoint"
    (`List
       [
         checkpoint job
           [
             ( "assessing",
               cursor ~next:(code_points text) ~size:2609 ~found:2 ~emitted:1
                 ~errors:0 );
           ];
       ])
    (checkpoints served);
  let extra =
    [
      ( "requestParams",
        `List [ `Assoc [ ("label", str "tone"); ("value", str "critical") ] ] );
    ]
  in
  expect_progress served job
    [
      report ~extra job 10 (code "loading");
      report ~extra job 30 (code "analyzing");
      creating ~extra job 1;
      complete_created ~extra job "assessing" 1;
    ];
  (* Two proposed, one recorded: the second is the first again, and the one
     with nothing to say is not counted. *)
  Check.equal "its completion"
    (completed job [ ("found", `Int 2); ("persisted", `Int 1) ])
    completion

let tests implementations =
  in_world implementations "assessing" ~category:[ "an assessing job" ]
    [
      case
        "asks as its tone says, commits one annotation for a span and an \
         assessment proposed twice, and takes a blank assessment for no \
         proposal"
        end_to_end;
    ]
