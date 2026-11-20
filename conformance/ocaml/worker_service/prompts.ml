(* prompts.test.ts: the other ways each kind of mark job is asked
   (WORKER-SERVICE.md § The five kinds of mark job): what a job's
   instructions, tone, density and languages, or the lack of each, make of
   the prompt its model is sent. Each job, its prompt and its request are
   worker/prompt-cases.json's case of that name. *)

open Xynolog_conformance
open Support
open Case

(* The cases of worker/prompt-cases.json a case asks as its file says. *)
let asked =
  [
    "highlighting-dense";
    "commenting-plain";
    "commenting-toned";
    "assessing-plain";
    "assessing-instructed";
    "tagging-language";
  ]

let as_its_file_says name (w : W.t) =
  let job = case_job w.own name in
  Ollama.script w.ollama [ answer "[]" ];
  let served = W.start w in
  let completion = settled served job in
  expect_generations w.ollama [ generation model name ];
  Check.like "its completion"
    (identified job
       [ ("result", `Assoc [ ("found", `Int 0); ("persisted", `Int 0) ]) ])
    completion

let descriptive (w : W.t) =
  let job = case_job w.own "linking-descriptive" in
  Ollama.script w.ollama [ answer "[]"; counting 0 ];
  let served = W.start w in
  ignore (settled served job : Yojson.Safe.t);
  expect_generations w.ollama
    [
      generation model "linking-descriptive";
      (* The count is asked of the same text, of the same types, in the same
         words whatever the extraction was asked. *)
      generation model "linking-person-count";
    ]

let tests implementations =
  in_world implementations "prompts"
    ~category:[ "what a mark job asks its model" ]
    (List.map
       (fun name ->
         case
           (Printf.sprintf "asks %s as its file says" name)
           (as_its_file_says name))
       asked
    @ [
        case
          "asks a linking job that takes descriptive references for them, and \
           counts as it does any other"
          descriptive;
      ])
