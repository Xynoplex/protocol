(* The jobs a worker does not do (WORKER-SERVICE.md
   § Declines): a resource with nothing to read is declined in the job's
   completion; a resource that can never be read, and a job the worker
   cannot run, are failed as deterministic. *)

open Xynolog_conformance
open Support
open Case

(* A job that ends before any work: nothing was asked of the model, nothing
   committed, and no progress was reported. *)
let did_no_work (w : W.t) served (job : W.job) =
  expect_shows w [];
  expect_generations_count w 0;
  expect_commits_count w 0;
  Check.equal "its progress reports" (`List [])
    (`List (W.progress served job.id));
  said_nothing served [ "job:checkpoint" ]

let declined_as_empty about (w : W.t) =
  let job = highlighting_job ~about w.own "empty" in
  let served = W.start w in
  let completion = settled served job in
  Check.equal "its completion"
    (identified job
       [
         ("result", `Assoc [ ("declined", `Bool true); ("reason", str "empty") ]);
       ])
    completion;
  expect_sequence served
    [
      "emit job:claim";
      "emit job:start";
      "emit browse:resource-requested";
      "GET /resources/" ^ job.resource_id;
      "emit job:complete";
      "emit job:claim";
    ];
  did_no_work w served job

let no_text (w : W.t) =
  let resource_id = "res-ws-image" in
  W.describe w.own resource_id "image/png";
  let job =
    W.queued w.own "mark"
      (`Assoc
         [ ("resourceId", str resource_id); ("motivation", str "commenting") ])
  in
  let served = W.start w in
  let rest, error = failure_of (settled ~channel:"job:fail" served job) in
  Check.equal "its failure"
    (failed ~failure_class:"deterministic" ~will_retry:false job)
    rest;
  (* The error names the job, the resource and the media type. *)
  says "its error" error
    (Support.error "unextractable"
       [
         ("job", mark_named "commenting");
         ("resourceId", resource_id);
         ("mediaType", "image/png");
       ]);
  expect_sequence served
    [
      "emit job:claim";
      "emit job:start";
      "emit browse:resource-requested";
      "emit job:fail";
      "emit job:claim";
    ];
  did_no_work w served job

let unknown_resource (w : W.t) =
  let job =
    W.queued w.own "mark"
      (`Assoc
         [
           ("resourceId", str "res-ws-unknown");
           ("motivation", str "highlighting");
         ])
  in
  let served = W.start w in
  let failure = settled ~channel:"job:fail" served job in
  Check.equal "its failure"
    (identified job
       [ ("error", str "Resource not found"); ("willRetry", `Bool true) ])
    failure;
  expect_sequence served
    [
      "emit job:claim";
      "emit job:start";
      "emit browse:resource-requested";
      "emit job:fail";
      "emit job:claim";
    ];
  did_no_work w served job

let not_served (w : W.t) =
  let job = mark_job w.own "not-served" (motivated "assessing") in
  (* No dispatcher does this: the claim asks for highlighting jobs, and is
     handed an assessing one. *)
  w.own.hooks <- { w.own.hooks with hand_any_job = true };
  let served =
    W.start
      ~engines:[ W.entry w (List.hd W.ollama_engines) highlighting_only ]
      w
  in
  let rest, error = failure_of (settled ~channel:"job:fail" served job) in
  Check.equal "its failure"
    (failed ~failure_class:"deterministic" ~will_retry:false job)
    rest;
  says "its error" error
    (Support.error "not-served" [ ("job", mark_named "assessing") ]);
  expect_sequence served
    [ "emit job:claim"; "emit job:start"; "emit job:fail"; "emit job:claim" ];
  Check.equal "the descriptions it read" (`List [])
    (strs (Dynarray.to_list w.own.descriptor_reads));
  did_no_work w served job

let no_schema (w : W.t) =
  let job =
    mark_job w.own "no-schema"
      (`Assoc
         [
           ("motivation", str "tagging");
           ("schemaId", str "argument");
           ("categories", strs [ "Claim" ]);
         ])
  in
  let served = W.start w in
  let rest, error = failure_of (settled ~channel:"job:fail" served job) in
  Check.equal "its failure"
    (failed ~failure_class:"deterministic" ~will_retry:false job)
    rest;
  says "its error" error
    (Support.error "no-processor" [ ("job", mark_named "tagging") ]);
  expect_generations_count w 0;
  expect_commits_count w 0

let tests implementations =
  in_world implementations "declines"
    ~category:[ "a job a worker does not do" ]
    (List.map
       (fun (what, about) ->
         case
           (Printf.sprintf
              "completes, declined as empty, a job on a text that %s" what)
           (declined_as_empty about))
       [ ("is empty", ""); ("is nothing but white space", " \n\t\n   \n") ]
    @ [
        case
          "fails as deterministic a job on a resource whose media type has no \
           text, having read no bytes"
          no_text;
        case
          "fails, with no class, a job on a resource the record does not have"
          unknown_resource;
        case
          "fails as deterministic a job its engine does not serve, having \
           started it and read nothing"
          not_served;
        case
          "fails as deterministic a tagging job handed over without the schema \
           it names"
          no_schema;
      ])
