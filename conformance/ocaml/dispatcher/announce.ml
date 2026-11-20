(* Announcements (JOBS.md § job:queued): a pending job is announced with its
   description less its input, which is what a claim is matched against. A
   mark job's is every parameter it was created with; a yield job's is what
   it is asked to make, without the context it is made from. Nothing the
   dispatcher adds to the job it holds is announced. *)

open Xynolog_conformance
open Support
open Case

let announced ?n w job_id = (D.announced ?n w job_id).payload

let mark_params w =
  let creator = D.person w "creator" in
  let resource_id = D.resource_id () in
  let params =
    `Assoc
      [
        motivation "commenting";
        ("instructions", str "what a first reader would ask");
        ("tone", str "explanatory");
        ("density", `Int 4);
        ("language", str "de");
        ("sourceLanguage", str "en");
      ]
  in
  let job_id = D.created ~resource_id creator "mark" params in
  Check.matching "the announcement"
    [
      ("jobId", str job_id);
      ("jobType", str "mark");
      ("resourceId", str resource_id);
      ("userId", str creator.did);
    ]
    (announced w job_id);
  Check.equal "the announcement's params" params
    (Check.at (announced w job_id) [ "params" ])

let tagging w =
  let creator = D.person w "creator" in
  let worker = D.worker w "tagger" in
  let params =
    `Assoc
      [
        motivation "tagging";
        ("schemaId", str "irac");
        ("categories", `List [ str "Issue"; str "Rule" ]);
      ]
  in
  let job_id =
    D.created ~resource_id:(D.resource_id ()) creator "mark" params
  in
  Check.equal "the announcement's params" params
    (Check.at (announced w job_id) [ "params" ]);
  (* The job a claim hands over does carry it. *)
  has "the claimed job"
    (D.claimed worker [ D.marks "tagging" ])
    [ "params"; "schema" ]

let yield_job w =
  let creator = D.person w "creator" in
  let worker = D.worker w "generator" in
  let resource_id = D.resource_id () in
  let asked =
    [
      ("title", str "Ouranos");
      ("storageUri", str "file://generated/ouranos.md");
      ("prompt", str "in one page");
      ("entityTypes", `List [ str "Person" ]);
      ("language", str "en");
      ("maxTokens", `Int 800);
      ("outputMediaType", str "text/markdown");
      ("task", str "summary");
      ("cite", `Bool true);
    ]
  in
  let job_id =
    D.created creator "yield" (D.generation ~extra:asked resource_id)
  in
  let announcement = announced w job_id in
  Check.matching "the announcement"
    [
      ("jobId", str job_id);
      ("jobType", str "yield");
      ("resourceId", str resource_id);
      ("userId", str creator.did);
    ]
    announcement;
  Check.equal "the announcement's params" (`Assoc asked)
    (Check.at announcement [ "params" ]);
  (* The job a claim hands over does carry it. *)
  has "the claimed job" (D.claimed worker [ D.yields ]) [ "params"; "context" ]

let retried w =
  let r =
    D.running
      ~job:
        (D.Mark
           [
             motivation "tagging";
             ("schemaId", str "irac");
             ("categories", `List [ str "Issue" ]);
           ])
      w
  in
  let job_id = r.ref.job_id in
  let first = announced w job_id in
  D.fail r.worker r.ref "the model timed out";
  ignore
    (D.until_status r.creator job_id "the job to be re-queued" "pending"
      : Yojson.Safe.t);
  Check.equal "the second announcement" first (announced ~n:2 w job_id)

let tests implementations =
  with_dispatcher implementations "job:queued"
    [
      case
        "announces a mark job with every parameter it was created with, its \
         motivation among them"
        mark_params;
      case
        "announces a tagging job as it was created, without the schema the \
         dispatcher resolved for its worker"
        tagging;
      case
        "announces a yield job with what it is asked to make, and without its \
         context"
        yield_job;
      case "announces a retried job as it announced it first" retried;
    ]
