(* Concluding an attempt (JOBS.md § job:complete, § job:fail, § Retries):
   completion, and a failure retried or final by the retry predicate, with
   the checkpoint it carries merged either way. A conclusion names a job that
   must be running; for anything else it has no effect. *)

open Xynolog_conformance
open Support
open Case

let highlights =
  `Assoc [ ("found", `Int 4); ("persisted", `Int 3); ("errors", `Int 1) ]

let status_is what expected s =
  Check.equal what (str expected) (Check.at s [ "status" ])

(* The moment at [path] of [json], which must be a time. *)
let moment json path =
  match Ptime.of_rfc3339 (D.text json path) with
  | Ok (t, _, _) -> t
  | Error _ ->
      failwith
        (Printf.sprintf "%s at %s is not a time" (Check.shown json)
           (String.concat "." path))

let not_before what later earlier =
  Check.that what (Ptime.compare later earlier >= 0)

let completed what (r : D.running) =
  D.until_status r.creator r.ref.job_id what "complete"

(* ── job:complete ─────────────────────────────────────────────────── *)

let completes w =
  let r = D.running w in
  D.complete ~result:highlights r.worker r.ref;
  let status = completed "the job to complete" r in
  Check.matching "the status"
    [ ("startedAt", Check.at r.job [ "startedAt" ]); ("result", highlights) ]
    status;
  not_before "it completed no earlier than it started"
    (moment status [ "completedAt" ])
    (moment r.job [ "startedAt" ]);
  lacks "the status" status [ "progress" ]

(* Offered at the door with [result], the completion is refused, and the
   job stays running. *)
let completion_refused ?job result w =
  let r = D.running ?job w in
  let reply =
    D.offered r.worker "job:complete"
      (`Assoc (D.ref_members r.ref @ [ ("result", result) ]))
  in
  Check.equal "the gateway's answer" (`Int 400) (`Int reply.status);
  D.settle ();
  status_is "the job's status" "running" (D.status_of r.creator r.ref.job_id)

let unreported =
  [
    ( "counts under a motivation's own names",
      `Assoc [ ("highlightsFound", `Int 4); ("highlightsCreated", `Int 3) ] );
    ( "a kind beside its counts",
      `Assoc
        [
          ("kind", str "highlight-annotation");
          ("found", `Int 4);
          ("persisted", `Int 3);
        ] );
    ( "counts and a resource at once",
      `Assoc
        [
          ("found", `Int 4);
          ("persisted", `Int 3);
          ("resourceId", str "res-made");
          ("resourceName", str "Made");
          ("truncated", `Bool false);
        ] );
    ( "a count of no errors, which is said by leaving it out",
      `Assoc [ ("found", `Int 4); ("persisted", `Int 4); ("errors", `Int 0) ] );
    ( "a decline that says more",
      `Assoc
        [ ("declined", `Bool true); ("reason", str "empty"); ("found", `Int 0) ]
    );
  ]

let made =
  `Assoc
    [
      ("resourceId", str "res-made");
      ("resourceName", str "Made");
      ("truncated", `Bool false);
    ]

let counts = `Assoc [ ("found", `Int 4); ("persisted", `Int 3) ]

let of_another_verb =
  [
    ("a mark job the resource a yield job makes", D.highlighting, made);
    ("a yield job the counts a mark job reports", D.Yield, counts);
  ]

let crossed =
  [
    ("a yield job's completion of a mark job", D.highlighting, "yield", made);
    ("a mark job's completion of a yield job", D.Yield, "mark", counts);
  ]

let crossed_completion job completed_as result w =
  let r = D.running ~job w in
  D.complete ~result r.worker { r.ref with job_type = completed_as };
  D.settle ();
  status_is "the job's status" "running" (D.status_of r.creator r.ref.job_id);
  D.complete r.worker r.ref;
  Check.equal "the job's result" (`Assoc [])
    (Check.at (completed "the job to complete" r) [ "result" ])

let stored =
  [
    ( "a mark job's counts",
      D.highlighting,
      `Assoc
        [
          ("found", `Int 4);
          ("persisted", `Int 3);
          ("errors", `Int 1);
          ("byCategory", `Assoc [ ("Issue", `Int 2); ("Rule", `Int 1) ]);
          ("underReportedPieces", `Int 1);
        ] );
    ( "the resource a yield job made",
      D.Yield,
      `Assoc
        [
          ("resourceId", str "res-made");
          ("resourceName", str "Made");
          ("truncated", `Bool true);
        ] );
    ( "a decline",
      D.highlighting,
      `Assoc [ ("declined", `Bool true); ("reason", str "no-text-layer") ] );
  ]

let stored_as_sent job result w =
  let r = D.running ~job w in
  D.complete ~result r.worker r.ref;
  Check.equal "the job's result" result
    (Check.at (completed "the job to complete" r) [ "result" ])

let empty_result w =
  let r = D.running ~job:(D.Mark [ motivation "commenting" ]) w in
  D.complete r.worker r.ref;
  Check.equal "the job's result" (`Assoc [])
    (Check.at (completed "the job to complete" r) [ "result" ])

let not_running_complete w =
  let creator = D.person w "creator" in
  let worker = D.worker w "premature" in
  let resource_id = D.resource_id () in
  let job_id =
    D.created ~resource_id creator "mark" (`Assoc [ motivation "highlighting" ])
  in
  D.complete ~result:highlights worker
    { job_id; job_type = "mark"; resource_id };
  D.complete ~result:highlights worker
    { job_id = Fresh.job_id (); job_type = "mark"; resource_id };
  D.settle ();
  status_is "the job's status" "pending" (D.status_of creator job_id);
  Check.equal "the job claimed" (str job_id)
    (str (D.id_of (D.claimed worker [ D.marks "highlighting" ])))

let absorbing w =
  let r = D.running w in
  D.complete ~result:highlights r.worker r.ref;
  ignore (completed "the job to complete" r : Yojson.Safe.t);
  D.fail r.worker r.ref "too late";
  D.cancel r.worker r.ref;
  D.settle ();
  Check.matching "the status"
    [ ("status", str "complete"); ("result", highlights) ]
    (D.status_of r.creator r.ref.job_id)

(* ── job:fail ─────────────────────────────────────────────────────── *)

let requeued what (r : D.running) =
  D.until_status r.creator r.ref.job_id what "pending"

let failed what (r : D.running) =
  D.until_status r.creator r.ref.job_id what "failed"

let classed word = [ ("failureClass", str word) ]

let first_failure_retried w =
  let r = D.running w in
  ignore (D.announced w r.ref.job_id : Bus_stream.frame);
  D.fail r.worker r.ref "the model timed out";
  let status = requeued "the job to be re-queued" r in
  lacks "the status" status [ "startedAt" ];
  lacks "the status" status [ "error" ];
  ignore (D.announced ~n:2 w r.ref.job_id : Bus_stream.frame);
  let retried = D.claimed r.worker [ D.marks "highlighting" ] in
  Check.matching "the retry's metadata"
    [
      ("id", str r.ref.job_id);
      ("retryCount", `Int 1);
      ("maxRetries", max_retries "mark");
    ]
    (Check.at retried [ "metadata" ]);
  Check.equal "the retry's progress" (`Assoc [])
    (Check.at retried [ "progress" ])

let transient_retried w =
  let r = D.running ~job:(D.Mark [ motivation "commenting" ]) w in
  D.fail ~extra:(classed "transient") r.worker r.ref "rate limited";
  ignore (requeued "the job to be re-queued" r : Yojson.Safe.t)

let budget_spent w =
  let r = D.running w in
  D.fail r.worker r.ref "first";
  ignore (requeued "the job to be re-queued" r : Yojson.Safe.t);
  let retried = D.claimed r.worker [ D.marks "highlighting" ] in
  D.fail r.worker r.ref "second, and last";
  let status = failed "the job to fail" r in
  Check.equal "the job's error" (str "second, and last")
    (Check.at status [ "error" ]);
  lacks "the status" status [ "failureClass" ];
  not_before "it failed no earlier than its retry started"
    (moment status [ "completedAt" ])
    (moment retried [ "startedAt" ])

let last_class_kept w =
  let r = D.running w in
  D.fail ~extra:(classed "transient") r.worker r.ref "rate limited";
  ignore (requeued "the job to be re-queued" r : Yojson.Safe.t);
  ignore (D.claimed r.worker [ D.marks "highlighting" ] : Yojson.Safe.t);
  D.fail ~extra:(classed "transient") r.worker r.ref "rate limited again";
  Check.equal "the job's failure class" (str "transient")
    (Check.at (failed "the job to fail" r) [ "failureClass" ])

let generation_never_retried w =
  let r = D.running ~job:D.Yield w in
  D.fail r.worker r.ref "the model refused";
  Check.equal "the job's error" (str "the model refused")
    (Check.at (failed "the job to fail" r) [ "error" ])

let deterministic w =
  let r = D.running ~job:(D.Mark [ motivation "assessing" ]) w in
  D.fail ~extra:(classed "deterministic") r.worker r.ref "the resource is empty";
  Check.equal "the job's failure class" (str "deterministic")
    (Check.at (failed "the job to fail" r) [ "failureClass" ])

let withheld w =
  let r = D.running ~job:(D.Mark [ motivation "commenting" ]) w in
  Check.matching "the job's metadata"
    [ ("retryCount", `Int 0); ("maxRetries", max_retries "mark") ]
    (Check.at r.job [ "metadata" ]);
  let error = "The provider withheld its answer: refusal" in
  D.fail ~extra:(classed "withheld") r.worker r.ref error;
  let status = failed "the job to fail" r in
  Check.equal "the job's error" (str error) (Check.at status [ "error" ]);
  Check.equal "the job's failure class" (str "withheld")
    (Check.at status [ "failureClass" ]);
  declined "a claim's answer" (D.claim r.worker [ D.marks "commenting" ])

let not_running_fail w =
  let creator = D.person w "creator" in
  let worker = D.worker w "premature" in
  let resource_id = D.resource_id () in
  let job_id =
    D.created ~resource_id creator "mark" (`Assoc [ motivation "commenting" ])
  in
  D.fail ~extra:(classed "deterministic") worker
    { job_id; job_type = "mark"; resource_id }
    "never started";
  D.settle ();
  status_is "the job's status" "pending" (D.status_of creator job_id);
  Check.equal "the claim's retry count" (`Int 0)
    (Check.at
       (D.claimed worker [ D.marks "commenting" ])
       [ "metadata"; "retryCount" ])

let units_carried w =
  let r = D.running ~job:linking w in
  D.checkpoint r.worker r.ref.job_id [ "Person" ];
  D.settle ();
  D.fail
    ~extra:[ ("completedUnits", units [ "Place" ]) ]
    r.worker r.ref "interrupted";
  ignore (requeued "the job to be re-queued" r : Yojson.Safe.t);
  let retried = D.claimed r.worker [ D.marks "linking" ] in
  Check.equal "the retry's finished units"
    (units [ "Person"; "Place" ])
    (sorted (Check.at retried [ "metadata"; "completedUnits" ]))

let cursors_carried w =
  let r = D.running ~job:linking w in
  let person = cursor 3 10 5 4 1 in
  D.fail
    ~extra:[ ("unitCursors", `Assoc [ ("Person", person) ]) ]
    r.worker r.ref "interrupted";
  ignore (requeued "the job to be re-queued" r : Yojson.Safe.t);
  Check.equal "the retry's cursors"
    (`Assoc [ ("Person", person) ])
    (Check.at
       (D.claimed r.worker [ D.marks "linking" ])
       [ "metadata"; "unitCursors" ])

let finished_cursor_carried w =
  let r = D.running ~job:linking w in
  let person = cursor 9 10 5 4 1 and place = cursor 9 7 2 2 0 in
  D.checkpoint
    ~unit_cursors:[ ("Person", person) ]
    r.worker r.ref.job_id [ "Person" ];
  D.settle ();
  D.fail
    ~extra:
      [
        ("completedUnits", units [ "Place" ]);
        ("unitCursors", `Assoc [ ("Place", place) ]);
      ]
    r.worker r.ref "interrupted";
  ignore (requeued "the job to be re-queued" r : Yojson.Safe.t);
  let retried = D.claimed r.worker [ D.marks "linking" ] in
  Check.equal "the retry's finished units"
    (units [ "Person"; "Place" ])
    (sorted (Check.at retried [ "metadata"; "completedUnits" ]));
  Check.equal "the retry's cursors"
    (`Assoc [ ("Person", person); ("Place", place) ])
    (Check.at retried [ "metadata"; "unitCursors" ])

let tests implementations =
  with_dispatcher implementations "job:complete"
    ([
       case
         "completes a running job with its result, keeping the time it started"
         completes;
     ]
    @ List.map
        (fun (what, result) ->
          case
            (Printf.sprintf
               "refuses at the door a completion whose result is %s: none of \
                the three a job reports, and the job stays running"
               what)
            (completion_refused result))
        unreported
    @ List.map
        (fun (what, job, result) ->
          case
            (Printf.sprintf
               "refuses at the door a completion that gives %s: a result is \
                its verb's, and the job stays running"
               what)
            (completion_refused ~job result))
        of_another_verb
    @ List.map
        (fun (what, job, completed_as, result) ->
          case
            (Printf.sprintf
               "has no effect when it is %s: each is well formed for its own \
                verb, and the job stays running until a completion of its own \
                verb"
               what)
            (crossed_completion job completed_as result))
        crossed
    @ List.map
        (fun (what, job, result) ->
          case
            (Printf.sprintf "completes a job with %s, stored as it was sent"
               what)
            (stored_as_sent job result))
        stored
    @ [
        case "records an empty result when the completion carries none"
          empty_result;
        case
          "has no effect on a job that is not running, and the dispatcher \
           keeps serving"
          not_running_complete;
        case "leaves a completed job complete: terminal states are absorbing"
          absorbing;
      ])
  @ with_dispatcher implementations "job:fail"
      [
        case
          "retries a first failure of a job with budget: pending again, \
           announced again, and claimed with one retry counted"
          first_failure_retried;
        case "retries a failure classed transient as it does an unclassed one"
          transient_retried;
        case "fails a job whose budget is spent, recording the error and when"
          budget_spent;
        case
          "keeps on the record of a job failed for good the class its last \
           failure was given"
          last_class_kept;
        case "never retries a generation job" generation_never_retried;
        case
          "fails at once a failure classed deterministic, whatever budget is \
           left"
          deterministic;
        case
          "fails at once a failure classed withheld, whatever budget is left, \
           and does not queue the job again"
          withheld;
        case "has no effect on a job that is not running" not_running_fail;
        case
          "carries the failure's finished units into the retry, with those \
           already checkpointed"
          units_carried;
        case "carries the failure's cursors into the retry" cursors_carried;
        case
          "carries into the retry the cursor of a unit the failure names \
           finished, and keeps the one a checkpoint left for a unit finished \
           before"
          finished_cursor_carried;
      ]
