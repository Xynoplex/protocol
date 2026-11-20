(* The dispatcher's clocks (JOBS.md § Periodic work, § job:report-progress),
   run fast: the document's timing shrinks each to a fraction of a
   second. *)

open Xynolog_conformance
open Support
open Case

let tick = 300
let stale = 1_500

let linking =
  D.Mark
    [
      ("motivation", str "linking");
      ("entityTypes", `List [ str "Person"; str "Place" ]);
    ]

let id (r : D.running) = D.text r.job [ "metadata"; "id" ]

let tick_announces w =
  let job_id =
    D.created ~resource_id:(D.resource_id ()) (D.person w "creator") "mark"
      (`Assoc [ ("motivation", str "highlighting") ])
  in
  ignore (D.announced ~n:3 ~timeout:5.0 w job_id : Bus_stream.frame)

let silent_requeued w =
  let r = D.running ~job:(D.Mark [ ("motivation", str "commenting") ]) w in
  let announced = List.length (D.announcements w (id r)) in
  ignore
    (D.until_status ~timeout:10.0 r.creator (id r)
       "the sweep to re-queue the job" "pending"
      : Yojson.Safe.t);
  ignore (D.announced ~n:(announced + 1) w (id r) : Bus_stream.frame);
  let retried = D.claimed r.worker [ D.marks "commenting" ] in
  Check.equal "the retry's count" (`Int 1)
    (Check.at retried [ "metadata"; "retryCount" ]);
  D.complete r.worker (D.ref_of retried)

let silent_failed w =
  let r = D.running ~job:D.Yield w in
  let status =
    D.until_status ~timeout:10.0 r.creator (id r) "the sweep to fail the job"
      "failed"
  in
  let presumed = D.error_row "presumed-dead" in
  Check.equal "the job's error"
    (str
       (Protocol.fill_template presumed.error
          [ ("minutes", D.shortest (D.timing w "staleRunningMs" /. 60.0)) ]))
    (Check.at status [ "error" ]);
  Check.equal "the job's failure class"
    (str
       (Xynolog_protocol.Tables.Dispatcher_messages.failure_class_value
          presumed.failure_class))
    (Check.at status [ "failureClass" ])

(* Keep [r]'s worker signalling with [signal] every 400 ms for twice the
   stale window: the job stays running. *)
let kept_alive w (r : D.running) signal =
  let rec go elapsed =
    if elapsed < 2 * stale then (
      signal elapsed;
      D.settle ~seconds:0.4 ();
      go (elapsed + 400))
  in
  go 0;
  Check.equal "the job's status" (str "running")
    (Check.at (D.status_of r.creator (id r)) [ "status" ]);
  D.complete r.worker r.ref

let progress_keeps w =
  let r = D.running w in
  kept_alive w r (fun elapsed ->
      D.report_progress r.worker r.ref
        (`Float (Float.min 99.0 (float_of_int elapsed /. 40.0))))

let checkpoints_keep w =
  let r = D.running ~job:linking w in
  kept_alive w r (fun elapsed ->
      D.checkpoint r.worker (id r) [ Printf.sprintf "unit-%d" elapsed ])

let cursor next size found emitted errors =
  `Assoc
    [
      ("next", `Int next);
      ("size", `Int size);
      ("found", `Int found);
      ("emitted", `Int emitted);
      ("errors", `Int errors);
    ]

let requeue_keeps_checkpoint w =
  let r = D.running ~job:linking w in
  let ended = cursor 9 5 4 4 0 and partway = cursor 2 5 3 2 1 in
  D.checkpoint
    ~unit_cursors:[ ("Person", ended); ("Place", partway) ]
    r.worker (id r) [ "Person" ];
  ignore
    (D.until_status ~timeout:10.0 r.creator (id r)
       "the sweep to re-queue the job" "pending"
      : Yojson.Safe.t);
  let retried = D.claimed r.worker [ D.marks "linking" ] in
  Check.equal "the retry's finished units"
    (`List [ str "Person" ])
    (Check.at retried [ "metadata"; "completedUnits" ]);
  Check.equal "the retry's cursors"
    (`Assoc [ ("Person", ended); ("Place", partway) ])
    (Check.at retried [ "metadata"; "unitCursors" ]);
  D.complete r.worker (D.ref_of retried)

let retention w =
  let creator = D.person w "creator" in
  let completed = D.running w in
  D.complete
    ~result:(`Assoc [ ("found", `Int 0); ("persisted", `Int 0) ])
    completed.worker completed.ref;
  let failed = D.running ~job:D.Yield w in
  D.fail failed.worker failed.ref "broken";
  let commenting = `Assoc [ ("motivation", str "commenting") ] in
  let cancelled =
    D.created ~resource_id:(D.resource_id ()) creator "mark" commenting
  in
  ignore
    (D.cancel_request creator (`Assoc [ ("jobId", str cancelled) ]) : D.answer);
  let running = D.running ~job:(D.Mark [ ("motivation", str "assessing") ]) w in
  let pending =
    D.created ~resource_id:(D.resource_id ()) creator "mark" commenting
  in
  List.iter
    (fun job_id ->
      Run.eventually (job_id ^ " to be deleted") 10.0 (fun () ->
          if
            D.message_of (D.status creator job_id)
            = Some (str (D.refusal_text "job-not-found"))
          then Some ()
          else None))
    [ id completed; id failed; cancelled ];
  Check.equal "the running job's status" (str "running")
    (Check.at (D.status_of creator (id running)) [ "status" ]);
  Check.equal "the pending job's status" (str "pending")
    (Check.at (D.status_of creator pending) [ "status" ])

let throttle w =
  let r = D.running w in
  let percentage () =
    Protocol.at (D.status_of r.creator (id r)) [ "progress"; "percentage" ]
  in
  let polled n =
    Run.eventually (Printf.sprintf "the progress to show %d" n) 1.0 (fun () ->
        match percentage () with
        | Some (`Int p) when p = n -> Some ()
        | Some (`Float p) when p = float_of_int n -> Some ()
        | _ -> None)
  in
  D.report_progress r.worker r.ref (`Int 10);
  polled 10;
  D.report_progress r.worker r.ref (`Int 20);
  D.settle ();
  Check.equal "the progress written" (`Int 10)
    (Option.value ~default:`Null (percentage ()));
  D.settle ~seconds:(D.timing w "progressWriteIntervalMs") ();
  D.report_progress r.worker r.ref (`Int 30);
  polled 30

let tests implementations =
  with_dispatcher implementations
    ~timing:
      [
        ("tickMs", tick);
        ("staleRunningMs", stale);
        ("progressWriteIntervalMs", 100);
      ]
    "the re-announce tick and the dead-worker sweep"
    [
      case "announces an unclaimed pending job again at every tick"
        tick_announces;
      case
        "re-queues a job whose worker went silent, when its budget allows: \
         announced again, and claimed with one retry counted"
        silent_requeued;
      case
        "fails a silent job with no budget left, saying its worker is presumed \
         dead, and classes it transient"
        silent_failed;
      case "leaves alone a job whose worker keeps reporting progress"
        progress_keeps;
      case "leaves alone a job whose worker keeps checkpointing"
        checkpoints_keep;
      case "keeps the checkpoint of a job it re-queues" requeue_keeps_checkpoint;
    ]
  @ with_dispatcher implementations
      ~timing:[ ("retentionMs", 1_000); ("retentionSweepMs", 300) ]
      "retention"
      [
        case
          "deletes finished jobs past the window, and keeps pending and \
           running ones"
          retention;
      ]
  @ with_dispatcher implementations
      ~timing:[ ("progressWriteIntervalMs", 2_000) ]
      "the progress throttle"
      [
        case
          "drops a report arriving inside the window after the last one \
           written, and writes the next one outside it"
          throttle;
      ]
