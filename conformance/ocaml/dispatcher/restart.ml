(* Restarts and the broker (JOBS.md § Storage, § job:queued, § Periodic
   work): the queue outlives its dispatcher, a restarted dispatcher
   announces the pending jobs it receives, and the dispatcher rides out its
   broker going away and coming back. *)

open Xynolog_conformance
open Support
open Case

(* Admit a job once the queue is reachable again; a request made while it
   reconnects may be refused. *)
let admitted_again creator params =
  Run.eventually "the queue to admit jobs again" 30.0 (fun () ->
      let answer =
        D.create ~resource_id:(D.resource_id ()) creator "mark" (`Assoc params)
      in
      if answer.ok then Some (D.job_id_of answer) else None)

let with_timing changes document =
  List.fold_left
    (fun d (name, ms) -> Edit.set_at [ "timing"; name ] (`Int ms) d)
    document changes

let announces_at_once w =
  let creator = D.person w "creator" in
  let job_id = mark creator [ motivation "commenting" ] in
  ignore (D.announced w job_id : Bus_stream.frame);
  ignore (Dispatcher.stop w.dispatcher : int option);
  (* Longer than the acknowledgement window: the broker delivers the job to
     the next dispatcher as soon as it connects. *)
  D.settle ~seconds:(2.0 *. D.timing w "ackWaitMs") ();
  D.restart_dispatcher w;
  ignore (D.announced ~n:2 ~timeout:5.0 w job_id : Bus_stream.frame)

let keeps_every_job w =
  let r = D.running w in
  D.complete
    ~result:(`Assoc [ ("found", `Int 0); ("persisted", `Int 0) ])
    r.worker r.ref;
  let creator = D.person w "creator" in
  ignore
    (D.until_status creator r.ref.job_id "the job to complete" "complete"
      : Yojson.Safe.t);
  let pending = mark creator [ motivation "assessing" ] in
  D.restart_dispatcher w;
  Check.equal "the finished job's status" (str "complete")
    (Check.at (D.status_of creator r.ref.job_id) [ "status" ]);
  Check.equal "the job a claim hands out after the restart" (str pending)
    (str
       (D.id_of
          (D.claimed (D.worker w "after-restart") [ D.marks "assessing" ])))

let dead_dispatcher_recovered w =
  let r = D.running w in
  D.crash_and_restart
    ~change:(with_timing [ ("tickMs", 300); ("staleRunningMs", 1_000) ])
    w;
  ignore
    (D.until_status ~timeout:15.0 r.creator r.ref.job_id
       "the sweep to re-queue the job" "pending"
      : Yojson.Safe.t);
  let retried = D.claimed (D.worker w "recovery") [ D.marks "highlighting" ] in
  Check.matching "the retried job's metadata"
    [ ("id", str r.ref.job_id); ("retryCount", `Int 1) ]
    (Check.at retried [ "metadata" ]);
  D.restart_dispatcher
    ~change:
      (with_timing
         [
           ("tickMs", List.assoc "tickMs" Dispatcher.quiet_timing);
           ( "staleRunningMs",
             List.assoc "staleRunningMs" Dispatcher.quiet_timing );
         ])
    w

let broker_restart w =
  Nats.restart w.D.broker;
  let creator = D.person w "creator" in
  let worker = D.worker w "after-broker" in
  let job_id =
    admitted_again creator
      [
        motivation "tagging";
        ("schemaId", str "irac");
        ("categories", `List [ str "Rule" ]);
      ]
  in
  Check.equal "the job a claim hands out" (str job_id)
    (str (D.id_of (D.claimed worker [ D.marks "tagging" ])))

let broker_outage w =
  Nats.down w.D.broker;
  D.settle ~seconds:30.0 ();
  Nats.up w.broker;
  let creator = D.person w "creator" in
  let job_id = admitted_again creator [ motivation "commenting" ] in
  Check.equal "the job a claim hands out" (str job_id)
    (str
       (D.id_of
          (D.claimed (D.worker w "after-outage") [ D.marks "commenting" ])))

let original = { Nats.user = "dispatcher"; password = "original" }

let other_credentials w =
  let creator = D.person w "creator" in
  let highlighting = [ motivation "highlighting" ] in
  ignore (mark creator highlighting : string);
  Nats.restart ~credentials:{ original with password = "rotated" } w.D.broker;
  let refused () =
    let answer =
      D.create ~resource_id:(D.resource_id ()) creator "mark"
        (`Assoc highlighting)
    in
    Check.equal "the reply's channel"
      (str (Protocol.operation_for "job:create").failure)
      (str answer.channel)
  in
  refused ();
  D.settle ~seconds:2.0 ();
  refused ();
  (* A dispatcher started with the broker's credentials serves again. *)
  Nats.restart ~credentials:original w.broker;
  D.restart_dispatcher w;
  ignore (mark creator highlighting : string)

let tests implementations =
  (* The acknowledgement window is short so the broker hands a stopped
     dispatcher's deliveries to the next within the case; the tick is long,
     so an announcement inside the case is never the tick's. *)
  with_dispatcher implementations
    ~timing:[ ("ackWaitMs", 1_000) ]
    "a restarted dispatcher"
    [
      case
        "announces the pending jobs it receives at once, not at its first tick"
        announces_at_once;
      case
        "keeps every job: a pending job is claimable, and a finished one still \
         reads back"
        keeps_every_job;
      case
        "recovers a job whose dispatcher died while it ran, through the \
         dead-worker sweep of the next"
        dead_dispatcher_recovered;
    ]
  @ with_dispatcher implementations "the queue's broker restarting"
      [
        case
          "is ridden out: once the broker is back, jobs are admitted and \
           claimed again"
          broker_restart;
        case ~timeout:120.0
          "is ridden out even when the broker stays away longer than the \
           client's own reconnect budget"
          broker_outage;
      ]
  @ with_dispatcher implementations ~credentials:original
      "the queue's broker returning with other credentials"
      [
        case
          "is not recovered from: jobs are refused rather than accepted and \
           lost"
          other_credentials;
      ]
