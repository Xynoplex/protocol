(* Cancellation (JOBS.md § job:cancel-requested, § job:cancel,
   § Cancellation): the dispatcher cancels the pending job a request names;
   a running job is its worker's to stop, which it confirms with
   job:cancel. *)

open Xynolog_conformance
open Support
open Case

let named job_id = `Assoc [ ("jobId", str job_id) ]
let id (r : D.running) = D.text r.job [ "metadata"; "id" ]

let status_word (c : D.client) job_id =
  str (D.status_word (D.status_of c job_id))

let mark motivation (c : D.client) =
  D.created ~resource_id:(D.resource_id ()) c "mark"
    (`Assoc [ ("motivation", str motivation) ])

let unknown w =
  let answer =
    D.cancel_request (D.person w "canceller") (named (D.unknown_job_id ()))
  in
  Check.equal "the reply's channel" (str "job:cancel-ok") (str answer.channel);
  Check.that "it answers false" (not (D.cancelled answer))

let pending w =
  let creator = D.person w "creator" in
  let job_id = mark "highlighting" creator in
  Check.that "it answers true"
    (D.cancelled (D.cancel_request creator (named job_id)));
  let status = D.status_of creator job_id in
  Check.equal "the job's status" (str "cancelled") (str (D.status_word status));
  has "the status" status [ "completedAt" ];
  lacks "the status" status [ "startedAt" ];
  Check.equal "a later claim's code"
    (str (D.code_word None_pending))
    (Option.value ~default:`Null
       (D.code_of (D.claim (D.worker w "late") (D.every_job ()))))

let running w =
  let r = D.running ~job:(D.Mark [ ("motivation", str "commenting") ]) w in
  Check.that "it answers true"
    (D.cancelled (D.cancel_request r.creator (named (id r))));
  D.settle ();
  Check.equal "the job's status" (str "running") (status_word r.creator (id r))

let finished w =
  let r = D.running w in
  D.complete
    ~result:(`Assoc [ ("found", `Int 0); ("persisted", `Int 0) ])
    r.worker r.ref;
  ignore
    (D.until_status r.creator (id r) "the job to complete" "complete"
      : Yojson.Safe.t);
  Check.that "it answers false"
    (not (D.cancelled (D.cancel_request r.creator (named (id r)))));
  Check.equal "the job's status" (str "complete") (status_word r.creator (id r))

let only_named w =
  let creator = D.person w "creator" in
  let target = mark "highlighting" creator in
  let alike = mark "highlighting" creator in
  let yielding = D.created creator "yield" (D.generation (D.resource_id ())) in
  Check.that "it answers true"
    (D.cancelled (D.cancel_request creator (named target)));
  Check.equal "the named job's status" (str "cancelled")
    (status_word creator target);
  Check.equal "the alike job's status" (str "pending")
    (status_word creator alike);
  Check.equal "the yield job's status" (str "pending")
    (status_word creator yielding)

let by_id_only w =
  let creator = D.person w "creator" in
  let pending = mark "highlighting" creator in
  List.iter
    (fun payload ->
      let reply = D.offered creator "job:cancel-requested" payload in
      Check.equal
        (Printf.sprintf "the gateway's status for %s: %s" (Check.shown payload)
           (Call.shown reply.text))
        (`Int 400) (`Int reply.status))
    [ `Assoc []; `Assoc [ ("jobType", str "mark") ] ];
  D.settle ();
  Check.equal "the job's status" (str "pending") (status_word creator pending)

let linking =
  D.Mark
    [
      ("motivation", str "linking");
      ("entityTypes", `List [ str "Person"; str "Place" ]);
    ]

let worker_stopped w =
  let r = D.running ~job:linking w in
  D.cancel r.worker r.ref;
  let status =
    D.until_status r.creator (id r) "the job to be cancelled" "cancelled"
  in
  has "the status" status [ "completedAt" ]

let finished_stays w =
  let r = D.running ~job:linking w in
  D.fail
    ~extra:[ ("failureClass", str "deterministic") ]
    r.worker r.ref "broken";
  ignore
    (D.until_status r.creator (id r) "the job to fail" "failed" : Yojson.Safe.t);
  D.cancel r.worker r.ref;
  D.settle ();
  Check.equal "the job's status" (str "failed") (status_word r.creator (id r))

let tests implementations =
  with_dispatcher implementations "job:cancel-requested"
    [
      case "answers false for a job it does not know" unknown;
      case
        "cancels a pending job named by id: answered true, recorded, and never \
         claimable after"
        pending;
      case
        "leaves a running job named by id to its worker, and answers true: it \
         is accepted, not stopped"
        running;
      case "answers false for a finished job, and leaves it as it was" finished;
      case
        "cancels the job a request names and no other, of its type or of \
         another"
        only_named;
      case
        "refuses at the door a request that names no job: a cancellation \
         selects by id, and no other way"
        by_id_only;
    ]
  @ with_dispatcher implementations "job:cancel"
      [
        case "cancels the running job its worker stopped" worker_stopped;
        case "leaves a finished job as it was" finished_stays;
      ]
