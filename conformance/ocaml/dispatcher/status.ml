(* Reading a job (JOBS.md § job:status-requested): what the status carries in
   each state, and a refusal for a job the dispatcher does not know. *)

open Xynolog_conformance
open Support
open Case

let fields =
  [ "startedAt"; "completedAt"; "error"; "failureClass"; "progress"; "result" ]

(* The conditional fields [status] carries. *)
let carried what expected status =
  Check.equal
    (what ^ "'s conditional fields")
    (`List (List.map str expected))
    (`List
       (List.filter_map
          (fun f ->
            if Protocol.at status [ f ] <> None then Some (str f) else None)
          fields))

let unknown w =
  let answer = D.status (D.person w "reader") (Fresh.job_id ()) in
  Check.equal "the reply's channel" (str "job:status-failed")
    (str answer.channel);
  member_is "the reply's message"
    (Some (str (D.refusal_text "job-not-found")))
    (D.message_of answer);
  member_is "the reply's code"
    (Option.map
       (fun code -> str (D.code_word code))
       (D.refusal "job-not-found").code)
    (D.code_of answer)

let every_state w =
  let creator = D.person w "creator" in
  let pending_id = mark creator [ motivation "commenting" ] in
  let pending = D.status_of creator pending_id in
  Check.matching "the pending status"
    [
      ("jobId", str pending_id);
      ("type", str "mark");
      ("status", str "pending");
      ("userId", str creator.did);
    ]
    pending;
  is_time "the pending status's created" pending [ "created" ];
  carried "a pending status" [] pending;
  let worker = D.worker w "reporter" in
  let job = D.claimed worker [ D.marks "commenting" ] in
  carried "a running status"
    [ "startedAt"; "progress" ]
    (D.status_of creator pending_id);
  let complete = D.running w in
  D.complete
    ~result:(`Assoc [ ("found", `Int 1); ("persisted", `Int 1) ])
    complete.worker complete.ref;
  carried "a complete status"
    [ "startedAt"; "completedAt"; "result" ]
    (D.until_status creator complete.ref.job_id "completion" "complete");
  D.fail
    ~extra:[ ("failureClass", str "deterministic") ]
    worker (D.ref_of job) "broken";
  let failed = D.until_status creator pending_id "failure" "failed" in
  carried "a failed status" [ "completedAt"; "error"; "failureClass" ] failed;
  Check.equal "the failed status's class" (str "deterministic")
    (Check.at failed [ "failureClass" ]);
  let cancelled_id = mark creator [ motivation "commenting" ] in
  ignore
    (D.cancel_request creator (`Assoc [ ("jobId", str cancelled_id) ])
      : D.answer);
  carried "a cancelled status" [ "completedAt" ]
    (D.status_of creator cancelled_id)

let tests implementations =
  with_dispatcher implementations "job:status-requested"
    [
      case "answers a job it does not know with Job not found" unknown;
      case
        "carries, in every state, who asked for the job and when, and only the \
         fields that state has"
        every_state;
    ]
