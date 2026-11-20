(* A job's life, end to end (JOBS.md § The job record, § Channels):
   admitted, announced, claimed and recorded, completed, and read back. *)

open Xynolog_conformance
open Support
open Case

let end_to_end w =
  let creator = D.person w "alice" in
  let worker = D.worker w "highlighter" in
  let resource_id = D.resource_id () in
  let params = [ ("motivation", str "highlighting"); ("density", `Int 3) ] in
  let job_id = D.created ~resource_id creator "mark" (`Assoc params) in
  Check.equal "the job id's refusals as a JobId" (`List [])
    (`List
       (List.map (fun r -> `String r) (Protocol.against "JobId" (str job_id))));
  let announcement = D.announced w job_id in
  Check.matching "the announcement"
    [
      ("jobId", str job_id);
      ("jobType", str "mark");
      ("resourceId", str resource_id);
      ("userId", str creator.did);
      ("params", `Assoc params);
    ]
    announcement.payload;
  let job = D.claimed worker [ D.marks "highlighting" ] in
  Check.matching "the job's metadata"
    [
      ("id", str job_id);
      ("type", str "mark");
      ("userId", str creator.did);
      ("retryCount", `Int 0);
      ("maxRetries", max_retries "mark");
    ]
    (Check.at job [ "metadata" ]);
  Check.equal "the job's params"
    (`Assoc (("resourceId", str resource_id) :: params))
    (Check.at job [ "params" ]);
  Check.equal "the job's status" (str "running") (Check.at job [ "status" ]);
  Check.equal "the job's progress" (`Assoc []) (Check.at job [ "progress" ]);
  let assignment = D.assigned w job_id in
  Check.matching "the assignment"
    [
      ("jobId", str job_id);
      ("jobType", str "mark");
      ("resourceId", str resource_id);
      ("holder", str worker.did);
      ("requester", str creator.did);
    ]
    assignment.payload;
  Check.that "the assignment carries no correlationId"
    (assignment.correlation_id = None);
  let result = `Assoc [ ("found", `Int 2); ("persisted", `Int 2) ] in
  D.complete ~result worker (D.ref_of job);
  let status = D.until_status creator job_id "the job to complete" "complete" in
  Check.matching "the status"
    [
      ("jobId", str job_id);
      ("type", str "mark");
      ("userId", str creator.did);
      ("startedAt", Check.at job [ "startedAt" ]);
      ("result", result);
    ]
    status;
  has "the status" status [ "completedAt" ]

let tests implementations =
  with_dispatcher implementations "a job from admission to completion"
    [
      case
        "is admitted, announced, claimed, recorded as assigned, completed and \
         read back"
        end_to_end;
    ]
