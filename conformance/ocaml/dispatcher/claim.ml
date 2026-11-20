(* Claiming (JOBS.md § job:claim, § job:assign): only a worker may claim; a
   claim names the jobs it takes by fields of the job description and is
   atomic; nothing to claim is a decline; an accepted claim hands out the
   whole running record and is recorded as an assignment. *)

open Xynolog_conformance
open Support
open Case

(* What a refusal dispatcher/messages.json names carries: its code, when it
   has one, and its words. *)
let refused_as name =
  (match (D.refusal name).code with
    | Some code -> [ ("code", str (D.code_word code)) ]
    | None -> [])
  @ [ ("message", str (D.refusal_text name)) ]

let whole_record w =
  let creator = D.person w "creator" in
  let worker = D.worker w "claimant" in
  let resource_id = D.resource_id () in
  let params =
    [
      motivation "linking";
      ("entityTypes", `List [ str "Place" ]);
      ("includeDescriptiveReferences", `Bool true);
    ]
  in
  let admitted = D.create ~resource_id creator "mark" (`Assoc params) in
  let job_id = D.job_id_of admitted in
  let job = D.claimed worker [ D.marks "linking" ] in
  Check.matching "the claimed job"
    [
      ("status", str "running");
      ("params", `Assoc (("resourceId", str resource_id) :: params));
      ("progress", `Assoc []);
    ]
    job;
  Check.matching "the claimed job's metadata"
    [
      ("id", str job_id);
      ("type", str "mark");
      ("userId", str creator.did);
      ("retryCount", `Int 0);
      ("maxRetries", max_retries "mark");
    ]
    (Check.at job [ "metadata" ]);
  is_time "the job's startedAt" job [ "startedAt" ];
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
  (* Emitted as the dispatcher, the principal every reply of its comes
     from. *)
  Check.equal "the assignment's _userId"
    (Check.at admitted.payload [ "_userId" ])
    (Check.at assignment.payload [ "_userId" ]);
  Check.that "the assignment's _userId is not the worker's"
    (Check.at assignment.payload [ "_userId" ] <> str worker.did)

let not_a_worker who w =
  let creator = D.person w "creator" in
  let job_id = mark creator [ motivation "highlighting" ] in
  let answer = D.claim (who w) (D.every_job ()) in
  Check.equal "the reply's channel" (str "job:claim-failed")
    (str answer.channel);
  Check.matching "the refusal" (refused_as "not-a-worker") answer.payload;
  Check.equal "the job's status" (str "pending")
    (str (D.status_now creator job_id));
  Check.equal "the job a worker claims" (str job_id)
    (str (D.id_of (D.claimed (D.worker w "rightful") (D.every_job ()))))

let roles_from_token w =
  let person = D.person w "pretender" in
  ignore (mark (D.person w "creator") [ motivation "highlighting" ] : string);
  let answer =
    D.request person "job:claim"
      (`Assoc
         [
           ("accepts", `List (D.every_job ()));
           ("_roles", `List [ str (Protocol.worker_role ()) ]);
         ])
  in
  Check.matching "the refusal" (refused_as "not-a-worker") answer.payload

let assignments (w : D.t) =
  List.length (Bus_stream.frames ~channel:"job:assign" w.observer)

let declines w =
  let before = assignments w in
  let answer = D.claim (D.worker w "early") (D.every_job ()) in
  Check.equal "the reply's channel" (str "job:claim-failed")
    (str answer.channel);
  Check.matching "the decline" (refused_as "none-pending") answer.payload;
  Check.equal "the assignments recorded" (`Int before) (`Int (assignments w))

let one_motivation w =
  let creator = D.person w "creator" in
  let worker = D.worker w "tagger" in
  let highlighting = mark creator [ motivation "highlighting" ] in
  let yielding = D.created creator "yield" (D.generation (D.resource_id ())) in
  declined "the claim's code" (D.claim worker [ D.marks "tagging" ]);
  Check.equal "the highlighting job's status" (str "pending")
    (str (D.status_now creator highlighting));
  Check.equal "the yield job's status" (str "pending")
    (str (D.status_now creator yielding));
  let tagging =
    D.created ~resource_id:(D.resource_id ()) creator "mark"
      (`Assoc
         [
           motivation "tagging";
           ("schemaId", str "irac");
           ("categories", `List [ str "Issue" ]);
         ])
  in
  Check.equal "the job claimed" (str tagging)
    (str (D.id_of (D.claimed worker [ D.marks "tagging" ])));
  Check.equal "the highlighting job's status" (str "pending")
    (str (D.status_now creator highlighting))

let yields_only w =
  let creator = D.person w "creator" in
  let worker = D.worker w "generator" in
  let commenting = mark creator [ motivation "commenting" ] in
  declined "the claim's code" (D.claim worker [ D.yields ]);
  let yielding = D.created creator "yield" (D.generation (D.resource_id ())) in
  Check.equal "the job claimed" (str yielding)
    (str (D.id_of (D.claimed worker [ D.yields ])));
  Check.equal "the commenting job's status" (str "pending")
    (str (D.status_now creator commenting))

let any_filter w =
  let creator = D.person w "creator" in
  let worker = D.worker w "picky" in
  let job_id = mark creator [ motivation "commenting" ] in
  declined "the claim's code" (D.claim worker [ D.marks "tagging"; D.yields ]);
  Check.equal "the job's status" (str "pending")
    (str (D.status_now creator job_id));
  Check.equal "the job claimed" (str job_id)
    (str
       (D.id_of (D.claimed worker [ D.marks "tagging"; D.marks "commenting" ])))

let filter job_type params =
  `Assoc [ ("jobType", str job_type); ("params", `Assoc params) ]

let at_the_door =
  [
    ("names no job", `Assoc [ ("accepts", `List []) ]);
    ( "names mark jobs of no motivation",
      `Assoc [ ("accepts", `List [ `Assoc [ ("jobType", str "mark") ] ]) ] );
    ( "names a motivation that is none",
      `Assoc
        [ ("accepts", `List [ filter "mark" [ motivation "unheard-of" ] ]) ] );
    ( "names jobs by a type alone, as claims once did",
      `Assoc [ ("types", `List [ str "mark" ]) ] );
    ( "states a field no filter states",
      `Assoc
        [
          ( "accepts",
            `List [ filter "yield" [ ("title", str "A generated resource") ] ]
          );
        ] );
  ]

let one_of_many w =
  let creator = D.person w "creator" in
  let workers =
    List.map
      (fun n -> D.worker w (Printf.sprintf "racer-%d" n))
      [ 1; 2; 3; 4; 5 ]
  in
  for _ = 1 to 3 do
    let job_id = mark creator [ motivation "highlighting" ] in
    let answers =
      Eio.Fiber.List.map (fun c -> D.claim c [ D.marks "highlighting" ]) workers
    in
    let won = List.filter (fun (a : D.answer) -> a.ok) answers in
    Check.equal "the claims that won" (`Int 1) (`Int (List.length won));
    Check.equal "the job won" (str job_id)
      (Check.at (List.hd won).payload [ "response"; "metadata"; "id" ]);
    List.iter
      (fun (a : D.answer) ->
        if not a.ok then declined "the code of a claim that lost" a)
      answers
  done

let two_of_two w =
  let creator = D.person w "creator" in
  let first = mark creator [ motivation "assessing" ] in
  let second = mark creator [ motivation "assessing" ] in
  let left = D.worker w "left" in
  let right = D.worker w "right" in
  let claimed =
    Eio.Fiber.List.map
      (fun c -> D.id_of (D.claimed c [ D.marks "assessing" ]))
      [ left; right ]
  in
  Check.equal "the jobs claimed"
    (`List (List.map str (List.sort compare [ first; second ])))
    (`List (List.map str (List.sort compare claimed)))

let tests implementations =
  with_dispatcher implementations "job:claim"
    ([
       case
         "hands a worker the whole running record, then records the assignment \
          under the dispatcher's name"
         whole_record;
     ]
    @ List.map
        (fun (who, make) ->
          case
            (Printf.sprintf
               "refuses %s, before the queue is consulted: the pending job \
                stays for a worker"
               who)
            (not_a_worker make))
        [
          ("a person", fun w -> D.person w "not-a-worker");
          ( "a sidecar holding the service role only",
            fun w -> D.sidecar w "not-a-worker" );
        ]
    @ [
        case
          "refuses a person who writes the worker role into the claim: roles \
           come from the token alone"
          roles_from_token;
        case "declines when nothing is pending, and records no assignment"
          declines;
        case
          "hands a claim for one motivation no job of another, and no yield job"
          one_motivation;
        case "hands a claim for yield jobs no mark job" yields_only;
        case "hands over a job that matches any one of a claim's filters"
          any_filter;
      ]
    @ List.map
        (fun (what, claim) ->
          case (Printf.sprintf "refuses at the door a claim that %s" what)
            (fun w -> refused_at_door (D.worker w "vague") "job:claim" claim))
        at_the_door
    @ [
        case "gives one pending job to exactly one of many simultaneous claims"
          one_of_many;
        case "gives two pending jobs to two simultaneous claims, one each"
          two_of_two;
      ])
