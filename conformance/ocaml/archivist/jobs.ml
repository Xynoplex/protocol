(* Jobs in the record (ARCHIVIST.md § Attribution, § Vocabulary, profiles and
   jobs): the lifecycle the dispatcher and a worker report, recorded in the
   resource's stream, and a worker's writes attributed to whoever the job was
   assigned for. *)

open Xynolog_conformance
open Support
open Case

let text = "A worker reads this and marks it up.\n"
let whole ?extra = whole ?extra text
let type_of event = Check.at event [ "type" ]
let payload_of event = Check.at event [ "payload" ]

(* Wait for the first event of [id]'s stream that [matches] accepts. *)
let recorded w id what matches =
  ignore
    (Run.eventually what 10.0 (fun () -> List.find_opt matches (A.stored w id))
      : Yojson.Safe.t)

let of_type type_ event = type_of event = str type_

let completed_as verb event =
  of_type "job:completed" event
  && Check.at event [ "payload"; "jobType" ] = str verb

(* A resource, and a job on it assigned to [worker] for [requester], as the
   dispatcher records a claim. *)
let assigned ?(job_type = "mark") w (worker : A.client) (requester : A.client) =
  let id =
    A.created w requester.did
      [
        ("name", "Worked on");
        ("storageUri", Printf.sprintf "file://jobs/%s.md" (Fresh.uuid ()));
      ]
      text
  in
  let job_id = minted_job_id () in
  A.emit (A.sidecar w "dispatcher") "job:assign"
    (`Assoc
       [
         ("jobId", str job_id);
         ("jobType", str job_type);
         ("resourceId", str id);
         ("holder", str worker.did);
         ("requester", str requester.did);
       ]);
  recorded w id "the assignment to be recorded" (of_type "job:assigned");
  (id, job_id)

let worker_roles () = [ Protocol.service_role (); Protocol.worker_role () ]

(* An upload as [worker], holding its roles, as the gateway sends one on. *)
let upload w (worker : A.client) fields content =
  A.upload w ~roles:(worker_roles ()) worker.did fields content

let made_resource (reply : Call.reply) =
  Check.equal "the upload's status" (`Int 200) (`Int reply.status);
  A.string_member "resourceId" (Option.get reply.json)

let job_frame id job_id job_type members =
  `Assoc
    ([
       ("resourceId", str id); ("jobId", str job_id); ("jobType", str job_type);
     ]
    @ members)

let commit ?job_id id annotations =
  `Assoc
    ([ ("resourceId", str id) ]
    @ (match job_id with Some j -> [ ("jobId", str j) ] | None -> [])
    @ [ ("annotations", `List annotations) ])

let ids annotations = `List (List.map (fun a -> str (id_of a)) annotations)
let named did = `Assoc [ ("@id", str did) ]

(* Who a worker's write on a job for [requester] is attributed to. *)
let attributed (requester : A.client) (worker : A.client) =
  [
    ("creator", `Assoc [ ("@type", str "Person"); ("@id", str requester.did) ]);
    ("generator", `Assoc [ ("@type", str "Software"); ("@id", str worker.did) ]);
    ("wasAttributedTo", `List [ named requester.did; named worker.did ]);
  ]

let marks_added w id = List.filter (of_type "mark:added") (A.stored w id)

let lifecycle w =
  let worker = A.worker w "lifecycle" in
  let requester = A.person w "requester" in
  let id, job_id = assigned w worker requester in
  let follower = A.person ~scopes:[ id ] w "follower" in
  (* Each is recorded before the next is sent: the channels are not ordered
     against each other. *)
  A.emit worker "job:start" (job_frame id job_id "mark" []);
  recorded w id "the start" (of_type "job:started");
  A.emit worker "job:fail"
    (job_frame id job_id "mark"
       [
         ("error", str "the model was unavailable");
         ("attempt", `Int 1);
         ("failureClass", str "transient");
         ("willRetry", `Bool true);
       ]);
  recorded w id "the failure" (of_type "job:failed");
  let result =
    `Assoc [ ("found", `Int 2); ("persisted", `Int 1); ("errors", `Int 1) ]
  in
  A.emit worker "job:complete"
    (job_frame id job_id "mark" [ ("attempt", `Int 2); ("result", result) ]);
  recorded w id "the completion" (of_type "job:completed");
  let events = List.tl (A.stored w id) in
  Check.equal "the events"
    (`List
       (List.map
          (fun (t, n) -> `List [ str t; `Int n ])
          [
            ("job:assigned", 2);
            ("job:started", 3);
            ("job:failed", 4);
            ("job:completed", 5);
          ]))
    (`List
       (List.map
          (fun e ->
            `List [ type_of e; Check.at e [ "metadata"; "sequenceNumber" ] ])
          events));
  let event n = List.nth events n in
  matching "the assignment"
    (`Assoc
       [
         ( "payload",
           `Assoc
             [
               ("jobId", str job_id);
               ("jobType", str "mark");
               ("resourceId", str id);
               ("holder", str worker.did);
               ("requester", str requester.did);
             ] );
       ])
    (event 0);
  let by_worker payload =
    `Assoc [ ("userId", str worker.did); ("payload", `Assoc payload) ]
  in
  matching "the start"
    (by_worker [ ("jobId", str job_id); ("jobType", str "mark") ])
    (event 1);
  matching "the failure"
    (by_worker
       [
         ("jobId", str job_id);
         ("error", str "the model was unavailable");
         ("attempt", `Int 1);
         ("failureClass", str "transient");
         ("willRetry", `Bool true);
       ])
    (event 2);
  matching "the completion"
    (by_worker
       [ ("jobId", str job_id); ("attempt", `Int 2); ("result", result) ])
    (event 3);
  (* Each is published to the resource's scope. *)
  let completed =
    Bus_stream.next follower.stream "the scoped completion" (fun m ->
        match m.frame with
        | Some f -> f.channel = "job:completed" && f.scope = Some id
        | None -> false)
  in
  (match completed.frame with
  | Some frame -> (
      Check.equal "the scoped completion's sequence number" (`Int 5)
        (Check.at frame.payload [ "metadata"; "sequenceNumber" ]);
      match Bus_stream.expected_id frame with
      | Persisted expected ->
          Check.equal "the scoped completion's id" (str expected)
            (match completed.id with Some i -> str i | None -> `Null)
      | Reply _ | Ephemeral ->
          failwith "the scoped completion is given no persisted id")
  | None -> failwith "the scoped completion carried no frame");
  (* The view counts them and is otherwise unchanged. *)
  matching "the view"
    (`Assoc
       [
         ("lastSequence", `Int 5);
         ( "annotations",
           `Assoc [ ("version", `Int 5); ("annotations", `List []) ] );
         ("resource", `Assoc [ ("archived", `Bool false) ]);
       ])
    (Option.get (A.view w id))

let counts = `Assoc [ ("found", `Int 2); ("persisted", `Int 2) ]

let made =
  `Assoc
    [
      ("resourceId", str "res-made");
      ("resourceName", str "Made");
      ("truncated", `Bool false);
    ]

(* A completion as the record holds it. *)
let completion job_id verb result =
  `Assoc [ ("jobId", str job_id); ("jobType", str verb); ("result", result) ]

let completions w id =
  `List
    (List.map payload_of
       (List.filter (of_type "job:completed") (A.stored w id)))

(* Frames of one channel are recorded in the order they arrive, so once the
   job's own completion is stored, the one sent before it has been handled. *)
let other_verb (verb, own, other_verb, others) w =
  let worker = A.worker w "other-verb" in
  let requester = A.person w "requester" in
  let id, job_id = assigned ~job_type:verb w worker requester in
  A.emit worker "job:start" (job_frame id job_id verb []);
  recorded w id "the start" (of_type "job:started");
  A.emit worker "job:complete"
    (job_frame id job_id other_verb [ ("result", others) ]);
  A.emit worker "job:complete" (job_frame id job_id verb [ ("result", own) ]);
  recorded w id "the job's own completion" (completed_as verb);
  Check.equal "the completions"
    (`List [ completion job_id verb own ])
    (completions w id);
  (* It says so, in the words the dispatcher says it in. Its log reaches
     this suite by a pipe, which can trail what it has already stored. *)
  let message = (logged "completion-of-another-verb").message in
  let said () =
    List.filter
      (fun line ->
        Re.execp (Re.compile (Re.str message)) line
        && Re.execp (Re.compile (Re.str job_id)) line)
      (A.output w)
  in
  Run.eventually "the Archivist to say so" 10.0 (fun () ->
      if said () <> [] then Some () else None);
  Check.equal "the lines saying so" (`Int 1) (`Int (List.length (said ())))

let unassigned_start w =
  let worker = A.worker w "unassigned" in
  let requester = A.person w "requester" in
  let id =
    A.created w requester.did
      [
        ("name", "Worked on");
        ("storageUri", Printf.sprintf "file://jobs/%s.md" (Fresh.uuid ()));
      ]
      text
  in
  let job_id = minted_job_id () in
  A.emit worker "job:start" (job_frame id job_id "mark" []);
  recorded w id "the start" (of_type "job:started");
  A.emit worker "job:complete"
    (job_frame id job_id "yield" [ ("result", made) ]);
  A.emit worker "job:complete"
    (job_frame id job_id "mark" [ ("result", counts) ]);
  recorded w id "the job's own completion" (completed_as "mark");
  Check.equal "the completions' verbs"
    (`List [ str "mark" ])
    (`List
       (List.map
          (fun e -> Check.at e [ "payload"; "jobType" ])
          (List.filter (of_type "job:completed") (A.stored w id))))

let unheard_of w =
  let worker = A.worker w "unheard-of" in
  let requester = A.person w "requester" in
  (* Another job of the other verb is recorded on the same resource: it says
     nothing of this one. *)
  let id, _ = assigned ~job_type:"mark" w worker requester in
  let job_id = minted_job_id () in
  A.emit worker "job:complete"
    (job_frame id job_id "yield" [ ("result", made) ]);
  recorded w id "the completion" (of_type "job:completed");
  Check.equal "the completions"
    (`List [ completion job_id "yield" made ])
    (completions w id)

let annotations_attributed w =
  let worker = A.worker w "annotator" in
  let requester = A.person w "requester" in
  let id, job_id = assigned w worker requester in
  let batch = [ whole id "worker"; whole id "marks" ] in
  Check.equal "the commit"
    (`Assoc [ ("persisted", `Int 2); ("annotationIds", ids batch) ])
    (A.ask worker "mark:commit" (commit ~job_id id batch));
  let recorded = marks_added w id in
  Check.equal "the annotations recorded" (`Int 2) (`Int (List.length recorded));
  List.iter
    (fun event ->
      Check.equal "the event's userId" (str worker.did)
        (Check.at event [ "userId" ]);
      let annotation = Check.at event [ "payload"; "annotation" ] in
      matching "the annotation"
        (`Assoc (attributed requester worker))
        annotation;
      (* The worker supplied no generator, and its DID says who it is and
         nothing it runs. *)
      List.iter
        (fun said -> lacks "the annotation" annotation [ "generator"; said ])
        [ "name"; "provider"; "model" ])
    recorded

let refused_writes w =
  let worker = A.worker w "refused" in
  let other = A.worker w "another" in
  let requester = A.person w "requester" in
  let id, job_id = assigned w worker requester in
  let before = List.length (A.stored w id) in
  let refusal = refusal ~channel:"mark:commit" in
  Check.equal "the refusal of a write citing no job"
    (str (refusal "uncited-worker" []))
    (str (A.refused worker "mark:commit" (commit id [ whole id "worker" ])));
  let unassigned = minted_job_id () in
  Check.equal "the refusal of a write citing a job never assigned"
    (str
       (refusal "unassigned-job" [ ("jobId", unassigned); ("resourceId", id) ]))
    (str
       (A.refused worker "mark:commit"
          (commit ~job_id:unassigned id [ whole id "worker" ])));
  Check.equal "the refusal of a write citing a job another holds"
    (str
       (refusal "not-the-holder"
          [ ("jobId", job_id); ("holder", worker.did); ("writer", other.did) ]))
    (str
       (A.refused other "mark:commit" (commit ~job_id id [ whole id "worker" ])));
  Check.equal "the stream's length" (`Int before)
    (`Int (List.length (A.stored w id)))

let generators w =
  let worker = A.worker w "generator" in
  let requester = A.person w "requester" in
  let id, job_id = assigned w worker requester in
  let stranger =
    `Assoc
      [
        ("@type", str "Software");
        ("@id", str (Protocol.agent_did w.A.world.kb.domain "someone-else"));
        ("name", str "conformance someone-else");
        ("provider", str "conformance");
        ("model", str "someone-else");
      ]
  in
  let refusal = refusal ~channel:"mark:commit" in
  Check.equal "the refusal of a generator that is not the executor"
    (str
       (refusal "generator-not-executor"
          [
            ("generator", A.string_member "@id" stranger);
            ("executor", worker.did);
          ]))
    (str
       (A.refused worker "mark:commit"
          (commit ~job_id id
             [ whole id "worker" ~extra:[ ("generator", stranger) ] ])));
  Check.equal "the refusal of a generator from a person"
    (str (refusal "executor-not-software" [ ("executor", requester.did) ]))
    (str
       (A.refused requester "mark:commit"
          (commit id [ whole id "worker" ~extra:[ ("generator", stranger) ] ])));
  let many =
    whole id "worker" ~extra:[ ("generator", `List [ stranger; stranger ]) ]
  in
  Check.equal "the refusal of a multi-agent generator"
    (str (refusal "multi-agent-annotation" [ ("annotationId", id_of many) ]))
    (str (A.refused worker "mark:commit" (commit ~job_id id [ many ])))

(* An upload a worker's job must cite, refused with [refusal]'s words. *)
let upload_refused what refusal (reply : Call.reply) =
  let fill = [ ("refusal", refusal) ] in
  Check.equal (what ^ ": its status")
    (`Int (http_status "upload-refused"))
    (`Int reply.status);
  Check.equal (what ^ ": its body")
    (error_body "upload-refused" fill)
    (Option.get reply.json)

let generated_attributed w =
  let worker = A.worker w "writer" in
  let requester = A.person w "requester" in
  let id, job_id = assigned ~job_type:"yield" w worker requester in
  let content = "Generated prose.\n" in
  let generated =
    made_resource
      (upload w worker
         [
           ("name", "Generated");
           ( "storageUri",
             Printf.sprintf "file://jobs/generated-%s.md" (Fresh.uuid ()) );
           ("jobId", job_id);
           ("sourceResourceId", id);
           ("generationPrompt", "Write about it");
         ]
         content)
  in
  let created = List.hd (A.stored w generated) in
  matching "the generated resource's record"
    (`Assoc
       [
         ("type", str "yield:created");
         ("userId", str worker.did);
         ( "payload",
           `Assoc
             (("contentChecksum", str (sha256 content))
             :: attributed requester worker) );
       ])
    created;
  let resource = Check.at (Option.get (A.view w generated)) [ "resource" ] in
  matching "the generated resource"
    (`Assoc
       [
         ("generator", named worker.did);
         ("wasAttributedTo", `List [ named requester.did; named worker.did ]);
       ])
    resource;
  (* Its record names the job, and the resource it was generated from, with
     no annotation named. *)
  Check.equal "the record's job" (str job_id)
    (Check.at created [ "payload"; "jobId" ]);
  Check.equal "the record's source"
    (`Assoc [ ("resourceId", str id) ])
    (Check.at created [ "payload"; "generatedFrom" ]);
  matching "the generated resource"
    (`Assoc [ ("wasDerivedFrom", str id) ])
    resource;
  (* Without the job, or without the resource the job was assigned on, a
     worker's upload is not recorded. *)
  let refusal = refusal ~channel:"yield:create" in
  upload_refused "an upload citing no job"
    (refusal "uncited-worker" [])
    (upload w worker
       [
         ("name", "Uncited");
         ( "storageUri",
           Printf.sprintf "file://jobs/uncited-%s.md" (Fresh.uuid ()) );
       ]
       content);
  upload_refused "an upload naming no source"
    (refusal "sourceless-create" [])
    (upload w worker
       [
         ("name", "Sourceless");
         ( "storageUri",
           Printf.sprintf "file://jobs/sourceless-%s.md" (Fresh.uuid ()) );
         ("jobId", job_id);
       ]
       content)

(* What [worker] makes under [job_id], generated from [id]: the new
   resource. *)
let made_under w worker ~name ~prefix id job_id =
  made_resource
    (upload w worker
       [
         ("name", name);
         ( "storageUri",
           Printf.sprintf "file://jobs/%s-%s.md" prefix (Fresh.uuid ()) );
         ("jobId", job_id);
         ("sourceResourceId", id);
       ]
       text)

let annotations_on_made w =
  let worker = A.worker w "citer" in
  let requester = A.person w "requester" in
  let id, job_id = assigned ~job_type:"yield" w worker requester in
  let generated = made_under w worker ~name:"Made" ~prefix:"made" id job_id in
  let batch = [ whole generated "worker"; whole generated "marks" ] in
  Check.equal "the commit"
    (`Assoc [ ("persisted", `Int 2); ("annotationIds", ids batch) ])
    (A.ask worker "mark:commit" (commit ~job_id generated batch));
  let recorded = marks_added w generated in
  Check.equal "the annotations recorded" (`Int 2) (`Int (List.length recorded));
  List.iter
    (fun event ->
      Check.equal "the event's userId" (str worker.did)
        (Check.at event [ "userId" ]);
      matching "the annotation"
        (`Assoc (attributed requester worker))
        (Check.at event [ "payload"; "annotation" ]))
    recorded

let trespass w =
  let worker = A.worker w "trespasser" in
  let other = A.worker w "bystander" in
  let requester = A.person w "requester" in
  let id, job_id = assigned ~job_type:"yield" w worker requester in
  let generated = made_under w worker ~name:"Made" ~prefix:"made" id job_id in
  let refusal = refusal ~channel:"mark:commit" in
  let unassigned resource =
    refusal "unassigned-job" [ ("jobId", job_id); ("resourceId", resource) ]
  in
  (* A resource the job has nothing to do with. *)
  let elsewhere =
    A.created w requester.did
      [
        ("name", "Elsewhere");
        ( "storageUri",
          Printf.sprintf "file://jobs/elsewhere-%s.md" (Fresh.uuid ()) );
      ]
      text
  in
  Check.equal "the refusal on a resource the job has nothing to do with"
    (str (unassigned elsewhere))
    (str
       (A.refused worker "mark:commit"
          (commit ~job_id elsewhere [ whole elsewhere "worker" ])));
  (* A resource the same worker made under another job. *)
  let second_id, second_job = assigned ~job_type:"yield" w worker requester in
  let second_made =
    made_under w worker ~name:"Made by another job" ~prefix:"second" second_id
      second_job
  in
  Check.equal "the refusal on a resource another job made"
    (str (unassigned second_made))
    (str
       (A.refused worker "mark:commit"
          (commit ~job_id second_made [ whole second_made "worker" ])));
  (* The resource the job made, written on by another worker citing the
     job. *)
  Check.equal "the refusal of another writer on the resource the job made"
    (str
       (refusal "not-the-holder"
          [ ("jobId", job_id); ("holder", worker.did); ("writer", other.did) ]))
    (str
       (A.refused other "mark:commit"
          (commit ~job_id generated [ whole generated "worker" ])));
  List.iter
    (fun resource ->
      Check.equal
        ("the annotations recorded on " ^ resource)
        (`List [])
        (`List (marks_added w resource)))
    [ elsewhere; second_made; generated ]

let known_old form =
  Printf.sprintf
    "the Archivist at images bb38209 writes %s, where archivist/messages.json \
     says mark:commit refused: …"
    form

let tests implementations =
  with_archivist implementations "a job, in the record"
    ([ case "has its lifecycle recorded in its resource's stream" lifecycle ]
    @ List.map
        (fun (what, verb, own, other, others) ->
          case
            (Printf.sprintf
               "records no completion of another verb than the job's: %s, each \
                well formed for its own verb"
               what)
            (other_verb (verb, own, other, others)))
        [
          ( "a mark job, and a yield job's completion",
            "mark",
            counts,
            "yield",
            made );
          ( "a yield job, and a mark job's completion",
            "yield",
            made,
            "mark",
            counts );
        ]
    @ [
        case
          "learns a job's verb from its start when it recorded no assignment \
           of it"
          unassigned_start;
        case
          "records a completion of a job it has recorded nothing of, whichever \
           verb's it is: it refuses only what its own record shows is another \
           verb's"
          unheard_of;
        case
          "attributes a worker's annotations to whoever the job was assigned \
           for, with the worker as generator"
          annotations_attributed;
        case
          ~known:(known_old "refused: cites job …")
          "refuses a worker's write that cites no job, a job never assigned, \
           or a job another holds"
          refused_writes;
        case
          ~known:(known_old "attribution: generator …")
          "refuses a generator that is not the executor, and one from a person"
          generators;
        case
          "attributes a generated resource to whoever the generation was \
           assigned for"
          generated_attributed;
        case
          "attributes a worker's annotations on the resource its job made to \
           whoever the job was assigned for"
          annotations_on_made;
        case
          ~known:(known_old "refused: cites job …")
          "refuses a write citing a job on a resource it neither was assigned \
           on nor made, on one another job made, and from another writer on \
           one it made"
          trespass;
      ])
