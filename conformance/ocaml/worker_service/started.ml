(* started.test.ts: a worker that has started at the one knowledge base it
   serves (WORKER-SERVICE.md § The agent, § Health): the one sign-in it makes
   as its account there, the stream it asks which knowledge base it is on and
   the one stream of its own it opens, what it says it is called once the
   knowledge base is confirmed, the filters each engine claims with, the
   engine a job is run on and who it is attributed to, and what GET /health
   answers. *)

open Xynolog_conformance
open Support
open Case

let first = List.nth W.ollama_engines 0
let second = List.nth W.ollama_engines 1

(* The reply channels of the operation a worker awaits. *)
let replies operation =
  let op = Protocol.operation_for operation in
  [ op.result; op.failure ]

(* The reply channels of the operations a worker awaits, and the two
   broadcasts it reads. *)
let named_by_a_worker () =
  List.sort compare
    (List.concat_map replies
       [
         "job:claim";
         "mark:commit";
         "browse:annotation-requested";
         "browse:resource-requested";
       ]
    @ [ "job:queued"; "job:cancel-requested" ])

let instant = function `String s -> is_instant s | _ -> false

(* The jobs of a worker of two engines: the first used for two motivations
   of mark jobs, the second for yield jobs. *)
let first_jobs () =
  [ Dispatcher_world.marks "highlighting"; Dispatcher_world.marks "commenting" ]

let second_jobs () = [ Dispatcher_world.yields ]

(* A worker of two engines, started. *)
let two_engines (w : W.t) =
  let served =
    W.start
      ~engines:
        [ W.entry w first (first_jobs ()); W.entry w second (second_jobs ()) ]
      w
  in
  (* Each engine claims once the stream is open, and is told nothing is
     pending. *)
  Run.eventually "both engines to have claimed" 15.0 (fun () ->
      if Dynarray.length w.own.claims >= 2 then Some () else None);
  served

let scope_of = function Some scope -> str scope | None -> `Null

(* In an order of their own, whatever order a party wrote their members in:
   by what [key] reads of each. *)
let sorted_by key items =
  let order item = Yojson.Safe.to_string (Check.canonical (key item)) in
  List.sort (fun a b -> compare (order a) (order b)) items

(* Claims in a fixed order of their own: by the filters they carry. *)
let by_filters = sorted_by (member "accepts")
let by_model = sorted_by (member "model")

let signed_in_as served =
  strs (List.map (fun (s : W.sign_in) -> s.signed_in_as) (W.sign_ins served))

let signs_in_once (w : W.t) =
  let before = grants w.own in
  let served = two_engines w in
  (* One grant of its account, and one question of the gateway, whatever the
     engines it runs. *)
  Check.equal "who it signed in as" (strs [ W.did w.own ]) (signed_in_as served);
  Check.equal "the grants of its account"
    (`Int (before + 1))
    (`Int (grants w.own))

let client_of_its_document (w : W.t) =
  let before = grants w.own in
  (* A client the issuer does not hold: a worker that signed in as it would
     be refused. *)
  let served =
    W.start
      ~unlisted:
        [ ("XYNOLOG_OIDC_CLIENT_ID", "xynolog-worker-of-the-environment") ]
      w
  in
  Check.equal "who it signed in as" (strs [ W.did w.own ]) (signed_in_as served);
  Check.equal "the grants of its account"
    (`Int (before + 1))
    (`Int (grants w.own))

let sorted_texts items = strs (List.sort compare (List.map text_of items))

let opens_its_streams (w : W.t) =
  let served = two_engines w in
  let stream global =
    `Assoc
      [
        ("by", str (W.did w.own));
        ("global", strs (List.sort compare global));
        ("scoped", `List []);
      ]
  in
  Check.equal "the streams it opened"
    (`List
       [
         stream (replies "browse:kb-requested");
         stream (named_by_a_worker () @ [ "job:limits-requested" ]);
       ])
    (`List
       (List.map
          (fun (s : W.opened) ->
            `Assoc
              [
                ("by", by_of s.opened_by);
                ("global", sorted_texts (Check.list s.body [ "global" ]));
                ("scoped", member "scoped" s.body);
              ])
          (W.streams served)));
  Check.equal "which of its streams have ended"
    (`List [ `Bool true; `Bool false ])
    (`List (List.map (fun (s : W.opened) -> `Bool s.closed) (W.streams served)))

let claims_with_its_filters (w : W.t) =
  let served = two_engines w in
  let did = str (W.did w.own) in
  let claims = Dynarray.to_list w.own.claims in
  Check.equal "its claims"
    (`List
       (by_filters
          (List.map
             (fun jobs ->
               `Assoc
                 [ ("by", did); ("accepts", `List jobs); ("answered", `Null) ])
             [ first_jobs (); second_jobs () ])))
    (`List
       (by_filters
          (List.map
             (fun (c : W.claim) ->
               `Assoc
                 [
                   ("by", c.claimed_by);
                   ("accepts", `List c.accepts);
                   ("answered", by_of c.answered);
                 ])
             claims)));
  (* Each claim carries the roles of the agent's account, as its token
     carries them. *)
  List.iter
    (fun (c : W.claim) ->
      Check.equal "the roles its claim carried"
        (strs (W.worker_roles ()))
        c.roles)
    claims;
  (* On the wire a claim is its filters and nothing else, said to no
     scope. *)
  Check.equal "its claims on the wire"
    (`List
       (by_filters
          (List.map
             (fun jobs ->
               `Assoc
                 [
                   ("by", did);
                   ("accepts", `List jobs);
                   ("payload", `Assoc [ ("accepts", `List jobs) ]);
                   ("scope", `Null);
                 ])
             [ first_jobs (); second_jobs () ])))
    (`List
       (by_filters
          (List.map
             (fun (e : W.emitted) ->
               `Assoc
                 [
                   ("by", by_of e.emitted_by);
                   ("accepts", member "accepts" e.payload);
                   ("payload", e.payload);
                   ("scope", scope_of e.scope);
                 ])
             (W.emits ~channel:"job:claim" served))))

let says_its_name (w : W.t) =
  let served = two_engines w in
  Check.equal "what it said it is called"
    (`List
       [
         `Assoc
           [
             ("by", str (W.did w.own));
             ("payload", `Assoc [ ("name", str W.worker_client) ]);
             ("scope", `Null);
             ("status", `Int 202);
           ];
       ])
    (`List (List.map said_on (W.emits ~channel:"principal:profile" served)));
  (* It asked which knowledge base this is, had the answer on its stream,
     and only then said what it is called, and then claimed. *)
  match
    ( W.emits ~channel:"browse:kb-requested" served,
      W.emits ~channel:"principal:profile" served,
      W.emits ~channel:"job:claim" served )
  with
  | asked :: _, profile :: _, claim :: _ ->
      let answer = answer_to served asked in
      Check.equal "the knowledge base it was told it is on"
        (`Assoc
           [
             ("name", str w.own.world.kb.name); ("domain", str (W.domain w.own));
           ])
        (Check.at answer.frame [ "payload"; "response" ]);
      Check.equal "the order of what it did"
        (`List [ `Bool true; `Bool true; `Bool true ])
        (`List
           [
             `Bool (asked.arrived < answer.carried_at);
             `Bool (answer.carried_at < profile.arrived);
             `Bool (profile.arrived < claim.arrived);
           ])
  | _ ->
      failwith
        "it did not ask which knowledge base this is, say what it is called \
         and claim"

let answers_health (w : W.t) =
  let served = two_engines w in
  Check.equal "its health"
    (`Assoc
       [
         ("status", str "ok");
         ( "knowledgeBases",
           `List
             [
               `Assoc
                 [
                   ("domain", str (W.domain w.own));
                   ("state", str "working");
                   ("did", str (W.did w.own));
                   ( "engines",
                     `List
                       [
                         idle_engine first (`List (first_jobs ()));
                         idle_engine second (`List (second_jobs ()));
                       ] );
                 ];
             ] );
       ])
    (health served.worker)

(* What its health port answers [meth path] with; it must answer. *)
let reply_of (served : W.served) meth path =
  match Worker_service.http served.worker meth path with
  | Some reply -> reply
  | None -> failwith (Printf.sprintf "%s %s was not answered" meth path)

(* That [meth path] is answered 404, with no body. *)
let not_found served meth path =
  let reply = reply_of served meth path in
  Check.equal (meth ^ " " ^ path ^ "'s status") (`Int 404) (`Int reply.status);
  Check.equal (meth ^ " " ^ path ^ "'s body") (str "") (str reply.text)

let answers_404 (w : W.t) =
  let served = two_engines w in
  List.iter (not_found served "GET")
    [ "/"; "/api/health"; "/healthz"; "/health/live" ]

let health_alone (w : W.t) =
  let served = two_engines w in
  let plain = health served.worker in
  List.iter
    (fun path ->
      let reply = reply_of served "GET" path in
      Check.equal (path ^ "'s status") (`Int 200) (`Int reply.status);
      Check.equal (path ^ "'s answer") plain
        (match reply.json with
        | Some json -> json
        | None -> failwith (path ^ " was answered with no JSON: " ^ reply.text)))
    [ "/health?probe=1"; "/health?" ];
  (* A query string is no part of another path either. *)
  Check.equal "/healthz?probe=1's status" (`Int 404)
    (`Int (reply_of served "GET" "/healthz?probe=1").status);
  List.iter
    (fun meth -> not_found served meth "/health")
    [ "POST"; "PUT"; "DELETE"; "PATCH"; "HEAD"; "OPTIONS" ]

let first_of what = function item :: _ -> item | [] -> failwith what

(* What the first engine of its knowledge base says of itself in its
   health. *)
let vitals (served : W.served) =
  first_of "its knowledge base states no engine"
    (Check.list
       (first_of "its health states no knowledge base"
          (knowledge_bases_of (health served.worker)))
       [ "engines" ])

(* The jobs its document says the first engine of its knowledge base is
   used for. *)
let jobs_of_its_document (served : W.served) =
  Check.at
    (first_of "its document's knowledge base names no engine"
       (Check.list
          (first_of "its document names no knowledge base"
             (Check.list served.worker.launch.settings [ "knowledgeBases" ]))
          [ "engines" ]))
    [ "jobs" ]

let is_an_instant what value =
  Check.that
    (Printf.sprintf "%s is an instant: %s" what (Check.shown value))
    (instant value)

let says_in_health (w : W.t) =
  let engine = first in
  let served = W.start w in
  Run.eventually "the engine to have claimed" 15.0 (fun () ->
      if Dynarray.length w.own.claims >= 1 then Some () else None);
  Check.equal "what its engine says of itself"
    (idle_engine engine (jobs_of_its_document served))
    (vitals served);
  (* A job is announced: the engine claims it, and holds it while its model
     is silent. *)
  let job = mark_job w.own "held" (params "highlighting") in
  Ollama.script w.ollama [ Ollama.Hold ];
  W.announce w.own job;
  Ollama.asked w.ollama 1;
  let holding = vitals served in
  let active = Check.at holding [ "activeJob" ] in
  (match active with
  | `Assoc members ->
      (match List.assoc_opt "since" members with
      | Some (`String _) -> ()
      | _ ->
          failwith
            ("the job it holds is held since no text: " ^ Check.shown active));
      Check.equal "the job it holds"
        (`Assoc [ ("jobId", str job.id); ("type", str "mark") ])
        (`Assoc (List.remove_assoc "since" members))
  | other -> failwith ("the job it holds is no object: " ^ Check.shown other));
  is_an_instant "since" (Check.at active [ "since" ]);
  List.iter
    (fun stamp -> is_an_instant stamp (Check.at holding [ stamp ]))
    [ "lastQueuedEventAt"; "lastClaimAt"; "lastActivityAt" ];
  Check.equal "lastFinishedAt" `Null (Check.at holding [ "lastFinishedAt" ]);
  Check.equal "jobsCompleted" (`Int 0) (Check.at holding [ "jobsCompleted" ]);
  Ollama.release w.ollama "[]";
  ignore (settled served job : Yojson.Safe.t);
  let after = vitals served in
  Check.equal "activeJob" `Null (Check.at after [ "activeJob" ]);
  Check.equal "jobsCompleted" (`Int 1) (Check.at after [ "jobsCompleted" ]);
  is_an_instant "lastFinishedAt" (Check.at after [ "lastFinishedAt" ])

let runs_on_its_engine (w : W.t) =
  let did = str (W.did w.own) in
  let highlights = mark_job w.own "by-first" (params "highlighting") in
  let comments = mark_job w.own "by-second" (motivated "commenting") in
  (* The two engines work at once, so the provider answers by which model
     asked. *)
  w.ollama.choose <-
    Some
      (fun body ->
        Some
          (proposed
             [
               (if member "model" body = str first.model then
                  exact "the first program"
                else
                  `Assoc
                    [
                      ("exact", str "Charles Babbage");
                      ("comment", str "He designed the engine.");
                    ]);
             ]));
  let served =
    W.start
      ~engines:
        [
          W.entry w first highlighting_only;
          W.entry w second [ Dispatcher_world.marks "commenting" ];
        ]
      w
  in
  ignore (settled served highlights : Yojson.Safe.t);
  ignore (settled served comments : Yojson.Safe.t);
  (* Each claim was answered with the job its filters take, and every claim
     was the one agent's. *)
  Check.equal "the claims answered"
    (`List
       (by_filters
          [
            `Assoc
              [
                ("by", did);
                ("accepts", `List highlighting_only);
                ("answered", str highlights.id);
              ];
            `Assoc
              [
                ("by", did);
                ("accepts", `List [ Dispatcher_world.marks "commenting" ]);
                ("answered", str comments.id);
              ];
          ]))
    (`List
       (by_filters
          (List.filter_map
             (fun (c : W.claim) ->
               Option.map
                 (fun id ->
                   `Assoc
                     [
                       ("by", c.claimed_by);
                       ("accepts", `List c.accepts);
                       ("answered", str id);
                     ])
                 c.answered)
             (Dynarray.to_list w.own.claims))));
  (* Each engine asked its own model, once for its limits and once for the
     job. *)
  Check.equal "what it asked of its models' limits"
    (`List
       (by_model
          [
            `Assoc [ ("model", str first.model) ];
            `Assoc [ ("model", str second.model) ];
          ]))
    (`List (by_model (Dynarray.to_list w.ollama.shows)));
  let asked model =
    Dynarray.of_list
      (List.filter
         (fun (body, _) -> member "model" body = str model)
         (Dynarray.to_list w.ollama.generations))
  in
  expect_generations
    { w.ollama with generations = asked first.model }
    [ generation first.model "highlighting" ];
  Check.equal "the models the second engine's generations named"
    (strs [ second.model ])
    (`List
       (List.map
          (fun (body, _) -> member "model" body)
          (Dynarray.to_list (asked second.model))));
  (* Everything said of a job was said by the agent, and what it made names
     the agent and the engine that made it. *)
  let commits_of (job : W.job) =
    List.filter
      (fun (c : W.commit) -> c.committed_job = str job.id)
      (Dynarray.to_list w.own.commits)
  in
  List.iter
    (fun ((job : W.job), engine) ->
      let said = List.filter (about_job job) (W.emits served) in
      Check.that
        (Printf.sprintf "it said something of %s: it said %d things" job.id
           (List.length said))
        (List.length said > 0);
      Check.equal
        ("who said what it said of " ^ job.id)
        (`List [ did ])
        (`List
           (List.sort_uniq compare
              (List.map (fun (e : W.emitted) -> by_of e.emitted_by) said)));
      Check.equal ("the start of " ^ job.id)
        (`List [ identity job ])
        (`List
           (List.filter
              (fun p -> member "jobId" p = str job.id)
              (W.payloads served "job:start")));
      let commits = commits_of job in
      Check.equal
        ("who committed for " ^ job.id)
        (`List [ did ])
        (`List (List.map (fun (c : W.commit) -> c.committed_by) commits));
      let made =
        Yojson.Safe.Util.to_list
          (without_created
             (`List
                (first_of ("it committed nothing for " ^ job.id) commits)
                  .annotations))
      in
      Check.equal
        ("the generators of what it made for " ^ job.id)
        (`List [ W.generator ~engine w.own ])
        (`List (List.map (member "generator") made)))
    [ (highlights, first); (comments, second) ];
  Check.equal "what it made for its highlights"
    (`List
       [
         text_annotation
           (W.generator ~engine:first w.own)
           highlights.resource_id "highlighting" "6Sa0pcDOQxgkzQu8ZFUzD"
           ~start:23 ~stop:40 ~prefix:"Ada Lovelace published "
           ~suffix:
             " in 1843 \u{2014} a method for computing Bernoulli numbers on \
              the Analytical"
           "the first program";
       ])
    (without_created
       (`List
          (first_of "it committed nothing for its highlights"
             (commits_of highlights))
            .annotations));
  Check.equal "the text from 23 to 40" (str "the first program")
    (str (code_point_slice text 23 40))

let tests implementations =
  in_world implementations "started" ~category:[ "a started worker" ]
    [
      case
        "signs in once, as the client its document names, and is named by the \
         gateway as that account"
        signs_in_once;
      case
        "reads the client it signs in as from its document alone, whatever its \
         environment says"
        client_of_its_document;
      case
        "asks which knowledge base it is on a stream naming that answer alone, \
         and then opens one stream of its own, naming what a worker reads and \
         the request it answers"
        opens_its_streams;
      case
        "claims for each engine with its own filters and no others, as a \
         worker, all as the one agent"
        claims_with_its_filters;
      case
        "says it is called by the client its document names, once, as itself, \
         once the knowledge base is confirmed and before it claims"
        says_its_name;
      case
        "answers GET /health, without a token, with its knowledge base, \
         working there, who it is there, its engines and what each can say of \
         itself"
        answers_health;
      case "answers 404, with no body, anywhere else" answers_404;
      case
        "matches /health without its query string, and answers it by GET alone"
        health_alone;
      case
        "says in /health when an engine heard an announcement, the job it \
         holds, and the jobs it has completed"
        says_in_health;
      case
        "runs each job on the engine whose claim took it, and attributes what \
         it makes to the agent and that engine"
        runs_on_its_engine;
    ]
