(* stop.test.ts: stopping (WORKER-SERVICE.md § Stopping): what a worker does
   on SIGTERM, idle and while it holds a job; the one refusal of a claim that
   refuses the knowledge base it was made at; and the exit once every
   knowledge base it serves has refused it, one having been confirmed first
   (§ Knowledge bases). *)

open Xynolog_conformance
open Support
open Case

(* How the dispatcher refuses a claim from an account that is no worker's. *)
let unauthorized () = Some (Some "unauthorized", "This agent is not a worker")
let refusing (kb : W.kb) refuse = kb.hooks <- { kb.hooks with claim = refuse }

let claimed_at what (kbs : W.kb list) =
  Run.eventually what 15.0 (fun () ->
      if List.for_all (fun (kb : W.kb) -> Dynarray.length kb.claims >= 1) kbs
      then Some ()
      else None)

let highlighting_job kb name =
  mark_job kb name (`Assoc [ ("motivation", str "highlighting") ])

let exit_code code =
  match code with Some code -> `Int code | None -> `String "a signal"

(* Its exit code once it has exited of its own accord. *)
let exited (served : W.served) =
  match
    Eio.Time.with_timeout (Run.clock ()) 30.0 (fun () ->
        Ok (Eio.Promise.await served.worker.process.exited))
  with
  | Ok code -> exit_code code
  | Error `Timeout -> failwith "the worker did not exit within 30 s"

let stopped (served : W.served) = exit_code (Worker_service.stop served.worker)

let refusals (served : W.served) =
  logged (Worker_service.stdout served.worker) (kb_log "refused")

(* A line its log wrote of a refused knowledge base: an error, of [kb], with
   a reason. *)
let refusal_of (kb : W.kb) line =
  Check.like "the refusal its log wrote"
    (`Assoc [ ("level", str "error"); ("domain", str (W.domain kb)) ])
    line;
  Check.that "the refusal says its reason"
    (match member "reason" line with `String _ -> true | _ -> false)

let says_nothing_on_stderr (served : W.served) =
  Check.equal "what it wrote on stderr" (`List [])
    (strs (Worker_service.stderr served.worker))

let idle (w : W.t) =
  let served = W.start w in
  claimed_at "the worker to have claimed" [ w.own ];
  let said = List.length (W.emits served) in
  Check.equal "its exit code" (`Int 0) (stopped served);
  Check.equal "what it said on the bus after SIGTERM" (`List [])
    (`List
       (List.map
          (fun (e : W.emitted) -> str e.channel)
          (List.filteri (fun at _ -> at >= said) (W.emits served))));
  Check.that "it answers /health no more"
    (Option.is_none (Worker_service.http served.worker "GET" "/health"))

let holding (w : W.t) =
  let job = highlighting_job w.own "stopped" in
  (* The model never answers: the worker holds the job when it is told to
     stop. *)
  Ollama.script w.ollama [ Hold ];
  let served = W.start w in
  Ollama.asked w.ollama 1;
  Check.equal "its exit code" (`Int 0) (stopped served);
  let failures = W.emits ~channel:"job:fail" served in
  Check.equal "the gateway's answers to its failures"
    (`List [ `Int 202 ])
    (`List
       (List.map
          (fun (e : W.emitted) ->
            match e.status with Some s -> `Int s | None -> `Null)
          failures));
  let failure, error = failure_of (List.hd failures).payload in
  (* No class is stated: the job's retry budget decides. *)
  Check.equal "its failure" (failed ~will_retry:true job) failure;
  Check.that
    (Printf.sprintf "its error says it stopped: %s" error)
    (Check.matches_re "stopped" error);
  said_nothing served [ "job:complete" ];
  (* Having failed the job, it claims no other. *)
  Check.equal "its claims" (`Int 1)
    (`Int (List.length (W.emits ~channel:"job:claim" served)))

let refused_alone (w : W.t) =
  let served = W.start w in
  claimed_at "the worker to have claimed" [ w.own ];
  (* The next claim is refused as the dispatcher refuses one from an account
     that is no worker's: its grant is gone, and will not come back. *)
  refusing w.own (Some unauthorized);
  let job = highlighting_job w.own "unauthorized" in
  W.announce w.own job;
  Check.equal "its exit code" (`Int 1) (exited served);
  ignore (Worker_service.stop served.worker : int option);
  said_nothing served [ "job:start" ];
  (match refusals served with
  | [ line ] -> refusal_of w.own line
  | lines ->
      failwith
        (Printf.sprintf "its log says %d times that a knowledge base refused it"
           (List.length lines)));
  (* The knowledge base was confirmed first, so the service had started:
     everything it wrote is its log. *)
  says_nothing_on_stderr served

let refused_one_then_the_other (w : W.t) =
  let second = W.second w in
  let served =
    W.start
      ~serving:
        [
          ( second,
            [
              W.entry w (List.hd W.ollama_engines)
                (Dispatcher_world.every_job ());
            ],
            None );
        ]
      w
  in
  claimed_at "the worker to have claimed at both" [ w.own; second ];
  refusing second (Some unauthorized);
  W.announce second (highlighting_job second "unauthorized-there");
  let there = W.at served second in
  Client_proxy.until ~timeout:15.0 there.proxy "its stream there to end"
    (fun () ->
      if List.for_all (fun (s : W.opened) -> s.closed) (W.streams there) then
        Some ()
      else None);
  Run.eventually "the refusal in its log" 15.0 (fun () ->
      if refusals served <> [] then Some () else None);
  (match refusals served with
  | [ line ] -> refusal_of second line
  | lines ->
      failwith
        (Printf.sprintf "its log says %d times that a knowledge base refused it"
           (List.length lines)));
  let state kb =
    `Assoc
      (List.filter_map
         (fun name -> Option.map (fun v -> (name, v)) (Protocol.at kb [ name ]))
         [ "domain"; "state"; "did" ])
  in
  Check.equal "the knowledge bases its health names"
    (`List
       [
         `Assoc
           [
             ("domain", str (W.domain w.own));
             ("state", str "working");
             ("did", str (W.did w.own));
           ];
         `Assoc [ ("domain", str (W.domain second)); ("state", str "refused") ];
       ])
    (`List (List.map state (knowledge_bases_of (health served.worker))));
  (* The other knowledge base's work goes on, and the refused one is claimed
     at no more. *)
  let claimed_there = Dynarray.length second.claims in
  let job = highlighting_job w.own "still-here" in
  Ollama.script w.ollama [ answer "[]" ];
  W.announce w.own job;
  ignore (settled served job : Yojson.Safe.t);
  Check.equal "its claims at the refused one" (`Int claimed_there)
    (`Int (Dynarray.length second.claims));
  (* The last knowledge base not refused refuses it: it exits. *)
  refusing w.own (Some unauthorized);
  W.announce w.own (highlighting_job w.own "unauthorized-here");
  Check.equal "its exit code" (`Int 1) (exited served);
  ignore (Worker_service.stop served.worker : int option);
  Check.equal "the knowledge bases its log says refused it"
    (strs [ W.domain second; W.domain w.own ])
    (`List (List.map (fun line -> member "domain" line) (refusals served)));
  says_nothing_on_stderr served

let refused_otherwise (w : W.t) =
  let served = W.start w in
  claimed_at "the worker to have claimed" [ w.own ];
  refusing w.own
    (Some
       (fun () -> Some (Some "peer-unavailable", "The queue cannot be reached")));
  let job = highlighting_job w.own "refused-claim" in
  W.announce w.own job;
  Run.eventually "the refused claim" 15.0 (fun () ->
      if Dynarray.length w.own.claims >= 2 then Some () else None);
  (* It is still up, and the next announcement is claimed and run. *)
  Check.equal "the state of its knowledge base" (str "working")
    (member "state" (List.hd (knowledge_bases_of (health served.worker))));
  w.own.hooks <- W.no_hooks;
  Ollama.script w.ollama [ answer "[]" ];
  W.announce w.own job;
  ignore (W.emitted ~matches:(about_job job) served "job:complete" : W.emitted)

let tests implementations =
  in_world implementations "stop" ~category:[ "a worker that stops" ]
    [
      case
        "exits 0 on SIGTERM when it holds nothing, having said nothing more on \
         the bus, and answers /health no more"
        idle;
      case
        "fails the job it holds before it exits 0 on SIGTERM, saying that the \
         worker stopped and whether the job will be retried"
        holding;
      case
        "refuses its one knowledge base when a claim there is refused as \
         unauthorized, and exits 1, saying so in its log and nothing on stderr"
        refused_alone;
      case
        "refuses only the knowledge base a claim was refused at as \
         unauthorized, goes on at the other, and exits 1 once that one refuses \
         it too"
        refused_one_then_the_other;
      case "goes on claiming after a claim refused for any other reason"
        refused_otherwise;
    ]
