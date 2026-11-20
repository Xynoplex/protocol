(* knowledge-bases.test.ts: a worker of two knowledge bases (WORKER-SERVICE.md
   § The agent, § Knowledge bases, § Health). At each it signs in as the
   client its document names there, asks which knowledge base it reached,
   says what it is called, and runs the jobs that knowledge base hands it, as
   the agent it is there. One that answers as another, or refuses it, is
   refused alone. One whose stream drops keeps its work there, and one in
   trouble is started again alone. Its health answers once one is
   confirmed. *)

open Xynolog_conformance
open Support
open Case

let here = List.nth W.ollama_engines 0
let there = List.nth W.ollama_engines 1
let reconnect_ms () = Protocol.client_timing "reconnectMs"

(* The id of the job the first engine of a knowledge base holds, as
   GET /health says it. *)
let held_by kb =
  match Protocol.at kb [ "engines"; "0"; "activeJob"; "jobId" ] with
  | Some (`String id) -> Some id
  | _ -> (
      match Check.list kb [ "engines" ] with
      | engine :: _ -> (
          match Protocol.at engine [ "activeJob"; "jobId" ] with
          | Some (`String id) -> Some id
          | _ -> None)
      | [] -> None)

let held_is what (job : W.job) kb =
  Check.equal what (str job.id)
    (match held_by kb with Some id -> str id | None -> `Null)

(* Whether the worker's health gives any answer within [seconds], asked
   again and again, as whatever supervises it asks. *)
let health_answers_within (served : W.served) seconds =
  let deadline = Run.now () +. seconds in
  let rec ask () =
    let remaining = deadline -. Run.now () in
    if remaining <= 0.0 then false
    else
      match
        Eio.Time.with_timeout (Run.clock ()) remaining (fun () ->
            Ok (Worker_service.http served.worker "GET" "/health"))
      with
      | Ok (Some _) -> true
      | Ok None ->
          Run.sleep 0.1;
          ask ()
      | Error `Timeout -> false
  in
  ask ()

(* Once its confirmation has been asked for, a knowledge base answers only
   when the function this returns is called. *)
let hold_confirmation (kb : W.kb) =
  let released, release = Eio.Promise.create () in
  kb.hooks <-
    {
      kb.hooks with
      confirm =
        Some
          (fun () ->
            Eio.Promise.await released;
            None);
    };
  fun () -> Eio.Promise.resolve release ()

(* A worker of the world's knowledge base and the second, one engine at
   each, each used for highlighting: the world's first engine at the world's,
   its second at the second. *)
let serving_both ?client ?(spawn = false) (w : W.t) second =
  let highlighting = [ Dispatcher_world.marks "highlighting" ] in
  let engines = [ W.entry w here highlighting ]
  and serving = [ (second, [ W.entry w there highlighting ], client) ] in
  if spawn then W.spawn ~engines ~serving w else W.start ~engines ~serving w

(* Each knowledge base's sign-ins, by the DID the gateway named the worker
   by. *)
let signed_in_as served kbs =
  `List
    (List.map
       (fun kb ->
         strs
           (List.map
              (fun (s : W.sign_in) -> s.signed_in_as)
              (W.sign_ins (W.at served kb))))
       kbs)

let grants (kb : W.kb) =
  Option.value ~default:0 (Hashtbl.find_opt kb.world.issuer.grants kb.client)

let both_claimed_at (w : W.t) second =
  Run.eventually "both knowledge bases to have been claimed at" 15.0 (fun () ->
      if
        Dynarray.length w.own.claims >= 1
        && Dynarray.length second.W.claims >= 1
      then Some ()
      else None)

let opened_by (s : W.opened) =
  match s.opened_by with Some did -> str did | None -> `Null

let streams_by served = `List (List.map opened_by (W.streams served))

let lines (served : W.served) name =
  logged (Worker_service.stdout served.worker) (kb_log name)

let domains_of entries =
  strs
    (List.sort compare
       (List.map (fun e -> text_of (Check.at e [ "domain" ])) entries))

let states (served : W.served) = knowledge_bases_of (health served.worker)

(* Hold a line its log wrote of a knowledge base: at [level], of [domain],
   with each of [said] a text. *)
let logs_of ~level ~domain ?(said = []) line =
  Check.like "the line its log wrote"
    (`Assoc [ ("level", str level); ("domain", str domain) ])
    line;
  List.iter
    (fun name ->
      Check.that
        (Printf.sprintf "the line says %s" name)
        (match Protocol.at line [ name ] with
        | Some (`String _) -> true
        | _ -> false))
    said

let working (kb : W.kb) state =
  Check.like
    ("the state of " ^ W.domain kb)
    (`Assoc
       [
         ("domain", str (W.domain kb));
         ("state", str "working");
         ("did", str (W.did kb));
       ])
    state

(* Nothing it wrote carries [secrets]. *)
let keeps_secret (served : W.served) secrets =
  let written = String.concat "\n" (Worker_service.output served.worker) in
  List.iter
    (fun secret ->
      Check.that "what it wrote carries no secret"
        (not (Re.execp (Re.compile (Re.str secret)) written)))
    secrets

let key (w : W.t) = List.assoc "XYNOLOG_OIDC_CLIENT_KEY" w.env

let signs_in_once (w : W.t) =
  let second = W.second w in
  let before = [ grants w.own; grants second ] in
  let served = serving_both w second in
  both_claimed_at w second;
  Check.equal "who it signed in as at each"
    (`List [ strs [ W.did w.own ]; strs [ W.did second ] ])
    (signed_in_as served [ w.own; second ]);
  Check.equal "the grants of each issuer"
    (`List (List.map (fun n -> `Int (n + 1)) before))
    (`List [ `Int (grants w.own); `Int (grants second) ]);
  (* At each, the stream it asks which knowledge base it is on, and then one
     of its own, opened as the agent it is there. *)
  Check.equal "who opened each stream at each"
    (`List
       [
         strs [ W.did w.own; W.did w.own ]; strs [ W.did second; W.did second ];
       ])
    (`List (List.map (fun kb -> streams_by (W.at served kb)) [ w.own; second ]))

let subscriptions (at : W.served) =
  List.filter
    (fun (r : Client_proxy.request) -> r.path = "/bus/subscribe")
    (W.requests at)

(* The answer to the worker's question of which knowledge base this is, as
   its stream carried it. *)
let answer_to (at : W.served) (asked : W.emitted) =
  match
    List.find_opt
      (fun (c : W.carried) ->
        Some (member "correlationId" c.frame)
        = Option.map str asked.correlation_id)
      (W.carried at "browse:kb-result")
  with
  | Some answer -> answer
  | None -> failwith "its question was answered on no stream it read"

let said_on (e : W.emitted) =
  `Assoc
    [
      ("by", match e.emitted_by with Some d -> str d | None -> `Null);
      ("payload", e.payload);
      ("scope", match e.scope with Some s -> str s | None -> `Null);
      ("status", match e.status with Some s -> `Int s | None -> `Null);
    ]

let asks_once (w : W.t) =
  let second = W.second w in
  let served = serving_both w second in
  both_claimed_at w second;
  List.iter
    (fun (kb : W.kb) ->
      let at = W.at served kb in
      Check.equal
        ("what it asked " ^ W.domain kb)
        (`List
           [
             `Assoc
               [
                 ("by", str (W.did kb));
                 ("payload", `Assoc []);
                 ("scope", `Null);
                 ("status", `Int 202);
               ];
           ])
        (`List (List.map said_on (W.emits ~channel:"browse:kb-requested" at)));
      (* Asked once the stream it asks on is open, so that the answer comes
         back on it; its own is opened once it has been answered, so that no
         request it answers reaches it before it can, and the stream it asked
         on is closed. *)
      match (subscriptions at, W.emits ~channel:"browse:kb-requested" at) with
      | asking :: own :: _, asked :: _ ->
          let answer = answer_to at asked in
          Check.equal
            ("the order of what it did at " ^ W.domain kb)
            (`List [ `Bool true; `Bool true; `Bool true; `Bool false ])
            (`List
               [
                 `Bool
                   (match asking.answered with
                   | Some answered -> answered < asked.arrived
                   | None -> false);
                 `Bool (answer.carried_at < own.arrived);
                 `Bool asking.closed;
                 `Bool own.closed;
               ])
      | _ -> failwith ("it opened two streams and asked once at " ^ W.domain kb))
    [ w.own; second ];
  let confirmed = lines served "confirmed" in
  Check.equal "the knowledge bases its log says are confirmed"
    (domains_of
       (List.map
          (fun d -> `Assoc [ ("domain", str d) ])
          [ W.domain w.own; W.domain second ]))
    (domains_of confirmed);
  List.iter
    (fun line ->
      let kb =
        if Check.at line [ "domain" ] = str (W.domain w.own) then w.own
        else second
      in
      Check.like "the line its log wrote"
        (`Assoc
           [
             ("level", str "info");
             ("domain", str (W.domain kb));
             ("did", str (W.did kb));
           ])
        line)
    confirmed

let says_its_name (w : W.t) =
  let second = W.second w in
  let served = serving_both w second in
  both_claimed_at w second;
  List.iter
    (fun ((kb : W.kb), client) ->
      let at = W.at served kb in
      Check.equal
        ("what it said it is called at " ^ W.domain kb)
        (`List
           [
             `Assoc
               [
                 ("by", str (W.did kb));
                 ("payload", `Assoc [ ("name", str client) ]);
                 ("scope", `Null);
                 ("status", `Int 202);
               ];
           ])
        (`List (List.map said_on (W.emits ~channel:"principal:profile" at)));
      match
        ( W.emits ~channel:"browse:kb-requested" at,
          W.emits ~channel:"principal:profile" at,
          W.emits ~channel:"job:claim" at )
      with
      | asked :: _, profile :: _, claim :: _ ->
          let answer = answer_to at asked in
          Check.equal
            ("the order of what it said at " ^ W.domain kb)
            (`List [ `Bool true; `Bool true ])
            (`List
               [
                 `Bool (answer.carried_at < profile.arrived);
                 `Bool (profile.arrived < claim.arrived);
               ])
      | _ -> failwith ("it asked, named itself and claimed at " ^ W.domain kb))
    [ (w.own, W.worker_client); (second, W.second_worker_client) ]

let runs_each_job (w : W.t) =
  let second = W.second w in
  let jobs =
    [
      mark_job w.own "here" (params "highlighting");
      mark_job second "there" (params "highlighting");
    ]
  in
  (* The two knowledge bases' engines work at once, so the provider answers
     by which model asked. *)
  w.ollama.choose <-
    Some
      (fun body ->
        Some
          (answer
             (Yojson.Safe.to_string
                (`List
                   [
                     `Assoc
                       [
                         ( "exact",
                           str
                             (if member "model" body = str here.model then
                                "the first program"
                              else "Charles Babbage") );
                       ];
                   ]))));
  let served = serving_both w second in
  List.iter2
    (fun ((kb : W.kb), engine) (job : W.job) ->
      let at = W.at served kb in
      ignore (settled at job : Yojson.Safe.t);
      (* It was claimed there, as the agent there, and everything said of it
         was said there, by that agent. *)
      Check.equal
        ("the claims answered at " ^ W.domain kb)
        (`List [ `Assoc [ ("by", str (W.did kb)); ("answered", str job.id) ] ])
        (`List
           (List.filter_map
              (fun (c : W.claim) ->
                Option.map
                  (fun id ->
                    `Assoc [ ("by", c.claimed_by); ("answered", str id) ])
                  c.answered)
              (Dynarray.to_list kb.claims)));
      let said = List.filter (about_job job) (W.emits at) in
      Check.that ("it said something of its job at " ^ W.domain kb) (said <> []);
      Check.equal
        ("who said it at " ^ W.domain kb)
        (strs [ W.did kb ])
        (`List
           (List.sort_uniq compare
              (List.map
                 (fun (e : W.emitted) ->
                   match e.emitted_by with Some d -> str d | None -> `Null)
                 said)));
      (* What it made names the agent by its DID there, and the engine. *)
      let commits = Dynarray.to_list kb.commits in
      Check.equal
        ("what it committed at " ^ W.domain kb)
        (`List
           [
             `Assoc
               [ ("by", str (W.did kb)); ("resourceId", str job.resource_id) ];
           ])
        (`List
           (List.map
              (fun (c : W.commit) ->
                `Assoc
                  [ ("by", c.committed_by); ("resourceId", str c.committed_to) ])
              commits));
      Check.equal
        ("the generator of what it made at " ^ W.domain kb)
        (`List [ W.generator ~engine kb ])
        (`List
           (List.map
              (fun a -> member "generator" a)
              (List.hd commits).annotations)))
    [ (w.own, here); (second, there) ]
    jobs;
  Check.equal "the models asked"
    (strs (List.sort compare [ here.model; there.model ]))
    (`List
       (List.sort compare
          (List.map
             (fun (body, _) -> member "model" body)
             (Dynarray.to_list w.ollama.generations))))

let health_in_order (w : W.t) =
  let second = W.second w in
  let served = serving_both w second in
  Run.eventually "both knowledge bases to have been confirmed" 15.0 (fun () ->
      if
        List.for_all
          (fun kb -> Check.at kb [ "state" ] = str "working")
          (states served)
      then Some ()
      else None);
  let engine_of engine =
    `Assoc
      ([
         ( "engine",
           `Assoc
             [
               ("provider", str engine.W.provider); ("model", str engine.model);
             ] );
         ("jobs", `List [ Dispatcher_world.marks "highlighting" ]);
       ]
      @ match idle with `Assoc members -> members | _ -> [])
  in
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
                   ("engines", `List [ engine_of here ]);
                 ];
               `Assoc
                 [
                   ("domain", str (W.domain second));
                   ("state", str "working");
                   ("did", str (W.did second));
                   ("engines", `List [ engine_of there ]);
                 ];
             ] );
       ])
    (health served.worker)

(* The other knowledge base works as it would alone. *)
let works_beside (w : W.t) served name =
  let job = mark_job w.own name (params "highlighting") in
  Ollama.script w.ollama [ answer "[]" ];
  W.announce w.own job;
  ignore (settled served job : Yojson.Safe.t)

let refuses_one confirmation (w : W.t) =
  let second = W.second w in
  let confirmation = confirmation w in
  second.hooks <-
    { W.no_hooks with confirm = Some (fun () -> Some confirmation) };
  let served = serving_both w second in
  let at_second = W.at served second in
  let refused_at =
    Run.eventually "the refusal in its log" 15.0 (fun () ->
        if lines served "refused" <> [] then Some (Run.now ()) else None)
  in
  (match lines served "refused" with
  | [ line ] ->
      logs_of ~level:"error" ~domain:(W.domain second) ~said:[ "reason" ] line
  | lines ->
      failwith
        (Printf.sprintf "its log says %d times that a knowledge base refused it"
           (List.length lines)));
  (* Its stream there is closed, and nothing was said there after the
     question: no name, no claim. *)
  Client_proxy.until ~timeout:15.0 at_second.proxy "its stream there to end"
    (fun () ->
      if List.for_all (fun (s : W.opened) -> s.closed) (W.streams at_second)
      then Some ()
      else None);
  Check.equal "what it said there"
    (strs [ "browse:kb-requested" ])
    (strs (List.map (fun (e : W.emitted) -> e.channel) (W.emits at_second)));
  Check.equal "its claims there" (`Int 0) (`Int (Dynarray.length second.claims));
  (match states served with
  | [ first; refused ] ->
      working w.own first;
      Check.equal "the state of the refused one"
        (`Assoc [ ("domain", str (W.domain second)); ("state", str "refused") ])
        refused
  | _ -> failwith "its health names two knowledge bases");
  works_beside w served "beside-a-refusal";
  (* Longer than a knowledge base in trouble waits before it is started
     again: the refused one is not. *)
  Run.sleep
    (Float.max 0.0
       (refused_at
       +. (float_of_int (reconnect_ms ()) /. 1000.0)
       +. 1.0 -. Run.now ()));
  Check.equal "its sign-ins there" (`Int 1)
    (`Int (List.length (W.sign_ins at_second)));
  Check.equal "its streams there" (`Int 1)
    (`Int (List.length (W.streams at_second)));
  Check.equal "the refusals in its log" (`Int 1)
    (`Int (List.length (lines served "refused")));
  keeps_secret served ((key w :: W.tokens served) @ W.tokens at_second)

let issuer_refuses (w : W.t) =
  let second = W.second w in
  (* A client the second issuer does not hold. *)
  let served = serving_both ~client:"xynolog-worker-unregistered" w second in
  Run.eventually "the refusal in its log" 15.0 (fun () ->
      if lines served "refused" <> [] then Some () else None);
  (match lines served "refused" with
  | [ line ] ->
      logs_of ~level:"error" ~domain:(W.domain second) ~said:[ "reason" ] line
  | lines ->
      failwith
        (Printf.sprintf "its log says %d times that a knowledge base refused it"
           (List.length lines)));
  (* It got no token there, so it asked that gateway nothing. *)
  Check.equal "what it asked that gateway" (`Int 0)
    (`Int (List.length (W.requests (W.at served second))));
  (match states served with
  | [ first; refused ] ->
      working w.own first;
      Check.equal "the state of the refused one"
        (`Assoc [ ("domain", str (W.domain second)); ("state", str "refused") ])
        refused
  | _ -> failwith "its health names two knowledge bases");
  works_beside w served "beside-an-issuer-refusal";
  keeps_secret served [ key w ]

(* The other kept its one sign-in, its one stream of its own and its job,
   which it finishes there. *)
let other_kept (w : W.t) served second (held : W.job) =
  let at_second = W.at served second in
  Check.equal "its sign-ins at the other" (`Int 1)
    (`Int (List.length (W.sign_ins at_second)));
  Check.equal "whether its streams at the other are closed"
    (`List [ `Bool true; `Bool false ])
    (`List
       (List.map (fun (s : W.opened) -> `Bool s.closed) (W.streams at_second)));
  held_is "the job the other holds" held (List.nth (states served) 1);
  Ollama.release w.ollama "[]";
  ignore (settled at_second held : Yojson.Safe.t)

let stream_drops (w : W.t) =
  let second = W.second w in
  let reconnect = float_of_int (reconnect_ms ()) /. 1000.0 in
  (* The second knowledge base's job is held at its model throughout;
     whatever the world's asks is answered. *)
  let held = mark_job second "kept-through-a-drop" (params "highlighting") in
  w.ollama.choose <-
    Some
      (fun body ->
        Some
          (if member "model" body = str there.model then Hold else answer "[]"));
  let served = serving_both w second in
  Ollama.asked w.ollama 1;
  Run.eventually "the world's knowledge base to have been claimed at" 15.0
    (fun () -> if Dynarray.length w.own.claims >= 1 then Some () else None);
  let before = grants w.own in
  (* The world's knowledge base's stream drops: it is working there still. *)
  let cut_at = Run.now () in
  Client_proxy.cut served.proxy;
  working w.own (List.hd (states served));
  (* It opens its stream again, claims once it is open, and runs a job it is
     handed there. The first stream it opened is the one it asked which
     knowledge base it is on; the second its own. *)
  let reopened =
    Client_proxy.until ~timeout:20.0 served.proxy
      "its stream to be opened again" (fun () ->
        List.nth_opt
          (List.filter
             (fun (r : Client_proxy.request) -> r.status = Some 200)
             (subscriptions served))
          2)
  in
  ignore
    (W.emitted ~timeout:15.0
       ~matches:(fun e ->
         match reopened.answered with
         | Some answered -> e.arrived > answered
         | None -> false)
       served "job:claim"
      : W.emitted);
  let job = mark_job w.own "after-a-drop" (params "highlighting") in
  W.announce w.own job;
  ignore (settled served job : Yojson.Safe.t);
  (* Longer than a knowledge base in trouble waits before it is started
     again: this one was not. *)
  Run.sleep (Float.max 0.0 (cut_at +. reconnect +. 1.0 -. Run.now ()));
  Check.equal "the trouble in its log" (`List [])
    (`List (lines served "trouble"));
  Check.equal "how often its log says the world's is confirmed" (`Int 1)
    (`Int
       (List.length
          (List.filter
             (fun l -> Check.at l [ "domain" ] = str (W.domain w.own))
             (lines served "confirmed"))));
  Check.equal "the grants of its issuer" (`Int before) (`Int (grants w.own));
  Check.equal "who it signed in as"
    (`List [ strs [ W.did w.own ] ])
    (signed_in_as served [ w.own ]);
  Check.equal "who opened each stream"
    (strs [ W.did w.own; W.did w.own; W.did w.own ])
    (streams_by served);
  Check.equal "the tokens it carried" (`Int 1)
    (`Int (List.length (W.tokens served)));
  Check.equal "how often it asked which knowledge base this is" (`Int 1)
    (`Int (List.length (W.emits ~channel:"browse:kb-requested" served)));
  Check.equal "how often it said what it is called" (`Int 1)
    (`Int (List.length (W.emits ~channel:"principal:profile" served)));
  working w.own (List.hd (states served));
  other_kept w served second held

let trouble failure (w : W.t) =
  let second = W.second w in
  let reconnect_ms = reconnect_ms () in
  (* The second knowledge base's job is held at its model throughout. *)
  let held =
    mark_job second
      ("kept-through-"
      ^
      match failure with
      | Issuer.Unavailable -> "unavailable"
      | Unanswered -> "unanswered")
      (params "highlighting")
  in
  Ollama.script w.ollama [ Hold ];
  let before = grants w.own in
  Queue.add failure w.own.world.issuer.failing;
  let served = serving_both w second in
  let trouble =
    Run.eventually "the trouble in its log" 15.0 (fun () ->
        match lines served "trouble" with line :: _ -> Some line | [] -> None)
  in
  logs_of ~level:"warn" ~domain:(W.domain w.own) ~said:[ "error" ] trouble;
  Check.equal "how long it says it waits" (`Int reconnect_ms)
    (Check.at trouble [ "waitMs" ]);
  let failed_at =
    match Dynarray.to_list w.own.world.issuer.met with
    | (met, at) :: _ ->
        Check.that "the failure the issuer met is the one it was told"
          (met = failure);
        at
    | [] -> failwith "the issuer met no failure it was told"
  in
  (* While it waits it is starting there, and has asked that gateway
     nothing; the other is at work on its job. *)
  Ollama.asked w.ollama 1;
  Check.equal "what it asked the world's gateway" (`Int 0)
    (`Int (List.length (W.requests served)));
  (match states served with
  | [ first; other ] ->
      Check.equal "the state of the world's"
        (`Assoc [ ("domain", str (W.domain w.own)); ("state", str "starting") ])
        first;
      working second other;
      held_is "the job the other holds" held other
  | _ -> failwith "its health names two knowledge bases");
  (* It is started again once it has waited: a grant of its own, the gateway
     asked who it is, its stream, the question, its name, and its claims. *)
  let named = W.emitted served "principal:profile" in
  ignore
    (W.emitted ~timeout:15.0
       ~matches:(fun e -> e.arrived > named.arrived)
       served "job:claim"
      : W.emitted);
  (match
     List.find_opt
       (fun (r : Client_proxy.request) -> r.path = "/api/users/me")
       (W.requests served)
   with
  | Some sign_in ->
      Check.that
        (Printf.sprintf
           "it signed in again %g ms after its grant failed, and waits %d ms"
           ((sign_in.arrived_at -. failed_at) *. 1000.0)
           reconnect_ms)
        ((sign_in.arrived_at -. failed_at) *. 1000.0
        >= float_of_int reconnect_ms)
  | None -> failwith "it did not sign in again");
  Check.equal "the grants of its issuer"
    (`Int (before + 1))
    (`Int (grants w.own));
  Check.equal "who it signed in as"
    (`List [ strs [ W.did w.own ] ])
    (signed_in_as served [ w.own ]);
  Check.equal "who opened each stream"
    (strs [ W.did w.own; W.did w.own ])
    (streams_by served);
  Check.equal "how often it asked which knowledge base this is" (`Int 1)
    (`Int (List.length (W.emits ~channel:"browse:kb-requested" served)));
  (match
     List.filter
       (fun l -> Check.at l [ "domain" ] = str (W.domain w.own))
       (lines served "confirmed")
   with
  | [ line ] ->
      Check.like "the confirmation" (`Assoc [ ("did", str (W.did w.own)) ]) line
  | lines ->
      failwith
        (Printf.sprintf "its log says %d times that the world's is confirmed"
           (List.length lines)));
  Check.equal "the trouble in its log" (`Int 1)
    (`Int (List.length (lines served "trouble")));
  working w.own (List.hd (states served));
  other_kept w served second held;
  keeps_secret served
    ((key w :: W.tokens served) @ W.tokens (W.at served second))

let health_waits (w : W.t) =
  let second = W.second w in
  let confirm_here = hold_confirmation w.own in
  let confirm_there = hold_confirmation second in
  let served = serving_both ~spawn:true w second in
  (* Both have been asked which they are, and neither has answered. *)
  Run.eventually "both knowledge bases to have been asked which they are" 20.0
    (fun () ->
      if
        List.for_all
          (fun kb ->
            List.exists
              (fun (e : W.emitted) -> e.status <> None)
              (W.emits ~channel:"browse:kb-requested" (W.at served kb)))
          [ w.own; second ]
      then Some ()
      else None);
  Check.that "its health does not answer"
    (not (health_answers_within served 1.5));
  (* The second answers: the worker's health answers, the first still
     starting. *)
  confirm_there ();
  Worker_service.healthy served.worker;
  (match states served with
  | [ first; other ] ->
      Check.equal "the state of the world's"
        (`Assoc [ ("domain", str (W.domain w.own)); ("state", str "starting") ])
        first;
      working second other
  | _ -> failwith "its health names two knowledge bases");
  (* The first answers too. *)
  confirm_here ();
  Run.eventually "the first to have been confirmed" 15.0 (fun () ->
      if Check.at (List.hd (states served)) [ "state" ] = str "working" then
        Some ()
      else None);
  working w.own (List.hd (states served))

let tests implementations =
  in_world implementations "knowledge-bases"
    ~category:[ "a worker of two knowledge bases" ]
    ([
       case
         "signs in at each knowledge base once, as the client its document \
          names there, and is named at each by that gateway"
         signs_in_once;
       case
         "asks each knowledge base which it is, once, on a stream that names \
          that answer alone, and opens its own there once it is confirmed by \
          the domain its document names, saying so in its log"
         asks_once;
       case
         "says at each knowledge base, once it is confirmed and before it \
          claims there, that it is called by the client its document names \
          there"
         says_its_name;
       case
         "runs the job each knowledge base hands it on the engine that claimed \
          it there, and attributes what it makes to the agent it is at that \
          knowledge base and to that engine"
         runs_each_job;
       case
         "answers GET /health with each knowledge base in the order of its \
          document: each working, with the DID it is named by there and the \
          engines it runs there"
         health_in_order;
     ]
    @ List.map
        (fun (what, confirmation) ->
          case
            (Printf.sprintf
               "refuses a knowledge base that %s, saying so once in its log, \
                says nothing more there, does not sign in there again, and \
                works at the other"
               what)
            (refuses_one confirmation))
        [
          (* The world's own domain: the gateway the second entry names is
             another knowledge base's. *)
          ( "answers as another knowledge base",
            fun (w : W.t) -> W.Domain (W.domain w.own) );
          ( "answers browse:kb-failed",
            fun _ -> W.Kb_failed "This knowledge base declares no domain" );
        ]
    @ [
        case
          "refuses a knowledge base whose issuer refuses its sign-in, saying \
           so once in its log, and works at the other"
          issuer_refuses;
        case
          "keeps its work at a knowledge base whose stream drops: it opens the \
           stream again with the bearer it holds, is not started again there, \
           and goes on claiming, while the other keeps its stream and the job \
           it holds"
          stream_drops;
      ]
    @ List.map
        (fun (what, failure) ->
          case
            (Printf.sprintf
               "takes a knowledge base whose issuer %s as trouble: it says so \
                in its log, waits, and signs in there afresh, while the other \
                keeps its stream and the job it holds"
               what)
            (trouble failure))
        [
          ("answers its grant 503", Issuer.Unavailable);
          ("ends its grant's connection with no answer", Issuer.Unanswered);
        ]
    @ [
        case
          "does not answer GET /health until a knowledge base is confirmed, \
           and then answers it with the one not yet confirmed starting"
          health_waits;
      ])
