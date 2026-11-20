(* limits.test.ts: the limits a worker answers (WORKER-SERVICE.md
   § Limits): one reply to job:limits-requested on each knowledge base's
   stream, with what each of the engines it runs for that knowledge base can
   take as its provider states it. *)

open Xynolog_conformance
open Support
open Case

(* Ask for the limits on [kb]'s bus as a client does, and read the one
   reply: its payload without the members the gateway adds, and who the
   gateway says sent it. *)
let limits_at (kb : W.kb) =
  let listener = W.listen kb [ "job:limits-result"; "job:limits-failed" ] in
  let correlation_id = Fresh.uuid () in
  let reply =
    World.emit kb.world listener.token
      (`Assoc
         [
           ("channel", str "job:limits-requested");
           ("payload", `Assoc []);
           ("correlationId", str correlation_id);
           ("clientId", str listener.client_id);
         ])
  in
  Check.equal "the gateway's answer to the suite's limits request" (`Int 202)
    (`Int reply.status);
  let message =
    Bus_stream.next ~timeout:15.0 listener.stream "the limits" (fun m ->
        match m.frame with
        | Some f -> f.correlation_id = Some correlation_id
        | None -> false)
  in
  let frame =
    match message.frame with
    | Some frame -> frame
    | None -> failwith "the limits came in no frame"
  in
  Check.equal "the channel the limits came on" (str "job:limits-result")
    (str frame.channel);
  match frame.payload with
  | `Assoc members ->
      ( `Assoc
          (List.filter
             (fun (key, _) -> not (String.starts_with ~prefix:"_" key))
             members),
        match List.assoc_opt "_userId" members with
        | Some by -> by
        | None -> failwith "the limits name no sender (_userId)" )
  | other -> failwith ("the limits are no object: " ^ Check.shown other)

let payload_at kb = fst (limits_at kb)

(* A window of [context_tokens], as an Ollama model's is stated: one window,
   shared by what goes in and what comes out. *)
let shared context_tokens =
  `Assoc
    [
      ("contextTokens", `Int context_tokens);
      ("maxOutputTokens", `Int context_tokens);
      ("acceptsTemperature", `Bool true);
    ]

let limit (engine : W.engine) limits =
  `Assoc
    [
      ( "engine",
        `Assoc
          [ ("provider", str engine.provider); ("model", str engine.model) ] );
      ("limits", limits);
    ]

let answered limits =
  `Assoc [ ("response", `Assoc [ ("limits", `List limits) ]) ]

let highlighting = [ Dispatcher_world.marks "highlighting" ]
let yields = [ Dispatcher_world.yields ]

let two_of = function
  | first :: second :: _ -> (first, second)
  | _ -> failwith "the world has fewer than two engines of a provider"

(* A worker of two engines, the first two of [engines]: one used for
   highlighting and one for generation. *)
let started (w : W.t) engines =
  let first, second = two_of engines in
  let served =
    W.start ~engines:[ W.entry w first highlighting; W.entry w second yields ] w
  in
  Run.eventually "both engines to have claimed" 15.0 (fun () ->
      if Dynarray.length w.own.claims >= 2 then Some () else None);
  served

let answered_by served =
  `List
    (List.map
       (fun (e : W.emitted) ->
         match e.emitted_by with Some did -> str did | None -> `Null)
       (W.emits ~channel:"job:limits-result" served))

let by_model shows =
  `List
    (List.sort
       (fun a b ->
         compare
           (Check.shown (member "model" a))
           (Check.shown (member "model" b)))
       shows)

let every_engine (w : W.t) =
  let first, second = two_of W.ollama_engines in
  let served = started w W.ollama_engines in
  let payload, by = limits_at w.own in
  Check.equal "its limits"
    (answered
       [
         limit first (shared context_length);
         limit second (shared context_length);
       ])
    payload;
  Check.equal "who answered" (str (W.did w.own)) by;
  (* It asked its provider of each model once, by name. *)
  Check.equal "what it asked of its models' limits"
    (by_model
       [
         `Assoc [ ("model", str first.model) ];
         `Assoc [ ("model", str second.model) ];
       ])
    (by_model (Dynarray.to_list w.ollama.shows));
  Check.equal "who answered each request"
    (strs [ W.did w.own ])
    (answered_by served);
  said_nothing served [ "job:limits-failed" ]

let remembers (w : W.t) =
  let first, second = two_of W.ollama_engines in
  ignore (started w W.ollama_engines : W.served);
  (* A provider that cannot be asked: its models are left out, and the
     request is still answered. *)
  w.ollama.show <- Ollama.Refuses 500;
  Check.equal "its limits, its provider refusing" (answered [])
    (payload_at w.own);
  (* It is asked again at the next request. *)
  w.ollama.show <- Ollama.Context 4096;
  let learned =
    answered [ limit first (shared 4096); limit second (shared 4096) ]
  in
  Check.equal "its limits, its provider answering" learned (payload_at w.own);
  (* What it learned it keeps: the provider is not asked a third time. *)
  w.ollama.show <- Ollama.Context 2048;
  let asked = Dynarray.length w.ollama.shows in
  Check.equal "its limits, learned" learned (payload_at w.own);
  Check.equal "how often it asked of its models' limits" (`Int asked)
    (`Int (Dynarray.length w.ollama.shows))

let requested (w : W.t) =
  List.map
    (fun (r : Anthropic.request) -> r.meth ^ " " ^ r.path)
    (Dynarray.to_list w.anthropic.requests)

let of_anthropic (w : W.t) =
  let first, second = two_of W.anthropic_engines in
  (* The second reads and writes less. *)
  Hashtbl.replace w.anthropic.models second.model
    {
      Anthropic.max_input_tokens = 180_000;
      max_output_tokens = 8192;
      structured_outputs = true;
    };
  ignore (started w W.anthropic_engines : W.served);
  let ceilings input output =
    `Assoc
      [
        ("contextTokens", `Int input);
        ("maxOutputTokens", `Int output);
        ("acceptsTemperature", `Bool false);
      ]
  in
  let stated =
    answered
      [
        limit first
          (ceilings W.anthropic_model.max_input_tokens
             W.anthropic_model.max_output_tokens);
        limit second (ceilings 180_000 8192);
      ]
  in
  let payload, by = limits_at w.own in
  Check.equal "its limits" stated payload;
  Check.equal "who answered" (str (W.did w.own)) by;
  (* Of each model, its ceilings, and nothing else. The two engines ask at
     once, so the two requests come in no fixed order. *)
  Check.equal "what it asked its provider"
    (strs
       (List.sort compare
          [ "GET /v1/models/" ^ first.model; "GET /v1/models/" ^ second.model ]))
    (strs (List.sort compare (requested w)));
  (* What it learned it keeps: the provider is asked nothing more. *)
  Check.equal "its limits, learned" stated (payload_at w.own);
  Check.equal "how often it asked its provider" (`Int 2)
    (`Int (Dynarray.length w.anthropic.requests))

let each_knowledge_base (w : W.t) =
  let second = W.second w in
  let first, other = two_of W.ollama_engines in
  (* The second knowledge base is offered the first's engine too, after one
     of its own. *)
  let served =
    W.start
      ~engines:[ W.entry w first highlighting ]
      ~serving:
        [
          ( second,
            [ W.entry w other highlighting; W.entry w first yields ],
            None );
        ]
      w
  in
  Run.eventually "every engine to have claimed" 15.0 (fun () ->
      if Dynarray.length w.own.claims >= 1 && Dynarray.length second.claims >= 2
      then Some ()
      else None);
  let here, here_by = limits_at w.own in
  Check.equal "its limits here"
    (answered [ limit first (shared context_length) ])
    here;
  Check.equal "who answered here" (str (W.did w.own)) here_by;
  let there, there_by = limits_at second in
  Check.equal "its limits there"
    (answered
       [
         limit other (shared context_length);
         limit first (shared context_length);
       ])
    there;
  Check.equal "who answered there" (str (W.did second)) there_by;
  (* Each was answered once, on the stream of the knowledge base it was asked
     at. *)
  Check.equal "who answered each request here"
    (strs [ W.did w.own ])
    (answered_by served);
  Check.equal "who answered each request there"
    (strs [ W.did second ])
    (answered_by (W.at served second))

let tests implementations =
  in_world implementations "limits"
    ~category:[ "the limits a worker answers" ]
    [
      case
        "answers a request once, as the agent, with the limits of every \
         engine, in the order of its engines"
        every_engine;
      case
        "remembers what a provider said, and asks it again only for a model it \
         could not learn of"
        remembers;
      case
        "answers, of an Anthropic model, what its provider says it reads and \
         writes, and that it takes no temperature, having asked of each model \
         once and in one request"
        of_anthropic;
      case
        "answers on each knowledge base's stream with the limits of the \
         engines it runs for that knowledge base alone, in their order there, \
         as the agent there"
        each_knowledge_base;
    ]
