(* Several gateways on one broker are one gateway to their clients: a reply
   reaches its requester whichever replica each is connected to, a claim
   made on one is known to the others, an emit's key held by one is
   recognised by the others, and a reply retained by one is recovered
   through another — or through the same one after it restarts.
   contract/TRANSPORT-HTTP.md § POST /bus/emit (Claims) and § Correlated-
   reply retention; specs/src/paths/emit-on-bus.json (A key: held on every
   replica). NATS only: the in-process plane is one process. *)

open Xynolog_conformance
open Support
open Case

let request = "browse:resource-requested"
let broadcast = "beckon:focus"

(* A request the participant leaves for the case to answer when it
   chooses. *)
let later = "later"

(* Something hears this channel and never answers. *)
let unanswered_channel = "browse:kb-requested"
let result () = (Protocol.operation_for request).result

type shared = {
  world : World.t;
  b : Gateway.t;
  participant : World.responder;
      (** It answers [request], and replies by hand to [later]; every request
          published reaches its stream. *)
}

let make implementations plane =
  let world = plain implementations plane in
  let b = World.replica world in
  (* The service that answers the request is connected to replica B. *)
  let participant =
    World.responder ~origin:b.origin world [ request ] (fun frame ->
        if Protocol.at frame.payload [ "resourceId" ] = Some (`String later)
        then None
        else Some (result (), described ()))
  in
  { world; b; participant }

let answer ?(name = "r") ?origin { world; b; participant } correlation_id =
  World.emit
    ~origin:(Option.value origin ~default:b.origin)
    world participant.token
    (`Assoc
       [
         ("channel", `String (result ()));
         ("payload", described ~name ());
         ("correlationId", `String correlation_id);
       ])

let body ?global ?pending_replies ?(client_id = Fresh.uuid ()) () =
  Bus_stream.subscribe_body ~client_id ?global ?pending_replies ()

let ask ?origin ?(resource = "r") ?(channel = request) (w : World.t) token
    ~client_id correlation_id =
  World.emit ?origin w token
    (`Assoc
       [
         ("channel", `String channel);
         ( "payload",
           if channel = request then `Assoc [ ("resourceId", `String resource) ]
           else `Assoc [] );
         ("correlationId", `String correlation_id);
         ("clientId", `String client_id);
       ])

let correlation_ids stream channel =
  List.map
    (fun (f : Bus_stream.frame) -> f.correlation_id)
    (Bus_stream.frames ~channel stream)

let response json = Check.at json [ "response" ]

let probe origin token wanted =
  match
    Bus_stream.subscribe origin (Some token) (body ~global:[ broadcast ] ())
  with
  | Stream stream ->
      Bus_stream.close stream;
      if wanted = 200 then Some () else None
  | Refused reply -> if reply.status = wanted then Some () else None

let streams_across { world = w; b; _ } =
  let baseline =
    Protocol.count
      (fst
         (Protocol.principal_limit Gateway "POST" "/bus/subscribe"
            "streamsPerPrincipal"))
  in
  let carol = World.person w "carol-streams" in
  for i = 0 to baseline - 1 do
    ignore
      (World.subscribe
         ~origin:(if i mod 2 = 0 then World.origin w else b.origin)
         w carol
         (body ~global:[ broadcast ] ())
        : Bus_stream.t)
  done;
  List.iter
    (fun origin ->
      Run.eventually (origin ^ " refuses past the limit") 10.0 (fun () ->
          probe origin carol 429))
    [ World.origin w; b.origin ]

let leases_end { world = w; _ } =
  let baseline =
    Protocol.count
      (fst
         (Protocol.principal_limit Gateway "POST" "/bus/subscribe"
            "streamsPerPrincipal"))
  in
  let heartbeat =
    float_of_int
      (Protocol.limit Gateway "POST" "/bus/subscribe" "heartbeatSeconds")
  in
  let c = World.replica w in
  let dave = World.person w "dave-streams" in
  for _ = 1 to baseline do
    ignore
      (World.subscribe ~origin:c.origin w dave (body ~global:[ broadcast ] ())
        : Bus_stream.t)
  done;
  Run.eventually "the other replica counts them" 10.0 (fun () ->
      probe (World.origin w) dave 429);
  Gateway.crash c;
  let died = Run.now () in
  Run.eventually "a stream admitted once the dead replica's leases end"
    ((2.0 *. heartbeat) +. 15.0)
    (fun () -> probe (World.origin w) dave 200);
  Check.that "not before the lease could have ended"
    (Run.now () -. died > heartbeat)

let carried mark (m : Bus_stream.message) =
  match m.frame with
  | Some f ->
      f.channel = broadcast
      && Protocol.at f.payload [ "annotationId" ] = Some (`String mark)
  | None -> false

let focus mark =
  `Assoc
    [
      ("channel", `String broadcast);
      ( "payload",
        `Assoc
          [ ("annotationId", `String mark); ("resourceId", `String "res-1") ] );
    ]

let one_id_through_every_replica { world = w; b; _ } =
  let here =
    World.subscribe w (World.person w "here") (body ~global:[ broadcast ] ())
  in
  let there =
    World.subscribe ~origin:b.origin w (World.person w "there")
      (body ~global:[ broadcast ] ())
  in
  let mark = Fresh.uuid () in
  status "the emit" 202 (World.emit w (World.person w "emitter") (focus mark));
  let id =
    (Bus_stream.next here "the frame on the replica it was emitted through"
       (carried mark))
      .id
  in
  Check.that "the other replica carries the same id"
    ((Bus_stream.next there "the frame on the other replica" (carried mark)).id
   = id)

let first_message_means_subscribed { world = w; _ } =
  let broker = Option.get w.broker in
  (* What this replica tells the broker waits in the proxy for a second. *)
  let proxy = Proxy.start broker.port in
  let slow = World.replica ~servers:proxy.url w in
  Fun.protect ~finally:(fun () ->
      Proxy.release proxy;
      Gateway.stop slow;
      Proxy.close proxy)
  @@ fun () ->
  Proxy.hold proxy;
  let stream =
    World.open_stream ~origin:slow.origin w
      (World.person w "slow-to-be-heard")
      (body ~global:[ broadcast ] ())
  in
  Eio.Fiber.fork ~sw:(Run.sw ()) (fun () ->
      Run.sleep 1.0;
      Proxy.release proxy);
  ignore
    (Bus_stream.next stream "the stream's first message" (fun _ -> true)
      : Bus_stream.message);
  let mark = Fresh.uuid () in
  status "the emit" 202 (World.emit w (World.person w "emitter") (focus mark));
  ignore
    (Bus_stream.frame ~timeout:5.0 stream broadcast ~matches:(fun f ->
         Protocol.at f.payload [ "annotationId" ] = Some (`String mark))
      : Bus_stream.frame)

let answered_through_that_replica { world = w; b; _ } =
  let result = result () in
  let alice = World.person w "alice" in
  let client_id = Fresh.uuid () in
  let requester =
    World.subscribe w alice (body ~client_id ~global:[ result ] ())
  in
  let bystander =
    World.subscribe ~origin:b.origin w alice (body ~global:[ result ] ())
  in
  let correlation_id = Fresh.uuid () in
  status "the request" 202 (ask w alice ~client_id correlation_id);
  let reply =
    Bus_stream.frame requester result ~matches:(fun f ->
        f.correlation_id = Some correlation_id)
  in
  Check.equal "the reply" (response (described ())) (response reply.payload);
  Bus_stream.quiet bystander result 0.5

let answered_on_the_other (shared : shared) =
  let { world = w; b; _ } = shared in
  let result = result () in
  let alice = World.person w "alice" in
  let client_id = Fresh.uuid () in
  let requester =
    World.subscribe ~origin:b.origin w alice
      (body ~client_id ~global:[ result ] ())
  in
  let bystander = World.subscribe w alice (body ~global:[ result ] ()) in
  let correlation_id = Fresh.uuid () in
  status "the request" 202
    (ask w alice ~resource:later ~client_id correlation_id);
  List.iter
    (fun name ->
      status "a reply" 202
        (answer ~name ~origin:(World.origin w) shared correlation_id))
    [ "one"; "two"; "three" ];
  ignore
    (Bus_stream.next requester "the third reply" (fun _ ->
         List.length (Bus_stream.frames ~channel:result requester) = 3)
      : Bus_stream.message);
  Check.equal "the replies, in order"
    (`List [ `String "one"; `String "two"; `String "three" ])
    (`List
       (List.map
          (fun (f : Bus_stream.frame) ->
            Check.at f.payload [ "response"; "resource"; "name" ])
          (Bus_stream.frames ~channel:result requester)));
  Bus_stream.quiet bystander result 0.3

let metacharacters { world = w; b; _ } =
  let result = result () in
  let alice = World.person w "alice" in
  let client_id = Fresh.uuid () in
  let correlation_id = "a:b.c *>/ é-" ^ Fresh.uuid () in
  let requester =
    World.subscribe w alice (body ~client_id ~global:[ result ] ())
  in
  status "the request" 202 (ask w alice ~client_id correlation_id);
  let live =
    Bus_stream.frame requester result ~matches:(fun f ->
        f.correlation_id = Some correlation_id)
  in
  Check.equal "the reply" (response (described ())) (response live.payload);
  List.iter
    (fun origin ->
      status
        ("the request again through " ^ origin)
        409
        (ask ~origin w alice ~client_id correlation_id))
    [ World.origin w; b.origin ];
  let recovered =
    World.subscribe ~origin:b.origin w alice
      (body ~client_id ~global:[ result ] ~pending_replies:[ correlation_id ] ())
  in
  Check.that "the reply is recovered"
    (correlation_ids recovered result = [ Some correlation_id ])

let outstanding_claim_answered_live (shared : shared) =
  let { world = w; _ } = shared in
  let result = result () in
  let alice = World.person w "alice" in
  let client_id = Fresh.uuid () in
  let early = Fresh.uuid () in
  status "the early request" 202 (ask w alice ~resource:later ~client_id early);
  let late = World.replica w in
  let on_late =
    World.subscribe ~origin:late.origin w alice
      (body ~client_id ~global:[ result ] ())
  in
  status "its reply" 202 (answer shared early);
  ignore
    (Bus_stream.frame on_late result ~matches:(fun f ->
         f.correlation_id = Some early)
      : Bus_stream.frame);
  let across = Fresh.uuid () in
  status "the request across a restart" 202
    (ask w alice ~resource:later ~client_id across);
  World.restart w;
  let on_restarted =
    World.subscribe w alice (body ~client_id ~global:[ result ] ())
  in
  status "its reply" 202 (answer shared across);
  ignore
    (Bus_stream.frame on_restarted result ~matches:(fun f ->
         f.correlation_id = Some across)
      : Bus_stream.frame)

let ask_again_anywhere { world = w; b; _ } =
  let max = Protocol.subscribe_max_items "pendingReplies" in
  let result = result () in
  let alice = World.person w "alice" in
  let client_id = Fresh.uuid () in
  let requester =
    World.subscribe w alice (body ~client_id ~global:[ result ] ())
  in
  for i = 0 to max - 1 do
    status
      (Printf.sprintf "request %d" i)
      202
      (ask w alice ~resource:(Printf.sprintf "r-%d" i) ~client_id
         (Fresh.uuid ()))
  done;
  ignore
    (Bus_stream.next ~timeout:30.0 requester
       (Printf.sprintf "all %d replies" max) (fun _ ->
         List.length (Bus_stream.frames ~channel:result requester) = max)
      : Bus_stream.message);
  Run.sleep 1.0;
  status "a request on B" 202
    (ask ~origin:b.origin w alice ~client_id (Fresh.uuid ()));
  World.restart w;
  status "a request after a restart" 202
    (ask w alice ~client_id (Fresh.uuid ()))

let silent_listener (w : World.t) =
  let silent, _ = World.agent w "silent" in
  ignore
    (World.subscribe w silent (body ~global:[ unanswered_channel ] ())
      : Bus_stream.t)

let claimed_once { world = w; b; _ } =
  let alice = World.person w "alice" in
  let correlation_id = Fresh.uuid () in
  (* Something hears this channel and never answers, so nothing settles the
     claim in the meantime: a request that reached nobody would be answered
     at once with peer-unavailable. *)
  silent_listener w;
  status "the first claim" 202
    (ask ~channel:unanswered_channel w alice ~client_id:(Fresh.uuid ())
       correlation_id);
  status "the second claim, on the other replica" 409
    (ask ~origin:b.origin ~channel:unanswered_channel w alice
       ~client_id:(Fresh.uuid ()) correlation_id)

let answered_as_first (first : Call.reply) (again : Call.reply) =
  conforms "POST" "/bus/emit" again;
  Check.equal "the repeat's answer"
    (`Assoc
       [
         ("status", `Int first.status);
         ("body", Option.value first.json ~default:`Null);
       ])
    (`Assoc
       [
         ("status", `Int again.status);
         ("body", Option.value again.json ~default:`Null);
       ])

let command_again_through_other { world = w; b; _ } =
  let watcher =
    World.subscribe w (World.person w "watcher") (body ~global:[ broadcast ] ())
  in
  let emitter = World.person w "emitter" in
  let mark = Fresh.uuid () and key = Fresh.uuid () in
  let first = World.emit ~key w emitter (focus mark) in
  status "the command" 202 first;
  let again = World.emit ~origin:b.origin ~key w emitter (focus mark) in
  answered_as_first first again;
  let carried (f : Bus_stream.frame) =
    Protocol.at f.payload [ "annotationId" ] = Some (`String mark)
  in
  ignore
    (Bus_stream.frame watcher broadcast ~matches:carried : Bus_stream.frame);
  Run.sleep 0.5;
  Check.that "published once"
    (List.length
       (List.filter carried (Bus_stream.frames ~channel:broadcast watcher))
    = 1)

let request_again_through_other (shared : shared) =
  let { world = w; b; participant } = shared in
  let result = result () in
  let alice = World.person w "alice" in
  let client_id = Fresh.uuid () in
  let requester =
    World.subscribe w alice (body ~client_id ~global:[ result ] ())
  in
  let correlation_id = Fresh.uuid () and key = Fresh.uuid () in
  let request_body =
    `Assoc
      [
        ("channel", `String request);
        ("payload", `Assoc [ ("resourceId", `String later) ]);
        ("correlationId", `String correlation_id);
        ("clientId", `String client_id);
      ]
  in
  let first = World.emit ~key w alice request_body in
  status "the request" 202 first;
  let again = World.emit ~origin:b.origin ~key w alice request_body in
  answered_as_first first again;
  status "its reply" 202 (answer shared correlation_id);
  let reply =
    Bus_stream.frame requester result ~matches:(fun f ->
        f.correlation_id = Some correlation_id)
  in
  Check.equal "the reply" (response (described ())) (response reply.payload);
  Run.sleep 0.5;
  Check.that "published once"
    (List.length
       (List.filter
          (fun c -> c = Some correlation_id)
          (correlation_ids participant.stream request))
    = 1)

let unanswered_count_everywhere { world = w; b; _ } =
  let max = Protocol.subscribe_max_items "pendingReplies" in
  let alice = World.person w "alice" in
  let client_id = Fresh.uuid () in
  (* Heard on one replica and answered by nobody, so every request stays
     unanswered: one that reached nobody would be answered at once. *)
  silent_listener w;
  for i = 0 to max - 1 do
    let origin = if i mod 2 = 0 then World.origin w else b.origin in
    status
      (Printf.sprintf "request %d" i)
      202
      (ask ~origin ~channel:unanswered_channel w alice ~client_id
         (Fresh.uuid ()))
  done;
  (* A probe that is accepted is a claim of its own, so each replica is
     probed once, after the shared table has had time to reach both. *)
  Run.sleep 1.0;
  List.iter
    (fun origin ->
      status origin 429
        (ask ~origin ~channel:unanswered_channel w alice ~client_id
           (Fresh.uuid ())))
    [ World.origin w; b.origin ]

let recovered_through_other { world = w; b; _ } =
  let result = result () in
  let alice = World.person w "alice" in
  let client_id = Fresh.uuid () and correlation_id = Fresh.uuid () in
  ignore (ask w alice ~client_id correlation_id : Call.reply);
  Run.sleep 0.5;
  let recovered =
    World.subscribe ~origin:b.origin w alice
      (body ~client_id ~global:[ result ] ~pending_replies:[ correlation_id ] ())
  in
  Check.that "the reply is recovered through the other replica"
    (List.mem (Some correlation_id) (correlation_ids recovered result))

let survive_restart { world = w; _ } =
  let result = result () in
  let alice = World.person w "alice" in
  let client_id = Fresh.uuid () in
  let answered = Fresh.uuid () in
  ignore (ask w alice ~client_id answered : Call.reply);
  let pending = Fresh.uuid () in
  ignore
    (ask ~channel:unanswered_channel w alice ~client_id pending : Call.reply);
  Run.sleep 0.5;
  World.restart w;
  status "the pending claim, after the restart" 409
    (ask ~channel:unanswered_channel w alice ~client_id pending);
  let recovered =
    World.subscribe w alice
      (body ~client_id ~global:[ result ] ~pending_replies:[ answered ] ())
  in
  Check.that "the retained reply survives the restart"
    (List.mem (Some answered) (correlation_ids recovered result))

let tests implementations =
  World.each_plane ~planes:[ Nats ] ~make:(make implementations)
    ~world:(fun { world; _ } -> world)
    ~close:(fun { world; _ } -> World.close world)
    "two replicas on one broker"
    [
      case "the streams one principal holds are counted across replicas"
        streams_across;
      case
        "the streams a replica held stop counting once their lease ends, when \
         the replica died without releasing them"
        leases_end;
      case
        "a frame with no correlationId and no position carries one id through \
         every replica"
        one_id_through_every_replica;
      case
        "a stream's first message says the broker has its subscription: a \
         frame emitted after it through another replica is carried, however \
         late the broker heard"
        first_message_means_subscribed;
      case
        "a request made through one replica is answered to its requester \
         through that replica, and to no one on the other"
        answered_through_that_replica;
      case
        "a request made through one replica is answered on the other, when \
         that is where its requester listens"
        answered_on_the_other;
      case
        "a correlationId is any string: one full of the broker's \
         metacharacters claims, conflicts, routes and is recovered like any \
         other"
        metacharacters;
      case
        "a claim outstanding when a replica starts, or restarts, is answered \
         live to a requester connected there"
        outstanding_claim_answered_live;
      case
        "a client whose requests were all answered may ask again on any \
         replica, and after a restart"
        ask_again_anywhere;
      case
        "a correlationId claimed on one replica cannot be claimed on the other"
        claimed_once;
      case
        "a command sent again under its key through the other replica is \
         recognised there: published once, and answered as the first was"
        command_again_through_other;
      case
        "a request sent again under its key through the other replica is \
         answered as the first was, never 409: published once, and its reply \
         reaches its requester"
        request_again_through_other;
      case "a client's unanswered requests count against it on every replica"
        unanswered_count_everywhere;
      case
        "a reply retained while its requester was away is recovered through \
         the other replica"
        recovered_through_other;
      case "claims and retained replies survive a replica's restart"
        survive_restart;
    ]
