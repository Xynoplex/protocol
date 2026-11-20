(* The client proxy does what the SDK suite leans on it for, against a real
   gateway: it carries a stream and an emit through unchanged, records what
   the client sent and what each stream carried, ends or refuses the
   client's connections from outside, keeps a request waiting, answers one
   itself, leaves one unanswered, keeps the gateway's answer to one from the
   client, and passes a stream on a byte at a time. *)

open Xynolog_conformance

let broadcast = "beckon:focus"

(* A broadcast whose payload is free text. *)
let text_channel = "browse:entity-type-clicked"
let failf format = Printf.ksprintf failwith format

(* [body] on a proxy in front of [world]'s gateway. *)
let behind world body =
  let proxy = Client_proxy.start (World.origin world) in
  Fun.protect ~finally:(fun () -> Client_proxy.close proxy) @@ fun () ->
  body proxy

let subscribe ?origin world token channels =
  World.subscribe ?origin world token
    (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ()) ~global:channels ())

let path_is path (r : Client_proxy.request) = r.path = path
let answered path (r : Client_proxy.request) = r.path = path && r.status <> None
let last (proxy : Client_proxy.t) = Dynarray.get_last proxy.requests

let status what expected (reply : Call.reply) =
  if reply.status <> expected then
    failf "%s: expected %d, received %d: %s" what expected reply.status
      (Call.shown reply.text)

let fails what call =
  match call () with
  | (reply : Call.reply) ->
      failf "%s was answered %d; it was to fail" what reply.status
  | exception (Eio.Io _ | End_of_file | Failure _) -> ()

let health origin = Call.call origin "GET" "/api/health"

let carries world =
  behind world @@ fun proxy ->
  let client_id = Fresh.uuid () in
  let watcher =
    World.subscribe ~origin:proxy.origin world
      (World.person world "watcher")
      (Bus_stream.subscribe_body ~client_id ~global:[ broadcast ] ())
  in
  let payload =
    `Assoc [ ("annotationId", `String "a-1"); ("resourceId", `String "res-1") ]
  in
  let emitted =
    World.emit ~origin:proxy.origin world
      (World.person world "emitter")
      (`Assoc [ ("channel", `String broadcast); ("payload", payload) ])
  in
  status "the emit" 202 emitted;
  Check.matching "the frame's payload"
    [ ("annotationId", `String "a-1"); ("resourceId", `String "res-1") ]
    (Bus_stream.frame watcher broadcast).payload;
  let subscribe =
    Client_proxy.next proxy "the subscribe" (path_is "/bus/subscribe")
  in
  Check.that "the subscribe is a POST answered 200"
    (subscribe.meth = "POST" && subscribe.status = Some 200);
  Check.matching "the subscribe's body"
    [ ("clientId", `String client_id); ("global", `List [ `String broadcast ]) ]
    (Option.value ~default:`Null subscribe.json);
  let emit = Client_proxy.next proxy "the emit" (answered "/bus/emit") in
  Check.that "the emit is a POST answered 202"
    (emit.meth = "POST" && emit.status = Some 202);
  Check.matching "the emit's body"
    [ ("channel", `String broadcast); ("payload", payload) ]
    (Option.value ~default:`Null emit.json);
  Check.that "the subscribe is recorded before the emit"
    (subscribe.arrived < emit.arrived)

let cut world =
  behind world @@ fun proxy ->
  let token = World.person world "watcher" in
  let first = subscribe ~origin:proxy.origin world token [ broadcast ] in
  Client_proxy.cut proxy;
  Bus_stream.ends first;
  let second = subscribe ~origin:proxy.origin world token [ broadcast ] in
  Check.that "the next stream is open" (not second.ended)

let down world =
  behind world @@ fun proxy ->
  let token = World.person world "watcher" in
  let open_ = subscribe ~origin:proxy.origin world token [ broadcast ] in
  Client_proxy.down proxy;
  Bus_stream.ends open_;
  fails "a request while the proxy is down" (fun () -> health proxy.origin);
  Client_proxy.up proxy;
  status "a request once it is up" 200 (health proxy.origin)

let refusal world =
  behind world @@ fun proxy ->
  let refused =
    Call.call
      ~json:
        (`Assoc
           [
             ("channel", `String broadcast);
             ("payload", `Assoc [ ("resourceId", `String "res-1") ]);
           ])
      proxy.origin "POST" "/bus/emit"
  in
  status "an emit with no token" 401 refused;
  let recorded =
    Client_proxy.next proxy "the refused emit" (answered "/bus/emit")
  in
  Check.that "it records the 401" (recorded.status = Some 401)

let rechunked world =
  behind world @@ fun proxy ->
  Client_proxy.rechunk proxy 1;
  let watcher =
    subscribe ~origin:proxy.origin world
      (World.person world "watcher")
      [ text_channel ]
  in
  (* Text whose characters are several bytes each, so that a byte at a time
     splits them. *)
  let entity_type = "naïve — 日本語 😀" in
  let payload = `Assoc [ ("entityType", `String entity_type) ] in
  status "the emit" 202
    (World.emit world
       (World.person world "emitter")
       (`Assoc [ ("channel", `String text_channel); ("payload", payload) ]));
  let received = Bus_stream.frame watcher text_channel in
  Check.matching "the frame's payload"
    [ ("entityType", `String entity_type) ]
    received.payload;
  let stream =
    match Dynarray.find_opt (path_is "/bus/subscribe") proxy.requests with
    | Some stream -> stream
    | None -> failwith "the proxy recorded no subscribe"
  in
  let carried =
    Client_proxy.until proxy "the frame to be recorded" (fun () ->
        Dynarray.find_opt
          (fun (e : Client_proxy.event) -> e.event = "bus-event")
          stream.events)
  in
  Check.equal "the event the stream carried"
    (`Assoc [ ("channel", `String text_channel); ("payload", received.payload) ])
    (Yojson.Safe.from_string carried.data);
  Check.that "the stream's first event is a ping"
    ((Dynarray.get stream.events 0).event = "ping");
  match stream.answered with
  | Some answered ->
      Check.that "the frame is recorded after the stream is answered"
        (carried.at > answered)
  | None -> failwith "the stream is recorded as unanswered"

let held world =
  behind world @@ fun proxy ->
  Client_proxy.hold_requests proxy (path_is "/api/health");
  Eio.Switch.run @@ fun sw ->
  let pending = Eio.Fiber.fork_promise ~sw (fun () -> health proxy.origin) in
  let waiting =
    Client_proxy.next proxy "the held request" (path_is "/api/health")
  in
  status "a request not held" 200
    (Call.call
       ~token:(World.person world "reader")
       proxy.origin "GET" "/api/status");
  Check.that "the held request is unanswered" (waiting.status = None);
  Client_proxy.release proxy;
  status "the held request, released" 200 (Eio.Promise.await_exn pending);
  let other =
    match Dynarray.find_opt (path_is "/api/status") proxy.requests with
    | Some r -> r
    | None -> failwith "the proxy recorded no /api/status"
  in
  match (waiting.answered, other.answered) with
  | Some held_at, Some other_at ->
      Check.that "the held request is answered after the other"
        (held_at > other_at)
  | _ -> failwith "a request is recorded as unanswered"

let scripted world =
  behind world @@ fun proxy ->
  let body =
    `Assoc [ ("error", `String "refused"); ("code", `String "emit-rate") ]
  in
  Client_proxy.answer proxy (path_is "/bus/emit")
    {
      Serve.status = 429;
      headers = [ ("content-type", "application/json"); ("retry-after", "2") ];
      body = Yojson.Safe.to_string body;
    }
    1;
  let token = World.person world "emitter" in
  let emit =
    `Assoc
      [
        ("channel", `String broadcast);
        ("payload", `Assoc [ ("resourceId", `String "res-1") ]);
      ]
  in
  let refused = World.emit ~origin:proxy.origin world token emit in
  status "the scripted answer" 429 refused;
  Check.equal "its body" body (Option.value ~default:`Null refused.json);
  let recorded = last proxy in
  Check.that "it records the 429 and its Retry-After"
    (recorded.status = Some 429 && recorded.retry_after = Some 2.0);
  Check.equal "it records the answer" body
    (Option.value ~default:`Null recorded.answer);
  status "the next emit" 202 (World.emit ~origin:proxy.origin world token emit)

let unanswered world =
  behind world @@ fun proxy ->
  Client_proxy.leave_unanswered proxy (path_is "/api/health") 1;
  fails "a request left unanswered" (fun () -> health proxy.origin);
  let dropped =
    Client_proxy.next proxy "the request left unanswered"
      (path_is "/api/health")
  in
  Client_proxy.until proxy "its connection to close" (fun () ->
      if dropped.closed then Some () else None);
  Check.that "it records no answer"
    (dropped.status = None && dropped.answered = None);
  status "the next request" 200 (health proxy.origin)

let lost world =
  behind world @@ fun proxy ->
  Client_proxy.lose_answer proxy (path_is "/api/health") 1;
  fails "a request whose answer is lost" (fun () -> health proxy.origin);
  let lost =
    Client_proxy.next proxy "the request whose answer was lost"
      (path_is "/api/health")
  in
  Client_proxy.until proxy "its connection to close" (fun () ->
      if lost.closed then Some () else None);
  Check.that "it records the gateway's 200, kept from the client"
    (lost.status = Some 200 && lost.answer_lost && lost.answered <> None);
  (match lost.answer with
  | Some (`Assoc members) ->
      List.iter
        (fun name ->
          match List.assoc_opt name members with
          | Some (`String _) -> ()
          | _ -> failf "the recorded answer has no %s, a string" name)
        [ "status"; "version" ]
  | _ -> failwith "the proxy recorded no answer");
  status "the next request" 200 (health proxy.origin);
  let next = last proxy in
  Check.that "the next is answered, and reaches its client"
    (next.status = Some 200 && not next.answer_lost)

let tests (implementations : Implementations.t) =
  World.each_plane ~planes:[ In_process ]
    ~make:(fun plane -> World.create ~command:implementations.gateway plane)
    ~world:Fun.id ~close:World.close "the client proxy"
    (List.map
       (fun (name, run) -> Case.case name run)
       [
         ( "carries a stream and an emit through to the gateway, and records \
            each request with its body",
           carries );
         ( "cut ends an open stream from outside the client, and the next \
            connection is accepted",
           cut );
         ("down refuses every new connection until up", down);
         ( "answers a refusal with the status the gateway gave, and records it",
           refusal );
         ( "records the events a stream carried as the gateway wrote them, and \
            a stream passed on a byte at a time reaches its client whole",
           rechunked );
         ( "hold keeps a request waiting, recorded and unanswered, until release",
           held );
         ( "answer answers as many requests as it is told to, itself, and \
            passes the next one on",
           scripted );
         ( "leaveUnanswered reads a request whole, closes its connection with \
            nothing written, and passes the next one on",
           unanswered );
         ( "loseAnswer passes a request on, records the gateway's answer, \
            closes the client's connection with none of it written, and passes \
            the next one on",
           lost );
       ])
