(* What one gateway process can hold (GatewayConfig capacity): the bytes
   queued for all its streams together, at which a new stream is refused 503
   until the queues drain; and the connections it holds open, past which one
   is closed unanswered. Each case runs a gateway given a small capacity. *)

open Xynolog_conformance
open Support

(* Unvalidated, so its payload can be any size. *)
let channel = "mark:added"

(* Well under the largest emit the gateway and the broker take. *)
let frame_bytes = 512 * 1024

(* Well under one stream's pendingWriteBytes, so one stalled reader can fill
   it. *)
let queued_bytes = 4 * 1024 * 1024
let connections = 8

let probe () =
  Bus_stream.subscribe_body ~client_id:(Fresh.uuid ())
    ~global:[ "beckon:focus" ] ()

let with_capacity ~queued ~connections implementations plane =
  World.create ~command:implementations.Implementations.gateway
    ~document:
      (World.with_member "capacity"
         (`Assoc
            [ ("queuedBytes", `Int queued); ("connections", `Int connections) ]))
    plane

let queued_bytes_reached (w : World.t) =
  let stalled =
    Bus_stream.stalled_subscriber (World.origin w) (World.person w "stalled")
      (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ()) ~global:[ channel ]
         ())
  in
  let emitter = World.person w "emitter" in
  let prober = World.person w "prober" in
  let filler = String.make frame_bytes 'x' in
  let refused =
    Run.eventually "a stream refused at capacity" 30.0 (fun () ->
        ignore
          (World.emit w emitter
             (`Assoc
                [
                  ("channel", `String channel);
                  ("payload", `Assoc [ ("filler", `String filler) ]);
                ])
            : Call.reply);
        match
          Bus_stream.subscribe (World.origin w) (Some prober) (probe ())
        with
        | Refused reply when reply.status = 503 -> Some reply
        | Refused _ -> None
        | Stream stream ->
            Bus_stream.close stream;
            None)
  in
  conforms "POST" "/bus/subscribe" refused;
  Check.that "the refusal's code is capacity"
    (text_member "code" refused = "capacity");
  stalled.resume ();
  Run.eventually "a stream admitted once the queues drained" 15.0 (fun () ->
      match Bus_stream.subscribe (World.origin w) (Some prober) (probe ()) with
      | Stream stream ->
          Bus_stream.close stream;
          Some ()
      | Refused _ -> None);
  stalled.close ()

let connection_cap (w : World.t) =
  let token = World.person w "many" in
  let opened () =
    match Bus_stream.subscribe (World.origin w) (Some token) (probe ()) with
    | Stream stream -> Some stream
    | Refused reply ->
        status "a stream under the cap" 200 reply;
        None
    | exception (End_of_file | Eio.Io _) -> None
  in
  let rec hold held =
    if List.length held >= connections + 4 then (held, false)
    else
      match opened () with
      | Some stream -> hold (stream :: held)
      | None -> (held, true)
  in
  let held, refused = hold [] in
  Check.that "a connection past the cap is closed unanswered" refused;
  Check.that
    (Printf.sprintf "%d connections were held, past the cap of %d"
       (List.length held) connections)
    (List.length held <= connections);
  let first = List.nth held (List.length held - 1) in
  Bus_stream.close first;
  let again = Run.eventually "a connection taken once one closed" 10.0 opened in
  Bus_stream.close again;
  List.iter Bus_stream.close held

let tests implementations =
  each_plane
    ~make:
      (with_capacity ~queued:queued_bytes ~connections:50_000 implementations)
    "a gateway whose queued bytes reach its capacity"
    [
      case
        "refuses a new stream 503 capacity, told when to return, until the \
         queues drain"
        queued_bytes_reached;
    ]
  @ each_plane
      ~make:
        (with_capacity
           ~queued:(1024 * 1024 * 1024)
           ~connections implementations)
      "a gateway at its connection cap"
      [
        case
          "closes a connection past the cap unanswered, and takes one again \
           once one closes"
          connection_cap;
      ]
