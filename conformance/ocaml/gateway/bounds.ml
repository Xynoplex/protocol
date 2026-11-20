(* The bounds on a connection (contract/TRANSPORT-HTTP.md § Limits). On
   /bus/subscribe (its x-xynolog-limits): a client that stops taking frames
   is disconnected once pendingWriteBytes are waiting for it, and a replay
   that live frames outrun past replayBufferEvents is abandoned. A
   disconnected client is visible on session:left. On any connection (the
   document's x-xynolog-limits): its first request's head must come whole
   within requestHeadSeconds of its opening; after an answer it is kept
   idleConnectionSeconds for its next request, and a head begun must come
   whole within requestHeadSeconds of its first byte. The close
   idleConnectionSeconds after an answer is too slow to wait out here; the
   accept loop's own tests hold it on a paused clock (consumer-owned Gateway
   implementation). *)

open Xynolog_conformance
open Support

(* Unvalidated, so its payload can be any size. *)
let channel = "mark:added"

(* Well under the largest emit the gateway and the broker take. *)
let frame_bytes = 512 * 1024

let connection_id (f : Bus_stream.frame) =
  Protocol.at f.payload [ "connectionId" ]

let stops_reading (w : World.t) =
  let limit =
    Protocol.limit Gateway "POST" "/bus/subscribe" "pendingWriteBytes"
  in
  let watcher =
    World.subscribe w (World.person w "watcher")
      (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ())
         ~global:[ "session:joined"; "session:left" ]
         ())
  in
  let reader =
    World.subscribe w (World.person w "reader")
      (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ()) ~global:[ channel ]
         ())
  in
  let stalled =
    Bus_stream.stalled_subscriber (World.origin w) (World.person w "stalled")
      (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ()) ~global:[ channel ]
         ())
  in
  let joined =
    Bus_stream.frame watcher "session:joined" ~matches:(fun f ->
        Protocol.at f.payload [ "participant" ]
        = Some (`String (World.person_did w "stalled")))
  in
  let left (f : Bus_stream.frame) = connection_id f = connection_id joined in
  let emitter, _ = World.agent w "flood" in
  let filler = String.make frame_bytes 'x' in
  (* Twice the bound, plus room for what the operating system buffers. *)
  let frames = (((2 * limit) + frame_bytes - 1) / frame_bytes) + 16 in
  let rec flood emitted =
    if
      emitted >= frames
      || List.exists left (Bus_stream.frames ~channel:"session:left" watcher)
    then emitted
    else (
      status "a flood's emit" 202
        (World.emit w emitter
           (`Assoc
              [
                ("channel", `String channel);
                ( "payload",
                  `Assoc [ ("n", `Int emitted); ("filler", `String filler) ] );
              ]));
      flood (emitted + 1))
  in
  let emitted = flood 0 in
  ignore
    (Bus_stream.frame ~timeout:20.0 watcher "session:left" ~matches:left
      : Bus_stream.frame);
  stalled.drain ();
  Run.eventually "the reader to take every frame" 30.0 (fun () ->
      if List.length (Bus_stream.frames ~channel reader) = emitted then Some ()
      else None);
  Check.that "the reader's stream is still open" (not reader.ended)

let outrun (w : World.t) =
  let limit =
    Protocol.limit Gateway "POST" "/bus/subscribe" "replayBufferEvents"
  in
  (* A worker's role makes its emits unlimited: the flood is the replay's
     bound to meet, not this principal's emit bucket. *)
  let emitter, _ =
    World.agent w
      ~roles:[ Protocol.service_role (); Protocol.worker_role () ]
      "outrunner"
  in
  let token = World.person w "resumer" in
  let run live =
    let scope = "res-" ^ Fresh.uuid () in
    Hashtbl.replace w.archivist.events scope
      [ Archivist_standin.stored_event channel scope 2 ];
    w.archivist.mode.replay_delay <- 8.0;
    let stream =
      World.open_stream w token
        (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ())
           ~scoped:
             [
               Bus_stream.scope
                 ~last_event_id:(Printf.sprintf "p-%s-1" scope)
                 scope [ channel ];
             ]
           ())
    in
    Run.sleep 0.3;
    Eio.Fiber.List.iter ~max_fibers:32
      (fun n ->
        ignore
          (World.emit w emitter
             (`Assoc
                [
                  ("channel", `String channel);
                  ("payload", `Assoc [ ("n", `Int n) ]);
                  ("scope", `String scope);
                ])
            : Call.reply))
      (List.init live Fun.id);
    w.archivist.mode.replay_delay <- 0.0;
    stream
  in
  let within = run limit in
  Run.eventually "every live frame after the replay" 30.0 (fun () ->
      if List.length (Bus_stream.frames ~channel within) = limit + 1 then
        Some ()
      else None);
  Check.that "a replay within the bound keeps its stream" (not within.ended);
  let over = run (limit + 1) in
  Bus_stream.ends ~timeout:30.0 over;
  Check.that "a replay past the bound delivers no more than the bound"
    (List.length (Bus_stream.frames ~channel over) < limit + 2)

(* A connection of the case's own to the gateway: what the case writes goes
   as it is, and what comes back is read as answers, each framed by its
   Content-Length. *)
type raw = {
  socket : [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t;
  received : Buffer.t;
  mutable framed : bool;
  mutable next : int;  (** Where in [received] the next answer begins. *)
  answers : (string * int) Dynarray.t;
      (** Each whole answer's status line, and the byte it ends at. *)
  opened : float;
  closed : float Eio.Promise.t;  (** When it was closed, from either side. *)
  health : string;
}

let content_length =
  Re.compile (Re.no_case (Re.Perl.re {|\r\ncontent-length: *(\d+)|}))

let frame_answers raw =
  let text = Buffer.contents raw.received in
  let rec more () =
    if raw.framed then
      match Re.exec_opt ~pos:raw.next (Re.compile (Re.str "\r\n\r\n")) text with
      | None -> ()
      | Some found -> (
          let head = Re.Group.start found 0 in
          let head_text = String.sub text raw.next (head - raw.next) in
          match Re.exec_opt content_length head_text with
          | None -> raw.framed <- false
          | Some length ->
              let ends = head + 4 + int_of_string (Re.Group.get length 1) in
              if String.length text >= ends then (
                Dynarray.add_last raw.answers
                  ( String.sub head_text 0 (min 12 (String.length head_text)),
                    ends );
                raw.next <- ends;
                more ()))
  in
  more ()

(* [body] on a raw connection to [origin], ended after it. *)
let with_raw origin body =
  let uri = Uri.of_string origin in
  let host = Option.get (Uri.host uri) and port = Option.get (Uri.port uri) in
  Eio.Switch.run @@ fun sw ->
  let socket =
    Eio.Net.connect ~sw (Run.net ())
      (`Tcp (Eio_unix.Net.Ipaddr.of_unix (Unix.inet_addr_of_string host), port))
  in
  let closed, close = Eio.Promise.create () in
  let raw =
    {
      socket :> [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t;
      received = Buffer.create 4096;
      framed = true;
      next = 0;
      answers = Dynarray.create ();
      opened = Run.now ();
      closed;
      health =
        Printf.sprintf "GET %s HTTP/1.1\r\nHost: %s:%d\r\n"
          (Protocol.path Gateway "GET" "/api/health")
          host port;
    }
  in
  let stopping, stop = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Fiber.first
        (fun () -> Eio.Promise.await stopping)
        (fun () ->
          let buffer = Cstruct.create 65536 in
          let rec read () =
            match Eio.Flow.single_read socket buffer with
            | n ->
                Buffer.add_string raw.received (Cstruct.to_string ~len:n buffer);
                frame_answers raw;
                read ()
            | exception (End_of_file | Eio.Io _) -> ()
          in
          read ();
          Eio.Promise.resolve close (Run.now ())));
  Fun.protect
    ~finally:(fun () -> Eio.Promise.resolve stop ())
    (fun () -> body raw)

(* Ask for /api/health. *)
let ask raw = Eio.Flow.copy_string (raw.health ^ "\r\n") raw.socket

(* Write a request's head for /api/health, all but the blank line that ends
   it; say when. *)
let begin_head raw =
  Eio.Flow.copy_string raw.health raw.socket;
  Run.now ()

(* The [n]th answer, counted from 1, once it has come whole. *)
let answer raw n =
  Run.eventually (Printf.sprintf "answer %d" n) 10.0 (fun () ->
      Check.that
        ("every answer states its Content-Length:\n"
        ^ Buffer.contents raw.received)
        raw.framed;
      if Dynarray.length raw.answers >= n then
        Some (Dynarray.get raw.answers (n - 1))
      else None)

(* How long a connection is left idle after its answer before a head
   begins: long enough that a wait counted from the answer would end clearly
   sooner than one counted from the head's first byte. *)
let idle_before_head = 5.0

let head () =
  float_of_int (Protocol.document_limit Gateway "requestHeadSeconds")

let idle () =
  float_of_int (Protocol.document_limit Gateway "idleConnectionSeconds")

(* Past requestHeadSeconds by the margin a close is given: a connection kept
   only requestHeadSeconds after its answer is closed by then. *)
let past_head () = head () *. 1.3

(* Less a quarter second in each lower bound: the gateway's wait and the
   case's clock start a moment apart. *)
let closed_within what began ended =
  Check.that
    (Printf.sprintf "%s: closed after %g s, within %g s of requestHeadSeconds"
       what (ended -. began) (head ()))
    (ended -. began >= head () -. 0.25 && ended -. began < head () *. 1.3)

let sends_nothing (w : World.t) =
  with_raw (World.origin w) (fun raw ->
      let ended = Eio.Promise.await raw.closed in
      Check.that "nothing is written" (Buffer.length raw.received = 0);
      closed_within "a connection that sends nothing" raw.opened ended)

let kept_after_answer (w : World.t) =
  Check.that "idleConnectionSeconds outlasts the wait"
    (idle () > past_head () +. 5.0);
  with_raw (World.origin w) (fun raw ->
      ask raw;
      Check.equal "the first answer's status" (`String "HTTP/1.1 200")
        (`String (fst (answer raw 1)));
      Run.sleep (past_head ());
      ask raw;
      Check.equal "the second answer's status" (`String "HTTP/1.1 200")
        (`String (fst (answer raw 2))))

let head_left_unfinished (w : World.t) =
  with_raw (World.origin w) (fun raw ->
      ask raw;
      let status, ends = answer raw 1 in
      Check.equal "the answer's status" (`String "HTTP/1.1 200")
        (`String status);
      Run.sleep idle_before_head;
      let begun = begin_head raw in
      let ended = Eio.Promise.await raw.closed in
      Check.that "nothing is written after the answer"
        (Buffer.length raw.received = ends);
      closed_within "a head left unfinished" begun ended)

let tests implementations =
  let make = plain implementations in
  each_plane ~make ~world:Fun.id ~close:World.close "connection bounds"
    [
      case ~timeout:120.0
        "a subscriber that stops reading is disconnected once \
         pendingWriteBytes are waiting for it; one that reads is not"
        stops_reading;
      case ~timeout:120.0
        "a replay that more than replayBufferEvents live frames outrun is \
         abandoned; exactly that many are delivered after it"
        outrun;
    ]
  (* Plane-independent, and slow: each case waits out requestHeadSeconds. *)
  @ each_plane ~planes:[ In_process ] ~make ~world:Fun.id ~close:World.close
      "how long a connection waits"
      [
        case
          ~timeout:((head () *. 2.0) +. 10.0)
          "a connection that sends nothing is closed requestHeadSeconds after \
           it opened, with nothing written to it"
          sends_nothing;
        case
          ~timeout:(past_head () +. 30.0)
          "after an answer, a request sent past requestHeadSeconds later is \
           answered on the same connection"
          kept_after_answer;
        case
          ~timeout:(idle_before_head +. (head () *. 2.0) +. 20.0)
          "after an answer, a head begun and left unfinished is closed \
           requestHeadSeconds after its first byte, with nothing more written"
          head_left_unfinished;
      ]
