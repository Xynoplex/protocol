(* POST /bus/subscribe: what reaches which connection, replay and its gaps,
   the recovery of replies, presence and the heartbeat. Every message every
   stream here carries is also checked against BusStreamMessage and the id
   rules (harness/bus_stream.ml). Semantics from contract/TRANSPORT-HTTP.md
   § POST /bus/subscribe and § Event id and resumption. *)

open Xynolog_conformance
open Support

let broadcast = "beckon:focus"
let request = "browse:resource-requested"
let persisted = "mark:added"
let other_persisted = "mark:removed"

let described_result =
  `Assoc
    [
      ( "response",
        `Assoc
          [
            ( "resource",
              `Assoc
                [
                  ("@context", `String "https://schema.org/");
                  ("@id", `String "r");
                  ("name", `String "r");
                  ("representations", `List []);
                ] );
            ("lastSequence", `Int 1);
          ] );
    ]

let result () = (Protocol.operation_for request).result

type shared = {
  world : World.t;
  responder : World.responder;
      (** The one participant answering [request] in this file; its token can
          emit replies. *)
}

let make implementations plane =
  let world = plain implementations plane in
  let responder =
    World.responder world [ request ] (fun _ ->
        Some (result (), described_result))
  in
  { world; responder }

let bus_events (stream : Bus_stream.t) =
  List.filter
    (fun (m : Bus_stream.message) -> m.event = "bus-event")
    (Dynarray.to_list stream.messages)

let first_ping stream =
  ignore
    (Bus_stream.next stream "the first ping" (fun m -> m.event = "ping")
      : Bus_stream.message)

let set_events (w : World.t) scope events =
  Hashtbl.replace w.archivist.events scope events

let replayed_in_order { world = w; _ } =
  let scope = "res-" ^ Fresh.uuid () in
  set_events w scope
    (List.map
       (fun (channel, n) -> Archivist_standin.stored_event channel scope n)
       [
         (persisted, 1);
         (persisted, 2);
         (persisted, 3);
         (other_persisted, 4);
         (persisted, 5);
       ]);
  let token = World.person w "resumer" in
  let stream =
    World.open_stream w token
      (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ())
         ~scoped:
           [
             Bus_stream.scope
               ~last_event_id:(Printf.sprintf "p-%s-2" scope)
               scope [ persisted ];
           ]
         ())
  in
  first_ping stream;
  let replayed = bus_events stream in
  Check.equal "the replayed ids"
    (`List
       [
         `String (Printf.sprintf "p-%s-3" scope);
         `String (Printf.sprintf "p-%s-5" scope);
       ])
    (`List
       (List.map
          (fun (m : Bus_stream.message) -> `String (Option.get m.id))
          replayed));
  Check.that "every replayed frame is the scope's, on its channel"
    (List.for_all
       (fun (m : Bus_stream.message) ->
         match m.frame with
         | Some f -> f.scope = Some scope && f.channel = persisted
         | None -> false)
       replayed);
  Check.equal "the first replayed frame's payload"
    (Archivist_standin.stored_event persisted scope 3)
    (Option.get (List.hd replayed).frame).payload;
  let fresh =
    World.open_stream w token
      (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ())
         ~scoped:[ Bus_stream.scope scope [ persisted ] ]
         ())
  in
  first_ping fresh;
  Check.that "a fresh scope replays nothing" (bus_events fresh = [])

let replayed_an_answer_at_a_time { world = w; _ } =
  let scope = "res-" ^ Fresh.uuid () in
  let longest =
    Protocol.limit Archivist "GET" "/events/{resourceId}" "maxEvents"
  in
  let last = longest + 3 in
  set_events w scope
    (List.init last (fun i ->
         Archivist_standin.stored_event persisted scope (i + 1)));
  let reads () =
    List.filter_map
      (fun (c : Archivist_standin.call) ->
        if
          String.starts_with ~prefix:(Printf.sprintf "/events/%s?" scope) c.path
        then Some (`String c.path)
        else None)
      (Dynarray.to_list w.archivist.calls)
  in
  let stream =
    World.open_stream w (World.person w "resumer")
      (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ())
         ~scoped:
           [
             Bus_stream.scope
               ~last_event_id:(Printf.sprintf "p-%s-1" scope)
               scope [ persisted ];
           ]
         ())
  in
  first_ping stream;
  Check.equal "the replayed ids"
    (`List
       (List.init (last - 1) (fun i ->
            `String (Printf.sprintf "p-%s-%d" scope (i + 2)))))
    (`List
       (List.filter_map
          (fun (m : Bus_stream.message) ->
            match m.frame with
            | Some f when f.channel = persisted ->
                Some (`String (Option.get m.id))
            | _ -> None)
          (bus_events stream)));
  Check.that "no gap" (Bus_stream.frames ~channel:"bus:resume-gap" stream = []);
  Check.equal "the reads of the Archivist"
    (`List
       [
         `String (Printf.sprintf "/events/%s?fromSequence=2" scope);
         `String
           (Printf.sprintf "/events/%s?fromSequence=%d" scope (longest + 2));
       ])
    (`List (reads ()))

let tests implementations =
  each_plane ~make:(make implementations)
    ~world:(fun { world; _ } -> world)
    ~close:(fun { world; _ } -> World.close world)
    "subscribing"
    [
      case
        "a scope's persisted events after its watermark are replayed in order, \
         on its channels only, before the live tail"
        replayed_in_order;
      case
        "a replay longer than one answer of the Archivist is read an answer at \
         a time, and delivered whole and in order"
        replayed_an_answer_at_a_time;
    ]
