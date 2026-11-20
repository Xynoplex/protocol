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

let mark_of (f : Bus_stream.frame) =
  match Protocol.at f.payload [ "annotationId" ] with
  | Some (`String m) -> Some m
  | _ -> None

let marked mark f = mark_of f = Some mark

let focus ?scope ?(extra = []) mark =
  `Assoc
    ([
       ("channel", `String broadcast);
       ( "payload",
         `Assoc
           ([ ("annotationId", `String mark); ("resourceId", `String "res-1") ]
           @ extra) );
     ]
    @ match scope with Some s -> [ ("scope", `String s) ] | None -> [])

let body ?global ?scoped ?pending_replies ?(client_id = Fresh.uuid ()) () =
  Bus_stream.subscribe_body ~client_id ?global ?scoped ?pending_replies ()

let emitted what reply = status what 202 reply

let ask_request (w : World.t) token ~client_id correlation_id =
  World.emit w token
    (`Assoc
       [
         ("channel", `String request);
         ("payload", `Assoc [ ("resourceId", `String "r") ]);
         ("correlationId", `String correlation_id);
         ("clientId", `String client_id);
       ])

let gaps stream =
  `List
    (List.map
       (fun (f : Bus_stream.frame) -> f.payload)
       (Bus_stream.frames ~channel:"bus:resume-gap" stream))

let gap reason scope last =
  `Assoc
    [
      ("reason", `String reason);
      ("scope", `String scope);
      ("lastSeenId", `String last);
    ]

let ids messages =
  `List
    (List.map
       (fun (m : Bus_stream.message) ->
         match m.id with Some id -> `String id | None -> `Null)
       messages)

let strings items = `List (List.map (fun s -> `String s) items)

let not_validating { world = w; _ } =
  let token = World.person w "subscriber" in
  let entry fields = `Assoc fields in
  let channels cs = ("channels", strings cs) in
  let cases =
    [
      ("no clientId", `Assoc [ ("global", strings [ broadcast ]) ]);
      ("nothing subscribed", `Assoc [ ("clientId", `String (Fresh.uuid ())) ]);
      ( "a scope named twice",
        `Assoc
          [
            ("clientId", `String (Fresh.uuid ()));
            ( "scoped",
              `List
                [
                  entry [ ("scope", `String "r"); channels [ persisted ] ];
                  entry [ ("scope", `String "r"); channels [ other_persisted ] ];
                ] );
          ] );
      ( "a scoped entry with no channels",
        `Assoc
          [
            ("clientId", `String (Fresh.uuid ()));
            ("scoped", `List [ entry [ ("scope", `String "r"); channels [] ] ]);
          ] );
      ( "a scoped entry with no scope",
        `Assoc
          [
            ("clientId", `String (Fresh.uuid ()));
            ("scoped", `List [ entry [ channels [ persisted ] ] ]);
          ] );
      ( "a scoped entry with an empty scope",
        `Assoc
          [
            ("clientId", `String (Fresh.uuid ()));
            ( "scoped",
              `List [ entry [ ("scope", `String ""); channels [ persisted ] ] ]
            );
          ] );
      ( "pendingReplies that are not a list",
        `Assoc
          [
            ("clientId", `String (Fresh.uuid ()));
            ("global", strings [ broadcast ]);
            ("pendingReplies", `String "nope");
          ] );
      ( "pendingReplies that are not strings",
        `Assoc
          [
            ("clientId", `String (Fresh.uuid ()));
            ("global", strings [ broadcast ]);
            ("pendingReplies", `List [ `Int 1 ]);
          ] );
    ]
    (* A scope is a resource's id (identifiers/kinds.json). *)
    @ List.map
        (fun (r : Xynolog_protocol.Tables.Identifiers_kinds.accepts_item) ->
          ( Printf.sprintf "a scoped entry whose scope is %s: %s"
              (Yojson.Safe.to_string (`String r.id))
              r.why,
            `Assoc
              [
                ("clientId", `String (Fresh.uuid ()));
                ( "scoped",
                  `List
                    [
                      entry [ ("scope", `String r.id); channels [ persisted ] ];
                    ] );
              ] ))
        (Protocol.identifiers "ResourceId").refuses
  in
  List.iter
    (fun (why, body) ->
      let reply =
        Call.call ~token ~json:body (World.origin w) "POST"
          (Protocol.path Gateway "POST" "/bus/subscribe")
      in
      status why 400 reply;
      conforms "POST" "/bus/subscribe" reply)
    cases

let scoped_or_not { world = w; _ } =
  let token = World.person w "subscriber" in
  let scope = "res-" ^ Fresh.uuid () in
  let global = World.subscribe w token (body ~global:[ broadcast ] ()) in
  let scoped =
    World.subscribe w token
      (body ~scoped:[ Bus_stream.scope scope [ broadcast ] ] ())
  in
  let elsewhere =
    World.subscribe w token
      (body
         ~scoped:[ Bus_stream.scope ("res-" ^ Fresh.uuid ()) [ broadcast ] ]
         ())
  in
  let emitter = World.person w "emitter" in
  let unscoped_mark = Fresh.uuid () in
  ignore (World.emit w emitter (focus unscoped_mark) : Call.reply);
  let unscoped =
    Bus_stream.frame global broadcast ~matches:(marked unscoped_mark)
  in
  Check.that "an unscoped frame carries no scope" (unscoped.scope = None);
  let scoped_mark = Fresh.uuid () in
  ignore
    (World.emit w emitter
       (`Assoc
          [
            ("channel", `String broadcast);
            ( "payload",
              `Assoc
                [
                  ("annotationId", `String scoped_mark);
                  ("resourceId", `String scope);
                ] );
            ("scope", `String scope);
          ])
      : Call.reply);
  let in_scope =
    Bus_stream.frame scoped broadcast ~matches:(marked scoped_mark)
  in
  Check.that "a scoped frame carries its scope" (in_scope.scope = Some scope);
  Bus_stream.quiet global broadcast 0.3 ~matches:(marked scoped_mark);
  Bus_stream.quiet scoped broadcast 0.0 ~matches:(marked unscoped_mark);
  Bus_stream.quiet elsewhere broadcast 0.0 ~matches:(fun f ->
      marked scoped_mark f || marked unscoped_mark f)

let several_scopes { world = w; _ } =
  let token = World.person w "subscriber" in
  let a = "res-" ^ Fresh.uuid () and b = "res-" ^ Fresh.uuid () in
  let stream =
    World.subscribe w token
      (body ~global:[ broadcast ]
         ~scoped:
           [
             Bus_stream.scope a [ broadcast ]; Bus_stream.scope b [ broadcast ];
           ]
         ())
  in
  let emitter = World.person w "emitter" in
  let mark = Fresh.uuid () in
  List.iter
    (fun scope ->
      emitted "an emit"
        (World.emit w emitter
           (focus ?scope mark
              ~extra:[ ("at", `String (Option.value scope ~default:"global")) ])))
    [ Some a; Some b; None ];
  let mine () =
    List.filter (marked mark) (Bus_stream.frames ~channel:broadcast stream)
  in
  ignore
    (Bus_stream.next stream "all three frames" (fun _ ->
         List.length (mine ()) = 3)
      : Bus_stream.message);
  Check.equal "each frame's scope"
    (`List
       [
         `List [ `String a; `String a ];
         `List [ `String b; `String b ];
         `List [ `String "global"; `Null ];
       ])
    (`List
       (List.map
          (fun (f : Bus_stream.frame) ->
            `List
              [
                Check.at f.payload [ "at" ];
                (match f.scope with Some s -> `String s | None -> `Null);
              ])
          (mine ())))

let scopes_resume_alone { world = w; _ } =
  let replaying = "res-" ^ Fresh.uuid ()
  and fresh = "res-" ^ Fresh.uuid ()
  and broken = "res-" ^ Fresh.uuid () in
  set_events w replaying
    [
      Archivist_standin.stored_event persisted replaying 1;
      Archivist_standin.stored_event persisted replaying 2;
    ];
  set_events w fresh [ Archivist_standin.stored_event persisted fresh 1 ];
  let reads () =
    List.filter_map
      (fun (c : Archivist_standin.call) ->
        if String.starts_with ~prefix:"/events/" c.path then
          let path = List.hd (String.split_on_char '?' c.path) in
          Some
            (`String
               (Uri.pct_decode (String.sub path 8 (String.length path - 8))))
        else None)
      (Dynarray.to_list w.archivist.calls)
  in
  let before = List.length (reads ()) in
  let stream =
    World.open_stream w (World.person w "resumer")
      (body
         ~scoped:
           [
             Bus_stream.scope
               ~last_event_id:(Printf.sprintf "p-%s-1" replaying)
               replaying [ persisted ];
             Bus_stream.scope fresh [ persisted ];
             Bus_stream.scope ~last_event_id:"p-res-elsewhere-3" broken
               [ persisted ];
           ]
         ())
  in
  first_ping stream;
  Check.equal "what was replayed"
    (strings [ Printf.sprintf "p-%s-2" replaying ])
    (ids
       (List.filter
          (fun (m : Bus_stream.message) ->
            match m.frame with Some f -> f.channel = persisted | None -> false)
          (bus_events stream)));
  Check.equal "the gaps"
    (`List [ gap "scope-mismatch" broken "p-res-elsewhere-3" ])
    (gaps stream);
  Check.equal "the reads of the Archivist" (strings [ replaying ])
    (`List (List.filteri (fun i _ -> i >= before) (reads ())))

let every_subscriber_in_order { world = w; _ } =
  let first =
    World.subscribe w
      (World.person w "subscriber")
      (body ~global:[ broadcast ] ())
  in
  let second =
    World.subscribe w (World.person w "another") (body ~global:[ broadcast ] ())
  in
  let emitter = World.person w "emitter" in
  let marks = List.init 5 (fun _ -> Fresh.uuid ()) in
  List.iter
    (fun mark -> emitted "an emit" (World.emit w emitter (focus mark)))
    marks;
  List.iter
    (fun stream ->
      ignore
        (Bus_stream.next stream "the fifth frame" (fun _ ->
             List.exists
               (marked (List.nth marks 4))
               (Bus_stream.frames ~channel:broadcast stream))
          : Bus_stream.message);
      Check.equal "the frames, in order" (strings marks)
        (strings
           (List.filter_map
              (fun f ->
                match mark_of f with
                | Some m when List.mem m marks -> Some m
                | _ -> None)
              (Bus_stream.frames ~channel:broadcast stream))))
    [ first; second ]

let one_id_everywhere { world = w; _ } =
  (* One client's two connections, as a handoff overlaps them. *)
  let token = World.person w "handed-over" in
  let client_id = Fresh.uuid () in
  let old =
    World.subscribe w token (body ~client_id ~global:[ broadcast ] ())
  in
  let replacement =
    World.subscribe w token (body ~client_id ~global:[ broadcast ] ())
  in
  let emitter = World.person w "emitter" in
  let marks = [ Fresh.uuid (); Fresh.uuid () ] in
  List.iter
    (fun mark -> emitted "an emit" (World.emit w emitter (focus mark)))
    marks;
  let ids_on stream =
    List.map
      (fun mark ->
        (Bus_stream.next stream ("the frame marked " ^ mark) (fun m ->
             match m.frame with
             | Some f -> f.channel = broadcast && marked mark f
             | None -> false))
          .id)
      marks
  in
  let on_old = ids_on old in
  Check.that "both connections carry the same ids" (ids_on replacement = on_old);
  Check.that "the two frames carry different ids"
    (List.nth on_old 0 <> List.nth on_old 1)

let reply_reaches_requester_only { world = w; responder } =
  let alice = World.person w "alice" and bob = World.person w "bob" in
  let client_id = Fresh.uuid () in
  let result = result () in
  let requester =
    World.subscribe w alice (body ~client_id ~global:[ result ] ())
  in
  let same_client_other_person =
    World.subscribe w bob (body ~client_id ~global:[ result ] ())
  in
  let same_person_other_client =
    World.subscribe w alice (body ~global:[ result ] ())
  in
  let correlation_id = Fresh.uuid () in
  emitted "the request" (ask_request w alice ~client_id correlation_id);
  let answer =
    Bus_stream.frame requester result ~matches:(fun f ->
        f.correlation_id = Some correlation_id)
  in
  Check.equal "the reply"
    (Check.at described_result [ "response" ])
    (Check.at answer.payload [ "response" ]);
  Bus_stream.quiet same_client_other_person result 0.3;
  Bus_stream.quiet same_person_other_client result 0.0;
  (* A reply to a request nobody claimed reaches nobody. *)
  let unclaimed = Fresh.uuid () in
  ignore
    (World.emit w responder.token
       (`Assoc
          [
            ("channel", `String result);
            ("payload", described_result);
            ("correlationId", `String unclaimed);
          ])
      : Call.reply);
  Bus_stream.quiet requester result 0.3 ~matches:(fun f ->
      f.correlation_id = Some unclaimed)

let recovered_by_requester_only { world = w; _ } =
  let alice = World.person w "alice" in
  let client_id = Fresh.uuid () and correlation_id = Fresh.uuid () in
  let result = result () in
  emitted "the request" (ask_request w alice ~client_id correlation_id);
  Run.sleep 0.3;
  let bob = World.person w "bob" in
  let recovering token client_id =
    World.subscribe w token
      (body ~client_id ~global:[ result ] ~pending_replies:[ correlation_id ] ())
  in
  let stranger = recovering bob client_id in
  let other_client = recovering alice (Fresh.uuid ()) in
  let recovered = recovering alice client_id in
  (match
     List.find_opt
       (fun (f : Bus_stream.frame) -> f.correlation_id = Some correlation_id)
       (Bus_stream.frames ~channel:result recovered)
   with
  | Some frame ->
      Check.equal "the recovered reply"
        (Check.at described_result [ "response" ])
        (Check.at frame.payload [ "response" ])
  | None -> failwith "the requester recovered no reply");
  Check.that "a stranger recovers nothing"
    (Bus_stream.frames ~channel:result stranger = []);
  Check.that "another client recovers nothing"
    (Bus_stream.frames ~channel:result other_client = [])

let recovery_first_reply { world = w; responder } =
  let alice = World.person w "alice" in
  let client_id = Fresh.uuid () and correlation_id = Fresh.uuid () in
  let result = result () in
  emitted "the request" (ask_request w alice ~client_id correlation_id);
  Run.sleep 0.3;
  let second =
    Support.set_at
      [ "response"; "resource"; "name" ]
      (`String "the second reply") described_result
  in
  emitted "the second reply"
    (World.emit w responder.token
       (`Assoc
          [
            ("channel", `String result);
            ("payload", second);
            ("correlationId", `String correlation_id);
          ]));
  Run.sleep 0.3;
  let never = Fresh.uuid () in
  let recovered =
    World.subscribe w alice
      (body ~client_id ~global:[ result ]
         ~pending_replies:[ correlation_id; never ] ())
  in
  let replies = Bus_stream.frames ~channel:result recovered in
  Check.equal "the recovered correlationIds"
    (strings [ correlation_id ])
    (`List
       (List.map
          (fun (f : Bus_stream.frame) ->
            match f.correlation_id with Some c -> `String c | None -> `Null)
          replies));
  Check.equal "the first reply"
    (Check.at described_result [ "response" ])
    (Check.at (List.hd replies).payload [ "response" ])

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

let unhonoured_watermarks { world = w; _ } =
  let token = World.person w "resumer" in
  let opened scope last_event_id =
    let stream =
      World.open_stream w token
        (body
           ~scoped:[ Bus_stream.scope ~last_event_id scope [ persisted ] ]
           ())
    in
    first_ping stream;
    stream
  in
  Check.equal "an unparseable watermark's gap"
    (`List [ gap "unparseable-last-event-id" "res-gap-1" "not-a-watermark" ])
    (gaps (opened "res-gap-1" "not-a-watermark"));
  Check.equal "another scope's watermark's gap"
    (`List [ gap "scope-mismatch" "res-gap-2" "p-res-another-7" ])
    (gaps (opened "res-gap-2" "p-res-another-7"));
  set_events w "res-gap-3"
    [
      Archivist_standin.stored_event persisted "res-gap-3" 6;
      Archivist_standin.stored_event persisted "res-gap-3" 7;
    ];
  let events = bus_events (opened "res-gap-3" "p-res-gap-3-2") in
  Check.equal "what a scope past its retention carries"
    (strings [ "bus:resume-gap"; persisted; persisted ])
    (`List
       (List.map
          (fun (m : Bus_stream.message) ->
            match m.frame with Some f -> `String f.channel | None -> `Null)
          events));
  Check.equal "its gap"
    (gap "retention-exceeded" "res-gap-3" "p-res-gap-3-2")
    (Option.get (List.hd events).frame).payload;
  Check.equal "what follows it"
    (strings [ "p-res-gap-3-6"; "p-res-gap-3-7" ])
    (ids (List.tl events));
  w.archivist.mode.replay_fails <- true;
  Fun.protect
    ~finally:(fun () -> w.archivist.mode.replay_fails <- false)
    (fun () ->
      Check.equal "a failed read's gap"
        (`List [ gap "query-error" "res-gap-4" "p-res-gap-4-1" ])
        (gaps (opened "res-gap-4" "p-res-gap-4-1")))

let off_spec_answer { world = w; _ } =
  let token = World.person w "resumer" in
  (* An event with no type: were it read on trust, it would match no
     subscribed channel and be dropped without a word. *)
  let event =
    match Archivist_standin.stored_event persisted "res-gap-5" 2 with
    | `Assoc members -> `Assoc (List.remove_assoc "type" members)
    | other -> other
  in
  w.archivist.mode.replay_answer <-
    Some (`Assoc [ ("events", `List [ event ]); ("lastSequence", `Int 2) ]);
  Fun.protect
    ~finally:(fun () -> w.archivist.mode.replay_answer <- None)
    (fun () ->
      let stream =
        World.open_stream w token
          (body
             ~scoped:
               [
                 Bus_stream.scope ~last_event_id:"p-res-gap-5-1" "res-gap-5"
                   [ persisted ];
               ]
             ())
      in
      first_ping stream;
      Check.equal "the gap"
        (`List [ gap "query-error" "res-gap-5" "p-res-gap-5-1" ])
        (gaps stream);
      Check.that "nothing was replayed"
        (Bus_stream.frames ~channel:persisted stream = []));
  let off_spec = Dynarray.to_list w.archivist.violations in
  Dynarray.clear w.archivist.violations;
  Check.that
    (Printf.sprintf "one off-spec answer, not %d" (List.length off_spec))
    (List.length off_spec = 1);
  Check.that
    ("the off-spec answer is named: " ^ List.hd off_spec)
    (matches_re
       {|GET /events/res-gap-5: 200 body does not match the declared schema|}
       (List.hd off_spec))

let live_after_replay { world = w; _ } =
  let scope = "res-" ^ Fresh.uuid () in
  set_events w scope
    (List.map (Archivist_standin.stored_event persisted scope) [ 3; 4; 5 ]);
  w.archivist.mode.replay_delay <- 1.5;
  Fun.protect
    ~finally:(fun () -> w.archivist.mode.replay_delay <- 0.0)
    (fun () ->
      let token = World.person w "resumer" in
      let stream =
        World.open_stream w token
          (body
             ~scoped:
               [
                 Bus_stream.scope
                   ~last_event_id:(Printf.sprintf "p-%s-2" scope)
                   scope [ persisted ];
               ]
             ())
      in
      Run.sleep 0.3;
      let emitter, _ = World.agent w "recorder" in
      List.iter
        (fun n ->
          emitted "a live frame"
            (World.emit w emitter
               (`Assoc
                  [
                    ("channel", `String persisted);
                    ("payload", Archivist_standin.stored_event persisted scope n);
                    ("scope", `String scope);
                  ])))
        [ 4; 6 ];
      first_ping stream;
      ignore
        (Bus_stream.next stream "the live frame" (fun m ->
             m.id = Some (Printf.sprintf "p-%s-6" scope))
          : Bus_stream.message);
      Check.equal "the frames, in order"
        (strings (List.map (Printf.sprintf "p-%s-%d" scope) [ 3; 4; 5; 6 ]))
        (ids (bus_events stream)))

let presence { world = w; _ } =
  let watcher =
    World.subscribe w (World.person w "watcher")
      (body ~global:[ "session:joined"; "session:left" ] ())
  in
  let present =
    World.subscribe w (World.person w "present") (body ~global:[ broadcast ] ())
  in
  let joined =
    Bus_stream.frame watcher "session:joined" ~matches:(fun f ->
        Protocol.at f.payload [ "participant" ]
        = Some (`String (World.person_did w "present")))
  in
  let connection_id = Protocol.at joined.payload [ "connectionId" ] in
  (match connection_id with
  | Some (`String _) -> ()
  | _ -> failwith "session:joined names no connectionId");
  Bus_stream.close present;
  let left =
    Bus_stream.frame watcher "session:left" ~matches:(fun f ->
        Protocol.at f.payload [ "connectionId" ] = connection_id)
  in
  Check.equal "who left"
    (`String (World.person_did w "present"))
    (Check.at left.payload [ "participant" ])

let heartbeat (w : World.t) =
  let seconds =
    float_of_int
      (Protocol.limit Gateway "POST" "/bus/subscribe" "heartbeatSeconds")
  in
  let opened = Run.now () in
  let stream =
    World.open_stream w (World.person w "idle") (body ~global:[ broadcast ] ())
  in
  let first =
    Bus_stream.next ~timeout:5.0 stream "the first ping" (fun m ->
        m.event = "ping")
  in
  Check.that "the first ping follows the catch-up at once"
    (first.at -. opened < 2.0);
  let second =
    Bus_stream.next ~timeout:(seconds +. 5.0) stream "the second ping" (fun m ->
        m.event = "ping" && m != first)
  in
  Check.that
    (Printf.sprintf "the second ping came %g s after the first, every %g s"
       (second.at -. first.at) seconds)
    (second.at -. first.at >= seconds -. 0.5
    && second.at -. first.at < seconds +. 3.0)

let retention (w : World.t) =
  let seconds =
    float_of_int
      (Protocol.limit Gateway "POST" "/bus/subscribe" "replyRetentionSeconds")
  in
  let result = result () in
  ignore
    (World.responder w [ request ] (fun _ -> Some (result, described_result))
      : World.responder);
  let alice = World.person w "alice" in
  let client_id = Fresh.uuid () and correlation_id = Fresh.uuid () in
  emitted "the request" (ask_request w alice ~client_id correlation_id);
  Run.sleep 0.5;
  let recovering () =
    World.subscribe w alice
      (body ~client_id ~global:[ result ] ~pending_replies:[ correlation_id ] ())
  in
  let within = recovering () in
  Check.that "the reply is recovered within replyRetentionSeconds"
    (List.map
       (fun (f : Bus_stream.frame) -> f.correlation_id)
       (Bus_stream.frames ~channel:result within)
    = [ Some correlation_id ]);
  Bus_stream.close within;
  Run.sleep (seconds +. 2.0);
  let after = recovering () in
  Check.that "the reply is not recovered after it"
    (Bus_stream.frames ~channel:result after = []);
  status "the request again, while its claim stands" 409
    (ask_request w alice ~client_id correlation_id)

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
  let make_plain = plain implementations in
  each_plane ~make:(make implementations)
    ~world:(fun { world; _ } -> world)
    ~close:(fun { world; _ } -> World.close world)
    "subscribing"
    [
      case "a subscription that does not validate is a 400 ErrorResponse"
        not_validating;
      case
        "an unscoped frame reaches the global subscribers of its channel; a \
         scoped one only that scope's"
        scoped_or_not;
      case
        "one connection holds global channels and several scopes: each frame \
         arrives tagged with its own scope, a global one untagged"
        several_scopes;
      case
        "each scope on a connection resumes on its own: a watermarked one \
         replays, a fresh sibling stays silent, and a watermark that names no \
         position gaps only its own scope without a read"
        scopes_resume_alone;
      case
        "every subscriber of a channel receives each frame, in the order it \
         was emitted"
        every_subscriber_in_order;
      case
        "a frame with no correlationId and no position carries one id on every \
         connection, and no other frame's"
        one_id_everywhere;
      case
        "a reply reaches only the connections of the client and principal that \
         made the request"
        reply_reaches_requester_only;
      case
        "a reply published while its requester was away is recovered by naming \
         it in pendingReplies — by that requester only"
        recovered_by_requester_only;
      case
        "recovery returns the first reply to a request, and nothing for a \
         correlationId never claimed"
        recovery_first_reply;
      case
        "a scope's persisted events after its watermark are replayed in order, \
         on its channels only, before the live tail"
        replayed_in_order;
      case
        "each watermark the gateway cannot honour yields a scoped \
         bus:resume-gap naming why"
        unhonoured_watermarks;
      case
        "an answer the Archivist gives outside its spec is no replay: the \
         scope gets a query-error gap, not a silent skip"
        off_spec_answer;
      case
        "a replay longer than one answer of the Archivist is read an answer at \
         a time, and delivered whole and in order"
        replayed_an_answer_at_a_time;
      case
        "live frames arriving during a replay follow it, less any the replay \
         already delivered"
        live_after_replay;
      case
        "opening a stream publishes session:joined and closing it \
         session:left, naming the principal and the connection"
        presence;
    ]
  (* Plane-independent, and slow: waits out one heartbeat interval. *)
  @ each_plane ~planes:[ In_process ] ~make:make_plain ~world:Fun.id
      ~close:World.close "the heartbeat"
      [
        case
          "a ping follows the catch-up at once, then one every heartbeatSeconds"
          heartbeat;
      ]
  (* Plane-independent, and slow: waits out replyRetentionSeconds. *)
  @ each_plane ~planes:[ In_process ] ~make:make_plain ~world:Fun.id
      ~close:World.close "reply retention"
      [
        case ~timeout:120.0
          "a reply is recovered for replyRetentionSeconds, and not after — \
           while its claim still stands"
          retention;
      ]
