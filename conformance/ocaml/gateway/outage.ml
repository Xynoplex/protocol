(* The broker going away under a running gateway: a stream open across the
   outage carries on without reconnecting — the gateway reconnects on its
   own — and an emit made while it is down is refused, never accepted and
   lost (contract/TRANSPORT-HTTP.md § POST /bus/emit;
   https://github.com/xynolog/docs/blob/main/operator/administration/TROUBLESHOOTING.md
   § Commands fail, or real-time updates stop). A broker that comes back
   refusing the gateway's credentials is not recovered from, and every emit
   after it is refused. NATS only. *)

open Xynolog_conformance
open Support

let broadcast = "beckon:focus"

(* Where the configuration document tells the gateway to find its broker
   credential. *)
let user = "CONFORMANCE_BROKER_USER"
let password = "CONFORMANCE_BROKER_PASSWORD"

(* A broker that admits only a user and password, and a gateway given
   them. *)
let with_credentials implementations plane =
  World.create ~command:implementations.Implementations.gateway
    ~credentials:{ Nats.user = "gateway"; password = "the-password" }
    ~document:(fun document ->
      World.with_member "signal"
        (World.with_member "userEnv" (`String user)
           (World.with_member "passwordEnv" (`String password)
              (Yojson.Safe.Util.member "signal" document)))
        document)
    ~env:[ (user, Some "gateway"); (password, Some "the-password") ]
    plane

let focus ?annotation () =
  `Assoc
    [
      ("channel", `String broadcast);
      ( "payload",
        `Assoc
          ((match annotation with
             | Some id -> [ ("annotationId", `String id) ]
             | None -> [])
          @ [ ("resourceId", `String "res-1") ]) );
    ]

let refused_until what timeout (w : World.t) emitter =
  Run.eventually what timeout (fun () ->
      let reply = World.emit w emitter (focus ()) in
      if reply.status = 503 then Some reply else None)

let across_an_outage (w : World.t) =
  let broker = Option.get w.broker in
  let watcher =
    World.subscribe w (World.person w "watcher")
      (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ())
         ~global:[ broadcast ] ())
  in
  let emitter = World.person w "emitter" in
  Nats.down broker;
  conforms "POST" "/bus/emit"
    (refused_until "an emit to be refused while the broker is down" 10.0 w
       emitter);
  Nats.up broker;
  let after = Fresh.uuid () in
  Run.eventually "an emit after the broker is back to reach the stream" 20.0
    (fun () ->
      if (World.emit w emitter (focus ~annotation:after ())).status <> 202 then
        None
      else (
        Run.sleep 0.25;
        if
          List.exists
            (fun (f : Bus_stream.frame) ->
              Protocol.at f.payload [ "annotationId" ] = Some (`String after))
            (Bus_stream.frames ~channel:broadcast watcher)
        then Some ()
        else None));
  Check.that "the stream stayed open across the outage" (not watcher.ended)

let other_credentials (w : World.t) =
  let emitter = World.person w "emitter" in
  status "an emit before the restart" 202 (World.emit w emitter (focus ()));
  Nats.restart
    ~credentials:{ user = "gateway"; password = "another-password" }
    (Option.get w.broker);
  conforms "POST" "/bus/emit"
    (refused_until "an emit to be refused" 30.0 w emitter);
  Run.sleep 3.0;
  let still = World.emit w emitter (focus ()) in
  status "an emit long after the restart" 503 still;
  conforms "POST" "/bus/emit" still

let tests implementations =
  let make = with_credentials implementations in
  each_plane ~planes:[ Nats ] ~make ~world:Fun.id ~close:World.close
    "a broker outage"
    [
      case ~timeout:40.0
        "while the broker is down an emit is refused with 503, never accepted \
         and lost; a stream open across the outage carries frames again once \
         it is back, without reconnecting"
        across_an_outage;
    ]
  @ each_plane ~planes:[ Nats ] ~make ~world:Fun.id ~close:World.close
      "a broker that comes back with other credentials"
      [
        case ~timeout:60.0
          "is not recovered from: every emit after it is refused with 503, and \
           stays refused"
          other_credentials;
      ]
