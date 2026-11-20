(* What the gateway leaves on the broker it shares with the dispatcher. The
   signal plane is never a record: no frame it carries is captured by a
   stream (contract/EVENT-BUS.md § Where the bus lives) — the only streams it
   creates are the key-value tables its ledger keeps claims and retained
   replies in — and nothing it publishes lands in the dispatcher's job
   stream, which shares the broker. NATS only. *)

open Xynolog_conformance
open Case

let request = "browse:resource-requested"
let jobs () = (Protocol.tables ()).jobs_storage.stream.name

type world = { world : World.t; nc : Nats.client }

let make implementations plane =
  let world = Support.plain implementations plane in
  let broker = Option.get world.broker in
  let nc = Nats.connect broker.port in
  (* The dispatcher's stream, as the dispatcher declares it. *)
  let subjects =
    [ `String ((Protocol.tables ()).jobs_storage.stream.subject_root ^ ".>") ]
  in
  ignore
    (Nats.jetstream nc
       ("STREAM.CREATE." ^ jobs ())
       (`Assoc [ ("name", `String (jobs ())); ("subjects", `List subjects) ]));
  { world; nc }

let close { world; nc } =
  nc.close ();
  World.close world

let holds_no_stream { world; nc } =
  (* Traffic of every kind: a broadcast, a scoped broadcast, a request, its reply, a recovery. *)
  let result = (Protocol.operation_for request).result in
  ignore
  @@ World.responder world [ request ] (fun _ ->
      Some (result, Support.described ()));
  let alice = World.person world "alice" in
  let client_id = Fresh.uuid () in
  let stream =
    World.subscribe world alice
      (Bus_stream.subscribe_body ~client_id ~global:[ result; "beckon:focus" ]
         ~scoped:[ Bus_stream.scope "res-broker" [ "beckon:focus" ] ]
         ())
  in
  let emitted body =
    let reply = World.emit world alice body in
    if reply.status <> 202 then
      failwith (Printf.sprintf "emit answered %d: %s" reply.status reply.text)
  in
  let focus = `Assoc [ ("resourceId", `String "res-broker") ] in
  emitted (`Assoc [ ("channel", `String "beckon:focus"); ("payload", focus) ]);
  emitted
    (`Assoc
       [
         ("channel", `String "beckon:focus");
         ("payload", focus);
         ("scope", `String "res-broker");
       ]);
  let correlation_id = Fresh.uuid () in
  emitted
    (`Assoc
       [
         ("channel", `String request);
         ("payload", `Assoc [ ("resourceId", `String "r") ]);
         ("correlationId", `String correlation_id);
         ("clientId", `String client_id);
       ]);
  ignore
    (Bus_stream.frame stream result ~matches:(fun f ->
         f.correlation_id = Some correlation_id)
      : Bus_stream.frame);
  ignore
    (World.subscribe world alice
       (Bus_stream.subscribe_body ~client_id ~global:[ result ]
          ~pending_replies:[ correlation_id ] ())
      : Bus_stream.t);

  let listed = Nats.jetstream nc "STREAM.LIST" (`Assoc []) in
  let names =
    List.map
      (fun s ->
        Yojson.Safe.Util.(member "config" s |> member "name" |> to_string))
      Yojson.Safe.Util.(to_list (member "streams" listed))
  in
  let kv name = String.starts_with ~prefix:"KV_" name in
  (match List.filter (fun n -> n <> jobs () && not (kv n)) names with
  | [] -> ()
  | others ->
      failwith
        ("the broker holds streams the gateway made beside its tables: "
       ^ String.concat ", " others));
  if not (List.exists kv names) then
    failwith "the gateway made no key-value table";
  let info = Nats.jetstream nc ("STREAM.INFO." ^ jobs ()) (`Assoc []) in
  match
    Yojson.Safe.Util.(member "state" info |> member "messages" |> to_int)
  with
  | 0 -> ()
  | n ->
      failwith
        (Printf.sprintf "the job stream holds %d messages the gateway published"
           n)

let tests implementations =
  Support.(
    World.each_plane ~planes:[ Nats ] ~make:(make implementations)
      ~world:(fun { world; _ } -> world)
      ~close "the broker"
      [
        case ~timeout:120.0
          "holds no stream the gateway made but its key-value tables, and \
           nothing it published is in the job stream"
          holds_no_stream;
      ])
