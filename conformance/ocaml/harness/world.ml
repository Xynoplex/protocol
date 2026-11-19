(* A world: everything one gateway needs around it — the trusted issuer, a
   stand-in Archivist, a broker when the plane is NATS — and the gateway
   itself, with the tokens a case signs in with. *)

(* The service account the gateway reaches the Archivist with. *)
let gateway_client = "xynolog-gateway"

type t = {
  kb : Protocol.kb;
  plane : Gateway.plane;
  issuer : Issuer.t;
  archivist : Archivist_standin.t;
  broker : Nats.broker option;
  env : (string * string) list;
  gateway : Gateway.t;
  streams : Bus_stream.t Dynarray.t;  (** Those a case opened. *)
  standing : Bus_stream.t Dynarray.t;  (** Those that outlive a case. *)
}

let create ~command ?(accounts = []) plane =
  let kb = Protocol.kb_identity Protocol.suite_kb in
  let secret = Fresh.hex 24 in
  let issuer =
    Issuer.start kb.resource
      ((gateway_client, { Issuer.secret; roles = [ Protocol.service_role () ] })
      :: accounts)
  in
  let archivist = Archivist_standin.start issuer in
  let broker =
    match (plane : Gateway.plane) with
    | Nats -> Some (Nats.start_broker ())
    | In_process -> None
  in
  let env =
    [
      ("JWT_SECRET", Fresh.hex 32);
      ("XYNOLOG_OIDC_CLIENT_ID", gateway_client);
      ("XYNOLOG_OIDC_CLIENT_SECRET", secret);
    ]
  in
  let document =
    Gateway.document ~kb ~issuer:issuer.origin
      ~archivist:(archivist.host, archivist.port)
      ~plane
      ?nats:(Option.map (fun (b : Nats.broker) -> b.url) broker)
      ()
  in
  let gateway = Gateway.start ~command ~document env in
  {
    kb;
    plane;
    issuer;
    archivist;
    broker;
    env;
    gateway;
    streams = Dynarray.create ();
    standing = Dynarray.create ();
  }

let origin t = t.gateway.origin

(* The knowledge base as a service's document names the one it serves. *)
let served t =
  `Assoc [ ("name", `String t.kb.name); ("domain", `String t.kb.domain) ]

let person t sub = Issuer.person t.issuer sub
let person_did t sub = Protocol.person_did t.kb.domain sub

(* An agent's token, its own account's, for an account the suite names
   [name]; and the DID the gateway names it by. *)
let agent t ?(roles = [ Protocol.service_role () ]) name =
  let client = "conformance-agent-" ^ name in
  ( Issuer.service t.issuer ~roles client,
    Protocol.agent_did t.kb.domain (Issuer.subject_of client) )

(* Open a stream; fails unless the gateway answers 200. It is open once its
   first message has arrived, after any replay. *)
let subscribe ?(standing = false) t token body =
  match Bus_stream.subscribe (origin t) (Some token) body with
  | Refused reply ->
      failwith
        (Printf.sprintf "subscribe answered %d: %s" reply.status reply.text)
  | Stream stream ->
      Dynarray.add_last (if standing then t.standing else t.streams) stream;
      ignore
        (Bus_stream.next stream "the stream to catch up" (fun _ -> true)
          : Bus_stream.message);
      stream

(* An emit, as an SDK makes one: under a new Idempotency-Key. *)
let emit t token body =
  Call.call ~token ~json:body
    ~headers:[ ("idempotency-key", Fresh.uuid ()) ]
    (origin t) "POST"
    (Protocol.path Gateway "POST" "/bus/emit")

(* A participant the suite plays: subscribed to [channels], answering each
   frame with what [answer] returns, emitted with the frame's correlationId.
   It stands for the rest of the file. *)
let responder t channels answer =
  let token, _ = agent t "participant" in
  let client_id = Fresh.uuid () in
  let stream =
    subscribe ~standing:true t token
      (Bus_stream.subscribe_body ~client_id ~global:channels ())
  in
  Bus_stream.on stream (fun message ->
      match message.frame with
      | Some frame when List.mem frame.channel channels -> (
          match answer frame with
          | None -> ()
          | Some (channel, payload) ->
              Eio.Fiber.fork ~sw:(Run.sw ()) (fun () ->
                  match
                    emit t token
                      (`Assoc
                         ([ ("channel", `String channel); ("payload", payload) ]
                         @ (match frame.correlation_id with
                           | Some c -> [ ("correlationId", `String c) ]
                           | None -> [])
                         @ [ ("clientId", `String client_id) ]))
                  with
                  | { status = 202; _ } -> ()
                  | reply ->
                      Bus_stream.violation stream
                        (Printf.sprintf
                           "the gateway refused the responder's %s: %d %s"
                           channel reply.status reply.text)
                  | exception failure ->
                      Bus_stream.violation stream
                        (Printf.sprintf "the responder could not emit %s: %s"
                           channel
                           (Printexc.to_string failure))))
      | _ -> ())

(* Every violation since the last call: carried by the streams a case
   opened, which are then closed, and by the standing ones, and anything the
   gateway sent the Archivist, or the stand-in answered, outside its spec. *)
let drain t =
  let taken list =
    let all = Dynarray.to_list list in
    Dynarray.clear list;
    all
  in
  let violations =
    List.concat_map
      (fun (s : Bus_stream.t) -> taken s.violations)
      (Dynarray.to_list t.streams @ Dynarray.to_list t.standing)
    @ taken t.archivist.violations
  in
  Dynarray.iter Bus_stream.close t.streams;
  Dynarray.clear t.streams;
  violations

let close t =
  ignore (drain t : string list);
  Dynarray.iter Bus_stream.close t.standing;
  Gateway.stop t.gateway;
  Option.iter Nats.stop_broker t.broker;
  Archivist_standin.close t.archivist;
  Issuer.close t.issuer

(* The end of a case, as every suite ends one: [account] is what the case
   shows of what its processes said while it ran, read before [settle]
   forgets the case. The account is shown when the case failed or settling
   found anything, and the case fails when settling found anything. *)
let end_case ~account ~settle body =
  match body () with
  | () -> (
      match settle () with
      | [] -> ()
      | violations ->
          prerr_endline (account ());
          failwith
            (String.concat "\n"
               ("the case let pass what no case may:" :: violations)))
  | exception failure ->
      let trace = Printexc.get_raw_backtrace () in
      prerr_endline (account ());
      (match settle () with
      | [] -> ()
      | violations ->
          prerr_endline
            (String.concat "\n"
               ("and the case let pass what no case may:" :: violations)));
      Printexc.raise_with_backtrace failure trace
