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
  mutable gateway : Gateway.t;
  replicas : Gateway.t Dynarray.t;  (** Further gateways a case started. *)
  streams : Bus_stream.t Dynarray.t;  (** Those a case opened. *)
  standing : Bus_stream.t Dynarray.t;  (** Those that outlive a case. *)
}

(* [env] changes the gateway's environment: a variable with a value is set,
   one with none is withheld. [document] changes its settings before it
   starts. [credentials] are what the broker admits alone; [jetstream] false
   runs it without JetStream. [kb] is the knowledge base the world is, when
   it is not the suite's own. [accounts] are service accounts the issuer
   grants beside the gateway's. *)
let create ~command ?(accounts = []) ?kb ?(document = Fun.id) ?(env = [])
    ?jetstream ?credentials plane =
  let kb =
    match kb with
    | Some kb -> kb
    | None -> Protocol.kb_identity Protocol.suite_kb
  in
  let secret = Fresh.hex 24 in
  let issuer =
    Issuer.start kb.resource
      ((gateway_client, { Issuer.secret; roles = [ Protocol.service_role () ] })
      :: accounts)
  in
  let archivist = Archivist_standin.start issuer in
  let broker =
    match (plane : Gateway.plane) with
    | Nats -> Some (Nats.start_broker ?jetstream ?credentials ())
    | In_process -> None
  in
  let base =
    [
      ("JWT_SECRET", Fresh.hex 32);
      ("XYNOLOG_OIDC_CLIENT_ID", gateway_client);
      ("XYNOLOG_OIDC_CLIENT_SECRET", secret);
    ]
  in
  let env =
    List.filter (fun (name, _) -> not (List.mem_assoc name env)) base
    @ List.filter_map
        (fun (name, value) -> Option.map (fun v -> (name, v)) value)
        env
  in
  let settings =
    document
      (Gateway.document ~kb ~issuer:issuer.origin
         ~archivist:(archivist.host, archivist.port)
         ~plane
         ?nats:(Option.map (fun (b : Nats.broker) -> b.url) broker)
         ())
  in
  let gateway = Gateway.start ~command ~document:settings env in
  {
    kb;
    plane;
    issuer;
    archivist;
    broker;
    gateway;
    replicas = Dynarray.create ();
    streams = Dynarray.create ();
    standing = Dynarray.create ();
  }

let origin t = t.gateway.origin
let env t = t.gateway.launch.env
let document t = t.gateway.launch.document

(* The knowledge base as a service's document names the one it serves. *)
let served t =
  `Assoc [ ("name", `String t.kb.name); ("domain", `String t.kb.domain) ]

let with_member name value = function
  | `Assoc members ->
      `Assoc (List.filter (fun (k, _) -> k <> name) members @ [ (name, value) ])
  | other -> other

(* A second gateway on this world's issuer, Archivist and broker, configured
   as the first: another replica. It reaches the broker at [servers] when
   that is given: through a proxy a case put in front of it. *)
let replica ?servers t =
  let port = Net.free_port () in
  let document =
    document t
    |> with_member "port" (`Int port)
    |> with_member "publicUrl"
         (`String (Printf.sprintf "http://127.0.0.1:%d" port))
  in
  let document =
    match (servers, Yojson.Safe.Util.member "signal" document) with
    | Some servers, (`Assoc _ as signal) ->
        with_member "signal"
          (with_member "servers" (`String servers) signal)
          document
    | _ -> document
  in
  let replica = Gateway.start_launch { t.gateway.launch with document } in
  Dynarray.add_last t.replicas replica;
  replica

(* Stop this world's gateway and start it again on the same port and
   settings. *)
let restart t =
  Gateway.stop t.gateway;
  t.gateway <- Gateway.start_launch t.gateway.launch

(* A token signed with this gateway's own signing key, the head of its
   JWT_SECRET ring, or with [key]: what a case forges to see the gateway
   refuse what it would never have minted. [claims] alone, under HS256. *)
let signed ?key t claims =
  let key =
    match key with
    | Some key -> key
    | None ->
        String.trim
          (List.hd (String.split_on_char ',' (List.assoc "JWT_SECRET" (env t))))
  in
  let jwk = Jose.Jwk.make_oct key in
  match
    Jose.Jwt.sign
      ~header:(Jose.Header.make_header ~typ:"JWT" ~alg:`HS256 jwk)
      ~payload:
        (List.fold_left
           (fun payload (name, value) -> Jose.Jwt.add_claim name value payload)
           Jose.Jwt.empty_payload claims)
      jwk
  with
  | Ok jwt -> Jose.Jwt.to_string jwt
  | Error (`Msg why) -> failwith ("could not sign a token: " ^ why)

let person t ?claims sub = Issuer.person t.issuer ?claims sub
let person_did t sub = Protocol.person_did t.kb.domain sub

(* An agent's token, its own account's, for an account the suite names
   [name]; and the DID the gateway names it by. *)
let agent t ?(roles = [ Protocol.service_role () ]) name =
  let client = "conformance-agent-" ^ name in
  ( Issuer.service t.issuer ~roles client,
    Protocol.agent_did t.kb.domain (Issuer.subject_of client) )

(* Open a stream without waiting for it to catch up: a replay case reads
   what comes first. Fails unless the gateway answers 200. *)
let open_stream ?origin:at ?(standing = false) t token body =
  match
    Bus_stream.subscribe (Option.value at ~default:(origin t)) (Some token) body
  with
  | Refused reply ->
      failwith
        (Printf.sprintf "subscribe answered %d: %s" reply.status reply.text)
  | Stream stream ->
      Dynarray.add_last (if standing then t.standing else t.streams) stream;
      stream

(* Open a stream; fails unless the gateway answers 200. It is open once its
   first message has arrived, after any replay. *)
let subscribe ?origin ?standing t token body =
  let stream = open_stream ?origin ?standing t token body in
  ignore
    (Bus_stream.next stream "the stream to catch up" (fun _ -> true)
      : Bus_stream.message);
  stream

(* An emit, as an SDK makes one: under a new Idempotency-Key, or under [key]
   when a case sends one again. *)
let emit ?origin:at ?key t token body =
  Call.call ~token ~json:body
    ~headers:
      [
        ("idempotency-key", match key with Some k -> k | None -> Fresh.uuid ());
      ]
    (Option.value at ~default:(origin t))
    "POST"
    (Protocol.path Gateway "POST" "/bus/emit")

(* An upload, as an SDK makes one: [parts] as a multipart form, under a new
   Idempotency-Key, or under [key] when a case sends one again. *)
let upload ?key t token parts =
  let content_type, body = Call.multipart parts in
  Call.call ~token ~body
    ~headers:
      [
        ("content-type", content_type);
        ("idempotency-key", match key with Some k -> k | None -> Fresh.uuid ());
      ]
    (origin t) "POST"
    (Protocol.path Gateway "POST" "/resources")

type responder = {
  stream : Bus_stream.t;
  token : string;  (** The agent token it emits with. *)
  client_id : string;
}

(* A participant the suite plays: subscribed to [channels], answering each
   frame with what [answer] returns, emitted with the frame's correlationId.
   It stands for the rest of the file. *)
let responder ?origin t channels answer =
  let token, _ = agent t "participant" in
  let client_id = Fresh.uuid () in
  let stream =
    subscribe ?origin ~standing:true t token
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
                    emit ?origin t token
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
      | _ -> ());
  { stream; token; client_id }

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
  Dynarray.iter Gateway.stop t.replicas;
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
