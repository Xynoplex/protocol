(* An emit or an upload sent again under its Idempotency-Key
   (specs/src/paths/emit-on-bus.json and paths/upload-resource.json, A key):
   a repeat with the body its key was first sent with is answered as the
   first was, and carried out once; one with another body is refused 422;
   one that arrives while the first is in hand waits for it; a key is its
   principal's; and one without a key, or with a key that is not 1 to 255
   characters, is refused 400. The gateway answers a repeated emit itself,
   and passes an upload's key to the Archivist, whose stand-in answers its
   repeats (harness/archivist_standin.ml). A key recognised on another
   replica: replicas.ml; an upload recorded and never answered:
   archivist_waits.ml. *)

open Xynolog_conformance
open Support
open Case

(* A broadcast every participant may emit: a command, not an operation's
   request. *)
let broadcast = "beckon:focus"

(* An operation's request whose payload validates with resourceId alone. *)
let request = "browse:resource-requested"

(* A command a watcher can pick out by [marker]. *)
let command marker =
  `Assoc
    [
      ("channel", `String broadcast);
      ( "payload",
        `Assoc
          [ ("annotationId", `String marker); ("resourceId", `String "res-1") ]
      );
    ]

let marked marker (f : Bus_stream.frame) =
  Protocol.at f.payload [ "annotationId" ] = Some (`String marker)

(* What a repeat must equal: the first's status and body. *)
let answered (reply : Call.reply) =
  `Assoc
    [
      ("status", `Int reply.status);
      ("body", match reply.json with Some json -> json | None -> `Null);
    ]

(* The frames on [channel] that [matches] accepts, once the first has come
   and a second has had time to. *)
let settled stream channel matches =
  ignore (Bus_stream.frame stream channel ~matches : Bus_stream.frame);
  Run.sleep 0.5;
  List.filter matches (Bus_stream.frames ~channel stream)

let exactly_one what frames =
  Check.that
    (Printf.sprintf "%s: %d frames, not one" what (List.length frames))
    (List.length frames = 1)

(* The longest Idempotency-Key the spec admits on POST [path]. *)
let longest_key path =
  match
    List.find_map
      (fun (p : Xynolog_protocol_schema.Openapi.parameter) ->
        match p.schema with
        | Xynolog_protocol_schema.Schema.Schema { max_length = Some n; _ }
          when p.name = "Idempotency-Key" ->
            Some n
        | _ -> None)
      (match (Protocol.operation Gateway "POST" path).parameters with
      | Some parameters -> parameters
      | None -> failwith ("POST " ^ path ^ " declares no parameters"))
  with
  | Some n -> n
  | None ->
      failwith ("POST " ^ path ^ " declares no Idempotency-Key with a maxLength")

(* The keys the spec refuses on POST [path]: none, an empty one, and one a
   character too long. *)
let refused_keys path =
  let longest = longest_key path in
  [
    ("no key", []);
    ("an empty key", [ ("idempotency-key", "") ]);
    ( Printf.sprintf "a key of %d characters" (longest + 1),
      [ ("idempotency-key", String.make (longest + 1) 'k') ] );
  ]

(* A key as long as the longest the spec admits. *)
let padded length =
  let uuid = Fresh.uuid () in
  uuid ^ String.make (length - String.length uuid) 'k'

(* An upload's fields, at a storageUri of its own. *)
let upload_fields () =
  [
    ("name", "Keyed");
    ("format", "text/plain");
    ("storageUri", "file://uploads/" ^ Fresh.uuid ());
  ]

let form fields bytes =
  List.map (fun (name, value) -> Call.Field (name, value)) fields
  @ [
      Call.File
        { name = "file"; filename = "keyed.txt"; media = "text/plain"; bytes };
    ]

let storage_uri fields = List.assoc "storageUri" fields

let uploads_of (w : World.t) fields =
  List.filter
    (fun (u : Archivist_standin.upload) ->
      List.assoc_opt "storageUri" u.fields = Some (storage_uri fields))
    (Dynarray.to_list w.archivist.uploads)

let watcher (w : World.t) =
  World.subscribe w (World.person w "watcher")
    (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ()) ~global:[ broadcast ]
       ())

let emit_path () = Protocol.path Gateway "POST" "/bus/emit"

let emit_without_key (w : World.t) =
  let watching = watcher w in
  let token = World.person w "keyless" in
  let refused =
    List.map
      (fun (why, headers) ->
        let marker = Fresh.uuid () in
        let reply =
          Call.call ~token ~json:(command marker) ~headers (World.origin w)
            "POST" (emit_path ())
        in
        status why 400 reply;
        conforms "POST" "/bus/emit" reply;
        marker)
      (refused_keys "/bus/emit")
  in
  let taken =
    World.emit w token
      (command (Fresh.uuid ()))
      ~key:(padded (longest_key "/bus/emit"))
  in
  status "an emit under the longest key" 202 taken;
  Bus_stream.quiet watching broadcast 0.5 ~matches:(fun f ->
      List.exists (fun marker -> marked marker f) refused)

let upload_without_key (w : World.t) =
  let token = World.person w "keyless-uploader" in
  let uploads_sent () =
    List.length
      (List.filter
         (fun (c : Archivist_standin.call) ->
           c.meth = "POST" && c.path = "/resources")
         (Dynarray.to_list w.archivist.calls))
  in
  let sent = uploads_sent () in
  List.iter
    (fun (why, headers) ->
      let content_type, body =
        Call.multipart (form (upload_fields ()) "the bytes")
      in
      let reply =
        Call.call ~token ~body
          ~headers:(("content-type", content_type) :: headers)
          (World.origin w) "POST"
          (Protocol.path Gateway "POST" "/resources")
      in
      status why 400 reply;
      conforms "POST" "/resources" reply)
    (refused_keys "/resources");
  Check.that "no refused upload reached the Archivist" (uploads_sent () = sent);
  let key = padded (longest_key "/resources") in
  let fields = upload_fields () in
  let taken = World.upload w token (form fields "the bytes") ~key in
  status "an upload under the longest key" 202 taken;
  Check.that "the Archivist was sent the key as it came"
    (match uploads_of w fields with
    | upload :: _ -> upload.key = key
    | [] -> false)

let command_repeated (w : World.t) =
  let watching = watcher w in
  let token = World.person w "repeats" in
  let marker = Fresh.uuid () and key = Fresh.uuid () in
  let first = World.emit w token (command marker) ~key in
  status "the first" 202 first;
  let again = World.emit w token (command marker) ~key in
  conforms "POST" "/bus/emit" again;
  Check.equal "the repeat's answer" (answered first) (answered again);
  exactly_one "the command's frames"
    (settled watching broadcast (marked marker))

let request_repeated (w : World.t) =
  let result = (Protocol.operation_for request).result in
  (* Heard, and left unanswered until the case answers it, so the claim is
     live when the repeat arrives. *)
  let server, _ = World.agent w "answers-by-hand" in
  let heard =
    World.subscribe w server
      (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ()) ~global:[ request ]
         ())
  in
  let requester = World.person w "requests-twice" in
  let client_id = Fresh.uuid () in
  let replies =
    World.subscribe w requester
      (Bus_stream.subscribe_body ~client_id ~global:[ result ] ())
  in
  let correlation_id = Fresh.uuid () and key = Fresh.uuid () in
  let body =
    `Assoc
      [
        ("channel", `String request);
        ("payload", `Assoc [ ("resourceId", `String "r") ]);
        ("correlationId", `String correlation_id);
        ("clientId", `String client_id);
      ]
  in
  let first = World.emit w requester body ~key in
  status "the first" 202 first;
  let again = World.emit w requester body ~key in
  conforms "POST" "/bus/emit" again;
  Check.equal "the repeat's answer" (answered first) (answered again);
  let ours (f : Bus_stream.frame) = f.correlation_id = Some correlation_id in
  ignore (Bus_stream.frame heard request ~matches:ours : Bus_stream.frame);
  let reply =
    World.emit w server
      (`Assoc
         [
           ("channel", `String result);
           ("payload", described ());
           ("correlationId", `String correlation_id);
         ])
  in
  status "the reply" 202 reply;
  let delivered = Bus_stream.frame replies result ~matches:ours in
  Check.equal "the delivered reply"
    (Check.at (described ()) [ "response" ])
    (Check.at delivered.payload [ "response" ]);
  exactly_one "the request's frames" (settled heard request ours)

let other_body (w : World.t) =
  let watching = watcher w in
  let token = World.person w "reuses-a-key" in
  let key = Fresh.uuid () in
  let first_marker = Fresh.uuid () and other_marker = Fresh.uuid () in
  let first = World.emit w token (command first_marker) ~key in
  status "the first" 202 first;
  List.iter
    (fun attempt ->
      let refused = World.emit w token (command other_marker) ~key in
      status attempt 422 refused;
      conforms "POST" "/bus/emit" refused;
      Check.equal attempt
        (`String (Protocol.message_code "idempotency-key-reused"))
        (`String (text_member "code" refused)))
    [ "once"; "and again" ];
  let again = World.emit w token (command first_marker) ~key in
  Check.equal "the first body's repeat" (answered first) (answered again);
  Bus_stream.quiet watching broadcast 0.5 ~matches:(marked other_marker);
  exactly_one "the first command's frames"
    (List.filter (marked first_marker)
       (Bus_stream.frames ~channel:broadcast watching))

let at_once (w : World.t) =
  let watching = watcher w in
  let token = World.person w "sends-twice-at-once" in
  let marker = Fresh.uuid () and key = Fresh.uuid () in
  let one, two =
    Eio.Fiber.pair
      (fun () -> World.emit w token (command marker) ~key)
      (fun () -> World.emit w token (command marker) ~key)
  in
  status "one of the two" 202 one;
  List.iter (conforms "POST" "/bus/emit") [ one; two ];
  Check.equal "the other's answer" (answered one) (answered two);
  exactly_one "the command's frames"
    (settled watching broadcast (marked marker))

let principal_key (w : World.t) =
  let watching = watcher w in
  let person = World.person w "shares-a-key" in
  let agent, agent_did = World.agent w "shares-a-key" in
  let marker = Fresh.uuid () and key = Fresh.uuid () in
  List.iter
    (fun token ->
      status "an emit" 202 (World.emit w token (command marker) ~key))
    [ person; agent ];
  let ours () =
    List.filter (marked marker) (Bus_stream.frames ~channel:broadcast watching)
  in
  ignore
    (Bus_stream.next watching "both emits" (fun _ -> List.length (ours ()) = 2)
      : Bus_stream.message);
  let stamp = (Protocol.tables ()).bus_registry.stamps.emitter in
  Check.equal "the emitters stamped"
    (`List
       (List.map
          (fun d -> `String d)
          (List.sort compare [ World.person_did w "shares-a-key"; agent_did ])))
    (`List
       (List.sort compare
          (List.map
             (fun (f : Bus_stream.frame) -> Check.at f.payload [ stamp ])
             (ours ()))))

let upload_repeated (w : World.t) =
  let token = World.person w "uploads-twice" in
  let key = Fresh.uuid () in
  let fields = upload_fields () in
  let first = World.upload w token (form fields "the bytes") ~key in
  status "the first" 202 first;
  let again = World.upload w token (form fields "the bytes") ~key in
  status "the repeat" 202 again;
  conforms "POST" "/resources" again;
  match uploads_of w fields with
  | [ made ] ->
      Check.that "the resource was made under the key" (made.key = key);
      List.iter
        (fun (reply : Call.reply) ->
          Check.equal "an answer"
            (`Assoc [ ("resourceId", `String made.resource_id) ])
            (Option.get reply.json))
        [ first; again ]
  | made ->
      failwith
        (Printf.sprintf "%d resources were made, not one" (List.length made))

let upload_other_body (w : World.t) =
  let token = World.person w "reuses-an-upload-key" in
  let key = Fresh.uuid () in
  let fields = upload_fields () in
  let first = World.upload w token (form fields "the bytes") ~key in
  status "the first" 202 first;
  List.iter
    (fun (why, parts) ->
      let refused = World.upload w token parts ~key in
      status why 422 refused;
      conforms "POST" "/resources" refused;
      Check.equal why
        (`String (Protocol.message_code "idempotency-key-reused"))
        (`String (text_member "code" refused)))
    [
      ("other bytes", form fields "other bytes");
      ( "other fields",
        form
          (List.map
             (fun (k, v) -> if k = "name" then (k, "Another name") else (k, v))
             fields)
          "the bytes" );
    ];
  Check.that "nothing more was recorded" (List.length (uploads_of w fields) = 1)

(* Held between the gateway and its broker, the first is in hand for as long
   as the case chooses: only a broker can be stood between. On the
   in-process plane the two are sent at once, above. *)
let held_repeat (w : World.t) =
  let broker =
    match w.broker with
    | Some broker -> broker
    | None -> failwith "holding an emit on its way needs the broker"
  in
  let proxy = Proxy.start broker.port in
  let held = World.replica ~servers:proxy.url w in
  Fun.protect ~finally:(fun () ->
      Proxy.release proxy;
      Gateway.stop held;
      Proxy.close proxy)
  @@ fun () ->
  (* On the gateway that reaches the broker directly: the hold is on the
     other. *)
  let watching = watcher w in
  let token = World.person w "held" in
  let marker = Fresh.uuid () and key = Fresh.uuid () in
  Proxy.hold proxy;
  let sent () =
    Eio.Fiber.fork_promise ~sw:(Run.sw ()) (fun () ->
        World.emit w ~origin:held.origin token (command marker) ~key)
  in
  let first = sent () in
  Run.sleep 0.3;
  let second = sent () in
  Run.sleep 0.7;
  Proxy.release proxy;
  let one = Eio.Promise.await_exn first
  and two = Eio.Promise.await_exn second in
  status "the first" 202 one;
  List.iter (conforms "POST" "/bus/emit") [ one; two ];
  Check.equal "the repeat's answer" (answered one) (answered two);
  exactly_one "the command's frames"
    (settled watching broadcast (marked marker))

let tests implementations =
  let make = plain implementations in
  each_plane ~make ~world:Fun.id ~close:World.close "sending again under a key"
    [
      case
        "an emit with no key, or one that is not 1 to 255 characters, is \
         refused 400 and not published; one of 255 is taken"
        emit_without_key;
      case
        "an upload with no key, or one that is not 1 to 255 characters, is \
         refused 400 and never reaches the Archivist; one of 255 reaches it as \
         it came"
        upload_without_key;
      case
        "a command sent again under its key is published once, and the repeat \
         is answered as the first was, status and body"
        command_repeated;
      case
        "an operation's request sent again under its key is answered as the \
         first was, never 409: published once, and its reply reaches its \
         requester"
        request_repeated;
      case
        "a key sent again with another body is refused 422 unprocessable, \
         every time, and nothing is published; its first body is still \
         answered as it was"
        other_body;
      case
        "two sends under one key at once are published once, and answered alike"
        at_once;
      case
        "a key is its principal's: the same key and body from two principals \
         are two emits, each published and stamped with its own"
        principal_key;
      case
        "an upload sent again under its key makes one resource, and both \
         answers name it"
        upload_repeated;
      case
        "an upload's key sent again with other fields or other bytes is \
         refused 422 unprocessable, and nothing more is recorded"
        upload_other_body;
    ]
  @ each_plane ~planes:[ Nats ] ~make ~world:Fun.id ~close:World.close
      "a repeat that arrives while the first is in hand"
      [
        case ~timeout:30.0
          "waits for the first, held on its way to the broker, and is answered \
           as it was: one publish"
          held_repeat;
      ]
