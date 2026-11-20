(* POST /bus/emit: validation, the identity the gateway stamps, the profile
   it publishes for a writer, claims and their refusals, and the answer to a
   request nobody can answer. Semantics from contract/TRANSPORT-HTTP.md
   § POST /bus/emit; statuses and bodies from the spec. *)

open Xynolog_conformance
open Support

(* A request channel whose request payload validates with resourceId
   alone. *)
let request = "browse:resource-requested"

(* A broadcast every participant may emit, whose one required field is
   resourceId. *)
let broadcast = "beckon:focus"

(* A channel the registry says writes. *)
let write = "frame:add-entity-type"

(* Unvalidated, so its payload can be any size. *)
let unvalidated = "mark:added"

let body ?scope ?correlation_id ?client_id ?(payload = `Assoc []) channel =
  `Assoc
    ([ ("channel", `String channel); ("payload", payload) ]
    @ (match scope with Some s -> [ ("scope", `String s) ] | None -> [])
    @ (match correlation_id with
      | Some c -> [ ("correlationId", `String c) ]
      | None -> [])
    @ match client_id with Some c -> [ ("clientId", `String c) ] | None -> [])

let fields members = `Assoc (List.map (fun (k, v) -> (k, `String v)) members)
let resource = fields [ ("resourceId", "res-1") ]

let watch (w : World.t) channels =
  World.subscribe w (World.person w "watcher")
    (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ()) ~global:channels ())

let member f name = Protocol.at f.Bus_stream.payload [ name ]
let marked name value f = member f name = Some (`String value)

let refused_400 why reply =
  status why 400 reply;
  conforms "POST" "/bus/emit" reply

let accepted (w : World.t) =
  let token = World.person w "emitter" in
  let reply = World.emit w token (body broadcast ~payload:resource) in
  status "the emit" 202 reply;
  conforms "POST" "/bus/emit" reply;
  match (w.plane, reply.json) with
  | Nats, Some json -> Check.equal "the answer" (`Assoc []) json
  | In_process, Some json ->
      Check.that "the answer counts its subscribers"
        (match Protocol.at json [ "subscribers" ] with
        | Some (`Int _) -> true
        | _ -> false)
  | _, None -> failwith "the emit answered no JSON"

let largest (w : World.t) =
  (* A broker that takes less than the gateway does refuses the publish, and
     the emitter is told the plane is down. *)
  let limit = Protocol.limit Gateway "POST" "/bus/emit" "maxBodyBytes" in
  let reader =
    World.subscribe w (World.person w "reader")
      (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ())
         ~global:[ unvalidated ] ())
  in
  let marker = Fresh.uuid () in
  let with_filler filler =
    body unvalidated
      ~payload:(fields [ ("marker", marker); ("filler", filler) ])
  in
  let unfilled = String.length (Yojson.Safe.to_string (with_filler "")) in
  let filler = String.make (limit - unfilled) 'x' in
  let largest = with_filler filler in
  Check.that "the body is exactly maxBodyBytes"
    (String.length (Yojson.Safe.to_string largest) = limit);
  let emitter, _ = World.agent w "largest-emit" in
  status "the largest emit" 202 (World.emit w emitter largest);
  let frame =
    Bus_stream.frame ~timeout:20.0 reader unvalidated
      ~matches:(marked "marker" marker)
  in
  Check.that "the filler arrived whole" (marked "filler" filler frame)

let stamped (w : World.t) =
  let stamps = (Protocol.tables ()).bus_registry.stamps in
  let watcher = watch w [ broadcast ] in
  let person = World.person w "stamped" in
  let worker, worker_did =
    World.agent w
      ~roles:[ Protocol.service_role (); Protocol.worker_role () ]
      "stamp-model"
  in
  let forged =
    [
      (stamps.emitter, `String "did:web:forged.example:users:mallory");
      (stamps.roles, `List [ `String "xynolog-admin" ]);
    ]
  in
  let emitted token =
    let marker = Fresh.uuid () in
    ignore
      (World.emit w token
         (body broadcast
            ~payload:
              (`Assoc
                 ([
                    ("annotationId", `String marker);
                    ("resourceId", `String "res-1");
                  ]
                 @ forged)))
        : Call.reply);
    Bus_stream.frame watcher broadcast ~matches:(marked "annotationId" marker)
  in
  let from_person = emitted person in
  Check.equal "the person's stamp"
    (`String (World.person_did w "stamped"))
    (Check.at from_person.payload [ stamps.emitter ]);
  Check.that "a person's token with no roles is stamped with none"
    (member from_person stamps.roles = None);
  let from_worker = emitted worker in
  Check.equal "the worker's stamp" (`String worker_did)
    (Check.at from_worker.payload [ stamps.emitter ]);
  (* The roles its account holds, as its token carries them. *)
  Check.equal "the worker's roles"
    (`List
       [ `String (Protocol.service_role ()); `String (Protocol.worker_role ()) ])
    (Check.at from_worker.payload [ stamps.roles ]);
  (* A person may hold a role as an agent may; the issuer's token carries
     it. *)
  let roled =
    World.person w
      ~claims:[ ("roles", `List [ `String (Protocol.worker_role ()) ]) ]
      "stamped-roled"
  in
  let from_roled = emitted roled in
  Check.equal "the roled person's stamp"
    (`String (World.person_did w "stamped-roled"))
    (Check.at from_roled.payload [ stamps.emitter ]);
  Check.equal "the roled person's roles"
    (`List [ `String (Protocol.worker_role ()) ])
    (Check.at from_roled.payload [ stamps.roles ])

let concurrent (w : World.t) =
  let watcher = watch w [ broadcast ] in
  let people = List.init 8 (Printf.sprintf "concurrent-%d") in
  let tokens = Eio.Fiber.List.map (World.person w) people in
  let marks = List.map (fun _ -> Fresh.uuid ()) people in
  let replies =
    Eio.Fiber.List.map
      (fun (token, mark) ->
        World.emit w token
          (body broadcast
             ~payload:
               (fields [ ("annotationId", mark); ("resourceId", "res-1") ])))
      (List.combine tokens marks)
  in
  List.iter (status "a concurrent emit" 202) replies;
  List.iter2
    (fun person mark ->
      let frame =
        Bus_stream.frame watcher broadcast ~matches:(marked "annotationId" mark)
      in
      Check.equal person
        (`String (World.person_did w person))
        (Check.at frame.payload
           [ (Protocol.tables ()).bus_registry.stamps.emitter ]))
    people marks

let invalid (w : World.t) =
  let token = World.person w "emitter" in
  List.iter
    (fun (why, body) -> refused_400 why (World.emit w token body))
    [
      ("an unknown channel", body "no-such:channel");
      ("no channel", `Assoc [ ("payload", `Assoc []) ]);
      ("no payload", `Assoc [ ("channel", `String broadcast) ]);
      ( "a payload that is not an object",
        body broadcast ~payload:(`String "text") );
      ( "a payload that does not match",
        body request ~payload:(`Assoc [ ("resourceId", `Int 7) ]) );
      ( "a string where the schema says integer — never converted",
        body "tabs:reorder"
          ~payload:(fields [ ("oldIndex", "1"); ("newIndex", "2") ]) );
      ("an empty scope", body broadcast ~payload:resource ~scope:"");
    ]

let out_of_bound (w : World.t) =
  let token = World.person w "emitter" in
  let gather changed =
    `Assoc
      (List.map
         (fun (k, v) -> (k, Option.value ~default:v (List.assoc_opt k changed)))
         [
           ("depth", `Int 1);
           ("maxResources", `Int 10);
           ("includeContent", `Bool false);
           ("includeSummary", `Bool false);
         ])
  in
  List.iter
    (fun (why, channel, payload) ->
      refused_400 why (World.emit w token (body channel ~payload)))
    [
      ( "a page of no resource",
        "match:resources-requested",
        `Assoc [ ("search", `String "tide"); ("limit", `Int 0) ] );
      ( "a page of more than 200",
        "match:resources-requested",
        `Assoc [ ("search", `String "tide"); ("limit", `Int 201) ] );
      ( "an offset below 0",
        "match:resources-requested",
        `Assoc [ ("search", `String "tide"); ("offset", `Int (-1)) ] );
      ( "a window below 100",
        "gather:requested",
        `Assoc
          [
            ("annotationId", `String "ann-1");
            ("resourceId", `String "res-1");
            ("options", `Assoc [ ("contextWindow", `Int 99) ]);
          ] );
      ( "a window above 5,000",
        "gather:requested",
        `Assoc
          [
            ("annotationId", `String "ann-1");
            ("resourceId", `String "res-1");
            ("options", `Assoc [ ("contextWindow", `Int 5001) ]);
          ] );
      ( "a depth above 3",
        "gather:resource-requested",
        `Assoc
          [
            ("resourceId", `String "res-1");
            ("options", gather [ ("depth", `Int 4) ]);
          ] );
      ( "no related resource",
        "gather:resource-requested",
        `Assoc
          [
            ("resourceId", `String "res-1");
            ("options", gather [ ("maxResources", `Int 0) ]);
          ] );
      ( "more than 200 related resources",
        "gather:resource-requested",
        `Assoc
          [
            ("resourceId", `String "res-1");
            ("options", gather [ ("maxResources", `Int 201) ]);
          ] );
      ( "a word that is no motivation",
        "gather:referenced-by-requested",
        fields [ ("resourceId", "res-1"); ("motivation", "citing") ] );
      ( "an empty motivation",
        "gather:referenced-by-requested",
        fields [ ("resourceId", "res-1"); ("motivation", "") ] );
    ]

let taken_and_refused (w : World.t) kind emit_with =
  let token = World.person w "emitter" in
  let found = Protocol.identifiers kind in
  List.iter
    (fun (c : Xynolog_protocol.Tables.Identifiers_kinds.accepts_item) ->
      refused_400
        (Printf.sprintf "%S: %s" c.id c.why)
        (World.emit w token (emit_with c.id)))
    found.refuses;
  List.iter
    (fun (c : Xynolog_protocol.Tables.Identifiers_kinds.accepts_item) ->
      status
        (Printf.sprintf "%S: %s" c.id c.why)
        202
        (World.emit w token (emit_with c.id)))
    found.accepts

(* Each kind of id, on a channel whose payload carries one and needs nothing
   else. *)
let carriers =
  [
    ("ResourceId", broadcast, "resourceId");
    ("AnnotationId", "beckon:sparkle", "annotationId");
    ("JobId", "job:status-requested", "jobId");
  ]

let carrier (kind, channel, property) (w : World.t) =
  taken_and_refused w kind (fun id ->
      body channel ~payload:(fields [ (property, id) ]))

let scope_is_resource (w : World.t) =
  taken_and_refused w "ResourceId" (fun id ->
      body broadcast ~payload:resource ~scope:id)

let correlation_needs_client (w : World.t) =
  let token = World.person w "emitter" in
  refused_400 "a correlationId with no clientId"
    (World.emit w token
       (body request
          ~payload:(fields [ ("resourceId", "r") ])
          ~correlation_id:(Fresh.uuid ())))

let claimed_twice (w : World.t) =
  let token = World.person w "emitter" in
  let correlation_id = Fresh.uuid () in
  let ask () =
    World.emit w token
      (body request
         ~payload:(fields [ ("resourceId", "r") ])
         ~correlation_id ~client_id:(Fresh.uuid ()))
  in
  status "the first claim" 202 (ask ());
  let second = ask () in
  status "the second claim" 409 second;
  conforms "POST" "/bus/emit" second

let unanswered_requests (w : World.t) =
  (* Something must hear the requests and not answer them: a request that
     reaches nobody is answered at once (below). *)
  let silent, _ = World.agent w "silent" in
  ignore
    (World.subscribe w silent
       (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ()) ~global:[ request ]
          ())
      : Bus_stream.t);
  let token = World.person w "busy" in
  let client_id = Fresh.uuid () in
  let ask ?(client_id = client_id) ?(correlation_id = Fresh.uuid ()) resource =
    World.emit w token
      (body request
         ~payload:(fields [ ("resourceId", resource) ])
         ~correlation_id ~client_id)
  in
  let cids =
    List.init (Protocol.pending_replies_max ()) (fun i ->
        let correlation_id = Fresh.uuid () in
        status
          (Printf.sprintf "request %d" i)
          202
          (ask ~correlation_id (Printf.sprintf "r-%d" i));
        correlation_id)
  in
  let over = ask "over" in
  status "a request past pendingReplies' maxItems" 429 over;
  conforms "POST" "/bus/emit" over;
  (* Retry-After is when the oldest unanswered claim expires: all were just
     made. *)
  let claim_seconds =
    Protocol.limit Gateway "POST" "/bus/emit" "claimSeconds"
  in
  let retry_after =
    match Http.Header.get over.headers "retry-after" with
    | Some after -> float_of_string after
    | None -> failwith "the 429 states no Retry-After"
  in
  Check.that
    (Printf.sprintf "Retry-After %g is when the oldest claim expires"
       retry_after)
    (retry_after > float_of_int (claim_seconds - 30)
    && retry_after <= float_of_int claim_seconds);
  (* Refused, not made room for: the oldest claim still stands. *)
  status "the oldest claim, again" 409
    (ask ~correlation_id:(List.nth cids 1) "r-1");
  (* Another client of the same person is not affected. *)
  status "another client" 202 (ask ~client_id:(Fresh.uuid ()) "other");
  (* An answer frees a slot. *)
  let responder, _ = World.agent w "answerer" in
  status "the answer" 202
    (World.emit w responder
       (body (Protocol.operation_for request).result ~payload:(described ())
          ~correlation_id:(List.hd cids)));
  let rec freed attempt =
    if attempt = 50 then None
    else
      let reply = ask "freed" in
      if reply.status = 202 then Some reply
      else (
        Run.sleep 0.02;
        freed (attempt + 1))
  in
  Check.that "an answer frees a slot" (freed 0 <> None);
  (* One answer frees one slot. *)
  status "one answer frees one slot" 429 (ask "next")

let profiles_of watcher did =
  List.filter
    (fun f ->
      member f (Protocol.tables ()).bus_registry.stamps.emitter
      = Some (`String did))
    (Bus_stream.frames ~channel:"principal:profile" watcher)

let writes_publish_names (w : World.t) =
  let watcher = watch w [ "principal:profile" ] in
  Check.that "the registry says it writes"
    (List.mem write (Protocol.tables ()).bus_registry.effect_.writes);
  let did = World.person_did w "writes" in
  List.iter
    (fun name ->
      status "a write" 202
        (World.emit w
           (World.person w ~claims:[ ("name", `String name) ] "writes")
           (body write ~payload:(fields [ ("tag", "Conformance") ]))))
    [ "Wanda Writer"; "Wanda Writer"; "Wanda Renamed" ];
  ignore
    (Bus_stream.next watcher "the third profile" (fun _ ->
         List.length (profiles_of watcher did) = 3)
      : Bus_stream.message);
  Check.equal "the names, in order"
    (`List
       [
         `String "Wanda Writer"; `String "Wanda Writer"; `String "Wanda Renamed";
       ])
    (`List
       (List.map
          (fun f -> Check.at f.Bus_stream.payload [ "name" ])
          (profiles_of watcher did)))

let agent_write_named (w : World.t) =
  let watcher = watch w [ "principal:profile" ] in
  let subject = "named-writer-" ^ Fresh.uuid () in
  let did = Protocol.agent_did w.kb.domain subject in
  let token =
    Issuer.token w.issuer
      [
        ("sub", `String subject);
        (Protocol.kind_claim (), `String (Protocol.agent_kind ()));
        ("roles", `List [ `String (Protocol.service_role ()) ]);
        ("name", `String "The Named Agent");
      ]
  in
  status "the agent's write" 202
    (World.emit w token
       (body write ~payload:(fields [ ("tag", "Named agent") ])));
  let profile =
    Bus_stream.frame watcher "principal:profile" ~matches:(fun f ->
        member f (Protocol.tables ()).bus_registry.stamps.emitter
        = Some (`String did))
  in
  Check.equal "its name" (`String "The Named Agent")
    (Check.at profile.payload [ "name" ])

let no_profile (w : World.t) =
  let watcher = watch w [ "principal:profile" ] in
  Check.that "the registry says it reads"
    (List.mem request (Protocol.tables ()).bus_registry.effect_.reads);
  let reader =
    World.person w ~claims:[ ("name", `String "Rita Reader") ] "only-reads"
  in
  ignore
    (World.emit w reader
       (body request ~payload:(fields [ ("resourceId", "r") ]))
      : Call.reply);
  let agent, _ = World.agent w "writer" in
  status "the agent's write" 202
    (World.emit w agent (body write ~payload:(fields [ ("tag", "Agent") ])));
  (* A person's token, with no name. *)
  let nameless =
    Issuer.token w.issuer
      [
        ("sub", `String "nameless");
        ("email", `String "nameless@people.example");
        ("email_verified", `Bool true);
      ]
  in
  status "the nameless write" 202
    (World.emit w nameless
       (body write ~payload:(fields [ ("tag", "Nameless") ])));
  Bus_stream.quiet watcher "principal:profile" 0.5

let says_its_name (w : World.t) =
  let watcher = watch w [ "principal:profile" ] in
  let agent, agent_did = World.agent w "says-its-name" in
  let someone_else =
    Protocol.agent_did w.kb.domain ("someone-else-" ^ Fresh.uuid ())
  in
  let name = "says-its-name-" ^ Fresh.uuid () in
  status "the profile" 202
    (World.emit w agent
       (body "principal:profile"
          ~payload:
            (fields
               [
                 ((Protocol.tables ()).bus_registry.stamps.emitter, someone_else);
                 ("name", name);
               ])));
  let profile =
    Bus_stream.frame watcher "principal:profile" ~matches:(marked "name" name)
  in
  Check.equal "its stamp" (`String agent_did)
    (Check.at profile.payload
       [ (Protocol.tables ()).bus_registry.stamps.emitter ]);
  Check.that "nothing is stamped with the DID it wrote"
    (profiles_of watcher someone_else = [])

let reaches_nobody (w : World.t) =
  let token = World.person w "lonely" in
  let client_id = Fresh.uuid () in
  (* A request channel nothing in this world answers. *)
  let request = "browse:annotation-requested" in
  let failure = (Protocol.operation_for request).failure in
  let stream =
    World.subscribe w token
      (Bus_stream.subscribe_body ~client_id ~global:[ failure ] ())
  in
  let other_client =
    World.subscribe w token
      (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ()) ~global:[ failure ]
         ())
  in
  let correlation_id = Fresh.uuid () in
  let reply =
    World.emit w token
      (body request
         ~payload:(fields [ ("resourceId", "r-9"); ("annotationId", "a-9") ])
         ~correlation_id ~client_id)
  in
  status "the request" 202 reply;
  (* Observed on both planes: counted in-process, and a broker's own
     no-responders answer on NATS — never a zero nobody saw. *)
  Check.equal "its count"
    (`Assoc [ ("subscribers", `Int 0) ])
    (Option.get reply.json);
  let frame =
    Bus_stream.frame stream failure ~matches:(fun f ->
        f.correlation_id = Some correlation_id)
  in
  Check.matching "the failure"
    [
      ("resourceId", `String "r-9");
      ("annotationId", `String "a-9");
      ("code", `String (Protocol.failure "peer-unavailable").code);
    ]
    frame.payload;
  Check.that "its message is text"
    (match member frame "message" with Some (`String _) -> true | _ -> false);
  Check.that "the failure carries no emitter's stamp"
    (member frame (Protocol.tables ()).bus_registry.stamps.emitter = None);
  Check.that "the failure carries no roles"
    (member frame (Protocol.tables ()).bus_registry.stamps.roles = None);
  Bus_stream.quiet other_client failure 0.5;
  (* A request with no correlationId has nobody to answer. *)
  status "an uncorrelated request" 202
    (World.emit w token
       (body request
          ~payload:(fields [ ("resourceId", "r-10"); ("annotationId", "a-10") ])));
  Bus_stream.quiet stream failure 0.5 ~matches:(marked "resourceId" "r-10")

let someone_subscribes (w : World.t) =
  let token = World.person w "requester" in
  let client_id = Fresh.uuid () in
  let request = "browse:annotation-requested" in
  let failure = (Protocol.operation_for request).failure in
  let stream =
    World.subscribe w token
      (Bus_stream.subscribe_body ~client_id ~global:[ failure ] ())
  in
  let participant, _ = World.agent w "server" in
  let server =
    World.subscribe w participant
      (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ()) ~global:[ request ]
         ())
  in
  let correlation_id = Fresh.uuid () in
  let reply =
    World.emit w token
      (body request
         ~payload:(fields [ ("resourceId", "r-11"); ("annotationId", "a-11") ])
         ~correlation_id ~client_id)
  in
  status "the request" 202 reply;
  Check.equal "its answer"
    (match w.plane with
    | Nats -> `Assoc []
    | In_process -> `Assoc [ ("subscribers", `Int 1) ])
    (Option.get reply.json);
  ignore
    (Bus_stream.frame server request ~matches:(fun f ->
         f.correlation_id = Some correlation_id)
      : Bus_stream.frame);
  Bus_stream.quiet stream failure 0.5

let reply_uncorrelated (w : World.t) =
  let result = (Protocol.operation_for request).result in
  let watcher = watch w [ result ] in
  let participant, _ = World.agent w "uncorrelated" in
  status "the reply" 202
    (World.emit w participant (body result ~payload:(described ())));
  Bus_stream.quiet watcher result 0.5

let job_request (w : World.t) =
  let token = World.person w "claimer" in
  let client_id = Fresh.uuid () in
  let operation = Protocol.operation_for "job:claim" in
  let stream =
    World.subscribe w token
      (Bus_stream.subscribe_body ~client_id
         ~global:[ operation.result; operation.failure ]
         ())
  in
  let correlation_id = Fresh.uuid () in
  status "the claim" 202
    (World.emit w token
       (body "job:claim"
          ~payload:
            (`Assoc
               [
                 ("accepts", `List [ `Assoc [ ("jobType", `String "yield") ] ]);
               ])
          ~correlation_id ~client_id));
  (* The only answer is the gateway saying nobody is there to give one. *)
  let frame =
    Bus_stream.frame stream operation.failure ~matches:(fun f ->
        f.correlation_id = Some correlation_id)
  in
  Check.equal "its code" (`String (Protocol.failure "peer-unavailable").code)
    (Check.at frame.payload [ "code" ]);
  Bus_stream.quiet stream operation.result 0.5

let correlation_in_payload (w : World.t) =
  let watcher = watch w [ broadcast ] in
  let token = World.person w "emitter" in
  let marker = Fresh.uuid () in
  ignore
    (World.emit w token
       (body broadcast
          ~payload:
            (fields
               [
                 ("annotationId", marker);
                 ("resourceId", "res-1");
                 ("correlationId", "inside-the-payload");
               ]))
      : Call.reply);
  let frame =
    Bus_stream.frame watcher broadcast ~matches:(marked "annotationId" marker)
  in
  Check.equal "the payload's correlationId" (`String "inside-the-payload")
    (Check.at frame.payload [ "correlationId" ]);
  Check.that "the frame carries no correlationId" (frame.correlation_id = None)

let counting (w : World.t) =
  let token = World.person w "counter" in
  let count body =
    match
      Protocol.at (Option.get (World.emit w token body).json) [ "subscribers" ]
    with
    | Some (`Int n) -> n
    | _ -> failwith "the emit answered no count"
  in
  let scope = "res-" ^ Fresh.uuid () in
  let scoped =
    body broadcast ~payload:(fields [ ("resourceId", scope) ]) ~scope
  in
  let global = body broadcast ~payload:resource in
  let counted what expected body =
    let n = count body in
    Check.that
      (Printf.sprintf "%s: counted %d, not %d" what n expected)
      (n = expected)
  in
  counted "nobody, global" 0 global;
  counted "nobody, scoped" 0 scoped;
  let scoped_stream () =
    World.subscribe w token
      (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ())
         ~scoped:[ Bus_stream.scope scope [ broadcast ] ]
         ())
  in
  let first = scoped_stream () in
  counted "one scoped" 1 scoped;
  ignore (scoped_stream () : Bus_stream.t);
  counted "two scoped" 2 scoped;
  ignore
    (World.subscribe w token
       (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ())
          ~global:[ broadcast ] ())
      : Bus_stream.t);
  counted "two scoped, and one global" 2 scoped;
  counted "one global" 1 global;
  Bus_stream.close first;
  Run.eventually "the closed connection to stop counting" 5.0 (fun () ->
      if count scoped = 1 then Some () else None)

let tests implementations =
  let make = plain implementations in
  each_plane ~make ~world:Fun.id ~close:World.close "emitting"
    ([
       case
         "an emit is accepted with 202; the count of subscribers is present \
          only when the plane can count them"
         accepted;
       case
         "an emit of exactly maxBodyBytes is taken and reaches its subscriber \
          whole, on either plane"
         largest;
       case
         "the gateway stamps the verified emitter as _userId and its token's \
          roles as _roles, whoever issued the token, over whatever the caller \
          wrote"
         stamped;
       case
         "concurrent emits by different principals are each stamped with their \
          own"
         concurrent;
       case
         "an emit is refused with 400 for an unknown channel, a payload that \
          does not match its schema, or a scope that is empty"
         invalid;
       case
         "a request of the Librarian that is out of a bound its schema states \
          is refused with 400, and never reaches a Librarian"
         out_of_bound;
     ]
    @ List.map
        (fun ((kind, _, _) as carried) ->
          case
            (Printf.sprintf
               "a payload carrying a %s is taken when the kind accepts it and \
                refused with 400 when it does not (identifiers/kinds.json)"
               kind)
            (carrier carried))
        carriers
    @ [
        case
          "a scope is a resource's id: an emit under one the kind refuses is a \
           400, and under one it accepts is taken"
          scope_is_resource;
        case "a request carrying a correlationId needs a clientId"
          correlation_needs_client;
        case "a live correlationId cannot be claimed twice" claimed_twice;
        case
          "a client may have as many unanswered requests as pendingReplies may \
           name, and no more until one is answered"
          unanswered_requests;
        case
          "every write by a person publishes the name their token carries at \
           that write, in order"
          writes_publish_names;
        case
          "an agent's write publishes the name its token carries, as a \
           person's does"
          agent_write_named;
        case
          "a read, or a write by a token with no name, a person's or an \
           agent's, publishes no profile"
          no_profile;
        case
          "a principal that says what it is called says it of itself: its \
           principal:profile is stamped with its own DID, over whatever it \
           wrote"
          says_its_name;
        case
          "a request that reaches nobody is answered at once — to its \
           requester only — with its operation's failure echoing the request, \
           on either plane"
          reaches_nobody;
        case
          "a request someone subscribes to is delivered and answered by nobody \
           but them: no failure is synthesized, and a broker plane reports no \
           count it did not observe"
          someone_subscribes;
        case "a reply with no correlationId reaches no one" reply_uncorrelated;
        case
          "a job request gets no answer from the gateway: the queue is the \
           dispatcher's"
          job_request;
        case
          "a correlationId key inside a payload is the caller's data: \
           delivered unchanged, and it routes nothing"
          correlation_in_payload;
      ])
  (* Only a plane that can count reports a count (TRANSPORT-HTTP.md § POST
     /bus/emit). *)
  @ each_plane ~planes:[ In_process ] ~make ~world:Fun.id ~close:World.close
      "counting subscribers"
      [
        case
          "an emit reports exactly how many connections its channel and scope \
           reached"
          counting;
      ]
