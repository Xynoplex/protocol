(* A dispatcher world: a dispatcher process and everything it meets. That is
   the trusted issuer, which admits the dispatcher's service account; a
   gateway on the in-process plane, the dispatcher's only route to the bus;
   a JetStream broker for its queue, fresh for every world, so the clocks a
   case sets are the ones the broker's objects are made with; and the
   Archivist's Browser, answering the two vocabulary reads job:create makes
   over the bus.

   Cases meet the dispatcher as the bus does: as people, workers and
   sidecars emitting through the gateway and reading replies off their
   streams. Every frame those streams carry must be what the registry says
   its channel carries, and a case fails otherwise, whatever it was
   about. *)

(* The dispatcher's service account at the issuer. *)
let dispatcher_client = "xynolog-dispatcher"

(* What the Browser answers the vocabulary reads with. *)
type vocabulary = {
  entity_types : string list;
  tag_schemas : Yojson.Safe.t list;
}

let vocabulary =
  let tag name description example =
    `Assoc
      [
        ("name", `String name);
        ("description", `String description);
        ("examples", `List [ `String example ]);
      ]
  in
  {
    entity_types = [ "Person"; "Place" ];
    tag_schemas =
      [
        `Assoc
          [
            ("id", `String "irac");
            ("name", `String "IRAC");
            ("description", `String "Issue, rule, application, conclusion");
            ("domain", `String "legal");
            ( "tags",
              `List
                [
                  tag "Issue" "The question the case turns on"
                    "Whether the contract was formed";
                  tag "Rule" "The law that governs it"
                    "An offer must be accepted";
                ] );
          ];
      ];
  }

let entity_types_read = "browse:entity-types-requested"
let tag_schemas_read = "browse:tag-schemas-requested"

(* The operations the registry says the dispatcher answers. *)
let operations () =
  List.filter
    (fun (op : Protocol.Registry.operations_item) ->
      match op.answered_by with Dispatcher -> true | _ -> false)
    (Protocol.tables ()).bus_registry.operations

(* The words of the dispatcher's refusal [name] (dispatcher/messages.json),
   its placeholders filled from [fill]. *)
let refusal name : Xynolog_protocol.Tables.Dispatcher_messages.refusals_item =
  match
    List.find_opt
      (fun (r : Xynolog_protocol.Tables.Dispatcher_messages.refusals_item) ->
        r.name = name)
      (Protocol.tables ()).dispatcher_messages.refusals
  with
  | Some r -> r
  | None -> failwith ("dispatcher/messages.json states no refusal " ^ name)

let refusal_text ?(fill = []) name =
  Protocol.fill_template (refusal name).message fill

let code_word =
  Xynolog_protocol.Tables.Dispatcher_messages.command_error_code_value

(* The error the dispatcher records itself (dispatcher/messages.json's
   errors) named [name]. *)
let error_row name : Xynolog_protocol.Tables.Dispatcher_messages.errors_item =
  match
    List.find_opt
      (fun (e : Xynolog_protocol.Tables.Dispatcher_messages.errors_item) ->
        e.name = name)
      (Protocol.tables ()).dispatcher_messages.errors
  with
  | Some e -> e
  | None -> failwith ("dispatcher/messages.json states no error " ^ name)

(* [x] as the shortest decimal that reads back as it, without a fraction
   when it is whole. *)
let shortest x =
  if Float.is_integer x then Printf.sprintf "%.0f" x
  else
    let rec at precision =
      let text = Printf.sprintf "%.*g" precision x in
      if precision >= 17 || float_of_string text = x then text
      else at (precision + 1)
    in
    at 1

(* Wait out a frame's handling. The dispatcher does not serialize frames
   (JOBS.md § Channels), so a case that needs one one-way frame applied
   before it sends the next, and has no reply to wait on, waits. *)
let settle ?(seconds = 0.5) () = Run.sleep seconds

(* A job id the dispatcher never minted, of the form JobId states. *)
let unknown_job_id () =
  let id = "job-" ^ Fresh.hex 16 in
  (match Protocol.against "JobId" (`String id) with
  | [] -> ()
  | refusals ->
      failwith
        (Printf.sprintf "the suite's job id %s is no JobId: %s" id
           (String.concat "; " refusals)));
  id

(* A claim's filter for mark jobs of one motivation, and for yield jobs. *)
let marks motivation =
  `Assoc
    [
      ("jobType", `String "mark");
      ("params", `Assoc [ ("motivation", `String motivation) ]);
    ]

let yields = `Assoc [ ("jobType", `String "yield") ]

(* The motivations a mark job has: those the spec tells MarkJobParams apart
   by. *)
let motivations () =
  match
    Option.bind (Protocol.json_of "components/schemas/MarkJobParams.json")
      (fun schema -> Protocol.at schema [ "discriminator"; "mapping" ])
  with
  | Some (`Assoc mapping) -> List.map fst mapping
  | _ -> failwith "MarkJobParams states no discriminator mapping"

(* Filters that between them take every job. *)
let every_job () = List.map marks (motivations ()) @ [ yields ]

let text json path =
  match Check.at json path with
  | `String s -> s
  | other ->
      failwith
        (Printf.sprintf "%s at %s is not text" (Check.shown other)
           (String.concat "." path))

(* The filter that takes jobs like [job]: its type, and for a mark job its
   motivation. *)
let filter_of job =
  match text job [ "metadata"; "type" ] with
  | "mark" -> marks (text job [ "params"; "motivation" ])
  | _ -> yields

(* A reply to a correlated request. *)
type answer = { ok : bool; channel : string; payload : Yojson.Safe.t }

(* Whatever carries a job's identity on a worker's signals. *)
type job_ref = { job_id : string; job_type : string; resource_id : string }

let ref_members r =
  [
    ("jobId", `String r.job_id);
    ("jobType", `String r.job_type);
    ("resourceId", `String r.resource_id);
  ]

(* A job's id, as a claim hands it out. *)
let id_of job = text job [ "metadata"; "id" ]

(* A claimed job, as its worker's signals name it. *)
let ref_of job =
  {
    job_id = id_of job;
    job_type = text job [ "metadata"; "type" ];
    resource_id = text job [ "params"; "resourceId" ];
  }

let job_id_of answer = text answer.payload [ "response"; "jobId" ]
let code_of answer = Protocol.at answer.payload [ "code" ]
let message_of answer = Protocol.at answer.payload [ "message" ]

type t = {
  world : World.t;
  broker : Nats.broker;
  env : (string * string) list;
      (** The dispatcher's environment: its service account, the broker's
          credentials when named, and what the world was given. *)
  mutable dispatcher : Dispatcher.t;
  observer : Bus_stream.t;
      (** Every job:queued and job:assign the bus carried. *)
  browser_reads : (string, int) Hashtbl.t;
      (** How many times the Browser was asked each vocabulary read. *)
  unfinished : (string, unit) Hashtbl.t;
      (** The jobs this world's cases created that are not known to be over. *)
}

(* A participant on the bus: a person, a worker or a sidecar, subscribed to
   the replies of the dispatcher's operations under its own client id, and
   to any channel it names. *)
type client = {
  dw : t;
  token : string;
  did : string;
  client_id : string;
  stream : Bus_stream.t;
}

let settings t = t.dispatcher.launch.document
let timing t name = Dispatcher.clock (settings t) name

let client t token did channels =
  let replies =
    List.concat_map
      (fun (op : Protocol.Registry.operations_item) ->
        [ op.result; op.failure ])
      (operations ())
  in
  let client_id = Fresh.uuid () in
  let stream =
    World.subscribe t.world token
      (Bus_stream.subscribe_body ~client_id ~global:(replies @ channels) ())
  in
  Bus_stream.hold_to_registry stream;
  { dw = t; token; did; client_id; stream }

(* A person, signed in at the issuer. *)
let person ?(channels = []) t sub =
  client t (World.person t.world sub) (World.person_did t.world sub) channels

(* A worker: a software agent holding the worker role. *)
let worker ?(channels = []) t model =
  let token, did =
    World.agent t.world
      ~roles:[ Protocol.service_role (); Protocol.worker_role () ]
      model
  in
  client t token did channels

(* A sidecar: a software agent holding the service role and not the worker
   role. *)
let sidecar ?(channels = []) t model =
  let token, did = World.agent t.world model in
  client t token did channels

(* Emit a frame, uncorrelated unless given a correlationId; the gateway must
   accept it. *)
let emit ?correlation_id c channel payload =
  let reply =
    World.emit c.dw.world c.token
      (`Assoc
         ([ ("channel", `String channel); ("payload", payload) ]
         @
         match correlation_id with
         | Some id ->
             [
               ("correlationId", `String id); ("clientId", `String c.client_id);
             ]
         | None -> []))
  in
  if reply.status <> 202 then
    failwith
      (Printf.sprintf "the gateway refused %s: %d %s" channel reply.status
         (Call.shown reply.text))

(* Emit a frame and answer what the gateway said of it at its door: 202 when
   it took the frame, and its refusal when the payload is not what the
   channel's schema states. *)
let offered c channel payload =
  World.emit c.dw.world c.token
    (`Assoc [ ("channel", `String channel); ("payload", payload) ])

(* Emit [operation] with a fresh correlationId and read its one reply. *)
let request ?(timeout = 10.0) c operation payload =
  let op = Protocol.operation_for operation in
  let correlation_id = Fresh.uuid () in
  let from = Dynarray.length c.stream.messages in
  emit ~correlation_id c operation payload;
  let message =
    Bus_stream.next ~timeout ~from c.stream ("a reply to " ^ operation)
      (fun m ->
        match m.frame with
        | Some f ->
            f.correlation_id = Some correlation_id
            && (f.channel = op.result || f.channel = op.failure)
        | None -> false)
  in
  let frame = Option.get message.frame in
  {
    ok = frame.channel = op.result;
    channel = frame.channel;
    payload = frame.payload;
  }

let refused_with what answer =
  failwith
    (Printf.sprintf "%s was refused: %s" what (Check.shown answer.payload))

(* Create a job: a mark job names its resource and, in [params], its
   motivation; a yield job names its context. The world keeps the id of a
   job that was admitted, to cancel it once its case is over. *)
let create ?resource_id c job_type params =
  let answer =
    request c "job:create"
      (`Assoc
         ([ ("jobType", `String job_type); ("params", params) ]
         @
         match resource_id with
         | Some id -> [ ("resourceId", `String id) ]
         | None -> []))
  in
  if answer.ok then Hashtbl.replace c.dw.unfinished (job_id_of answer) ();
  answer

(* Create a job the dispatcher must admit, and answer its id. *)
let created ?resource_id c job_type params =
  let answer = create ?resource_id c job_type params in
  if not answer.ok then refused_with "job:create" answer;
  job_id_of answer

let claim c accepts =
  request c "job:claim" (`Assoc [ ("accepts", `List accepts) ])

(* Claim a job the dispatcher must hand over, and answer it. *)
let claimed c accepts =
  let answer = claim c accepts in
  if not answer.ok then refused_with "job:claim" answer;
  Check.at answer.payload [ "response" ]

let status c job_id =
  request c "job:status-requested" (`Assoc [ ("jobId", `String job_id) ])

(* The status of a job the dispatcher must know. *)
let status_of c job_id =
  let answer = status c job_id in
  if not answer.ok then refused_with "job:status-requested" answer;
  Check.at answer.payload [ "response" ]

let status_word s = text s [ "status" ]

(* The status word of a job the dispatcher must know, now. *)
let status_now c job_id = status_word (status_of c job_id)

(* Poll the status until [holds] accepts it. *)
let until ?(timeout = 10.0) c job_id what holds =
  Run.eventually what timeout (fun () ->
      let s = status_of c job_id in
      if holds s then Some s else None)

(* Poll until the job's status is [word]. *)
let until_status ?timeout c job_id what word =
  until ?timeout c job_id what (fun s -> status_word s = word)

let cancel_request c payload = request c "job:cancel-requested" payload

let cancelled answer =
  Check.at answer.payload [ "response"; "cancelled" ] = `Bool true

let complete ?result c r =
  emit c "job:complete"
    (`Assoc
       (ref_members r
       @ match result with Some result -> [ ("result", result) ] | None -> []))

let fail ?(extra = []) c r error =
  emit c "job:fail"
    (`Assoc (ref_members r @ [ ("error", `String error) ] @ extra))

let report_progress ?progress c r percentage =
  emit c "job:report-progress"
    (`Assoc
       (ref_members r
       @ [ ("percentage", percentage) ]
       @ match progress with Some p -> [ ("progress", p) ] | None -> []))

let checkpoint ?unit_cursors c job_id completed_units =
  emit c "job:checkpoint"
    (`Assoc
       ([
          ("jobId", `String job_id);
          ( "completedUnits",
            `List (List.map (fun u -> `String u) completed_units) );
        ]
       @
       match unit_cursors with
       | Some cursors -> [ ("unitCursors", `Assoc cursors) ]
       | None -> []))

let cancel c r = emit c "job:cancel" (`Assoc (ref_members r))

(* ── Jobs as cases state them ──────────────────────────────────────── *)

let resource_id () = "res-" ^ Fresh.uuid ()

(* A resource as a gathered context describes one. *)
let descriptor_of resource_id =
  `Assoc
    [
      ("@context", `String "https://schema.org");
      ("@id", `String resource_id);
      ("name", `String "The source");
      ( "representations",
        `List [ `Assoc [ ("mediaType", `String "text/plain") ] ] );
    ]

(* A gathered context with [focus], and nothing gathered around it. *)
let context_of focus =
  `Assoc
    [
      ("focus", focus);
      ("graph", `Assoc [ ("nodes", `List []); ("edges", `List []) ]);
      ("metadata", `Assoc []);
    ]

(* A yield job's params whose context focuses [resource_id], as
   gather.resource produces, with [extra] over them. *)
let generation ?(extra = []) resource_id =
  let given =
    [
      ("title", `String "A generated resource");
      ( "storageUri",
        `String (Printf.sprintf "file://generated/%s.md" (Fresh.uuid ())) );
      ( "context",
        context_of
          (`Assoc
             [
               ("kind", `String "resource");
               ("resource", descriptor_of resource_id);
             ]) );
    ]
  in
  `Assoc (List.filter (fun (k, _) -> not (List.mem_assoc k extra)) given @ extra)

(* A job a case has running: a mark job of [params], which name its
   motivation, or a yield job. *)
type job = Mark of (string * Yojson.Safe.t) list | Yield

let highlighting = Mark [ ("motivation", `String "highlighting") ]

type running = {
  creator : client;
  worker : client;
  job : Yojson.Safe.t;
  ref : job_ref;
}

(* ── The world ─────────────────────────────────────────────────────── *)

(* Wait until the dispatcher answers on the bus. Its health says the queue
   is connected and its handlers attached, not that its stream to the
   gateway is open; until it is, a request reaches no one. *)
let answering t =
  let probe = sidecar t "readiness" in
  Run.eventually "the dispatcher to answer on the bus" 20.0 (fun () ->
      let answer = status probe (unknown_job_id ()) in
      if code_of answer = Some (`String (code_word Peer_unavailable)) then None
      else Some ())

let start_browser t vocabulary =
  let reads =
    [
      ( entity_types_read,
        `Assoc
          [
            ( "response",
              `Assoc
                [
                  ( "entityTypes",
                    `List
                      (List.map (fun e -> `String e) vocabulary.entity_types) );
                ] );
          ] );
      ( tag_schemas_read,
        `Assoc
          [
            ("response", `Assoc [ ("tagSchemas", `List vocabulary.tag_schemas) ]);
          ] );
    ]
  in
  let responder =
    World.responder t.world (List.map fst reads) (fun frame ->
        Hashtbl.replace t.browser_reads frame.channel
          (1
          + Option.value ~default:0
              (Hashtbl.find_opt t.browser_reads frame.channel));
        Option.map
          (fun payload ->
            ((Protocol.operation_for frame.channel).result, payload))
          (List.assoc_opt frame.channel reads))
  in
  Bus_stream.hold_to_registry responder.stream

(* [timing] changes the dispatcher's clocks over the quiet ones.
   [vocabulary] is what the Browser answers; None runs no Browser, so the
   Archivist is absent. [credentials] are what the queue's broker admits
   alone, handed to the dispatcher by the variables its document names.
   [env] is more of its environment. *)
let start ~gateway ~command ?(timing = []) ?(vocabulary = Some vocabulary)
    ?credentials ?(env = []) () =
  let secret = Fresh.hex 24 in
  let world =
    World.create ~command:gateway
      ~accounts:
        [
          ( dispatcher_client,
            { Issuer.secret; roles = [ Protocol.service_role () ] } );
        ]
      In_process
  in
  let broker = Nats.start_broker ?credentials () in
  let user_env = "QUEUE_USER" and password_env = "QUEUE_PASSWORD" in
  let env =
    [
      ("XYNOLOG_OIDC_CLIENT_ID", dispatcher_client);
      ("XYNOLOG_OIDC_CLIENT_SECRET", secret);
    ]
    @ (match credentials with
      | Some { Nats.user; password } ->
          [ (user_env, user); (password_env, password) ]
      | None -> [])
    @ env
  in
  let document =
    `Assoc
      [
        ("kb", World.served world);
        ("gatewayUrl", `String (World.origin world));
        ("identity", `Assoc [ ("issuer", `String world.issuer.origin) ]);
        ( "queue",
          `Assoc
            ([ ("servers", `String broker.url) ]
            @
            match credentials with
            | Some _ ->
                [
                  ("userEnv", `String user_env);
                  ("passwordEnv", `String password_env);
                ]
            | None -> []) );
        ("port", `Int (Net.free_port ()));
        ( "timing",
          `Assoc
            (List.map
               (fun (name, quiet) ->
                 ( name,
                   `Int
                     (Option.value ~default:quiet (List.assoc_opt name timing))
                 ))
               Dispatcher.quiet_timing) );
        ("logLevel", `String "info");
        ("logFormat", `String "json");
      ]
  in
  List.iter
    (fun (name, _) ->
      if not (List.mem_assoc name Dispatcher.quiet_timing) then
        failwith ("the dispatcher has no clock " ^ name))
    timing;
  let observer_token, _ = World.agent world "observer" in
  let observer =
    World.subscribe ~standing:true world observer_token
      (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ())
         ~global:[ "job:queued"; "job:assign" ]
         ())
  in
  Bus_stream.hold_to_registry observer;
  let dispatcher =
    Dispatcher.start (Dispatcher.launched ~command ~document env)
  in
  let t =
    {
      world;
      broker;
      env;
      dispatcher;
      observer;
      browser_reads = Hashtbl.create 2;
      unfinished = Hashtbl.create 64;
    }
  in
  Option.iter (start_browser t) vocabulary;
  answering t;
  t

let reads t channel =
  Option.value ~default:0 (Hashtbl.find_opt t.browser_reads channel)

(* A job created by a person and claimed by a worker: the worker holds it,
   running. A mark job is created on a fresh resource; a yield job on a
   context focusing one. *)
let running ?(job = highlighting) t =
  let creator = person t "creator" in
  let filter, worker_name =
    match job with
    | Yield -> (yields, "worker-yield")
    | Mark params ->
        let motivation =
          match List.assoc_opt "motivation" params with
          | Some (`String m) -> m
          | _ -> failwith "a mark job's params name its motivation"
        in
        (marks motivation, "worker-" ^ motivation)
  in
  let worker = worker t worker_name in
  let job_id =
    match job with
    | Yield -> created creator "yield" (generation (resource_id ()))
    | Mark params ->
        created ~resource_id:(resource_id ()) creator "mark" (`Assoc params)
  in
  let claimed = claimed worker [ filter ] in
  let id = id_of claimed in
  if id <> job_id then
    failwith
      (Printf.sprintf "the claim handed out %s, not the job just created, %s" id
         job_id);
  { creator; worker; job = claimed; ref = ref_of claimed }

let frames_of t channel job_id =
  List.filter
    (fun (f : Bus_stream.frame) ->
      Protocol.at f.payload [ "jobId" ] = Some (`String job_id))
    (Bus_stream.frames ~channel t.observer)

(* The job:queued announcements of [job_id] so far. *)
let announcements t job_id = frames_of t "job:queued" job_id

let nth_of t channel what ?(n = 1) ?(timeout = 10.0) job_id =
  Run.eventually (Printf.sprintf "%s %d of %s" what n job_id) timeout (fun () ->
      List.nth_opt (frames_of t channel job_id) (n - 1))

(* Wait for the [n]th announcement of [job_id]. *)
let announced ?n ?timeout t job_id =
  nth_of t "job:queued" "announcement" ?n ?timeout job_id

(* Wait for the job:assign recording the [n]th claim of [job_id]. *)
let assigned ?n ?timeout t job_id =
  nth_of t "job:assign" "assignment" ?n ?timeout job_id

let start_again t document =
  t.dispatcher <- Dispatcher.start { t.dispatcher.launch with document };
  answering t

(* Start a dispatcher again, on the same broker and, unless [change] changes
   them, the same settings. *)
let restart_dispatcher ?(change = Fun.id) t =
  ignore (Dispatcher.stop t.dispatcher : int option);
  start_again t (change (settings t))

(* Kill the dispatcher without its shutdown, and start another on the same
   broker. *)
let crash_and_restart ?(change = Fun.id) t =
  Dispatcher.crash t.dispatcher;
  start_again t (change (settings t))

(* Cancel every pending job a case of this world created, each by its id, so
   that no case's jobs are claimable by the next: a claim names jobs by what
   they are, not by id. A job the dispatcher did not act on is over, and is
   not asked about again; one it did act on may be running, and is asked
   about after every case until it is over. *)
let clear_pending t =
  let sweeper = sidecar t "sweeper" in
  List.iter
    (fun job_id ->
      let answer =
        cancel_request sweeper (`Assoc [ ("jobId", `String job_id) ])
      in
      if not answer.ok then refused_with ("cancelling " ^ job_id) answer;
      if not (cancelled answer) then Hashtbl.remove t.unfinished job_id)
    (List.of_seq (Hashtbl.to_seq_keys t.unfinished))

let output t = Process.output t.dispatcher.process

let close t =
  ignore (Dispatcher.stop t.dispatcher : int option);
  World.close t.world;
  Nats.stop_broker t.broker
