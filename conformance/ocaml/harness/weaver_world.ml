(* A Weaver world: a Weaver process and everything it meets. That is an
   Archivist world (a knowledge base, a real Archivist that records and
   answers, a gateway, the trusted issuer), the Weaver's own service account
   at that issuer, and a real Neo4j, cleared when the world is made.

   Cases meet the Weaver in the three places the rest of Xynolog does: in
   the graph it writes, read by the suite's own reader (Graph_reader); on
   the bus, where it signals what it has applied and answers a rebuild; and
   at its health port. Nothing is recorded but through the Archivist, as
   people, workers and services record: the Weaver is given the events the
   record gives it. *)

let weaver_client = "xynolog-weaver"

(* The variable the Weaver's document names for the graph database's
   password. *)
let graph_password = "NEO4J_PASSWORD"

(* Who records, where a case does not say. *)
let person_sub = "ada"
let member = Yojson.Safe.Util.member

(* How a Weaver is launched: its configuration document, all but where it
   keeps its cursors, which is a file of its home; and its environment. A
   case changes the world's. *)
type launch = { document : Yojson.Safe.t; env : (string * string) list }

type t = {
  kb : Archivist_world.t;
  graph : Graph_reader.t;
  service : Implementations.weaver;
  base : launch;
  mutable home : string;
  signals : Bus_stream.t;
  mutable weaver : Process.t option;
  mutable last_output : string list;
  people : (string, Archivist_world.client) Hashtbl.t;
}

(* The Neo4j the suite is given: where it is, and its password. *)
let database () =
  let named variable =
    match Sys.getenv_opt variable with
    | Some value when value <> "" -> value
    | _ ->
        failwith
          (Printf.sprintf
             "The weaver suite runs against a real neo4j, which it clears, and \
              %s is not set. Start one for it alone \
              (NEO4J_AUTH=neo4j/<password>, and as its default database the \
              one graph/database-cases.json names for the suite's knowledge \
              base), and set NEO4J_HOST to its address and NEO4J_PASSWORD to \
              that password."
             variable)
  in
  (named "NEO4J_HOST", named graph_password)

let python (implementations : Implementations.t) =
  match List.assoc_opt "python" implementations.weavers with
  | Some service -> service
  | None ->
      failwith
        "the weaver suite starts the Python Weaver, and the implementations \
         file does not name it"

let fresh_home () = Filename.temp_dir "conformance-weaver-" ""

(* ── The process ────────────────────────────────────────────────────── *)

(* The world's launch, its document's member at [path] set to [value]. *)
let changed ?(launch : launch option) t path value =
  let launch = Option.value launch ~default:t.base in
  { launch with document = Edit.set_at path value launch.document }

(* The world's launch, its environment's [variable] set to [value], or taken
   away when [value] is None. *)
let with_variable ?(launch : launch option) t variable value =
  let launch = Option.value launch ~default:t.base in
  {
    launch with
    env =
      (List.remove_assoc variable launch.env
      @ match value with Some v -> [ (variable, v) ] | None -> []);
  }

let services_launch t (launch : launch) ~home : Services.weaver =
  {
    weaver = t.service;
    weaver_document =
      (fun cursors -> Edit.set_at [ "cursorsPath" ] cursors launch.document);
    weaver_env = launch.env;
    weaver_home = home;
  }

(* Every line the Weaver has written: the running one's, or those of the
   one that ran last. *)
let output t =
  match t.weaver with
  | Some process -> Process.output process
  | None -> t.last_output

(* The running Weaver. *)
let running t =
  match t.weaver with
  | Some process -> process
  | None -> failwith "the case stopped the Weaver and has not started it again"

(* Start the Weaver, as this world launches one or as [launch] says, in the
   world's home, and wait until its first sweep of the record is done and
   its reconcile has ended. *)
let start ?(launch : launch option) t =
  if t.weaver <> None then failwith "a Weaver is already running in this world";
  let launch = Option.value launch ~default:t.base in
  t.weaver <-
    Some (Services.start_weaver (services_launch t launch ~home:t.home))

(* SIGTERM, and wait: its exit code. *)
let stop t =
  let process = running t in
  let code = Process.stop process in
  t.last_output <- Process.output process;
  t.weaver <- None;
  code

(* Stop the Weaver and start it again in the same home: it finds what it
   kept. *)
let restart t =
  ignore (stop t : int option);
  start t

(* Stop the Weaver and start one that has kept nothing: a new home, as a new
   container has. *)
let replace t =
  ignore (stop t : int option);
  Gateway.remove t.home;
  t.home <- fresh_home ();
  start t

(* A Weaver that must refuse to start, launched as [launch] says in a home
   of its own: its exit code and its output. The world's own Weaver is
   stopped first, since both would answer on one health port. *)
let refused ?(timeout = 60.0) t launch =
  if t.weaver <> None then
    failwith
      "the world's Weaver is running: stop it before starting one that must \
       refuse, since both would answer on one health port";
  let home = Filename.temp_dir "conformance-weaver-refused-" "" in
  Fun.protect
    ~finally:(fun () -> Gateway.remove home)
    (fun () ->
      let code, output =
        Services.refused_weaver
          (services_launch t launch ~home)
          ~named:(Services.named_variables t.base.document)
          ~timeout
      in
      (code, String.concat "\n" output))

(* What the running Weaver's health answers, held to WeaverHealth. *)
let health t =
  match Services.health "WeaverHealth" (Services.weaver_origin t.service) with
  | Some health -> health
  | None ->
      failwith
        ("the Weaver's health did not answer:\n"
        ^ String.concat "\n" (Process.tail 20 (running t)))

(* ── What a case records, through the Archivist ─────────────────────── *)

let person t sub =
  match Hashtbl.find_opt t.people sub with
  | Some client -> client
  | None ->
      let client = Archivist_world.person t.kb sub in
      Hashtbl.replace t.people sub client;
      client

let ask t ?(by = person_sub) operation members =
  Archivist_world.ask (person t by) operation (`Assoc members)

(* Upload [content] as a resource named [name], as a person: its id, once
   the record holds it. The gateway of an Archivist world reads a
   resource's annotations from the harness's stand-in Archivist, which
   answers for this resource what the real Archivist answers when it is
   asked. *)
let uploaded t ?(by = person_sub) name content =
  let id =
    Archivist_world.created t.kb
      (World.person_did t.kb.world by)
      [
        ("name", name); ("storageUri", "file://weaver/" ^ Fresh.uuid () ^ ".md");
      ]
      content
  in
  Hashtbl.replace t.kb.world.archivist.annotations id (fun () ->
      Archivist_world.annotations t.kb id);
  id

(* A resource's view, as the record holds it now. *)
let view t resource_id =
  match Archivist_world.view t.kb resource_id with
  | Some view -> view
  | None -> failwith ("the record holds no resource " ^ resource_id)

(* The record's events of one type for a resource, oldest first. *)
let recorded t resource_id type_ =
  List.filter
    (fun event -> member "type" event = `String type_)
    (Archivist_world.stored t.kb resource_id)

(* ── What it says it has applied ────────────────────────────────────── *)

(* Every weave:applied the Weaver has signalled since the world was made,
   oldest first; of one resource when one is named. Each is its resource
   and the sequence number it took the resource's stream to. *)
let applied_signals ?resource_id t =
  List.filter_map
    (fun (frame : Bus_stream.frame) ->
      let payload = Protocol.unstamped frame.payload in
      match
        ( Protocol.at payload [ "resourceId" ],
          Protocol.at payload [ "sequenceNumber" ] )
      with
      | Some (`String id), Some (`Int n) -> (
          match resource_id with
          | Some wanted when wanted <> id -> None
          | _ -> Some (id, n))
      | _ -> None)
    (Bus_stream.frames ~channel:"weave:applied" t.signals)

(* The sequence of the last event of a resource's stream, as the record
   holds it now. A Weaver follows the record whole (WEAVER.md § What it
   follows): it signals how far it took a resource's stream up to its last
   event, whatever that event's type. *)
let last t resource_id =
  match List.rev (Archivist_world.stored t.kb resource_id) with
  | event :: _ ->
      Yojson.Safe.Util.to_int
        (member "sequenceNumber" (member "metadata" event))
  | [] -> failwith ("the record holds no event of " ^ resource_id)

(* Wait until the Weaver signals it has applied [resource_id] up to
   [sequence_number]. *)
let applied ?(timeout = 20.0) t resource_id sequence_number =
  Run.eventually
    (Printf.sprintf "weave:applied for %s to reach %d" resource_id
       sequence_number) timeout (fun () ->
      List.find_opt
        (fun (_, n) -> n >= sequence_number)
        (applied_signals ~resource_id t))
  |> ignore

(* Wait until the Weaver has taken everything the record holds for each of
   [resource_ids]. *)
let caught_up t resource_ids =
  List.iter (fun id -> applied t id (last t id)) resource_ids

(* ── What it is asked ───────────────────────────────────────────────── *)

(* Ask the Weaver to rebuild one resource, or the whole graph, as a service
   asks: its answer. *)
let rebuild ?resource_id t =
  Archivist_world.request ~timeout:60.0
    (Archivist_world.sidecar t.kb "rebuild")
    "weave:rebuild"
    (`Assoc
       (match resource_id with
       | Some id -> [ ("resourceId", `String id) ]
       | None -> []))

(* ── The graph, behind a Weaver's back ──────────────────────────────── *)

(* Empty the graph, as a wiped volume leaves it. Only while no Weaver
   runs. *)
let clean t =
  if t.weaver <> None then
    failwith "the graph is cleaned only while no Weaver runs: stop it first";
  Graph_reader.clean t.graph

(* Take one annotation out of the graph, as one lost behind a Weaver's back
   is. Only while no Weaver runs. *)
let lose t annotation_id =
  if t.weaver <> None then
    failwith "an annotation is lost only while no Weaver runs: stop it first";
  Graph_reader.lose t.graph annotation_id

(* ── The world ──────────────────────────────────────────────────────── *)

(* Every violation since the last call, carried by the streams the case
   opened, which are then closed. *)
let drain t =
  Hashtbl.reset t.people;
  let signalled = Dynarray.to_list t.signals.violations in
  Dynarray.clear t.signals.violations;
  Archivist_world.drain t.kb @ signalled

(* Stop the Weaver, and close everything the world made. *)
let close t =
  Fun.protect
    ~finally:(fun () ->
      Graph_reader.close t.graph;
      Archivist_world.close t.kb;
      Gateway.remove t.home)
    (fun () -> if t.weaver <> None then ignore (stop t : int option))

(* A world, its graph cleaned before its Weaver first starts. What was
   started is stopped when a start fails, and the failure reported is the
   first one. *)
let create (implementations : Implementations.t) =
  let host, password = database () in
  let service = python implementations in
  let secret = Fresh.hex 24 in
  let kb =
    Archivist_world.create ~git_sync:false ~gateway:implementations.gateway
      ~command:implementations.archivist
      ~accounts:
        [
          ( weaver_client,
            {
              Issuer.proof = Secret secret;
              roles = [ Protocol.service_role () ];
            } );
        ]
      ()
  in
  (* A standing stream of what the Weaver says it has applied, open before
     any Weaver is. *)
  let token, _ = World.agent kb.world "weaver-watch" in
  let signals =
    World.subscribe ~standing:true kb.world token
      (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ())
         ~global:[ "weave:applied" ] ())
  in
  Bus_stream.on signals (fun message ->
      Option.iter
        (fun (frame : Bus_stream.frame) ->
          match
            Protocol.against_channel frame.channel
              (Protocol.unstamped frame.payload)
          with
          | None -> ()
          | Some said ->
              Bus_stream.violation signals
                (Printf.sprintf "%s carried a payload that is %s: %s"
                   frame.channel said
                   (Call.shown (Yojson.Safe.to_string frame.payload))))
        message.frame);
  let database = Neo4j.database_of kb.world.kb.did in
  let document =
    `Assoc
      [
        ("kb", World.served kb.world);
        ("gatewayUrl", `String (World.origin kb.world));
        ("identity", `Assoc [ ("issuer", `String kb.world.issuer.origin) ]);
        ( "archivist",
          `Assoc
            [
              ("host", `String "127.0.0.1");
              ("port", `Int (Archivist_world.port kb));
            ] );
        ( "graph",
          `Assoc
            [
              ("type", `String "neo4j");
              ( "uri",
                `String (Printf.sprintf "bolt://%s:%d" host Neo4j.bolt_port) );
              ("username", `String "neo4j");
              ("database", `String database);
              ("passwordEnv", `String graph_password);
            ] );
        ("port", `Int service.health_port);
        ("logLevel", `String "info");
        ("logFormat", `String "simple");
      ]
  in
  let t =
    {
      kb;
      graph = Graph_reader.connect ~host ~password ~database;
      service;
      base =
        {
          document;
          env =
            [
              ("XYNOLOG_OIDC_CLIENT_ID", weaver_client);
              ("XYNOLOG_OIDC_CLIENT_SECRET", secret);
              (graph_password, password);
            ];
        };
      home = fresh_home ();
      signals;
      weaver = None;
      last_output = [];
      people = Hashtbl.create 8;
    }
  in
  match
    clean t;
    start t
  with
  | () -> t
  | exception failure ->
      let trace = Printexc.get_raw_backtrace () in
      (try Eio.Cancel.protect (fun () -> close t) with _ -> ());
      Printexc.raise_with_backtrace failure trace

(* The end of each case: the streams it opened must have carried nothing
   the spec does not allow. *)
let end_case world body =
  let t = world () in
  World.end_case
    ~account:(fun () ->
      String.concat "\n"
        [
          "Weaver output:\n" ^ String.concat "\n" (output t);
          "Archivist output:\n"
          ^ String.concat "\n" (Process.tail 40 t.kb.archivist.process);
        ])
    ~settle:(fun () -> drain t)
    (fun () -> body t)
