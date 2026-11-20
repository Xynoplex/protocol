(* A Smelter world: a Smelter process and everything it meets. That is an
   Archivist world (a knowledge base, a real Archivist that records and
   answers, a gateway, the trusted issuer), the Smelter's own service account
   at that issuer, a real Qdrant, cleared when the world is made and when it
   is closed, to which the Smelter presents a token minted from the Qdrant's
   key, and a stand-in for its embedding provider. The anchored-text store
   is the Archivist's: the Smelter mounts none of it, and keeps it on the
   Archivist's surface.

   Cases meet the Smelter where the rest of Xynolog does: in the index it
   writes and in the anchored-text store, each read here by the suite's own
   hand; on the bus, where it signals what it has decided and answers a
   rebuild; at its health port; at its provider; and in what it writes to its
   output. Nothing is recorded but through the Archivist, as people record:
   the Smelter is given the events the record gives it, and reads the bytes
   the Archivist serves. *)

let smelter_client = "xynolog-smelter"

(* The model the world's Smelter is told to embed with. The stand-in
   answers for any. *)
let embedding_model = "conformance-embedder"

(* Who records, where a case does not say. *)
let person_sub = "ada"
let ( // ) = Filename.concat
let member = Yojson.Safe.Util.member
let sha256 = Archivist_world.sha256

(* How a Smelter is launched: its configuration document, all but where it
   keeps its cursors, which is a file of its home; and its environment. A
   case changes the world's. *)
type launch = { document : Yojson.Safe.t; env : (string * string) list }

type t = {
  kb : Archivist_world.t;
  database : Qdrant.database;
  index : Qdrant.index;
  provider : Ollama_embed.t;
  command : string list;
  base : launch;
  home : string;
  signals : Bus_stream.t;
  mutable smelter : Process.t option;
  mutable last_output : string list;
  people : (string, Archivist_world.client) Hashtbl.t;
}

(* The Qdrant the suite is given: where it is, and the API key it was
   started with, which it must ask for. *)
let database () =
  let named variable =
    match Sys.getenv_opt variable with
    | Some value when value <> "" -> value
    | _ ->
        failwith
          (Printf.sprintf
             "The smelter suite runs against a real Qdrant, which it clears, \
              and %s is not set. Start a Qdrant for it alone \
              (QDRANT__SERVICE__API_KEY=<key>, \
              QDRANT__SERVICE__JWT_RBAC=true), and set QDRANT_HOST to its \
              address and QDRANT_API_KEY to its key."
             variable)
  in
  let database : Qdrant.database =
    { host = named "QDRANT_HOST"; key = named "QDRANT_API_KEY" }
  in
  Qdrant.guarded database;
  database

let python (implementations : Implementations.t) =
  match List.assoc_opt "python" implementations.smelters with
  | Some service -> service
  | None ->
      failwith
        "the smelter suite starts the Python Smelter, and the implementations \
         file does not name it"

(* ── The process ────────────────────────────────────────────────────── *)

(* The world's launch, its document's member at [path] set to [value], or
   taken away when [value] is None. *)
let changed ?(launch : launch option) t path value =
  let launch = Option.value launch ~default:t.base in
  {
    launch with
    document =
      (match value with
      | Some value -> Edit.set_at path value launch.document
      | None -> Edit.remove_at path launch.document);
  }

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

let services_launch t (launch : launch) ~home : Services.smelter =
  {
    smelter_command = t.command;
    smelter_document =
      (fun cursors -> Edit.set_at [ "cursorsPath" ] cursors launch.document);
    smelter_env = launch.env;
    smelter_home = home;
  }

(* Every line the Smelter has written: the running one's, or those of the
   one that ran last. *)
let output t =
  match t.smelter with
  | Some process -> Process.output process
  | None -> t.last_output

(* The running Smelter. *)
let running t =
  match t.smelter with
  | Some process -> process
  | None -> failwith "the case stopped the Smelter and has not started it again"

(* Start the Smelter, as this world launches one or as [launch] says, and
   wait until its start has ended, however it ended; with [`Serving], only
   until its health answers. Each start is of a container made again, as
   every [xynolog start] makes one: no cursor of an earlier start is left,
   so every resource the record holds is started by a reconcile (SMELTER.md
   § Reconcile). *)
let start ?(launch : launch option) ?until t =
  if t.smelter <> None then
    failwith "a Smelter is already running in this world";
  let launch = Option.value launch ~default:t.base in
  let s = services_launch t launch ~home:t.home in
  (match Services.smelter_cursors s with
  | `String cursors when Sys.file_exists cursors -> Sys.remove cursors
  | _ -> ());
  t.smelter <- Some (Services.start_smelter ?until s)

let stopped t process =
  t.last_output <- Process.output process;
  t.smelter <- None

(* SIGTERM, and wait: its exit code. *)
let stop t =
  let process = running t in
  let code = Process.stop process in
  stopped t process;
  code

(* SIGKILL, and wait: the Smelter ends with nothing settled on its way
   out. *)
let crash t =
  let process = running t in
  Process.crash process;
  stopped t process

let restart t =
  ignore (stop t : int option);
  start t

(* A Smelter that must refuse to start, launched as [launch] says: its exit
   code and its output. The world's own Smelter is stopped first, since a
   Smelter's health port is a constant. *)
let refused ?(timeout = 60.0) t launch =
  if t.smelter <> None then
    failwith
      "the world's Smelter is running: stop it before starting one that must \
       refuse, since both would answer on one health port";
  let home = Filename.temp_dir "conformance-smelter-refused-" "" in
  Fun.protect
    ~finally:(fun () -> Gateway.remove home)
    (fun () ->
      let code, output =
        Services.refused_smelter
          (services_launch t launch ~home)
          ~named:(Services.named_variables t.base.document)
          ~timeout
      in
      (code, String.concat "\n" output))

(* What the running Smelter's health answers, held to SmelterHealth. *)
let health t =
  match Services.health "SmelterHealth" Services.smelter_origin with
  | Some health -> health
  | None ->
      failwith
        ("the Smelter's health did not answer:\n"
        ^ String.concat "\n" (Process.tail 20 (running t)))

let phase health = Check.at health [ "reconcile"; "phase" ]

(* Wait until the Smelter's reconcile has ended, however it ended: its
   health then. *)
let reconciled ?(timeout = 60.0) t =
  Run.eventually "the Smelter's reconcile to end" timeout (fun () ->
      let health = health t in
      if List.mem (phase health) [ `String "done"; `String "failed" ] then
        Some health
      else None)

let events_processed health =
  Yojson.Safe.Util.to_int (Check.at health [ "eventsProcessed" ])

(* Wait until the Smelter's health says it has applied at least [count]
   events since it started: its health then. *)
let applied ?(timeout = 20.0) t count =
  Run.eventually
    (Printf.sprintf "the Smelter to have applied %d event(s)" count) timeout
    (fun () ->
      let health = health t in
      if events_processed health >= count then Some health else None)

(* ── What a case records, as a person, through the Archivist ────────── *)

let person t sub =
  match Hashtbl.find_opt t.people sub with
  | Some client -> client
  | None ->
      let client = Archivist_world.person t.kb sub in
      Hashtbl.replace t.people sub client;
      client

let ask t ?(by = person_sub) operation members =
  Archivist_world.ask (person t by) operation (`Assoc members)

let view t resource_id =
  match Archivist_world.view t.kb resource_id with
  | Some view -> view
  | None -> failwith ("the record holds no resource " ^ resource_id)

(* Where the record says a resource's content is stored. *)
let storage_uri t resource_id =
  match Protocol.at (view t resource_id) [ "resource"; "representations" ] with
  | Some (`List (first :: _)) | Some (`Assoc _ as first) ->
      Yojson.Safe.Util.to_string (Check.at first [ "storageUri" ])
  | _ -> failwith ("the record holds no stored content for " ^ resource_id)

(* The file of the working tree that holds a resource's content: a case
   removes it to leave the resource's bytes unreadable. *)
let content_file t resource_id =
  Archivist_world.content_path t.kb (storage_uri t resource_id)

(* A resource's entity types, as the record holds them now. *)
let entity_types t resource_id =
  match Protocol.at (view t resource_id) [ "resource"; "entityTypes" ] with
  | Some (`List types) -> List.map Yojson.Safe.Util.to_string types
  | _ -> []

(* Upload [content] as a resource of the media type [format]: its id, once
   the record holds it. *)
let created t ?(name = "A resource") ?(by = person_sub) content format =
  Archivist_world.created t.kb
    (World.person_did t.kb.world by)
    [
      ("name", name);
      ("storageUri", "file://smelter/" ^ Fresh.uuid ());
      ("format", format);
    ]
    content

(* Give a resource new content, which is put where its content is stored and
   recorded by yield:update: the new content's checksum, once the record
   holds it. *)
let updated t ?by resource_id content =
  let uri = storage_uri t resource_id in
  Archivist_world.write t.kb uri content;
  let checksum = sha256 content in
  ignore
    (ask t ?by "yield:update"
       [
         ("resourceId", `String resource_id);
         ("storageUri", `String uri);
         ("contentChecksum", `String checksum);
         ("byteSize", `Int (String.length content));
       ]
      : Yojson.Safe.t);
  checksum

let archived t ?by resource_id =
  ignore
    (ask t ?by "mark:archive" [ ("resourceId", `String resource_id) ]
      : Yojson.Safe.t)

let unarchived t ?by resource_id =
  ignore
    (ask t ?by "mark:unarchive" [ ("resourceId", `String resource_id) ]
      : Yojson.Safe.t)

let strings items = `List (List.map (fun s -> `String s) items)

let update_entity_types t ?by resource_id current updated =
  ignore
    (ask t ?by "mark:update-entity-types"
       [
         ("resourceId", `String resource_id);
         ("currentEntityTypes", strings current);
         ("updatedEntityTypes", strings updated);
       ]
      : Yojson.Safe.t)

(* Give a resource an entity type, having put the type in the knowledge
   base's vocabulary: once the record holds it. *)
let tagged t ?by resource_id entity_type =
  let current = entity_types t resource_id in
  if List.mem entity_type current then
    failwith
      (Printf.sprintf
         "%s already has the entity type %s: nothing would be recorded"
         resource_id entity_type);
  (* An entity type joins the vocabulary before a resource is given it: the
     Archivist refuses one that has not. *)
  let vocabulary =
    match Archivist_world.projection t.kb "entitytypes.json" with
    | Some projection -> (
        match Protocol.at projection [ "entityTypes" ] with
        | Some (`List types) -> types
        | _ -> [])
    | None -> []
  in
  if not (List.mem (`String entity_type) vocabulary) then
    ignore
      (ask t ?by "frame:add-entity-type" [ ("tag", `String entity_type) ]
        : Yojson.Safe.t);
  update_entity_types t ?by resource_id current (current @ [ entity_type ])

(* Take an entity type from a resource: once the record holds it. *)
let untagged t ?by resource_id entity_type =
  let current = entity_types t resource_id in
  if not (List.mem entity_type current) then
    failwith
      (Printf.sprintf
         "%s does not have the entity type %s: nothing would be recorded"
         resource_id entity_type);
  update_entity_types t ?by resource_id current
    (List.filter (fun e -> e <> entity_type) current)

let annotation t ?by ?(motivation = "highlighting") ?body resource_id target =
  Yojson.Safe.Util.to_string
    (Check.at
       (ask t ?by "mark:create-request"
          [
            ("resourceId", `String resource_id);
            ( "request",
              `Assoc
                ([ ("motivation", `String motivation); ("target", target) ]
                @ match body with Some b -> [ ("body", b) ] | None -> []) );
          ])
       [ "annotationId" ])

(* An annotation on a resource whose target quotes [exact]: its id, once the
   record holds it. The target says what it quotes and not where: a Smelter
   reads the quotation and nothing else of a target. *)
let annotated t ?by ?motivation ?body resource_id exact =
  annotation t ?by ?motivation ?body resource_id
    (`Assoc
       [
         ("type", `String "SpecificResource");
         ("source", `String resource_id);
         ( "selector",
           `Assoc
             [ ("type", `String "TextQuoteSelector"); ("exact", `String exact) ]
         );
       ])

(* An annotation of a resource as a whole, which quotes no text: its id,
   once the record holds it. *)
let annotated_whole t ?by ?motivation ?body resource_id =
  annotation t ?by ?motivation ?body resource_id
    (`Assoc [ ("source", `String resource_id) ])

(* A body by which an annotation states an entity type. *)
let tagging entity_type =
  `Assoc
    [
      ("type", `String "TextualBody");
      ("value", `String entity_type);
      ("purpose", `String "tagging");
    ]

(* Give an annotation's body an entity tag for each of [entity_types], after
   whatever it states: once the record holds it. *)
let body_tagged t ?by resource_id annotation_id entity_types =
  ignore
    (ask t ?by "bind:update-body"
       [
         ("resourceId", `String resource_id);
         ("annotationId", `String annotation_id);
         ( "operations",
           `List
             (List.map
                (fun e -> `Assoc [ ("op", `String "add"); ("item", tagging e) ])
                entity_types) );
       ]
      : Yojson.Safe.t)

(* Remove an annotation: once the record holds it. *)
let unannotated t ?by resource_id annotation_id =
  ignore
    (ask t ?by "mark:delete"
       [
         ("resourceId", `String resource_id);
         ("annotationId", `String annotation_id);
       ]
      : Yojson.Safe.t)

(* ── What it says it has decided ────────────────────────────────────── *)

(* Every decision the Smelter has signalled on smelt:settled since the world
   was made, oldest first; of one resource when one is named. *)
let settled_signals ?resource_id t =
  List.filter_map
    (fun (frame : Bus_stream.frame) ->
      let payload = Protocol.unstamped frame.payload in
      match resource_id with
      | Some id when Protocol.at payload [ "resourceId" ] <> Some (`String id)
        ->
          None
      | _ -> Some payload)
    (Bus_stream.frames ~channel:"smelt:settled" t.signals)

(* Wait for the [nth] decision the Smelter signals of a resource, its first
   unless said: that decision. *)
let settled ?(nth = 1) ?(timeout = 20.0) t resource_id =
  Run.eventually
    (Printf.sprintf "smelt:settled number %d of %s" nth resource_id) timeout
    (fun () -> List.nth_opt (settled_signals ~resource_id t) (nth - 1))

(* Know that the [nth] decision of a resource, its first unless said, is not
   signalled within [within] seconds. It watches for that long, and fails at
   once when the decision comes. *)
let unsettled ?(nth = 1) ?(within = 3.0) t resource_id =
  let deadline = Run.now () +. within in
  let rec watch () =
    match List.nth_opt (settled_signals ~resource_id t) (nth - 1) with
    | Some signal ->
        failwith
          (Printf.sprintf
             "expected no smelt:settled number %d of %s within %g s, and got %s"
             nth resource_id within
             (Yojson.Safe.to_string signal))
    | None when Run.now () > deadline -> ()
    | None ->
        Run.sleep 0.025;
        watch ()
  in
  watch ()

(* ── What it is asked ───────────────────────────────────────────────── *)

(* Ask the Smelter to derive anchored text again, of one resource or of
   every one, as a service asks: its answer. *)
let rebuild_anchors ?resource_id t =
  Archivist_world.request ~timeout:60.0
    (Archivist_world.sidecar t.kb "rebuild-anchors")
    "smelt:rebuild-anchors"
    (`Assoc
       (match resource_id with
       | Some id -> [ ("resourceId", `String id) ]
       | None -> []))

(* ── The index ──────────────────────────────────────────────────────── *)

(* Clear the index: every collection gone, as a wiped volume leaves it. Only
   while no Smelter runs: one that is running made its collections when it
   connected, and its next write would fail. *)
let clear_index t =
  if t.smelter <> None then
    failwith
      "the index is cleared only while no Smelter runs: stop it first, and \
       start it again afterwards";
  Qdrant.clear t.index

(* ── The anchored-text store ────────────────────────────────────────── *)

let store t = t.kb.dirs.anchored_text_dir

(* Every file of the anchored-text store, by its path from the store's
   directory, sorted. *)
let anchored_files t = List.map fst (Archivist_world.files_under (store t))

(* The stamp the store's writer states, in the file archivist/layout.json
   names for it; None when it states none. *)
let anchored_stamp t =
  let stamp = (Archivist_world.layout ()).anchored_text_stamp in
  let path = Archivist_world.placed t.kb stamp.tree stamp.path [] in
  if Sys.file_exists path then
    Some (String.trim (Archivist_world.read_file path))
  else None

(* The store's entry for bytes of [checksum], held to the schema the layout
   names; None when it has none. *)
let anchored_entry t checksum =
  Archivist_world.document_at
    (Archivist_world.anchored_text_entry_path t.kb checksum)
    (Archivist_world.layout ()).anchored_text_entry.schema
    "an anchored-text entry"

(* Take the entry for bytes of [checksum] out of the store, behind a
   Smelter's back. *)
let lose_anchored_entry t checksum =
  let path = Archivist_world.anchored_text_entry_path t.kb checksum in
  if not (Sys.file_exists path) then
    failwith
      ("the anchored-text store has no entry for " ^ checksum ^ " to lose");
  Sys.remove path

(* Empty the anchored-text store, its stamp included, as a wiped volume
   leaves it. Only while no Smelter runs: a writer states its stamp once. *)
let clear_anchored_text t =
  if t.smelter <> None then
    failwith
      "the anchored-text store is cleared only while no Smelter runs: stop it \
       first, and start it again afterwards";
  Array.iter
    (fun name -> Gateway.remove (store t // name))
    (Sys.readdir (store t))

(* ── The world ──────────────────────────────────────────────────────── *)

(* Every violation since the last call: carried by the streams the case
   opened, which are then closed, or asked of the provider. What the case
   scripted of the provider, and what the provider recorded, is
   forgotten. *)
let drain t =
  Hashtbl.reset t.people;
  let signalled = Dynarray.to_list t.signals.violations in
  Dynarray.clear t.signals.violations;
  let asked = Dynarray.to_list t.provider.violations in
  Ollama_embed.reset t.provider;
  Archivist_world.drain t.kb @ signalled @ asked

(* Stop the Smelter, leave the index empty for the next file, and close
   everything the world made. *)
let close t =
  Fun.protect
    ~finally:(fun () ->
      Ollama_embed.close t.provider;
      Archivist_world.close t.kb;
      Gateway.remove t.home)
    (fun () ->
      if t.smelter <> None then ignore (stop t : int option);
      Qdrant.clear t.index)

(* A world, its index cleared before its Smelter first starts. What was
   started is stopped when a start fails, and the failure reported is the
   first one. *)
let create (implementations : Implementations.t) =
  let database = database () in
  let command = python implementations in
  let secret = Fresh.hex 24 in
  let kb =
    Archivist_world.create ~git_sync:false ~gateway:implementations.gateway
      ~command:implementations.archivist
      ~accounts:
        [
          ( smelter_client,
            {
              Issuer.proof = Secret secret;
              roles = [ Protocol.service_role () ];
            } );
        ]
      ()
  in
  let provider = Ollama_embed.start () in
  (* A standing stream of what the Smelter says it has decided, open before
     any Smelter is. *)
  let token, _ = World.agent kb.world "smelter-watch" in
  let signals =
    World.subscribe ~standing:true kb.world token
      (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ())
         ~global:[ "smelt:settled" ] ())
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
  let did = kb.world.kb.did in
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
        ( "vectors",
          `Assoc
            [
              ("type", `String "qdrant");
              ("host", `String database.host);
              ("port", `Int Qdrant.port);
              ("tokenEnv", `String Qdrant.token_variable);
            ] );
        ( "embedding",
          `Assoc
            [
              ("type", `String "ollama");
              ("model", `String embedding_model);
              ("baseUrl", `String provider.origin);
            ] );
        ("chunking", `Assoc [ ("chunkSize", `Int 512); ("overlap", `Int 64) ]);
        ("pdfsAtOnce", `Int 1);
        ("port", `Int Services.smelter_port);
        ("logLevel", `String "info");
        ("logFormat", `String "simple");
      ]
  in
  let t =
    {
      kb;
      database;
      index = Qdrant.index database did;
      provider;
      command;
      base =
        {
          document;
          env =
            [
              ("XYNOLOG_OIDC_CLIENT_ID", smelter_client);
              ("XYNOLOG_OIDC_CLIENT_SECRET", secret);
              ( Qdrant.token_variable,
                Qdrant.minted database.key (Qdrant.claims did "smelter") );
            ];
        };
      home = Filename.temp_dir "conformance-smelter-" "";
      signals;
      smelter = None;
      last_output = [];
      people = Hashtbl.create 8;
    }
  in
  match
    clear_index t;
    start t
  with
  | () -> t
  | exception failure ->
      let trace = Printexc.get_raw_backtrace () in
      (try Eio.Cancel.protect (fun () -> close t) with _ -> ());
      Printexc.raise_with_backtrace failure trace

(* The end of each case: the streams it opened must have carried nothing
   the spec does not allow, and the provider must have been asked nothing
   it does not serve. *)
let end_case world body =
  let t = world () in
  World.end_case
    ~account:(fun () ->
      String.concat "\n"
        [
          "Smelter output:\n" ^ String.concat "\n" (output t);
          "Archivist output:\n"
          ^ String.concat "\n" (Process.tail 40 t.kb.archivist.process);
        ])
    ~settle:(fun () -> drain t)
    (fun () -> body t)
