(* A Librarian world: a Librarian process and everything it meets. That is
   an Archivist world (a knowledge base, a real Archivist, a gateway, the
   trusted issuer); the two services that write what a Librarian reads, a
   Weaver and a Smelter, each with a service account of its own; a real
   Neo4j and a real Qdrant, both cleared when the world is made and when it
   is closed, the Qdrant asked by the Smelter and the Librarian each with a
   token of its own, minted from the Qdrant's key; a stand-in for the model
   of each of the Librarian's two actors; and two stand-in embedding
   providers, one the Librarian asks and one the Smelter asks, which give
   the same text the same vector.

   Cases meet the Librarian where the rest of Xynolog does: on the bus, as a
   person who is signed in asks it; at its providers; and in its output.
   Nothing is put in the graph or the index but by the Weaver and the
   Smelter, from what a case records through the Archivist. A case stages a
   store that lags by stopping its writer.

   One read does not reach the real Archivist by itself: a Librarian reads a
   resource's anchored text from the gateway, and the gateway of an
   Archivist world asks the harness's stand-in Archivist. For every resource
   a case makes, the world has the stand-in answer what the real Archivist
   answers.

   The operations a Librarian answers are the registry's match: and gather:
   operations, read from it; nothing here restates them.

   Partly ported: no refused boots, entity types, archiving or unannotating,
   since no case ported here asks for them. *)

let librarian_client = "xynolog-librarian"
let weaver_client = "xynolog-weaver"
let smelter_client = "xynolog-smelter"

(* The variable the Librarian's and the Weaver's documents name for the
   graph database's password. *)
let graph_password = "NEO4J_PASSWORD"

(* The models the world's Librarian is told of. Each stand-in answers for any. *)
let matcher_model = "conformance-matcher"
let gatherer_model = "conformance-gatherer"
let embedding_model = "conformance-embedder"

(* What a deployment that states neither is given. *)
let settle_timeout_ms = 15_000
let semantic_floor = 0.6

(* Who records and who asks, where a case does not say. *)
let person_sub = "ada"

type stores = {
  graph_host : string;
  graph_password_value : string;
  vectors : Qdrant.database;
}

(* What a case may change of how the Librarian is launched. *)
type launch = { settle_ms : int; env : (string * string) list }

type t = {
  kb : Archivist_world.t;
  command : string list;
  document : settle_ms:int -> Yojson.Safe.t;
  base : launch;
  home : string;
  port : int;
  matcher : Ollama.t;
  gatherer : Ollama.t;
  embedder : Ollama_embed.t;
  smelter_embedder : Ollama_embed.t;
  weaver : Services.weaver;
  smelter : Services.smelter;
  index : Qdrant.index;
  graph : Neo4j.t;
  signals : Bus_stream.t;
  mutable librarian : Process.t option;
  mutable weaver_process : Process.t option;
  mutable smelter_process : Process.t option;
  mutable last_output : string list;
  mutable last_weaver_output : string list;
  mutable last_smelter_output : string list;
  people : (string, Archivist_world.client) Hashtbl.t;
  contents : (string, string) Hashtbl.t;
      (** What each resource a case made holds, as uploaded. *)
}

let stores () =
  let named variable =
    match Sys.getenv_opt variable with
    | Some value when value <> "" -> value
    | _ ->
        failwith
          (Printf.sprintf
             "The librarian suite runs against a real neo4j and a real Qdrant, \
              both of which it clears, and %s is not set. Start a neo4j \
              (NEO4J_AUTH=neo4j/<password>) and a Qdrant \
              (QDRANT__SERVICE__API_KEY=<key>, QDRANT__SERVICE__JWT_RBAC=true) \
              for it alone, and set NEO4J_HOST, NEO4J_PASSWORD, QDRANT_HOST \
              and QDRANT_API_KEY."
             variable)
  in
  {
    graph_host = named "NEO4J_HOST";
    graph_password_value = named "NEO4J_PASSWORD";
    vectors =
      ({ host = named "QDRANT_HOST"; key = named "QDRANT_API_KEY" }
        : Qdrant.database);
  }

let member = Yojson.Safe.Util.member
let ( // ) = Filename.concat

(* What the registry's schema says against a frame's payload, as a failure. *)
let held channel payload =
  match Protocol.against_channel channel payload with
  | None -> ()
  | Some said ->
      failwith
        (Printf.sprintf "%s carried a payload that is %s: %s" channel said
           (Call.shown (Yojson.Safe.to_string payload)))

let python what services =
  match List.assoc_opt "python" services with
  | Some service -> service
  | None ->
      failwith
        (Printf.sprintf
           "the librarian suite starts the Python %s, and the implementations \
            file does not name it"
           what)

(* ── The Librarian ──────────────────────────────────────────────────── *)

let person t sub =
  match Hashtbl.find_opt t.people sub with
  | Some client -> client
  | None ->
      let client = Archivist_world.person t.kb sub in
      Hashtbl.replace t.people sub client;
      client

let output t =
  match t.librarian with Some p -> Process.output p | None -> t.last_output

(* Start the Librarian, as this world launches one or as a case changes
   that, and wait until its health answers and it answers on the bus: its
   health says its stream to the gateway is asked for, not that it is open. *)
let start_librarian ?launch t =
  if t.librarian <> None then
    failwith "a Librarian is already running in this world";
  let launch = match launch with Some changed -> changed | None -> t.base in
  t.librarian <-
    Some
      (Services.start_librarian
         {
           librarian_command = t.command;
           librarian_document = t.document ~settle_ms:launch.settle_ms;
           librarian_env = launch.env;
           librarian_home = t.home;
           librarian_port = t.port;
         });
  let probe = person t person_sub in
  Run.eventually "the Librarian to answer on the bus" 30.0 (fun () ->
      match
        Archivist_world.request ~timeout:5.0 probe
          "gather:referenced-by-requested"
          (`Assoc [ ("resourceId", `String "asked-of-no-resource") ])
      with
      | answer
        when Protocol.at answer.payload [ "code" ]
             = Some (`String "peer-unavailable") ->
          None
      | _ -> Some ()
      | exception Run.Timed_out _ -> None)

let stop_librarian t =
  match t.librarian with
  | None ->
      failwith "the case stopped the Librarian and has not started it again"
  | Some process ->
      let code = Process.stop process in
      t.last_output <- Process.output process;
      t.librarian <- None;
      code

let restart_librarian ?launch t =
  ignore (stop_librarian t : int option);
  start_librarian ?launch t

(* ── The two writers ────────────────────────────────────────────────── *)

let weaver_output t =
  match t.weaver_process with
  | Some p -> Process.output p
  | None -> t.last_weaver_output

let smelter_output t =
  match t.smelter_process with
  | Some p -> Process.output p
  | None -> t.last_smelter_output

let start_weaver t =
  if t.weaver_process <> None then
    failwith "a Weaver is already running in this world";
  t.weaver_process <- Some (Services.start_weaver t.weaver)

let stop_weaver t =
  match t.weaver_process with
  | None -> failwith "the Weaver is not running in this world"
  | Some process ->
      let code = Process.stop process in
      t.last_weaver_output <- Process.output process;
      t.weaver_process <- None;
      code

let start_smelter t =
  if t.smelter_process <> None then
    failwith "a Smelter is already running in this world";
  t.smelter_process <- Some (Services.start_smelter t.smelter)

let stop_smelter t =
  match t.smelter_process with
  | None -> failwith "the Smelter is not running in this world"
  | Some process ->
      let code = Process.stop process in
      t.last_smelter_output <- Process.output process;
      t.smelter_process <- None;
      code

(* Empty the graph database and the index, as wiped volumes leave them.
   Only while neither writer runs. *)
let clear_stores t =
  if t.weaver_process <> None || t.smelter_process <> None then
    failwith
      "the stores are cleared only while neither the Weaver nor the Smelter \
       runs";
  Neo4j.clear t.graph;
  Qdrant.clear t.index

(* ── What a case records, as a person, through the Archivist ────────── *)

let person_did t ?(sub = person_sub) () = World.person_did t.kb.world sub

(* A resource as the Archivist answers browse:resource-requested for it. *)
let described t resource_id =
  member "resource"
    (Archivist_world.ask (person t person_sub) "browse:resource-requested"
       (`Assoc [ ("resourceId", `String resource_id) ]))

(* An annotation as the Archivist answers browse:annotation-requested for it. *)
let annotated t resource_id annotation_id =
  member "annotation"
    (Archivist_world.ask (person t person_sub) "browse:annotation-requested"
       (`Assoc
          [
            ("resourceId", `String resource_id);
            ("annotationId", `String annotation_id);
          ]))

(* Upload [content] as a resource: its id, once the record holds it. *)
let created t ?(name = "A resource") ?(format = "text/plain") ?(by = person_sub)
    content =
  let resource_id =
    Archivist_world.created t.kb (person_did t ~sub:by ()) ~name
      ~storage_uri:("file://librarian/" ^ Fresh.uuid ())
      ~format content
  in
  Hashtbl.replace t.contents resource_id content;
  Hashtbl.replace t.kb.world.archivist.anchored_texts resource_id (fun () ->
      Archivist_world.anchored_text t.kb resource_id);
  resource_id

(* How many code points [text] holds: a selector counts in them. *)
let code_points text =
  String.fold_left
    (fun n c -> if Char.code c land 0xC0 = 0x80 then n else n + 1)
    0 text

(* The target of an annotation on the first place [exact] occurs in a text
   a case made. *)
let target t resource_id exact =
  let content =
    match Hashtbl.find_opt t.contents resource_id with
    | Some content -> content
    | None ->
        failwith
          (resource_id
         ^ " is not a text this world uploaded: a case annotates the texts it \
            made")
  in
  let at =
    match Re.exec_opt (Re.compile (Re.str exact)) content with
    | Some group -> Re.Group.start group 0
    | None -> failwith (Printf.sprintf "%s does not hold %S" resource_id exact)
  in
  let start = code_points (String.sub content 0 at) in
  `Assoc
    [
      ("source", `String resource_id);
      ( "selector",
        `List
          [
            `Assoc
              [
                ("type", `String "TextPositionSelector");
                ("start", `Int start);
                ("end", `Int (start + code_points exact));
              ];
            `Assoc
              [
                ("type", `String "TextQuoteSelector"); ("exact", `String exact);
              ];
          ] );
    ]

let annotation t resource_id motivation exact by =
  let answer =
    Archivist_world.ask (person t by) "mark:create-request"
      (`Assoc
         [
           ("resourceId", `String resource_id);
           ( "request",
             `Assoc
               [
                 ("motivation", `String motivation);
                 ("target", target t resource_id exact);
               ] );
         ])
  in
  Yojson.Safe.Util.to_string (member "annotationId" answer)

let highlighted t ?(by = person_sub) resource_id exact =
  annotation t resource_id "highlighting" exact by

(* A reference on [exact], which links nowhere yet: its id, once the record
   holds it and each of [entity_types] as an entity tag in its body. *)
let referenced t ?(entity_types = []) ?(by = person_sub) resource_id exact =
  let annotation_id = annotation t resource_id "linking" exact by in
  if entity_types <> [] then
    ignore
      (Archivist_world.ask (person t by) "bind:update-body"
         (`Assoc
            [
              ("resourceId", `String resource_id);
              ("annotationId", `String annotation_id);
              ( "operations",
                `List
                  (List.map
                     (fun value ->
                       `Assoc
                         [
                           ("op", `String "add");
                           ( "item",
                             `Assoc
                               [
                                 ("type", `String "TextualBody");
                                 ("value", `String value);
                                 ("purpose", `String "tagging");
                               ] );
                         ])
                     entity_types) );
            ])
        : Yojson.Safe.t);
  annotation_id

(* Bind a reference to another resource. *)
let bound t ?(by = person_sub) resource_id annotation_id to_resource_id =
  ignore
    (Archivist_world.ask (person t by) "bind:update-body"
       (`Assoc
          [
            ("resourceId", `String resource_id);
            ("annotationId", `String annotation_id);
            ( "operations",
              `List
                [
                  `Assoc
                    [
                      ("op", `String "add");
                      ( "item",
                        `Assoc
                          [
                            ("type", `String "SpecificResource");
                            ("source", `String to_resource_id);
                            ("purpose", `String "linking");
                          ] );
                    ];
                ] );
          ])
      : Yojson.Safe.t)

(* ── What the two writers say they have done ────────────────────────── *)

let signalled t channel resource_id =
  List.filter_map
    (fun (frame : Bus_stream.frame) ->
      let payload = Protocol.unstamped frame.payload in
      if Protocol.at payload [ "resourceId" ] = Some (`String resource_id) then
        Some payload
      else None)
    (Bus_stream.frames ~channel t.signals)

(* Wait until the Weaver signals that it has taken everything the record
   holds now for each resource: the graph then holds it as the record does. *)
let woven t resource_ids =
  List.iter
    (fun resource_id ->
      let last =
        match List.rev (Archivist_world.stored t.kb resource_id) with
        | last :: _ ->
            Yojson.Safe.Util.to_int
              (member "sequenceNumber" (member "metadata" last))
        | [] -> failwith ("the record holds no event of " ^ resource_id)
      in
      Run.eventually
        (Printf.sprintf "weave:applied for %s to reach %d" resource_id last)
        20.0 (fun () ->
          List.find_opt
            (fun payload ->
              match Protocol.at payload [ "sequenceNumber" ] with
              | Some (`Int n) -> n >= last
              | _ -> false)
            (signalled t "weave:applied" resource_id))
      |> ignore)
    resource_ids

let sha256 content = Digestif.SHA256.(to_hex (digest_string content))

(* Wait until the Smelter signals its decision about the content a resource
   was uploaded with: that decision, held to the registry. *)
let settled ?(timeout = 20.0) t resource_id =
  let checksum =
    match Hashtbl.find_opt t.contents resource_id with
    | Some content -> sha256 content
    | None -> failwith (resource_id ^ " is not a resource this world uploaded")
  in
  Run.eventually
    (Printf.sprintf "smelt:settled of %s at %s" resource_id checksum) timeout
    (fun () ->
      List.find_opt
        (fun payload ->
          Protocol.against_channel "smelt:settled" payload = None
          && Protocol.at payload [ "contentChecksum" ] = Some (`String checksum))
        (signalled t "smelt:settled" resource_id))

(* Wait until each resource is woven and settled. *)
let ready t resource_ids =
  woven t resource_ids;
  List.iter (fun id -> ignore (settled t id : Yojson.Safe.t)) resource_ids

(* ── What a Librarian is asked ──────────────────────────────────────── *)

type reply = Answered of Yojson.Safe.t | Failed of Yojson.Safe.t

(* Ask one of the Librarian's operations, as a person who is signed in, and
   read the one reply, held to what the registry says its channel carries. *)
let ask ?(as_ = person_sub) ?(timeout = 30.0) t operation payload =
  if
    not
      (String.starts_with ~prefix:"match:" operation
      || String.starts_with ~prefix:"gather:" operation)
  then
    failwith
      (operation
     ^ " is no operation a Librarian answers: the registry's match: and \
        gather: operations are");
  let answer =
    Archivist_world.request ~timeout (person t as_) operation payload
  in
  held answer.channel answer.payload;
  if answer.ok then Answered answer.payload else Failed answer.payload

let answered ?as_ ?timeout t operation payload =
  match ask ?as_ ?timeout t operation payload with
  | Answered payload -> payload
  | Failed failure ->
      failwith
        (Printf.sprintf "%s failed, and should have been answered: %s" operation
           (Yojson.Safe.to_string failure))

let failed ?as_ ?timeout t operation payload =
  match ask ?as_ ?timeout t operation payload with
  | Failed failure -> failure
  | Answered payload ->
      failwith
        (Printf.sprintf "%s was answered, and should have failed: %s" operation
           (Call.shown (Yojson.Safe.to_string payload)))

(* ── The world ──────────────────────────────────────────────────────── *)

(* The world's parts, nothing started yet but its Archivist world and its
   stand-ins. *)
let assemble (implementations : Implementations.t) command =
  let stores = stores () in
  let weaver_service = python "Weaver" implementations.weavers in
  let smelter_command = python "Smelter" implementations.smelters in
  let catalogue =
    match implementations.model_catalogue with
    | Some file -> file
    | None ->
        failwith
          "a Librarian's document names a model catalogue, and the \
           implementations file names none (modelCatalogue)"
  in
  let secret () = Fresh.hex 24 in
  let librarian_secret = secret ()
  and weaver_secret = secret ()
  and smelter_secret = secret () in
  let service = [ Protocol.service_role () ] in
  let kb =
    Archivist_world.create ~gateway:implementations.gateway
      ~command:implementations.archivist
      ~accounts:
        [
          ( librarian_client,
            { Issuer.secret = librarian_secret; roles = service } );
          (weaver_client, { Issuer.secret = weaver_secret; roles = service });
          (smelter_client, { Issuer.secret = smelter_secret; roles = service });
        ]
  in
  let matcher = Ollama.start () and gatherer = Ollama.start () in
  let embedder = Ollama_embed.start ()
  and smelter_embedder = Ollama_embed.start () in
  (* A standing stream of what the Weaver says it has applied and the
     Smelter says it has decided, open before either runs. *)
  let token, _ = World.agent kb.world "librarian-watch" in
  let signals =
    World.subscribe ~standing:true kb.world token
      (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ())
         ~global:[ "weave:applied"; "smelt:settled" ]
         ())
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
  let graph =
    `Assoc
      [
        ("type", `String "neo4j");
        ( "uri",
          `String
            (Printf.sprintf "bolt://%s:%d" stores.graph_host Neo4j.bolt_port) );
        ("username", `String "neo4j");
        ("database", `String (Neo4j.database_of kb.world.kb.did));
        ("passwordEnv", `String graph_password);
      ]
  in
  let served = World.served kb.world in
  let gateway_url = `String (World.origin kb.world) in
  let identity = `Assoc [ ("issuer", `String kb.world.issuer.origin) ] in
  let archivist =
    `Assoc [ ("host", `String "127.0.0.1"); ("port", `Int kb.port) ]
  in
  let did = kb.world.kb.did in
  let vectors =
    `Assoc
      [
        ("type", `String "qdrant");
        ("host", `String stores.vectors.host);
        ("port", `Int Qdrant.port);
        ("tokenEnv", `String Qdrant.token_variable);
      ]
  in
  let ollama_engine model origin =
    `Assoc
      [
        ( "engine",
          `Assoc [ ("provider", `String "ollama"); ("model", `String model) ] );
        ("baseUrl", `String origin);
      ]
  in
  let embedding origin =
    `Assoc
      [
        ("type", `String "ollama");
        ("model", `String embedding_model);
        ("baseUrl", `String origin);
      ]
  in
  let logging =
    [ ("logLevel", `String "info"); ("logFormat", `String "simple") ]
  in
  let port = Net.free_port () in
  let document ~settle_ms =
    `Assoc
      ([
         ("kb", served);
         ("gatewayUrl", gateway_url);
         ("identity", identity);
         ("archivist", archivist);
         ("graph", graph);
         ("vectors", vectors);
         ("embedding", embedding embedder.origin);
         ("matcher", ollama_engine matcher_model matcher.origin);
         ("gatherer", ollama_engine gatherer_model gatherer.origin);
         ("modelCatalogue", `String catalogue);
         ("gather", `Assoc [ ("settleTimeoutMs", `Int settle_ms) ]);
         ("search", `Assoc [ ("semanticFloor", `Float semantic_floor) ]);
         ("port", `Int port);
       ]
      @ logging)
  in
  let minted service =
    Qdrant.minted stores.vectors.key (Qdrant.claims did service)
  in
  let home prefix = Filename.temp_dir prefix "" in
  let t =
    {
      kb;
      command;
      document;
      base =
        {
          settle_ms = settle_timeout_ms;
          env =
            [
              ("XYNOLOG_OIDC_CLIENT_ID", librarian_client);
              ("XYNOLOG_OIDC_CLIENT_SECRET", librarian_secret);
              (graph_password, stores.graph_password_value);
              (Qdrant.token_variable, minted "librarian");
            ];
        };
      home = home "conformance-librarian-";
      port;
      matcher;
      gatherer;
      embedder;
      smelter_embedder;
      weaver =
        {
          weaver = weaver_service;
          weaver_document =
            (fun cursors ->
              `Assoc
                ([
                   ("kb", served);
                   ("gatewayUrl", gateway_url);
                   ("identity", identity);
                   ("archivist", archivist);
                   ("graph", graph);
                   ("cursorsPath", cursors);
                   ("port", `Int weaver_service.health_port);
                 ]
                @ logging));
          weaver_env =
            [
              ("XYNOLOG_OIDC_CLIENT_ID", weaver_client);
              ("XYNOLOG_OIDC_CLIENT_SECRET", weaver_secret);
              (graph_password, stores.graph_password_value);
            ];
          weaver_home = home "conformance-librarian-weaver-";
        };
      smelter =
        {
          smelter_command;
          smelter_document =
            (fun cursors ->
              `Assoc
                ([
                   ("kb", served);
                   ("gatewayUrl", gateway_url);
                   ("identity", identity);
                   ("archivist", archivist);
                   ("embedding", embedding smelter_embedder.origin);
                   ("vectors", vectors);
                   ( "chunking",
                     `Assoc [ ("chunkSize", `Int 512); ("overlap", `Int 64) ] );
                   ("pdfsAtOnce", `Int 1);
                   ("cursorsPath", cursors);
                   ("port", `Int Services.smelter_port);
                 ]
                @ logging));
          smelter_env =
            [
              ("XYNOLOG_OIDC_CLIENT_ID", smelter_client);
              ("XYNOLOG_OIDC_CLIENT_SECRET", smelter_secret);
              (Qdrant.token_variable, minted "smelter");
            ];
          smelter_home = home "conformance-librarian-smelter-";
        };
      index = Qdrant.index stores.vectors did;
      graph =
        Neo4j.connect ~host:stores.graph_host
          ~password:stores.graph_password_value
          ~database:(Neo4j.database_of did);
      signals;
      librarian = None;
      weaver_process = None;
      smelter_process = None;
      last_output = [];
      last_weaver_output = [];
      last_smelter_output = [];
      people = Hashtbl.create 8;
      contents = Hashtbl.create 64;
    }
  in
  t

(* Every violation since the last call: carried by the streams the case
   opened, which are then closed, or asked of a stand-in. What the case
   scripted of the stand-ins, and what they recorded, is forgotten. *)
let drain t =
  Hashtbl.reset t.people;
  let of_standins =
    List.concat_map Dynarray.to_list
      [
        t.matcher.violations;
        t.gatherer.violations;
        t.embedder.violations;
        t.smelter_embedder.violations;
      ]
  in
  let signalled = Dynarray.to_list t.signals.violations in
  Dynarray.clear t.signals.violations;
  Ollama.reset t.matcher;
  Ollama.reset t.gatherer;
  Ollama_embed.reset t.embedder;
  Ollama_embed.reset t.smelter_embedder;
  Archivist_world.drain t.kb @ signalled @ of_standins

(* Stop the three services, leave both stores empty for the next file, and
   close everything the world made. *)
let close t =
  Fun.protect
    ~finally:(fun () ->
      Neo4j.close t.graph;
      Ollama.close t.matcher;
      Ollama.close t.gatherer;
      Ollama_embed.close t.embedder;
      Ollama_embed.close t.smelter_embedder;
      Archivist_world.close t.kb;
      List.iter Gateway.remove
        [ t.home; t.weaver.weaver_home; t.smelter.smelter_home ])
    (fun () ->
      if t.librarian <> None then ignore (stop_librarian t : int option);
      if t.smelter_process <> None then ignore (stop_smelter t : int option);
      if t.weaver_process <> None then ignore (stop_weaver t : int option);
      clear_stores t)

(* A world, its stores emptied before either writer runs, and the writers
   started before the reader. What was started is stopped when a start
   fails, and the failure reported is the first one. *)
let create implementations command =
  let t = assemble implementations command in
  match
    clear_stores t;
    start_weaver t;
    start_smelter t;
    start_librarian t
  with
  | () -> t
  | exception failure ->
      let trace = Printexc.get_raw_backtrace () in
      (try Eio.Cancel.protect (fun () -> close t) with _ -> ());
      Printexc.raise_with_backtrace failure trace

(* The end of each case: the streams it opened must have carried nothing
   the spec does not allow, and no stand-in must have been asked what it
   does not serve or what no case scripted. *)
let end_case world body =
  let t = world () in
  World.end_case
    ~account:(fun () ->
      let tail n lines =
        List.filteri (fun at _ -> at >= List.length lines - n) lines
      in
      String.concat "\n"
        [
          "Librarian output:\n" ^ String.concat "\n" (output t);
          "Weaver output:\n" ^ String.concat "\n" (tail 40 (weaver_output t));
          "Smelter output:\n" ^ String.concat "\n" (tail 40 (smelter_output t));
          "Archivist output:\n"
          ^ String.concat "\n" (Process.tail 40 t.kb.archivist);
        ])
    ~settle:(fun () -> drain t)
    (fun () -> body t)
