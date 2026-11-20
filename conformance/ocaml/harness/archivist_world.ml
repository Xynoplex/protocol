(* An Archivist world: an Archivist process and everything it meets — a
   knowledge base's working tree, made fresh; a state volume and an
   anchored-text store beside it; the trusted issuer, which admits the
   Archivist's service account; and a gateway on the in-process plane, the
   Archivist's only route to the bus.

   Partly ported: no git checkout (no case ported here syncs git), and none
   of the staging, restart or Smelter-facing helpers. *)

let archivist_client = "xynolog-archivist"

(* A service account for participants the suite plays. *)
let participant_client = "conformance-participant"

type dirs = { root : string; state_home : string; anchored_text_dir : string }

type t = {
  world : World.t;
  base : string;
  dirs : dirs;
  env : (string * string) list;
  command : string list;
  document : Yojson.Safe.t;
  port : int;
  archivist : Process.t;
  responses : string Dynarray.t;
}

type answer = {
  ok : bool;
  channel : string;
  payload : Yojson.Safe.t;
  response : Yojson.Safe.t;
}

type client = {
  kb : t;
  token : string;
  did : string;
  client_id : string;
  stream : Bus_stream.t;
}

(* ── The files it keeps ────────────────────────────────────────────── *)

(* The two-level shard of a key, as specs/src/archivist/shard-cases.json
   states the arithmetic, over the key's UTF-16 code units; held to the
   table's cases when first used. *)
let utf16_units key =
  let units = ref [] in
  let rec decode at =
    if at < String.length key then (
      let decoded = String.get_utf_8_uchar key at in
      let code = Uchar.to_int (Uchar.utf_decode_uchar decoded) in
      (if code < 0x10000 then units := code :: !units
       else
         let above = code - 0x10000 in
         units :=
           (0xDC00 lor (above land 0x3FF))
           :: (0xD800 lor (above lsr 10))
           :: !units);
      decode (at + Uchar.utf_decode_length decoded))
  in
  decode 0;
  List.rev !units

let shard_arithmetic key =
  let hash = ref 0l in
  List.iter
    (fun unit -> hash := Int32.add (Int32.mul !hash 31l) (Int32.of_int unit))
    (utf16_units key);
  let hex = Printf.sprintf "%04x" (abs (Int32.to_int !hash) mod 65536) in
  String.sub hex 0 2 ^ "/" ^ String.sub hex 2 2

let shards_checked =
  lazy
    (List.iter
       (fun (c : Xynolog_protocol.Tables.Archivist_shard_cases.cases_item) ->
         if shard_arithmetic c.key <> c.shard then
           failwith
             (Printf.sprintf
                "archivist/shard-cases.json: \"%s\": the arithmetic gives %s, \
                 the case says %s"
                c.why (shard_arithmetic c.key) c.shard))
       (Protocol.tables ()).archivist_shard_cases.cases)

let shard_of key =
  Lazy.force shards_checked;
  shard_arithmetic key

let ( // ) = Filename.concat

(* Where the Archivist keeps the record on disk (archivist/layout.json). *)
let layout () = (Protocol.tables ()).archivist_layout

module Layout = Xynolog_protocol.Tables.Archivist_layout

(* The file or directory at [path], a template of layout.json's, in [tree],
   for the resource [resource_id]. *)
let placed t (tree : Layout.tree) path resource_id =
  let filled =
    List.fold_left
      (fun text (hole, value) ->
        Re.replace_string (Re.compile (Re.str hole)) ~by:value text)
      path
      [ ("{shard}", shard_of resource_id); ("{resourceId}", resource_id) ]
  in
  List.fold_left ( // )
    (match tree with Root -> t.dirs.root | State_home -> t.dirs.state_home)
    (String.split_on_char '/' filled)

let stream_dir t resource_id =
  let stream = (layout ()).event_stream in
  placed t stream.tree stream.path resource_id

(* The name of a stream's event file [number]. *)
let event_file_name number =
  let file = (layout ()).event_file in
  Re.replace_string
    (Re.compile (Re.str "{number}"))
    ~by:(Printf.sprintf "%0*d" file.digits number)
    file.name

(* A stream's events, oldest first: every line of its files, in order. Its
   files are the layout's, numbered from the first with none missing, none
   over the layout's bound, and every line a stored event. *)
let stored t resource_id =
  let dir = stream_dir t resource_id in
  if not (Sys.file_exists dir) then []
  else
    let layout = layout () in
    let files = List.sort compare (Array.to_list (Sys.readdir dir)) in
    let expected =
      List.mapi
        (fun at _ -> event_file_name (layout.event_file.first + at))
        files
    in
    if files <> expected then
      failwith
        (Printf.sprintf
           "%s holds %s, and archivist/layout.json says a stream's files are %s"
           dir (String.concat ", " files)
           (String.concat ", " expected));
    List.concat_map
      (fun file ->
        let lines =
          List.filter
            (fun line -> line <> "")
            (String.split_on_char '\n'
               (In_channel.with_open_bin (dir // file) In_channel.input_all))
        in
        if List.length lines > layout.event_file.max_events then
          failwith
            (Printf.sprintf
               "%s holds %d events, and archivist/layout.json says a file \
                holds at most %d"
               (dir // file) (List.length lines) layout.event_file.max_events);
        List.map
          (fun line ->
            let event = Yojson.Safe.from_string line in
            (match Protocol.refusals layout.stored_event event with
            | [] -> ()
            | refusals ->
                failwith
                  (Printf.sprintf
                     "%s holds a line that is no stored event (%s): %s"
                     (dir // file)
                     (String.concat "; " refusals)
                     line));
            event)
          lines)
      files

(* A resource's view file, held to the schema the layout names. *)
let view t resource_id =
  let view = (layout ()).view in
  let path = placed t view.tree view.path resource_id in
  if not (Sys.file_exists path) then None
  else
    let json = Yojson.Safe.from_file path in
    match Protocol.refusals view.schema json with
    | [] -> Some json
    | refusals ->
        failwith
          (Printf.sprintf
             "%s is not what archivist/layout.json says a view is (%s)" path
             (String.concat "; " refusals))

(* A projection of what the __system__ stream adds up to, by its file's
   name, held to the schema the layout names: None while it is absent. *)
let projection t name =
  let projections = (layout ()).system_projections in
  let file =
    match
      List.find_opt
        (fun (file : Layout.files_item) -> file.name = name)
        projections.files
    with
    | Some file -> file
    | None -> failwith ("archivist/layout.json names no projection " ^ name)
  in
  let path = placed t projections.tree projections.path "" // name in
  if not (Sys.file_exists path) then None
  else
    let json = Yojson.Safe.from_file path in
    match Protocol.refusals file.schema json with
    | [] -> Some json
    | refusals ->
        failwith
          (Printf.sprintf
             "%s is not what archivist/layout.json says the projection is (%s)"
             path
             (String.concat "; " refusals))

(* ── Participants ───────────────────────────────────────────────────── *)

let uniq names =
  List.rev
    (List.fold_left
       (fun seen n -> if List.mem n seen then seen else n :: seen)
       [] names)

(* Holds every frame a stream carries to what the registry says its channel
   carries. *)
let hold_to_registry (stream : Bus_stream.t) =
  Bus_stream.on stream (fun message ->
      match message.frame with
      | None -> ()
      | Some frame -> (
          let short json = Call.shown (Yojson.Safe.to_string json) in
          match Protocol.carried frame.channel with
          | None ->
              Bus_stream.violation stream
                (Printf.sprintf
                   "a frame on %s, which the registry does not declare"
                   frame.channel)
          | Some Stored_event -> (
              match Protocol.against "StoredEventResponse" frame.payload with
              | [] ->
                  if
                    Protocol.at frame.payload [ "type" ]
                    <> Some (`String frame.channel)
                  then
                    Bus_stream.violation stream
                      (Printf.sprintf "%s carried an event of another type"
                         frame.channel)
              | refusals ->
                  Bus_stream.violation stream
                    (Printf.sprintf
                       "%s carried a payload that is not a stored event (%s): \
                        %s"
                       frame.channel
                       (String.concat "; " refusals)
                       (short frame.payload)))
          | Some (Schema schema) -> (
              match
                Protocol.against schema (Protocol.unstamped frame.payload)
              with
              | [] -> ()
              | refusals ->
                  Bus_stream.violation stream
                    (Printf.sprintf
                       "%s carried a payload that is not a %s (%s): %s"
                       frame.channel schema
                       (String.concat "; " refusals)
                       (short frame.payload)))
          | Some (Envelope _ | Nothing) -> ()))

let client t token did channels =
  let replies =
    List.concat_map
      (fun (op : Protocol.Registry.operations_item) ->
        [ op.result; op.failure ])
      (Protocol.tables ()).bus_registry.operations
  in
  let client_id = Fresh.uuid () in
  let stream =
    World.subscribe t.world token
      (Bus_stream.subscribe_body ~client_id
         ~global:(uniq (replies @ Protocol.facts () @ channels))
         ())
  in
  hold_to_registry stream;
  { kb = t; token; did; client_id; stream }

let person t sub =
  client t (World.person t.world sub) (World.person_did t.world sub) []

let sidecar t model =
  let token, did = World.agent t.world model in
  client t token did []

(* Emit a frame, uncorrelated unless given a correlationId; the gateway must
   accept it. *)
let emit c ?correlation_id channel payload =
  let reply =
    World.emit c.kb.world c.token
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
         reply.text)

(* Emit [operation] with a fresh correlationId and read its one reply. *)
let request ?(timeout = 20.0) c operation payload =
  let op = Protocol.operation_for operation in
  let correlation_id = Fresh.uuid () in
  let from = Dynarray.length c.stream.messages in
  emit c ~correlation_id operation payload;
  let message =
    Bus_stream.next ~timeout ~from c.stream ("a reply to " ^ operation)
      (fun m ->
        match m.frame with
        | Some f ->
            f.correlation_id = Some correlation_id
            && (f.channel = op.result || f.channel = op.failure)
        | None -> false)
  in
  match message.frame with
  | Some frame ->
      {
        ok = frame.channel = op.result;
        channel = frame.channel;
        payload = Protocol.unstamped frame.payload;
        response =
          (match Protocol.at frame.payload [ "response" ] with
          | Some (`Assoc _ as response) -> response
          | _ -> `Assoc []);
      }
  | None -> assert false

(* A request the Archivist must answer on the result channel: its response. *)
let ask c operation payload =
  let answer = request c operation payload in
  if not answer.ok then
    failwith
      (Printf.sprintf "%s was refused: %s" operation
         (Yojson.Safe.to_string answer.payload));
  answer.response

(* ── The HTTP surface ───────────────────────────────────────────────── *)

let origin t = Printf.sprintf "http://127.0.0.1:%d" t.port

let http t ?route ?json ?body ?(headers = []) meth path =
  let token = Issuer.service t.world.issuer participant_client in
  let reply = Call.call ~token ?json ?body ~headers (origin t) meth path in
  Option.iter
    (fun route ->
      List.iter
        (fun v ->
          Dynarray.add_last t.responses (Printf.sprintf "%s %s: %s" meth path v))
        (Call.non_conformance Archivist meth route reply))
    route;
  reply

let multipart fields file =
  let boundary = "conformance-" ^ Fresh.hex 12 in
  let part name extra value =
    Printf.sprintf
      "--%s\r\nContent-Disposition: form-data; name=\"%s\"%s\r\n\r\n%s\r\n"
      boundary name extra value
  in
  ( "multipart/form-data; boundary=" ^ boundary,
    String.concat ""
      (List.map (fun (name, value) -> part name "" value) fields
      @ [
          part "file"
            "; filename=\"upload\"\r\nContent-Type: application/octet-stream"
            file;
          "--" ^ boundary ^ "--\r\n";
        ]) )

(* Upload content as [principal] and have it recorded: POST /resources, as
   the gateway sends a person's upload on, under a new Idempotency-Key. The
   new resource's id. *)
let created t principal ~name ~storage_uri ~format content =
  let kind, body =
    multipart
      [ ("name", name); ("storageUri", storage_uri); ("format", format) ]
      content
  in
  let reply =
    http t ~route:"/resources" ~body
      ~headers:
        [
          ("content-type", kind);
          ("Xynolog-Principal", principal);
          ("Idempotency-Key", Fresh.uuid ());
        ]
      "POST" "/resources"
  in
  if reply.status <> 200 then
    failwith
      (Printf.sprintf "the upload was not recorded: %d %s" reply.status
         reply.text);
  Yojson.Safe.Util.(member "resourceId" (Option.get reply.json) |> to_string)

(* A resource's anchored text, read as the gateway reads it; the Archivist
   must answer it. *)
let anchored_text t resource_id =
  let reply =
    http t ~route:"/resources/{id}/anchored-text" "GET"
      ("/resources/" ^ Protocol.uri_component resource_id ^ "/anchored-text")
  in
  if reply.status <> 200 then
    failwith
      (Printf.sprintf "the anchored text was not answered: %d %s" reply.status
         reply.text);
  Option.get reply.json

(* ── The process ────────────────────────────────────────────────────── *)

let launch ~base ~document ~command ~env ~port =
  let path = base // "archivist.json" in
  Gateway.write_json path document;
  let process =
    Process.spawn "Archivist"
      (command @ [ "--config"; path ])
      (Process.run_environment () @ env)
  in
  Process.until_ready process ~timeout:60.0 "serve" (fun () ->
      match
        Call.reach
          (Printf.sprintf "http://127.0.0.1:%d" port)
          "GET"
          (Protocol.path Archivist "GET" "/health")
      with
      | Some r -> r.status = 200
      | None -> false);
  process

(* Wait until the Archivist answers on the bus: its health says it has
   booted, not that its stream to the gateway is open. *)
let answering t =
  let probe = sidecar t "readiness" in
  Run.eventually "the Archivist to answer on the bus" 30.0 (fun () ->
      match request ~timeout:5.0 probe "browse:kb-requested" (`Assoc []) with
      | answer
        when Protocol.at answer.payload [ "code" ]
             = Some (`String "peer-unavailable") ->
          None
      | _ -> Some ()
      | exception (Run.Timed_out _ | Failure _) -> None)

let create ~gateway ~command ~accounts =
  let secret = Fresh.hex 24 in
  let world =
    World.create ~command:gateway
      ~accounts:
        (( archivist_client,
           { Issuer.secret; roles = [ Protocol.service_role () ] } )
        :: accounts)
      In_process
  in
  let base = Filename.temp_dir "conformance-kb-" "" in
  let dirs =
    {
      root = base // "kb";
      state_home = base // "state";
      anchored_text_dir = base // "anchored-text";
    }
  in
  List.iter
    (fun dir -> Sys.mkdir dir 0o755)
    [
      dirs.root;
      dirs.root // ".xynolog";
      dirs.state_home;
      dirs.anchored_text_dir;
    ];
  let quoted text = Yojson.Safe.to_string (`String text) in
  Out_channel.with_open_bin
    (dirs.root // ".xynolog" // "config.yaml")
    (fun out ->
      Printf.fprintf out
        "project:\n\
        \  name: %s\n\
         git:\n\
        \  sync: false\n\
         site:\n\
        \  domain: %s\n\
        \  siteName: %s\n"
        (quoted world.kb.name) (quoted world.kb.domain) (quoted world.kb.name));
  let port = Net.free_port () in
  let document =
    `Assoc
      [
        ("gatewayUrl", `String (World.origin world));
        ("identity", `Assoc [ ("issuer", `String world.issuer.origin) ]);
        ("root", `String dirs.root);
        ("stateHome", `String dirs.state_home);
        ("anchoredTextDir", `String dirs.anchored_text_dir);
        ("engines", `Assoc [ ("jobs", `Assoc []); ("actors", `Assoc []) ]);
        ("port", `Int port);
        ("skipRebuild", `Bool false);
        ("staging", `Assoc [ ("flushMs", `Int 250); ("maxWaitMs", `Int 2_000) ]);
        ("logLevel", `String "info");
        ("logFormat", `String "json");
      ]
  in
  Gateway.held_to "ArchivistConfig" document;
  let env =
    [
      ("XYNOLOG_OIDC_CLIENT_ID", archivist_client);
      ("XYNOLOG_OIDC_CLIENT_SECRET", secret);
    ]
  in
  Gateway.check_environment Archivist ~named:[] env;
  let archivist = launch ~base ~document ~command ~env ~port in
  let t =
    {
      world;
      base;
      dirs;
      env;
      command;
      document;
      port;
      archivist;
      responses = Dynarray.create ();
    }
  in
  answering t;
  t

let drain t =
  let responses = Dynarray.to_list t.responses in
  Dynarray.clear t.responses;
  World.drain t.world @ responses

let close t =
  ignore (Process.stop t.archivist : int option);
  World.close t.world;
  Gateway.remove t.base
