(* An Archivist world: an Archivist process and everything it meets — a
   knowledge base's working tree, made fresh and, unless a file says
   otherwise, a git checkout; a state volume and an anchored-text store
   beside it; the trusted issuer, which admits the Archivist's service
   account; and a gateway on the in-process plane, the Archivist's only
   route to the bus.

   Cases meet the Archivist where the rest of Xynolog does: on the bus, as
   people, workers and sidecars emitting through the gateway and reading
   replies and facts off their streams; at its HTTP surface, as a service;
   and in the files it keeps, which other processes read. Every frame a
   stream carries must be what the registry says its channel carries, every
   response what the Archivist's API says, and every file what
   archivist/layout.json says it is. *)

let archivist_client = "xynolog-archivist"

(* A service account for participants the suite plays. *)
let participant_client = "conformance-participant"

type dirs = { root : string; state_home : string; anchored_text_dir : string }

type t = {
  world : World.t;
  base : string;
  dirs : dirs;
  mutable archivist : Archivist.t;
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

let ( // ) = Filename.concat

(* The document the Archivist now runs on, and what it was started with. *)
let settings t = t.archivist.launch.document
let env t = t.archivist.launch.env
let port t = Option.get (Archivist.port_of (settings t))
let output t = Archivist.output t.archivist

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

let sha256 bytes = Digestif.SHA256.(to_hex (digest_string bytes))

(* Where the Archivist keeps the record on disk (archivist/layout.json). *)
let layout () = (Protocol.tables ()).archivist_layout

module Layout = Xynolog_protocol.Tables.Archivist_layout

(* The file or directory at [path], a template of layout.json's, in [tree],
   each of [fill]'s holes filled, and [{shard}] with the shard of [key]. *)
let placed t (tree : Layout.tree) path ?key fill =
  let fill =
    match key with
    | Some key -> ("{shard}", shard_of key) :: fill
    | None -> fill
  in
  let filled =
    List.fold_left
      (fun text (hole, value) ->
        Re.replace_string (Re.compile (Re.str hole)) ~by:value text)
      path fill
  in
  List.fold_left ( // )
    (match tree with
    | Root -> t.dirs.root
    | State_home -> t.dirs.state_home
    | Anchored_text_dir -> t.dirs.anchored_text_dir)
    (String.split_on_char '/' filled)

let system = "__system__"

(* A resource's stream directory, or the __system__ stream's. *)
let stream_dir t resource_id =
  if resource_id = system then
    let stream = (layout ()).system_stream in
    placed t stream.tree stream.path []
  else
    let stream = (layout ()).event_stream in
    placed t stream.tree stream.path ~key:resource_id
      [ ("{resourceId}", resource_id) ]

(* The name of a stream's event file [number]. *)
let event_file_name number =
  let file = (layout ()).event_file in
  Re.replace_string
    (Re.compile (Re.str "{number}"))
    ~by:(Printf.sprintf "%0*d" file.digits number)
    file.name

(* The name of a stream's first event file. *)
let first_event_file () = event_file_name (layout ()).event_file.first

(* A stream's files, in order: the layout's, numbered from the first with
   none missing. *)
let stream_files t resource_id =
  let dir = stream_dir t resource_id in
  if not (Sys.file_exists dir) then []
  else
    let files = List.sort compare (Array.to_list (Sys.readdir dir)) in
    let expected =
      List.mapi
        (fun at _ -> event_file_name ((layout ()).event_file.first + at))
        files
    in
    if files <> expected then
      failwith
        (Printf.sprintf
           "%s holds %s, and archivist/layout.json says a stream's files are %s"
           dir (String.concat ", " files)
           (String.concat ", " expected));
    files

let read_file path = In_channel.with_open_bin path In_channel.input_all

(* Every line of a stream, as written, none parsed: each file ends with a
   newline, and holds at most the layout's bound. *)
let stream_lines t resource_id =
  let dir = stream_dir t resource_id in
  List.concat_map
    (fun file ->
      let text = read_file (dir // file) in
      if text <> "" && not (String.ends_with ~suffix:"\n" text) then
        failwith
          (Printf.sprintf "%s of %s does not end with a newline" file
             resource_id);
      let lines =
        match List.rev (String.split_on_char '\n' text) with
        | "" :: rest -> List.rev rest
        | all -> List.rev all
      in
      let bound = (layout ()).event_file.max_events in
      if List.length lines > bound then
        failwith
          (Printf.sprintf
             "%s holds %d events, and archivist/layout.json says a file holds \
              at most %d"
             (dir // file) (List.length lines) bound);
      lines)
    (stream_files t resource_id)

(* A stream's events, oldest first, each line held to the stored event the
   layout names. *)
let stored t resource_id =
  List.map
    (fun line ->
      let event = Yojson.Safe.from_string line in
      (match Protocol.refusals (layout ()).stored_event event with
      | [] -> ()
      | refusals ->
          failwith
            (Printf.sprintf
               "the stream of %s holds a line that is no stored event (%s): %s"
               resource_id
               (String.concat "; " refusals)
               line));
      event)
    (stream_lines t resource_id)

(* The JSON file at [path], held to [schema]: None while it is absent. *)
let document_at path schema what =
  if not (Sys.file_exists path) then None
  else
    let json = Yojson.Safe.from_file path in
    match Protocol.refusals schema json with
    | [] -> Some json
    | refusals ->
        failwith
          (Printf.sprintf "%s is not what archivist/layout.json says %s is (%s)"
             path what
             (String.concat "; " refusals))

let view_path t resource_id =
  let view = (layout ()).view in
  placed t view.tree view.path ~key:resource_id
    [ ("{resourceId}", resource_id) ]

(* A resource's view file, held to the schema the layout names. *)
let view t resource_id =
  document_at (view_path t resource_id) (layout ()).view.schema "a view"

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
  document_at
    (placed t projections.tree projections.path [] // name)
    file.schema ("the projection " ^ name)

let storage_uri_path t uri =
  let index = (layout ()).storage_uri_index in
  placed t index.tree index.path ~key:uri [ ("{uriHash}", sha256 uri) ]

(* The storage-uri index's entry for [uri], held to its schema. *)
let storage_uri_entry t uri =
  document_at (storage_uri_path t uri) (layout ()).storage_uri_index.schema
    "a storage-uri entry"

(* Where the anchored-text store keeps the entry for content of [checksum]. *)
let anchored_text_entry_path t checksum =
  let entry = (layout ()).anchored_text_entry in
  placed t entry.tree entry.path ~key:checksum
    [ ("{contentChecksum}", checksum) ]

(* Every file under [dir], by its path from there, with its bytes: none
   while [dir] is absent. *)
let files_under dir =
  let rec walk dir from =
    if not (Sys.file_exists dir) then []
    else
      List.concat_map
        (fun entry ->
          let path = dir // entry in
          let name = if from = "" then entry else from ^ "/" ^ entry in
          if Sys.is_directory path then walk path name
          else [ (name, read_file path) ])
        (List.sort compare (Array.to_list (Sys.readdir dir)))
  in
  walk dir ""

(* The file a storage URI names in the working tree. *)
let content_path t storage_uri =
  let scheme = "file://" in
  if not (String.starts_with ~prefix:scheme storage_uri) then
    failwith (storage_uri ^ " is no storage URI");
  t.dirs.root
  // String.sub storage_uri (String.length scheme)
       (String.length storage_uri - String.length scheme)

(* A path of the working tree, as git names it: from the root. *)
let in_tree t path =
  let root = t.dirs.root ^ "/" in
  if not (String.starts_with ~prefix:root path) then
    failwith (path ^ " is not in the working tree");
  String.sub path (String.length root) (String.length path - String.length root)

let rec make_dirs dir =
  if not (Sys.file_exists dir) then (
    make_dirs (Filename.dirname dir);
    Sys.mkdir dir 0o755)

(* Write [content] to the file at [path], making the directories it is
   in. *)
let write_file path content =
  make_dirs (Filename.dirname path);
  Out_channel.with_open_bin path (fun out -> output_string out content)

(* Put a file in the working tree, as a person editing the tree would. *)
let write t storage_uri content =
  write_file (content_path t storage_uri) content

(* ── git ────────────────────────────────────────────────────────────── *)

(* git in [root]: what it printed, or what it said on failing. *)
let git_in root args =
  let out = Buffer.create 256 and err = Buffer.create 256 in
  match
    Eio.Process.run (Run.process_mgr ())
      ~cwd:Eio.Path.(Eio.Stdenv.fs (Run.env ()) / root)
      ~stdout:(Eio.Flow.buffer_sink out) ~stderr:(Eio.Flow.buffer_sink err)
      ("git" :: args)
  with
  | () -> Ok (Buffer.contents out)
  | exception Eio.Exn.Io _ -> Error (Buffer.contents err)

let git t args =
  match git_in t.dirs.root args with
  | Ok out -> out
  | Error err ->
      failwith (Printf.sprintf "git %s failed: %s" (String.concat " " args) err)

let nul_separated text =
  List.filter (fun path -> path <> "") (String.split_on_char '\000' text)

(* The paths staged in the index, relative to the root. *)
let staged t = nul_separated (git t [ "diff"; "--cached"; "--name-only"; "-z" ])

(* The paths in the index: staged, or committed and unchanged. *)
let indexed t = nul_separated (git t [ "ls-files"; "-z" ])

(* Every run of git the tripwire recorded, by its arguments. *)
let git_runs t =
  let record = t.base // "git-runs" in
  if not (Sys.file_exists record) then []
  else
    List.filter
      (fun l -> l <> "")
      (String.split_on_char '\n' (read_file record))

(* The staging bound of the document the Archivist runs on, in seconds. *)
let max_wait t =
  match Protocol.at (settings t) [ "staging"; "maxWaitMs" ] with
  | Some (`Int ms) -> float_of_int ms /. 1000.0
  | _ -> failwith "the Archivist's document states no staging.maxWaitMs"

(* Wait until the index holds [path], within the staging bound and a
   margin. *)
let until_indexed t path =
  Run.eventually (path ^ " to be staged")
    (max_wait t +. 5.0)
    (fun () -> if List.mem path (indexed t) then Some () else None)

(* How long one git command may hold the index: far longer than a batch of
   staging takes. *)
let index_held_at_most = 10.0

(* The person running git by hand: a name to commit under, which a runner
   has none of, and no key, so a commit is not signed whatever the
   machine's own git says. *)
let person_running_git =
  [
    "-c";
    "user.name=Conformance";
    "-c";
    "user.email=conformance@example.com";
    "-c";
    "commit.gpgsign=false";
  ]

(* git, run by hand in the working tree [root], beside an Archivist that stages
   into the same index: what it printed. While the Archivist stages a batch
   it holds git's lock on the index, and a command that finds the index
   locked is run again once the lock is gone, as the Archivist runs its own
   (ARCHIVIST.md § Staging). Any other failure fails at once. *)
let git_by_hand root args =
  Run.eventually "git to find the index unlocked" index_held_at_most (fun () ->
      match git_in root (person_running_git @ args) with
      | Ok out -> Some out
      | Error err
        when Re.execp (Re.compile (Re.str "index.lock': File exists")) err ->
          None
      | Error err ->
          failwith
            (Printf.sprintf "git %s failed: %s" (String.concat " " args) err))

(* Hold the index of the working tree [root] as a writer would, after any
   current writer has released it: the release. *)
let hold_git_index root =
  let lock = root // ".git" // "index.lock" in
  let fd =
    Run.eventually "the git index lock to be available" index_held_at_most
      (fun () ->
        match Unix.openfile lock [ O_WRONLY; O_CREAT; O_EXCL ] 0o644 with
        | fd -> Some fd
        | exception Unix.Unix_error (EEXIST, _, _) -> None)
  in
  fun () ->
    Unix.close fd;
    Sys.remove lock

(* ── Participants ───────────────────────────────────────────────────── *)

let uniq names =
  List.rev
    (List.fold_left
       (fun seen n -> if List.mem n seen then seen else n :: seen)
       [] names)

(* A participant on the bus, subscribed under its own client id to the
   replies of every operation, to every fact, to any other channel it
   names, and to the scoped facts of each resource in [scopes]. *)
let client t token did ~channels ~scopes =
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
         ~scoped:
           (List.map
              (fun scope -> Bus_stream.scope scope (Protocol.facts ()))
              scopes)
         ())
  in
  Bus_stream.hold_to_registry stream;
  { kb = t; token; did; client_id; stream }

(* A person, signed in at the issuer. *)
let person ?(channels = []) ?(scopes = []) t sub =
  client t (World.person t.world sub)
    (World.person_did t.world sub)
    ~channels ~scopes

(* The roles a worker holds. *)
let worker_roles () = [ Protocol.service_role (); Protocol.worker_role () ]

(* A worker: a software agent holding the worker role. *)
let worker ?(channels = []) ?(scopes = []) t model =
  let token, did = World.agent t.world ~roles:(worker_roles ()) model in
  client t token did ~channels ~scopes

(* A sidecar: a software agent holding the service role and not the worker
   role. *)
let sidecar ?(channels = []) ?(scopes = []) t model =
  let token, did = World.agent t.world model in
  client t token did ~channels ~scopes

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

(* A request the Archivist must refuse: the refusal's message. *)
let refused c operation payload =
  let answer = request c operation payload in
  if answer.ok then
    failwith
      (Printf.sprintf "%s was answered, and should have been refused: %s"
         operation
         (Yojson.Safe.to_string answer.payload));
  match Protocol.at answer.payload [ "message" ] with
  | Some (`String message) -> message
  | _ ->
      failwith
        (Printf.sprintf "the refusal of %s carries no message: %s" operation
           (Yojson.Safe.to_string answer.payload))

(* The facts [c]'s stream has carried with no scope, oldest first, of
   [type_] when given: each the stored event, unstamped. *)
let facts ?type_ c =
  let facts = Protocol.facts () in
  List.filter_map
    (fun (f : Bus_stream.frame) ->
      if
        f.scope = None && List.mem f.channel facts
        && match type_ with Some t -> f.channel = t | None -> true
      then Some (Protocol.unstamped f.payload)
      else None)
    (Bus_stream.frames c.stream)

(* Wait for an unscoped fact of [type_] that [matches] accepts. *)
let fact ?(timeout = 10.0) ?(matches = fun _ -> true) c type_ =
  Run.eventually
    ("a " ^ type_ ^ " fact")
    timeout
    (fun () -> List.find_opt matches (facts ~type_ c))

(* ── The HTTP surface ───────────────────────────────────────────────── *)

let origin t = t.archivist.origin

(* A service account's token: what the Archivist's HTTP surface admits. *)
let service_token ?(roles = [ Protocol.service_role () ]) t =
  Issuer.service t.world.issuer ~roles participant_client

(* A request of the HTTP surface, as a service unless [token] or
   [anonymous] says otherwise; with [route], held to the Archivist's
   API. *)
let http t ?(anonymous = false) ?token ?route ?json ?body ?(headers = []) meth
    path =
  let token =
    if anonymous then None
    else Some (match token with Some t -> t | None -> service_token t)
  in
  let reply = Call.call ?token ?json ?body ~headers (origin t) meth path in
  Option.iter
    (fun route ->
      List.iter
        (fun v ->
          Dynarray.add_last t.responses (Printf.sprintf "%s %s: %s" meth path v))
        (Call.non_conformance Archivist meth route reply))
    route;
  reply

(* Upload [content] as [principal]: POST /resources, as the gateway sends a
   person's upload on, with [fields] beside its format (text/markdown unless
   they name one), under [key], its Idempotency-Key: a new one for each
   upload, unless a case sends an upload again. The reply. *)
let upload t ?(roles = []) ?(key = Fresh.uuid ()) principal fields content =
  let fields =
    if List.mem_assoc "format" fields then fields
    else ("format", "text/markdown") :: fields
  in
  let kind, body =
    Call.multipart
      (List.map (fun (name, value) -> Call.Field (name, value)) fields
      @ [
          Call.File
            {
              name = "file";
              filename = "upload";
              media = "application/octet-stream";
              bytes = content;
            };
        ])
  in
  http t ~route:"/resources" ~body
    ~headers:
      ([
         ("content-type", kind);
         ("Xynolog-Principal", principal);
         ("Idempotency-Key", key);
       ]
      @
      if roles = [] then [] else [ ("Xynolog-Roles", String.concat "," roles) ]
      )
    "POST" "/resources"

let string_member name json =
  match Protocol.at json [ name ] with
  | Some (`String s) -> s
  | _ ->
      failwith
        (Printf.sprintf "%s holds no %s" (Yojson.Safe.to_string json) name)

(* The id of the resource an upload was answered with; the Archivist must
   have recorded it. *)
let recorded_id (reply : Call.reply) =
  match (reply.status, reply.json) with
  | 200, Some json -> string_member "resourceId" json
  | _ ->
      failwith
        (Printf.sprintf "the upload was not recorded: %d %s" reply.status
           reply.text)

(* Upload content the Archivist must record: the new resource's id. *)
let created t principal fields content =
  recorded_id (upload t principal fields content)

let resource_path id suffix = "/resources/" ^ Protocol.uri_component id ^ suffix

(* Every stream the log holds, as the Archivist lists them: each id, with
   the sequence number of its last event. *)
let streams t =
  let listed = http t ~route:"/events" "GET" "/events" in
  match (listed.status, listed.json) with
  | 200, Some json ->
      List.map
        (fun stream ->
          match Protocol.at stream [ "lastSequence" ] with
          | Some (`Int last) -> (string_member "resourceId" stream, last)
          | _ -> failwith (Yojson.Safe.to_string stream ^ " is no listed stream"))
        (match Protocol.at json [ "streams" ] with
        | Some (`List streams) -> streams
        | _ -> failwith (Yojson.Safe.to_string json ^ " lists no streams"))
  | _ ->
      failwith
        (Printf.sprintf "the streams were not listed: %d %s" listed.status
           listed.text)

(* A resource's anchored text, read as the gateway reads it; the Archivist
   must answer it. *)
let anchored_text t resource_id =
  let reply =
    http t ~route:"/resources/{id}/anchored-text" "GET"
      (resource_path resource_id "/anchored-text")
  in
  if reply.status <> 200 then
    failwith
      (Printf.sprintf "the anchored text was not answered: %d %s" reply.status
         reply.text);
  Option.get reply.json

(* A resource's annotations, as the Archivist answers them over HTTP; None
   for a resource it does not hold. *)
let annotations t resource_id =
  let reply =
    http t ~route:"/resources/{id}/annotations" "GET"
      (resource_path resource_id "/annotations")
  in
  match reply.status with
  | 404 -> None
  | 200 -> reply.json
  | status ->
      failwith
        (Printf.sprintf "the annotations were not answered: %d %s" status
           reply.text)

(* ── The Smelter ────────────────────────────────────────────────────── *)

(* State the stamp the anchored-text store's entries are taken under, as
   the Smelter does when it starts. *)
let state_stamp t stamp =
  let stated =
    http t ~route:"/anchored-text/stamp"
      ~json:(`Assoc [ ("stamp", `String stamp) ])
      "PUT" "/anchored-text/stamp"
  in
  if stated.status <> 204 then
    failwith
      (Printf.sprintf "the stamp was not stated: %d %s" stated.status
         stated.text)

(* Store an anchored-text entry for a resource's bytes, as the Smelter
   does: the reply. *)
let store_anchored_text ?token t resource_id write =
  http t ?token ~route:"/resources/{id}/anchored-text" ~json:write "PUT"
    (resource_path resource_id "/anchored-text")

(* The entry the store holds for a resource's content, read with
   [checksum], as the Smelter reads it back: the reply. *)
let stored_anchored_text ?token t resource_id checksum =
  let query =
    match checksum with Some c -> "?contentChecksum=" ^ c | None -> ""
  in
  http t ?token ~route:"/resources/{id}/anchored-text/stored" "GET"
    (resource_path resource_id "/anchored-text/stored" ^ query)

(* The keys of the store's entries under the stamp last stated, as the
   Smelter lists them, sorted. *)
let anchored_text_keys t =
  let listed =
    http t ~route:"/anchored-text/entries" "GET" "/anchored-text/entries"
  in
  if listed.status <> 200 then
    failwith
      (Printf.sprintf "the store's keys were not listed: %d %s" listed.status
         listed.text);
  match Protocol.at (Option.get listed.json) [ "checksums" ] with
  | Some (`List keys) ->
      List.sort compare
        (List.map (function `String k -> k | _ -> failwith "a key") keys)
  | _ -> failwith "the listing holds no checksums"

(* The entry format AnchoredTextExtractedEntry states. *)
let entry_format () =
  match
    Option.bind (Protocol.json_of "archivist/AnchoredTextExtractedEntry.json")
      (fun schema -> Protocol.at schema [ "properties"; "v"; "enum" ])
  with
  | Some (`List [ `Int v ]) -> v
  | _ -> failwith "AnchoredTextExtractedEntry states no one entry format"

(* [entry], an entry's own members, as the store takes it: in the entry
   format, under [stamp], and held to AnchoredTextEntry. *)
let stamped stamp entry =
  let document =
    match entry with
    | `Assoc members ->
        `Assoc
          (("v", `Int (entry_format ())) :: ("stamp", `String stamp) :: members)
    | _ -> failwith "an entry is an object"
  in
  match
    Protocol.refusals (Protocol.service_schema "AnchoredTextEntry") document
  with
  | [] -> document
  | refusals ->
      failwith
        (Printf.sprintf
           "the suite's anchored-text entry is not an AnchoredTextEntry (%s)"
           (String.concat "; " refusals))

(* A write of [document] to the store, for content of [checksum]. *)
let entry_write checksum document =
  `Assoc [ ("contentChecksum", `String checksum); ("entry", document) ]

type outcome = Indexed | Skipped

(* The Smelter, as far as the Archivist meets it: state a stamp, store
   [entry] under it for content of [checksum], and signal that the resource
   has settled. The stamp and the entry go through the Archivist's
   surface. *)
let smelt t ?entry ?(outcome = Indexed) ?(stamp = "conformance-smelter")
    resource_id checksum =
  Option.iter
    (fun entry ->
      state_stamp t stamp;
      let stored =
        store_anchored_text t resource_id
          (entry_write checksum (stamped stamp entry))
      in
      if stored.status <> 204 then
        failwith
          (Printf.sprintf "the anchored-text entry was not stored: %d %s"
             stored.status stored.text))
    entry;
  emit (sidecar t "smelter") "smelt:settled"
    (`Assoc
       [
         ("resourceId", `String resource_id);
         ("contentChecksum", `String checksum);
         ( "outcome",
           `String
             (match outcome with Indexed -> "indexed" | Skipped -> "skipped") );
       ])

(* ── The process ────────────────────────────────────────────────────── *)

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

(* Stop the Archivist and start it again on the same trees and, unless
   [change] says otherwise, the same document. *)
let restart ?(change = Fun.id) t =
  let launch = t.archivist.launch in
  ignore (Archivist.stop t.archivist : int option);
  t.archivist <-
    Archivist.start { launch with document = change launch.document };
  answering t

(* Kill the Archivist without its shutdown, and start another on the same
   trees. *)
let crash_and_restart t =
  let launch = t.archivist.launch in
  Archivist.crash t.archivist;
  t.archivist <- Archivist.start launch;
  answering t

(* The project file a world's tree commits, of [kb], syncing git or not. *)
let project_file (kb : Protocol.kb) ~git_sync =
  let quoted text = Yojson.Safe.to_string (`String text) in
  Printf.sprintf
    "project:\n\
    \  name: %s\n\
     git:\n\
    \  sync: %b\n\
     site:\n\
    \  domain: %s\n\
    \  siteName: %s\n"
    (quoted kb.name) git_sync (quoted kb.domain) (quoted kb.name)

let no_engines = `Assoc [ ("jobs", `Assoc []); ("actors", `Assoc []) ]

(* [git_sync], true unless said, makes the tree a checkout whose project
   file syncs git. [engines] are the engines of each job and actor, none
   unless given. [staging] changes the staging bounds, a quarter of a
   second and two seconds unless given. [before] prepares the trees before
   the Archivist first boots. [env] is more of its environment, over its
   service account. [git_tripwire] puts a git that only records that it was
   run ahead of the real one: for a knowledge base that must run none.
   [accounts] are service accounts the issuer grants beside the
   Archivist's: another service the world runs. *)
let create ?(git_sync = true) ?(engines = no_engines) ?(staging = [])
    ?(before = fun _ -> ()) ?(env = []) ?(git_tripwire = false) ?(accounts = [])
    ~gateway ~command () =
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
  write_file
    (dirs.root // ".xynolog" // "config.yaml")
    (project_file world.kb ~git_sync);
  if git_sync then
    Result.iter_error
      (fun err -> failwith ("git init failed: " ^ err))
      (git_in dirs.root [ "init"; "--quiet" ]);
  before dirs;
  let document =
    `Assoc
      [
        ("gatewayUrl", `String (World.origin world));
        ("identity", `Assoc [ ("issuer", `String world.issuer.origin) ]);
        ("root", `String dirs.root);
        ("stateHome", `String dirs.state_home);
        ("anchoredTextDir", `String dirs.anchored_text_dir);
        ("engines", engines);
        ("port", `Int (Net.free_port ()));
        ("skipRebuild", `Bool false);
        ( "staging",
          `Assoc
            (List.map
               (fun (bound, ms) ->
                 ( bound,
                   `Int
                     (Option.value (List.assoc_opt bound staging) ~default:ms)
                 ))
               [ ("flushMs", 250); ("maxWaitMs", 2_000) ]) );
        ("logLevel", `String "info");
        ("logFormat", `String "json");
      ]
  in
  let path_first =
    if git_tripwire then (
      let dir = base // "tripwire" in
      Sys.mkdir dir 0o755;
      let script = dir // "git" in
      Out_channel.with_open_bin script (fun out ->
          Printf.fprintf out "#!/bin/sh\necho \"$@\" >> %s\nexit 1\n"
            (Filename.quote (base // "git-runs")));
      Unix.chmod script 0o755;
      Some dir)
    else None
  in
  let env =
    [
      ("XYNOLOG_OIDC_CLIENT_ID", archivist_client);
      ("XYNOLOG_OIDC_CLIENT_SECRET", secret);
    ]
    @ env
  in
  let archivist =
    Archivist.start (Archivist.launched ?path_first ~command ~document env)
  in
  let t = { world; base; dirs; archivist; responses = Dynarray.create () } in
  answering t;
  t

let drain t =
  let responses = Dynarray.to_list t.responses in
  Dynarray.clear t.responses;
  World.drain t.world @ responses

let close t =
  ignore (Archivist.stop t.archivist : int option);
  World.close t.world;
  Gateway.remove t.base
