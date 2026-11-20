(* Starting and stopping the three services a Librarian world runs: a
   Weaver, a Smelter and a Librarian. Each is its implementation's command,
   [--config <document>] and an environment. The document is the protocol's
   configuration document for it (WeaverConfig, SmelterConfig,
   LibrarianConfig), held to its schema and written into a home directory of
   the process's own; a Librarian that must refuse to start is given one its
   schema may refuse. The environment holds what
   specs/src/service-environment/variables.json lists for the service, the
   secrets its document names a variable for, and the process's own home,
   temporary directory and PATH; nothing else from the harness's own. Each
   health answer is held to the service's health schema (LibrarianHealth,
   WeaverHealth, SmelterHealth). *)

let ( // ) = Filename.concat
let member = Yojson.Safe.Util.member
let ensure_dir dir = if not (Sys.file_exists dir) then Sys.mkdir dir 0o755

(* What answers at [origin]'s health port, held to [schema]: None while
   nothing answers 200 there. *)
let health schema origin =
  match Call.reach origin "GET" "/health" with
  | Some { status = 200; json = Some json; _ } -> (
      match Protocol.refusals (Protocol.service_schema schema) json with
      | [] -> Some json
      | refusals ->
          failwith
            (Printf.sprintf "%s/health answered what is no %s (%s): %s" origin
               schema
               (String.concat "; " refusals)
               (Yojson.Safe.to_string json)))
  | _ -> None

let phase json = match member "phase" json with `String p -> p | _ -> ""

(* Every one a world starts is told the same health port, so a process is
   not started while anything answers there. *)
let refuse_if_answered name origin =
  if Call.reach origin "GET" "/health" <> None then
    failwith
      (Printf.sprintf
         "something already answers at %s/health: the %s that is running there \
          is stopped before another is started on its port"
         origin name)

(* The variables [document] names for its secrets: the value of each member
   whose name ends in Env. *)
let rec named_variables (document : Yojson.Safe.t) =
  match document with
  | `Assoc members ->
      List.concat_map
        (function
          | key, `String variable when String.ends_with ~suffix:"Env" key ->
              [ variable ]
          | _, value -> named_variables value)
        members
  | `List items -> List.concat_map named_variables items
  | _ -> []

(* Start a service on [document] and [env], the environment held to the
   spec first: every variable is one the service reads, or one [named] names
   for a secret. [own] is the process's home and temporary directory, which
   no table lists. *)
let launch name ~service ~command ~home ~file ?(own = []) ~named document env =
  Gateway.check_environment service ~named env;
  ensure_dir home;
  let path = home // file in
  Gateway.write_json path document;
  Process.spawn name
    (command @ [ "--config"; path ])
    (Process.run_environment () @ own @ env)

(* ── The Weaver ─────────────────────────────────────────────────────── *)

type weaver = {
  weaver : Implementations.weaver;
  weaver_document : Yojson.Safe.t -> Yojson.Safe.t;
      (** Given where it keeps its cursors. *)
  weaver_env : (string * string) list;
  weaver_home : string;
}

(* Start a Weaver and wait until its first sweep of the record is done and
   its reconcile has ended, however it ended. The home directory outlives a
   process: a Weaver started again finds its cursors there. *)
let start_weaver w =
  let temporary = w.weaver_home // "tmp" in
  ensure_dir w.weaver_home;
  ensure_dir temporary;
  let origin = Printf.sprintf "http://127.0.0.1:%d" w.weaver.health_port in
  let document =
    w.weaver_document (`String (temporary // "weaver-cursors.json"))
  in
  Gateway.held_to "WeaverConfig" document;
  let process =
    launch "Weaver" ~service:Weaver ~command:w.weaver.command
      ~home:w.weaver_home ~file:"weaver.json"
      ~own:[ ("HOME", w.weaver_home); ("TMPDIR", temporary) ]
      ~named:(named_variables document) document w.weaver_env
  in
  Process.until_ready process ~timeout:120.0 "finish starting" (fun () ->
      match health "WeaverHealth" origin with
      | Some h ->
          member "sweep" (member "following" h) = `String "done"
          && List.mem (phase (member "reconcile" h)) [ "done"; "failed" ]
      | None -> false);
  process

(* ── The Smelter ────────────────────────────────────────────────────── *)

(* The health port every Smelter is told. *)
let smelter_port = 24101
let smelter_origin = Printf.sprintf "http://127.0.0.1:%d" smelter_port

type smelter = {
  smelter_command : string list;
  smelter_document : Yojson.Safe.t -> Yojson.Safe.t;
      (** Given where it keeps its cursors. *)
  smelter_env : (string * string) list;
  smelter_home : string;
}

(* Start a Smelter and wait until its first sweep of the record is done, or
   due again. *)
let start_smelter s =
  refuse_if_answered "Smelter" smelter_origin;
  let document =
    s.smelter_document (`String (s.smelter_home // "cursors.json"))
  in
  Gateway.held_to "SmelterConfig" document;
  let process =
    launch "Smelter" ~service:Smelter ~command:s.smelter_command
      ~home:s.smelter_home ~file:"smelter.json"
      ~own:[ ("HOME", s.smelter_home) ]
      ~named:(named_variables document) document s.smelter_env
  in
  Process.until_ready process ~timeout:120.0 "finish starting" (fun () ->
      match health "SmelterHealth" smelter_origin with
      | Some h ->
          List.mem
            (member "sweep" (member "following" h))
            [ `String "done"; `String "due" ]
      | None -> false);
  process

(* ── The Librarian ──────────────────────────────────────────────────── *)

type librarian = {
  librarian_command : string list;
  librarian_document : Yojson.Safe.t;
  librarian_env : (string * string) list;
  librarian_home : string;
  librarian_port : int;
}

let librarian_origin l = Printf.sprintf "http://127.0.0.1:%d" l.librarian_port

let spawn_librarian l ~named =
  refuse_if_answered "Librarian" (librarian_origin l);
  launch "Librarian" ~service:Librarian ~command:l.librarian_command
    ~home:l.librarian_home ~file:"librarian.json" ~named l.librarian_document
    l.librarian_env

(* Start a Librarian and wait until its health answers. *)
let start_librarian l =
  Gateway.held_to "LibrarianConfig" l.librarian_document;
  let process =
    spawn_librarian l ~named:(named_variables l.librarian_document)
  in
  Process.until_ready process ~timeout:120.0 "serve" (fun () ->
      health "LibrarianHealth" (librarian_origin l) <> None);
  process

(* Start a Librarian that must refuse to start: it exits without its health
   ever answering. Its exit code (None when a signal ended it) and its
   output. Its document is not held to the schema, since a document it must
   refuse may be no LibrarianConfig; [named] is what the document it was
   changed from names for its secrets. One that serves, and one that has
   neither served nor exited by [timeout] seconds, is a failure. *)
let refused_librarian l ~named ~timeout =
  let process = spawn_librarian l ~named in
  let code =
    Process.refused process ~timeout (fun () ->
        Call.reach (librarian_origin l) "GET" "/health" <> None)
  in
  (code, Process.output process)
