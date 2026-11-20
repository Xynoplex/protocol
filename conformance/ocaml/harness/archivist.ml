(* Starting and stopping Archivist processes. An Archivist is the
   implementations file's [archivist], [--config <document>] and an
   environment. The document is the spec's ArchivistConfig, held to its
   schema before an Archivist that must serve is started; one a case means
   it to refuse is written as the case gives it.
   The environment holds only what
   specs/src/service-environment/variables.json lists for the Archivist,
   and PATH, which finds its command and git. *)

(* What an Archivist is started with. [args],
   given the document's path, are the arguments after the command when a
   case means to get them wrong; otherwise [--config <document>].
   [path_first] is a directory searched before the rest of PATH: where a
   case puts a command of its own in a real one's way. *)
type launch = {
  command : string list;
  document : Yojson.Safe.t;
  env : (string * string) list;
  args : (string -> string list) option;
  path_first : string option;
}

type t = { process : Process.t; origin : string; dir : string; launch : launch }

let launched ?args ?path_first ~command ~document env =
  { command; document; env; args; path_first }

let port_of document =
  match Yojson.Safe.Util.member "port" document with
  | `Int port -> Some port
  | _ -> None

let origin_of document =
  Option.map (Printf.sprintf "http://127.0.0.1:%d") (port_of document)

let health_path () = Protocol.path Archivist "GET" "/health"

let spawn l =
  Gateway.check_environment Archivist ~named:[] l.env;
  let dir = Filename.temp_dir "conformance-archivist-" "" in
  let path = Filename.concat dir "archivist.json" in
  Gateway.write_json path l.document;
  let args =
    match l.args with Some args -> args path | None -> [ "--config"; path ]
  in
  let path_env =
    match l.path_first with
    | Some first -> first ^ ":" ^ Sys.getenv "PATH"
    | None -> Sys.getenv "PATH"
  in
  let env =
    ("PATH", path_env)
    :: List.filter
         (fun (name, _) -> name <> "PATH")
         (Process.run_environment ())
    @ l.env
  in
  (Process.spawn "Archivist" (l.command @ args) env, dir)

(* Start an Archivist and wait until its health answers. *)
let start l =
  Gateway.held_to "ArchivistConfig" l.document;
  let process, dir = spawn l in
  let origin = Option.get (origin_of l.document) in
  (try
     Process.until_ready process ~timeout:60.0 "serve" (fun () ->
         match Call.reach origin "GET" (health_path ()) with
         | Some r -> r.status = 200
         | None -> false)
   with failure ->
     Gateway.remove dir;
     raise failure);
  { process; origin; dir; launch = l }

let output t = Process.output t.process

(* SIGTERM, and wait: its exit code. *)
let stop t =
  let code = Process.stop t.process in
  Gateway.remove t.dir;
  code

(* Kill it without its shutdown: nothing it would settle is settled. *)
let crash t =
  Process.crash t.process;
  Gateway.remove t.dir

(* Start an Archivist that must refuse to: it exits without its health ever
   answering. Its exit code (None when a signal ended it) and its output,
   one line to an element. Its document is not held to the schema, since
   one it must refuse may be no ArchivistConfig. *)
let refused_boot ?(timeout = 60.0) l =
  let process, dir = spawn l in
  let serving () =
    match origin_of l.document with
    | Some origin -> Call.reach origin "GET" (health_path ()) <> None
    | None -> false
  in
  Fun.protect
    ~finally:(fun () -> Gateway.remove dir)
    (fun () ->
      let code = Process.refused process ~timeout serving in
      (code, Process.output process))
