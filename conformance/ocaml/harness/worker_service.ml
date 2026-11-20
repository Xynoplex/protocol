(* Starting and stopping Worker service processes. A Worker service is one
   of the implementations file's [workerServices], [--config <document>] and
   an environment. The document is the spec's WorkerConfig, held to its
   schema before the worker is started; a case that needs a broken document
   writes [verbatim] instead. The environment holds what
   specs/src/service-environment/variables.json lists for the worker, the
   provider keys its document names, and PATH; nothing else from the
   harness's own. *)

(* What a case writes as the document in place of the settings: JSON that
   is no WorkerConfig, or text that is no JSON. *)
type verbatim = Json of Yojson.Safe.t | Text of string

(* What a worker is started with. [args], given the document's path, are
   the arguments after the command when a case means to get them wrong;
   otherwise [--config <document>]. [unlisted] are variables a worker does
   not read, set only by a case showing that they change nothing. *)
type launch = {
  command : string list;
  settings : Yojson.Safe.t;
  env : (string * string) list;
  verbatim : verbatim option;
  args : (string -> string list) option;
  unlisted : (string * string) list;
}

type t = {
  process : Process.t;
  launch : launch;
  origin : string;  (** Its health port's. *)
  dir : string;
}

let launched ?verbatim ?args ?(unlisted = []) ~command ~settings env =
  { command; settings; env; verbatim; args; unlisted }

let member = Yojson.Safe.Util.member

let origin_of settings =
  match member "port" settings with
  | `Int port -> Printf.sprintf "http://127.0.0.1:%d" port
  | _ -> failwith "the suite's worker settings name no port"

(* The provider keys the settings name: each engine's apiKeyEnv. *)
let named settings =
  List.concat_map
    (fun kb ->
      List.filter_map
        (fun engine ->
          match member "apiKeyEnv" engine with
          | `String variable -> Some variable
          | _ -> None)
        (Yojson.Safe.Util.to_list (member "engines" kb)))
    (Yojson.Safe.Util.to_list (member "knowledgeBases" settings))

let spawn (l : launch) =
  Gateway.check_environment Worker ~unlisted:l.unlisted
    ~named:(named l.settings) l.env;
  let dir = Filename.temp_dir "conformance-worker-" "" in
  let path = Filename.concat dir "worker.json" in
  (match l.verbatim with
  | None -> Gateway.write_json path l.settings
  | Some (Json json) -> Gateway.write_json path json
  | Some (Text text) ->
      Out_channel.with_open_bin path (fun out ->
          output_string out text;
          output_char out '\n'));
  let args =
    match l.args with Some args -> args path | None -> [ "--config"; path ]
  in
  let process =
    Process.spawn "worker" (l.command @ args)
      (Process.run_environment () @ l.unlisted @ l.env)
  in
  { process; launch = l; origin = origin_of l.settings; dir }

(* A request of its health port; None while nothing answers there. *)
let http t meth path = Call.reach t.origin meth path

(* Wait until its health answers 200. Fails, with what it wrote, when it
   exits first, or when its health has not answered within [timeout]
   seconds: it is then killed. *)
let healthy ?(timeout = 30.0) t =
  Process.until_ready t.process ~timeout "serve its health" (fun () ->
      match http t "GET" "/health" with
      | Some { status = 200; _ } -> true
      | _ -> false)

(* Start a worker and wait until its health answers. *)
let start (l : launch) =
  Gateway.held_to "WorkerConfig" l.settings;
  let t = spawn l in
  (try healthy t
   with failure ->
     Process.crash t.process;
     Gateway.remove t.dir;
     raise failure);
  t

(* SIGTERM, unless it has exited, and wait until it has and what it wrote
   is read to its end: its exit code. *)
let stop t =
  let code = Process.stop t.process in
  Eio.Promise.await t.process.drained;
  Gateway.remove t.dir;
  code

(* SIGKILL: nothing it would say on the way out is said. *)
let crash t =
  Process.crash t.process;
  Gateway.remove t.dir

let output t = Process.output t.process
let stdout t = Dynarray.to_list t.process.stdout
let stderr t = Dynarray.to_list t.process.stderr

type refusal = {
  code : int option;  (** None when a signal ended it. *)
  written : string;  (** Every line, stdout and stderr interleaved. *)
  out : string list;
  err : string;
}

(* Start a worker that must refuse: it exits without its health ever
   answering. Its settings are not held to the schema, since what it must
   refuse may be no WorkerConfig. One that serves, or neither serves nor
   exits within [timeout] seconds, is a failure. *)
let refused ?(timeout = 30.0) (l : launch) =
  let t = spawn l in
  Fun.protect
    ~finally:(fun () -> Gateway.remove t.dir)
    (fun () ->
      let code =
        Process.refused t.process ~timeout (fun () ->
            http t "GET" "/health" <> None)
      in
      Eio.Promise.await t.process.drained;
      {
        code;
        written = String.concat "\n" (output t);
        out = stdout t;
        err = String.concat "\n" (stderr t);
      })
