(* Starting and stopping dispatcher processes. A dispatcher is the
   implementations file's [dispatcher], [--config <document>] and an
   environment. The document is the spec's DispatcherConfig, held to its
   schema before a dispatcher that must serve is started; one a case means
   it to refuse is written as the case gives it. The environment holds only
   what specs/src/service-environment/variables.json lists for the
   dispatcher, the variables its document names, and PATH. Its health answer
   is held to DispatcherHealth. *)

(* What a dispatcher is started with. [args], given the document's path,
   are the arguments after the command when a case means to get them wrong;
   otherwise [--config <document>]. *)
type launch = {
  command : string list;
  document : Yojson.Safe.t;
  env : (string * string) list;
  args : (string -> string list) option;
}

type t = { process : Process.t; origin : string; dir : string; launch : launch }

(* The clocks a dispatcher runs with when a case says nothing else: long
   enough never to act inside a case. *)
let quiet_timing =
  [
    ("tickMs", 60_000);
    ("staleRunningMs", 30 * 60_000);
    ("ackWaitMs", 30_000);
    ("retentionMs", 24 * 60 * 60_000);
    ("retentionSweepMs", 60 * 60_000);
    ("progressWriteIntervalMs", 5_000);
    ("bootDeadlineMs", 20_000);
  ]

let launched ?args ~command ~document env = { command; document; env; args }
let health_path = "/health"

let origin_of document =
  match Yojson.Safe.Util.member "port" document with
  | `Int port -> Some (Printf.sprintf "http://127.0.0.1:%d" port)
  | _ -> None

let spawn l =
  Gateway.check_environment Dispatcher
    ~named:(Services.named_variables l.document)
    l.env;
  let dir = Filename.temp_dir "dispatcher-conformance-" "" in
  let path = Filename.concat dir "dispatcher.json" in
  Gateway.write_json path l.document;
  let args =
    match l.args with Some args -> args path | None -> [ "--config"; path ]
  in
  ( Process.spawn "dispatcher" (l.command @ args)
      (Process.run_environment () @ l.env),
    dir )

(* A clock of the document's timing, in seconds. *)
let clock document name =
  match Protocol.at document [ "timing"; name ] with
  | Some (`Int ms) -> float_of_int ms /. 1000.0
  | _ -> failwith ("the dispatcher's document states no timing." ^ name)

(* Start a dispatcher and wait until its health answers: within its boot
   deadline, and the time a process takes to start on top. *)
let start l =
  Gateway.held_to "DispatcherConfig" l.document;
  let process, dir = spawn l in
  let origin = Option.get (origin_of l.document) in
  (try
     Process.until_ready process
       ~timeout:(clock l.document "bootDeadlineMs" +. 30.0)
       "serve"
       (fun () -> Services.health "DispatcherHealth" origin <> None)
   with failure ->
     Gateway.remove dir;
     raise failure);
  { process; origin; dir; launch = l }

(* What it answers at [path], its health path unless a case names another. *)
let health ?(path = health_path) t = Call.call t.origin "GET" path

(* SIGTERM, and wait: its exit code. *)
let stop t =
  let code = Process.stop t.process in
  Gateway.remove t.dir;
  code

(* Kill it without its shutdown: nothing it would settle is settled. *)
let crash t =
  Process.crash t.process;
  Gateway.remove t.dir

(* Start a dispatcher that must refuse to: it exits without its health ever
   answering. Its exit code (None when a signal ended it), its output, and
   the seconds it took. Its document is not held to the schema, since one it
   must refuse may be no DispatcherConfig. *)
let refused_boot ?(timeout = 60.0) l =
  let started = Run.now () in
  let process, dir = spawn l in
  let serving () =
    match origin_of l.document with
    | Some origin -> Call.reach origin "GET" health_path <> None
    | None -> false
  in
  Fun.protect
    ~finally:(fun () -> Gateway.remove dir)
    (fun () ->
      let code = Process.refused process ~timeout serving in
      (code, Process.output process, Run.now () -. started))
