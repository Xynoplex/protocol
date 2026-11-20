(* Starting and stopping gateway processes. A gateway is the implementations
   file's [gateway], [--config <document>] and an environment. The document
   is the spec's GatewayConfig, held to its schema before the gateway is
   started; the environment holds only what
   specs/src/service-environment/variables.json lists for the gateway, and
   PATH. *)

type plane = Protocol.Service_rows.gateway_config_type

(* What a gateway is started with. [args], given the document's path, are
   the arguments after the command when a case means to get them wrong;
   otherwise [--config <document>]. *)
type launch = {
  command : string list;
  document : Yojson.Safe.t;
  env : (string * string) list;
  unlisted : (string * string) list;
  args : (string -> string list) option;
}

type t = {
  process : Process.t;
  origin : string;
  port : int;
  dir : string;
  launch : launch;
  served_after : float;  (** Seconds from its spawning to its first health. *)
}

let rec remove path =
  if Sys.file_exists path then
    if Sys.is_directory path then (
      Array.iter
        (fun name -> remove (Filename.concat path name))
        (Sys.readdir path);
      Sys.rmdir path)
    else Sys.remove path

let write_json path json =
  Out_channel.with_open_bin path (fun out ->
      output_string out (Yojson.Safe.pretty_to_string json);
      output_char out '\n')

(* Fail unless [document] is what the service's schema says its
   configuration document is. *)
let held_to schema document =
  match Protocol.refusals (Protocol.service_schema schema) document with
  | [] -> ()
  | refusals ->
      failwith
        (Printf.sprintf "the suite's %s is not one (%s): %s" schema
           (String.concat "; " refusals)
           (Yojson.Safe.to_string document))

(* The variables the document names for its broker's credential. *)
let named_variables document =
  List.filter_map
    (fun key ->
      match Yojson.Safe.Util.(member "signal" document |> member key) with
      | `String name -> Some name
      | _ -> None)
    [ "userEnv"; "passwordEnv" ]

(* Fail when the suite would set a variable the service does not read: a
   case setting one would prove nothing. [unlisted] are those a case sets to
   show they change nothing, which the service must not read. *)
let check_environment service ?(unlisted = []) ~named env =
  let listed = (Protocol.service_environment service @ named) @ [ "PATH" ] in
  (match List.filter (fun (name, _) -> not (List.mem name listed)) env with
  | [] -> ()
  | strays ->
      failwith
        (Printf.sprintf
           "the suite set %s, which \
            specs/src/service-environment/variables.json does not list and the \
            document does not name"
           (String.concat ", " (List.map fst strays))));
  match List.filter (fun (name, _) -> List.mem name listed) unlisted with
  | [] -> ()
  | read ->
      failwith
        (Printf.sprintf
           "the suite passed %s as unlisted, and the service reads it"
           (String.concat ", " (List.map fst read)))

(* The settings a gateway runs with when a case says nothing else. *)
let document ~(kb : Protocol.kb) ~issuer ~archivist:(host, port) ~plane ?nats ()
    =
  let own = Net.free_port () in
  `Assoc
    [
      ("kb", `Assoc [ ("name", `String kb.name); ("domain", `String kb.domain) ]);
      ("port", `Int own);
      ("publicUrl", `String (Printf.sprintf "http://127.0.0.1:%d" own));
      (* Generous: a case that means to meet the gateway's capacity sets its own. *)
      ( "capacity",
        `Assoc
          [
            ("queuedBytes", `Int (1024 * 1024 * 1024));
            ("connections", `Int 50_000);
          ] );
      ( "identity",
        `Assoc [ ("issuer", `String issuer); ("subjectClaim", `String "sub") ]
      );
      ("archivist", `Assoc [ ("host", `String host); ("port", `Int port) ]);
      ( "signal",
        match ((plane : plane), nats) with
        | Nats, Some servers ->
            `Assoc
              [
                ( "type",
                  `String (Protocol.Service_rows.gateway_config_type_value Nats)
                );
                ("servers", `String servers);
              ]
        | Nats, None -> failwith "a gateway on the NATS plane is given a broker"
        | In_process, _ ->
            `Assoc
              [
                ( "type",
                  `String
                    (Protocol.Service_rows.gateway_config_type_value In_process)
                );
              ] );
      ("logLevel", `String "warn");
      ("logFormat", `String "json");
    ]

let launched ?(unlisted = []) ?args ~command ~document env =
  { command; document; env; unlisted; args }

let spawn (l : launch) =
  check_environment Gateway ~unlisted:l.unlisted
    ~named:(named_variables l.document)
    l.env;
  let dir = Filename.temp_dir "gateway-conformance-" "" in
  let path = Filename.concat dir "gateway.json" in
  write_json path l.document;
  let args =
    match l.args with Some args -> args path | None -> [ "--config"; path ]
  in
  let process =
    Process.spawn "gateway" (l.command @ args)
      (Process.run_environment () @ l.unlisted @ l.env)
  in
  (process, dir)

let health_path () = Protocol.path Gateway "GET" "/api/health"

let start_launch (l : launch) =
  held_to "GatewayConfig" l.document;
  let spawned = Run.now () in
  let process, dir = spawn l in
  let port = Yojson.Safe.Util.(member "port" l.document |> to_int) in
  let origin = Printf.sprintf "http://127.0.0.1:%d" port in
  (try
     Process.until_ready process ~timeout:30.0 "serve" (fun () ->
         match Call.reach origin "GET" (health_path ()) with
         | Some reply -> reply.status = 200
         | None -> false)
   with failure ->
     remove dir;
     raise failure);
  {
    process;
    origin;
    port;
    dir;
    launch = l;
    served_after = Run.now () -. spawned;
  }

let start ~command ~document env =
  start_launch (launched ~command ~document env)

let stop t =
  ignore (Process.stop t.process : int option);
  remove t.dir

(* Kill it without its shutdown: nothing it would release is released. *)
let crash t =
  Process.crash t.process;
  remove t.dir

(* Start a gateway that must refuse to: it exits without ever answering its
   health. Its exit code (None when a signal ended it) and its output. Its
   document is not held to the schema, since one it must refuse may be no
   GatewayConfig. One that serves, or neither serves nor exits within
   [timeout] seconds, is a failure. *)
let refused_boot ?(timeout = 30.0) (l : launch) =
  let process, dir = spawn l in
  let serving () =
    match Yojson.Safe.Util.member "port" l.document with
    | `Int port ->
        Call.reach
          (Printf.sprintf "http://127.0.0.1:%d" port)
          "GET" (health_path ())
        <> None
    | _ -> false
  in
  Fun.protect
    ~finally:(fun () -> remove dir)
    (fun () ->
      let code = Process.refused process ~timeout serving in
      (code, Process.output process))
