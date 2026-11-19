(* Starting and stopping gateway processes. A gateway is the implementations
   file's [gateway], [--config <document>] and an environment. The document
   is the spec's GatewayConfig, held to its schema before the gateway is
   started; the environment holds only what
   specs/src/service-environment/variables.json lists for the gateway, and
   PATH. *)

type plane = Protocol.Service_rows.gateway_config_type

let plane_word : plane -> string = function
  | In_process -> "in-process"
  | Nats -> "nats"

type t = {
  process : Process.t;
  origin : string;
  port : int;
  document : Yojson.Safe.t;
  dir : string;
}

let temporary_dir prefix =
  let dir = Filename.temp_dir prefix "" in
  dir

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

(* Fail when the suite would set a variable the service does not read: a
   case setting one would prove nothing. *)
let check_environment service ~named env =
  let listed = (Protocol.service_environment service @ named) @ [ "PATH" ] in
  match List.filter (fun (name, _) -> not (List.mem name listed)) env with
  | [] -> ()
  | strays ->
      failwith
        (Printf.sprintf
           "the suite set %s, which \
            specs/src/service-environment/variables.json does not list"
           (String.concat ", " (List.map fst strays)))

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
                ("type", `String (plane_word Nats)); ("servers", `String servers);
              ]
        | Nats, None -> failwith "a gateway on the NATS plane is given a broker"
        | In_process, _ -> `Assoc [ ("type", `String (plane_word In_process)) ]
      );
      ("logLevel", `String "warn");
      ("logFormat", `String "json");
    ]

let start ~command ~document env =
  held_to "GatewayConfig" document;
  check_environment Gateway ~named:[] env;
  let dir = temporary_dir "gateway-conformance-" in
  let path = Filename.concat dir "gateway.json" in
  write_json path document;
  let port = Yojson.Safe.Util.(member "port" document |> to_int) in
  let process =
    Process.spawn "gateway"
      (command @ [ "--config"; path ])
      (Process.run_environment () @ env)
  in
  let origin = Printf.sprintf "http://127.0.0.1:%d" port in
  (try
     Process.until_ready process ~timeout:30.0 "serve" (fun () ->
         match
           Call.reach origin "GET" (Protocol.path Gateway "GET" "/api/health")
         with
         | Some reply -> reply.status = 200
         | None -> false)
   with failure ->
     remove dir;
     raise failure);
  { process; origin; port; document; dir }

let stop t =
  ignore (Process.stop t.process : int option);
  remove t.dir
