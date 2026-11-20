(* Boot, health and shutdown (JOBS.md § Configuration, § Health): the one
   document the dispatcher reads, every way it refuses to start, in the
   words service-environment/boot-refusals.json states, what its health
   answers, and a clean stop. *)

open Xynolog_conformance
open Support
open Case

let schema = "DispatcherConfig"

let holds ?members name fill output =
  Boot_refusals.holds ~line:true ?members ~service:Dispatcher ~schema name fill
    output

(* A launch like the world's, on a port of its own: its document changed by
   [change], its environment [env] when given. *)
let launch ?(change = Fun.id) ?env ?args (w : D.t) =
  Dispatcher.launched ?args ~command:w.dispatcher.launch.command
    ~document:
      (change (Edit.set_at [ "port" ] (`Int (Net.free_port ())) (D.settings w)))
    (Option.value env ~default:w.env)

(* What a dispatcher that must refuse wrote, once it has exited with the
   table's exit code. *)
let refused what l =
  let code, output, _ = Dispatcher.refused_boot l in
  let output = String.concat "\n" output in
  Check.equal
    (Printf.sprintf "%s: its exit code\n%s" what output)
    (`Int (Boot_refusals.exit_code ()))
    (match code with Some c -> `Int c | None -> `Null);
  output

let health w =
  let reply = Dispatcher.health w.D.dispatcher in
  Check.equal "its health's status" (`Int 200) (`Int reply.status);
  Check.that "its health is JSON"
    (match Http.Header.get reply.headers "content-type" with
    | Some t -> String.starts_with ~prefix:"application/json" t
    | None -> false);
  let json = Option.get reply.json in
  Check.equal "its health's refusals as a DispatcherHealth" (`List [])
    (`List
       (List.map
          (fun r -> `String r)
          (Protocol.refusals (Protocol.service_schema "DispatcherHealth") json)));
  Check.equal "its health"
    (`Assoc [ ("status", str "ok"); ("queue", str "jetstream") ])
    json;
  let elsewhere = Dispatcher.health ~path:"/api/health" w.dispatcher in
  Check.equal "/api/health's status" (`Int 404) (`Int elsewhere.status);
  Check.equal "/api/health's body" (str "") (str elsewhere.text)

let temporary name = Filename.concat (Filename.get_temp_dir_name ()) name

(* How it is started wrong, and the refusal it must write. *)
let unstarted =
  [
    ( "started without --config",
      fun w -> (launch ~args:(fun _ -> []) w, "unnamed", []) );
    ( "with --config naming no file",
      fun w ->
        let path =
          temporary (Printf.sprintf "absent-%s.json" (Fresh.uuid ()))
        in
        ( launch ~args:(fun _ -> [ "--config"; path ]) w,
          "unreadable",
          [ ("path", path) ] ) );
    ( "with a document that is not JSON",
      fun w ->
        let path =
          temporary
            (Printf.sprintf "dispatcher-not-json-%s.json" (Fresh.uuid ()))
        in
        Out_channel.with_open_bin path (fun out ->
            output_string out "gatewayUrl = \"http://gateway\"\n");
        ( launch ~args:(fun _ -> [ "--config"; path ]) w,
          "not-json",
          [ ("path", path) ] ) );
  ]

(* A document broken as a case means it, and the line of members the
   refusal names it by. *)
let broken =
  [
    ( "missing a clock",
      Edit.remove_at [ "timing"; "tickMs" ],
      ("missing", [ ("pointer", "/timing"); ("member", "tickMs") ]) );
    ( "missing the broker",
      Edit.remove_at [ "queue"; "servers" ],
      ("missing", [ ("pointer", "/queue"); ("member", "servers") ]) );
    ( "missing the issuer",
      Edit.remove_at [ "identity" ],
      ("missing", [ ("pointer", "/"); ("member", "identity") ]) );
    ( "missing the gateway",
      Edit.remove_at [ "gatewayUrl" ],
      ("missing", [ ("pointer", "/"); ("member", "gatewayUrl") ]) );
    ( "missing the knowledge base it serves",
      Edit.remove_at [ "kb" ],
      ("missing", [ ("pointer", "/"); ("member", "kb") ]) );
    ( "carrying a secret's value where its name belongs",
      Edit.set_at [ "queue"; "password" ] (str "hunter2"),
      ("undeclared", [ ("pointer", "/queue"); ("members", "password") ]) );
  ]

let unset_variable w =
  let output =
    refused "a named variable unset"
      (launch
         ~change:
           (Edit.set_at [ "queue"; "passwordEnv" ] (str "UNSET_QUEUE_PASSWORD"))
         w)
  in
  Check.contains_text "its output" "UNSET_QUEUE_PASSWORD" output;
  holds "unset"
    [ ("field", "/queue/passwordEnv"); ("name", "UNSET_QUEUE_PASSWORD") ]
    output

let without_account missing w =
  let output =
    refused ("without " ^ missing)
      (launch ~env:(List.filter (fun (name, _) -> name <> missing) w.D.env) w)
  in
  holds "no-service-account" [ ("missing", missing) ] output

let unreachable_broker w =
  let deadline = 2_000 in
  let code, output, after =
    Dispatcher.refused_boot
      (launch
         ~change:(fun d ->
           d
           |> Edit.set_at [ "queue" ]
                (`Assoc
                   [
                     ( "servers",
                       str
                         (Printf.sprintf "nats://127.0.0.1:%d"
                            (Net.free_port ())) );
                   ])
           |> Edit.set_at [ "timing"; "bootDeadlineMs" ] (`Int deadline))
         w)
  in
  Check.equal
    ("its exit code\n" ^ String.concat "\n" output)
    (`Int (Boot_refusals.exit_code ()))
    (match code with Some c -> `Int c | None -> `Null);
  Check.that
    (Printf.sprintf
       "it exited %g s after it started, past its deadline of %d ms and 15 s"
       after deadline)
    (after < (float_of_int deadline /. 1000.0) +. 15.0)

let clean_stop w =
  let extra = Dispatcher.start (launch w) in
  Check.equal "its exit code on SIGTERM" (`Int 0)
    (match Dispatcher.stop extra with Some c -> `Int c | None -> `Null);
  Check.that "it answers nothing after"
    (Call.reach extra.origin "GET" Dispatcher.health_path = None)

let tests implementations =
  with_dispatcher implementations "the dispatcher's boot"
    ([ case "answers exactly /health, with the queue it holds" health ]
    @ List.map
        (fun (how, start) ->
          case (Printf.sprintf "refuses to start %s, saying why" how) (fun w ->
              let l, name, fill = start w in
              holds name fill (refused how l)))
        unstarted
    @ List.map
        (fun (what, change, members) ->
          case (Printf.sprintf "refuses a document %s, naming the field" what)
            (fun w ->
              let output = refused what (launch ~change w) in
              holds ~members:[ members ] "invalid" [] output;
              Check.that "the secret's value is not in its output"
                (not (Check.matches_re "hunter2" output))))
        broken
    @ [
        case
          ~known:
            "the dispatcher at images bb38209 names the member dotted, \
             queue.passwordEnv, where boot-refusals.json names it by its JSON \
             pointer"
          "refuses a document naming a broker credential that is not set, \
           naming the variable"
          unset_variable;
      ]
    @ List.map
        (fun missing ->
          case
            (Printf.sprintf
               "refuses to start without its service account's %s, naming it"
               missing)
            (without_account missing))
        [ "XYNOLOG_OIDC_CLIENT_ID"; "XYNOLOG_OIDC_CLIENT_SECRET" ]
    @ [
        case
          "exits within its boot deadline when the broker cannot be reached, \
           so its supervisor can retry"
          unreachable_broker;
        case "stops on SIGTERM with status 0, and answers nothing after"
          clean_stop;
      ])
