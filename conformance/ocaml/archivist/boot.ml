(* Boot, health and shutdown (ARCHIVIST.md § Configuration, § Boot and
   shutdown): the one document the Archivist reads, every way it refuses to
   start, in the words service-environment/boot-refusals.json and
   archivist/messages.json state, a gateway of another knowledge base among
   them, what its health answers, and a clean stop. *)

open Xynolog_conformance
open Support
open Case

let schema = "ArchivistConfig"

let holds ?members name fill output =
  Boot_refusals.holds ~line:true ?members ~service:Archivist ~schema name fill
    output

(* A launch like the world's, on a port of its own: its document changed by
   [change], its environment [env] when given. *)
let launch ?(change = Fun.id) ?env ?args (w : A.t) =
  Archivist.launched ?args ~command:w.archivist.launch.command
    ~document:
      (change (Edit.set_at [ "port" ] (`Int (Net.free_port ())) (A.settings w)))
    (Option.value env ~default:(A.env w))

(* What an Archivist that must refuse wrote, once it has exited with the
   table's exit code. *)
let refused what l =
  let code, output = Archivist.refused_boot l in
  let output = String.concat "\n" output in
  Check.equal
    (Printf.sprintf "%s: its exit code\n%s" what output)
    (`Int (Boot_refusals.exit_code ()))
    (match code with Some c -> `Int c | None -> `Null);
  output

(* A knowledge base of its own beside the world's, for a boot that must be
   refused: its project file [config] when given, a checkout when [git]. *)
let tree ?config ~git run =
  let base = Filename.temp_dir "conformance-kb-refused-" "" in
  let root = Filename.concat base "kb" in
  Sys.mkdir root 0o755;
  Sys.mkdir (Filename.concat root ".xynolog") 0o755;
  Option.iter
    (fun config ->
      Out_channel.with_open_bin
        (Filename.concat (Filename.concat root ".xynolog") "config.yaml")
        (fun out -> output_string out config))
    config;
  if git then
    Result.iter_error
      (fun err -> failwith ("git init failed: " ^ err))
      (A.git_in root [ "init"; "--quiet" ]);
  Fun.protect ~finally:(fun () -> Gateway.remove base) (fun () -> run root)

let in_tree root = Edit.set_at [ "root" ] (str root)

let health w =
  let health = A.http w ~anonymous:true ~route:"/health" "GET" "/health" in
  Check.equal "its health's status" (`Int 200) (`Int health.status);
  Check.that "its health is JSON"
    (match Http.Header.get health.headers "content-type" with
    | Some t -> String.starts_with ~prefix:"application/json" t
    | None -> false);
  let json = Option.get health.json in
  Check.equal "its health's refusals as an ArchivistHealth" (`List [])
    (`List
       (List.map
          (fun r -> `String r)
          (Protocol.refusals (Protocol.service_schema "ArchivistHealth") json)));
  (* The schema holds each actor once; the order is its enum's. *)
  let actors =
    match
      Option.bind (Protocol.json_of "archivist/ArchivistHealth.json")
        (fun schema ->
          Protocol.at schema [ "properties"; "actors"; "items"; "enum" ])
    with
    | Some actors -> actors
    | None -> failwith "ArchivistHealth states no actors"
  in
  Check.equal "its health"
    (`Assoc [ ("status", str "ok"); ("actors", actors) ])
    json

let nowhere w =
  List.iter
    (fun path ->
      let reply = A.http w "GET" path in
      Check.equal (path ^ ": its status") (`Int 404) (`Int reply.status);
      Check.equal (path ^ ": its body") (str "") (str reply.text))
    [ "/api/health"; "/"; "/resources/abc" ]

let configured w =
  Check.equal "the world's document's refusals as an ArchivistConfig" (`List [])
    (`List
       (List.map
          (fun r -> `String r)
          (Protocol.refusals (Protocol.service_schema schema) (A.settings w))));
  ignore (Protocol.operation Archivist "GET" "/health")

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
            (Printf.sprintf "archivist-not-json-%s.json" (Fresh.uuid ()))
        in
        Out_channel.with_open_bin path (fun out ->
            output_string out "gatewayUrl = \"http://gateway\"\n");
        ( launch ~args:(fun _ -> [ "--config"; path ]) w,
          "not-json",
          [ ("path", path) ] ) );
  ]

let actors_engine engine =
  Edit.set_at [ "engines"; "actors" ] (`Assoc [ ("matcher", `Assoc engine) ])

(* A document broken as a case means it, and the line of members the
   refusal names it by. *)
let broken =
  [
    ( "missing the gateway",
      Edit.remove_at [ "gatewayUrl" ],
      ("missing", [ ("pointer", "/"); ("member", "gatewayUrl") ]) );
    ( "missing the issuer",
      Edit.remove_at [ "identity" ],
      ("missing", [ ("pointer", "/"); ("member", "identity") ]) );
    ( "missing the working tree",
      Edit.remove_at [ "root" ],
      ("missing", [ ("pointer", "/"); ("member", "root") ]) );
    ( "missing the state volume",
      Edit.remove_at [ "stateHome" ],
      ("missing", [ ("pointer", "/"); ("member", "stateHome") ]) );
    ( "missing the anchored-text store",
      Edit.remove_at [ "anchoredTextDir" ],
      ("missing", [ ("pointer", "/"); ("member", "anchoredTextDir") ]) );
    ( "missing a staging bound",
      Edit.remove_at [ "staging"; "maxWaitMs" ],
      ("missing", [ ("pointer", "/staging"); ("member", "maxWaitMs") ]) );
    ( "not saying whether to rebuild",
      Edit.remove_at [ "skipRebuild" ],
      ("missing", [ ("pointer", "/"); ("member", "skipRebuild") ]) );
    ( "with an engine of a provider it does not know",
      actors_engine [ ("provider", str "mistral"); ("model", str "m") ],
      ("other", [ ("pointer", "/engines/actors/matcher/provider") ]) );
    ( "with an engine carrying a credential",
      actors_engine
        [
          ("provider", str "ollama");
          ("model", str "m");
          ("apiKey", str "hunter2");
        ],
      ( "undeclared",
        [ ("pointer", "/engines/actors/matcher"); ("members", "apiKey") ] ) );
    ( "with a field the schema does not have",
      (function
      | `Assoc members -> `Assoc (members @ [ ("stateDir", str "/state") ])
      | other -> other),
      ("undeclared", [ ("pointer", "/"); ("members", "stateDir") ]) );
  ]

let without_account w =
  let output =
    refused "no service account"
      (launch
         ~env:
           (List.filter
              (fun (name, _) -> name <> "XYNOLOG_OIDC_CLIENT_SECRET")
              (A.env w))
         w)
  in
  holds "no-service-account"
    [ ("missing", "XYNOLOG_OIDC_CLIENT_SECRET") ]
    output

let no_project_file (w : A.t) =
  tree ~git:false (fun root ->
      holds_boot_refusal "no-project-file" []
        (refused "no project file" (launch ~change:(in_tree root) w)))

let not_a_checkout (w : A.t) =
  tree ~config:(A.project_file w.world.kb ~git_sync:true) ~git:false
    (fun root ->
      holds_boot_refusal "not-a-checkout"
        [ ("root", root) ]
        (refused "not a checkout" (launch ~change:(in_tree root) w)))

let gateway_elsewhere (w : A.t) =
  let other =
    Protocol.kb_identity "plain scalars, and a version, which names nothing"
  in
  tree ~config:(A.project_file other ~git_sync:false) ~git:false (fun root ->
      let state_home = Filename.concat (Filename.dirname root) "state" in
      Sys.mkdir state_home 0o755;
      let output =
        refused "a gateway of another knowledge base"
          (launch
             ~change:(fun d ->
               Edit.set_at [ "stateHome" ] (str state_home) (in_tree root d))
             w)
      in
      holds_boot_refusal "gateway-elsewhere"
        [
          ("gateway", World.origin w.world);
          ("served", w.world.kb.resource);
          ("root", root);
          ("tree", other.resource);
          ("did", other.did);
        ]
        output;
      Check.equal "the state volume" (`List [])
        (`List
           (List.map
              (fun f -> `String f)
              (Array.to_list (Sys.readdir state_home))));
      Check.equal "the tree's .xynolog"
        (`List [ str "config.yaml" ])
        (`List
           (List.map
              (fun f -> `String f)
              (Array.to_list (Sys.readdir (Filename.concat root ".xynolog"))))))

let stops w =
  tree ~config:(A.project_file w.A.world.kb ~git_sync:false) ~git:false
    (fun root ->
      let second = Archivist.start (launch ~change:(in_tree root) w) in
      Check.equal "its exit code on SIGTERM" (`Int 0)
        (match Archivist.stop second with Some c -> `Int c | None -> `Null);
      Check.that "it serves no more"
        (Call.reach second.origin "GET" (Archivist.health_path ()) = None))

let tests implementations =
  with_archivist implementations "the Archivist's boot"
    ([
       case "answers /health without a token, naming the actors it hosts" health;
       case "answers 404, with no body, anywhere its API names no route" nowhere;
       case "is configured by a document the spec accepts" configured;
     ]
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
        case "refuses to start without its service account" without_account;
        case
          "refuses a knowledge base that commits no project file, and so \
           declares no domain"
          no_project_file;
        case "refuses a knowledge base that syncs git and is not a checkout"
          not_a_checkout;
        case
          "refuses a gateway that serves another knowledge base than its tree, \
           naming both, and touches nothing of the tree"
          gateway_elsewhere;
        case "stops on SIGTERM with status 0, and serves no more" stops;
      ])
