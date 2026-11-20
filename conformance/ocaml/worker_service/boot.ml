(* What a Worker service is started with, and every way it
   refuses to start (WORKER-SERVICE.md § Configuration): the one document it
   reads, the key it signs in with, the provider keys the document names,
   and a document that names one knowledge base twice; and how it ends when
   every knowledge base it serves refuses it before any is confirmed
   (§ Knowledge bases). Its words are worker/messages.json's, and, for a
   refusal it shares, service-environment/boot-refusals.json's. *)

open Xynolog_conformance
open Support
open Case

let schema = "WorkerConfig"

(* The [fatal] lines of what a refused worker wrote to stderr. *)
let fatal err =
  List.filter
    (String.starts_with ~prefix:"[fatal]")
    (String.split_on_char '\n' err)

(* The refusal worker/messages.json's boot row [name] states, as a
   pattern, in the line a refusal is written in: its placeholders filled
   from [fill], [reasons] held as its reasons, and any other as some text. *)
let boot_pattern ?reasons name fill =
  let refusal =
    match
      List.find_opt
        (function
          | M.Own_boot_refusal { name = n; _ }
          | M.Shared_boot_refusal { name = n; _ } ->
              n = name)
        (messages ()).boot
    with
    | Some (Own_boot_refusal { refusal; _ }) ->
        Boot_refusals.pattern_of
          ?held:(Option.map (fun r -> [ ("reasons", r) ]) reasons)
          refusal fill
    | Some (Shared_boot_refusal { boot_refusal; _ }) ->
        Boot_refusals.pattern_of
          (Boot_refusals.refusal boot_refusal)
          (Boot_refusals.fill_of ~service:Worker ~schema fill)
    | None -> failwith ("worker/messages.json states no boot refusal " ^ name)
  in
  Boot_refusals.pattern_of
    ~held:[ ("refusal", refusal) ]
    (Boot_refusals.table ()).line []

(* The line worker/messages.json's [reasons] row [name] states, filled. *)
let reason name fill =
  match
    List.find_opt
      (fun (row : M.reasons_item) -> row.name = name)
      (messages ()).reasons
  with
  | Some row -> Boot_refusals.pattern_of row.line fill
  | None -> failwith ("worker/messages.json states no reason " ^ name)

let matches_whole pattern text =
  Re.execp (Re.compile (Re.whole_string pattern)) text

(* Fail unless a refused worker said one [fatal] line, the refusal [name]
   filled from [fill]. *)
let says_once name fill (refusal : Worker_service.refusal) =
  match fatal refusal.err with
  | [ line ] when matches_whole (boot_pattern name fill) line -> ()
  | lines ->
      failwith
        (Printf.sprintf
           "its stderr says no one [fatal] line of worker/messages.json's %s \
            refusal, filled with %s: it says\n\
            %s\n\
            in\n\
            %s"
           name
           (String.concat ", " (List.map (fun (k, v) -> k ^ "=" ^ v) fill))
           (String.concat "\n" lines) refusal.err)

(* Fail unless a refused worker said its document is no WorkerConfig: the
   head, and under it the one reason [why]. *)
let says_invalid why (refusal : Worker_service.refusal) =
  let lines = String.split_on_char '\n' refusal.err in
  let rec from = function
    | line :: (next :: _ as _rest)
      when String.starts_with ~prefix:"[fatal]" line ->
        Some (line ^ "\n" ^ next)
    | _ :: rest -> from rest
    | [] -> None
  in
  match from lines with
  | Some said when matches_whole (boot_pattern ~reasons:why "invalid" []) said
    ->
      ()
  | _ ->
      failwith
        ("its stderr does not say, in worker/messages.json's words, that its \
          document is no WorkerConfig for the one reason the case gave it:\n"
       ^ refusal.err)

let code_of (refusal : Worker_service.refusal) =
  match refusal.code with Some c -> `Int c | None -> `Null

let exits_refused (refusal : Worker_service.refusal) =
  Check.equal
    ("its exit code\n" ^ refusal.written)
    (`Int (Boot_refusals.exit_code ()))
    (code_of refusal)

let not_said what (refusal : Worker_service.refusal) value =
  Check.that
    (Printf.sprintf "its output does not repeat %s" what)
    (not (Re.execp (Re.compile (Re.str value)) refusal.written))

(* What a worker that started would have dialled: a grant of either
   issuer, its provider, a claim at either knowledge base. A refusal leaves
   each as it was. *)
let dialled (w : W.t) =
  let second = W.second w in
  `Assoc
    [
      ("grants", `List [ `Int (grants w.own); `Int (grants second) ]);
      ("shows", `Int (Dynarray.length w.ollama.shows));
      ( "claims",
        `List
          [
            `Int (Dynarray.length w.own.claims);
            `Int (Dynarray.length second.claims);
          ] );
    ]

(* Run [refuse], and hold that it dialled nothing. *)
let dialling_nothing w refuse =
  let before = dialled w in
  refuse ();
  Check.equal "what it dialled" before (dialled w)

let launch = W.launch

let refused ?settings ?env ?verbatim ?args w =
  let l = launch w in
  let refusal =
    W.refused
      {
        l with
        settings = Option.value settings ~default:l.settings;
        env = Option.value env ~default:l.env;
        verbatim;
        args;
      }
  in
  exits_refused refusal;
  refusal

let key w = List.assoc "XYNOLOG_OIDC_CLIENT_KEY" w.W.env

(* [doc]'s first knowledge base, changed by [f]. *)
let in_first_kb f doc =
  match Check.at doc [ "knowledgeBases" ] with
  | `List (first :: rest) ->
      Edit.set_at [ "knowledgeBases" ] (`List (f first :: rest)) doc
  | _ -> failwith "the document serves no knowledge base"

(* [doc]'s first knowledge base's first engine, changed by [f]. *)
let in_first_engine f =
  in_first_kb (fun kb ->
      match Check.at kb [ "engines" ] with
      | `List (first :: rest) ->
          Edit.set_at [ "engines" ] (`List (f first :: rest)) kb
      | _ -> failwith "the knowledge base is offered no engine")

let settings_of w = (launch w).settings
let first_kb doc = List.hd (Check.list doc [ "knowledgeBases" ])
let engines_of kb = Check.list kb [ "engines" ]

let accepted w =
  Check.equal "the refusals of its document as a WorkerConfig" (`List [])
    (strs (Protocol.refusals (Protocol.service_schema schema) (settings_of w)))

let without_config w =
  dialling_nothing w (fun () ->
      List.iter
        (fun args -> says_once "unnamed" [] (refused ~args w))
        [ (fun _ -> []); (fun _ -> [ "--config" ]); (fun _ -> [ "--config=" ]) ])

let no_such_file w =
  dialling_nothing w (fun () ->
      let absent =
        Filename.concat
          (Filename.get_temp_dir_name ())
          (Printf.sprintf "absent-%s.json" (Fresh.uuid ()))
      in
      says_once "unreadable"
        [ ("path", absent) ]
        (refused ~args:(fun _ -> [ "--config"; absent ]) w))

let not_json w =
  dialling_nothing w (fun () ->
      says_once "not-json" []
        (refused ~verbatim:(Text {|gatewayUrl = "http://gateway"|}) w))

(* Each member a document must have: what it is, the change that takes it
   away, and the member its refusal names. *)
let missing =
  [
    ( "its knowledge bases",
      Edit.remove_at [ "knowledgeBases" ],
      "knowledgeBases" );
    ( "a knowledge base's domain",
      in_first_kb (Edit.remove_at [ "domain" ]),
      "domain" );
    ( "a knowledge base's gateway",
      in_first_kb (Edit.remove_at [ "gatewayUrl" ]),
      "gatewayUrl" );
    ( "the account a knowledge base names",
      in_first_kb (Edit.remove_at [ "identity" ]),
      "identity" );
    ( "a knowledge base's issuer",
      in_first_kb (Edit.remove_at [ "identity"; "issuer" ]),
      "issuer" );
    ( "the client a knowledge base names",
      in_first_kb (Edit.remove_at [ "identity"; "clientId" ]),
      "clientId" );
    ( "the engines a knowledge base is offered",
      in_first_kb (Edit.remove_at [ "engines" ]),
      "engines" );
    ( "its model catalogue",
      Edit.remove_at [ "modelCatalogue" ],
      "modelCatalogue" );
    ("its port", Edit.remove_at [ "port" ], "port");
    ("its log level", Edit.remove_at [ "logLevel" ], "logLevel");
    ("its log format", Edit.remove_at [ "logFormat" ], "logFormat");
    ( "an engine's provider and model",
      in_first_engine (Edit.remove_at [ "engine" ]),
      "engine" );
    ( "the jobs an engine is used for",
      in_first_engine (Edit.remove_at [ "jobs" ]),
      "jobs" );
    ( "the address of an engine's provider",
      in_first_engine (Edit.remove_at [ "baseUrl" ]),
      "baseUrl" );
  ]

let lacking change member w =
  dialling_nothing w (fun () ->
      says_invalid
        (reason "missing" [ ("member", member) ])
        (refused ~verbatim:(Json (change (settings_of w))) w))

(* Each other way a document is no WorkerConfig: what it has, the change
   that makes it so, and the names on the way to the member its refusal
   names. *)
let wrong =
  [
    ( "no knowledge base at all",
      Edit.set_at [ "knowledgeBases" ] (`List []),
      "knowledgeBases" );
    ( "a knowledge base offered no engine",
      in_first_kb (Edit.set_at [ "engines" ] (`List [])),
      "knowledgeBases.engines" );
    ( "an engine used for no job",
      in_first_engine (Edit.set_at [ "jobs" ] (`List [])),
      "knowledgeBases.engines.jobs" );
    ( "a provider it does not know",
      in_first_engine
        (Edit.set_at [ "engine" ]
           (`Assoc [ ("provider", str "mistral"); ("model", str "m") ])),
      "knowledgeBases.engines.engine.provider" );
    ("a port that is not one", Edit.set_at [ "port" ] (`Int 70_000), "port");
    ( "a member the schema does not have",
      Edit.set_at [ "dispatcherUrl" ] (str "http://dispatcher"),
      "dispatcherUrl" );
    ( "a provider's key as a value",
      in_first_engine (Edit.set_at [ "apiKey" ] (str "hunter2-not-a-name")),
      "knowledgeBases.engines.apiKey" );
    ( "a secret beside a knowledge base's client",
      in_first_kb
        (Edit.set_at [ "identity"; "clientSecret" ] (str "hunter2-not-a-name")),
      "knowledgeBases.identity.clientSecret" );
  ]

let having change member w =
  dialling_nothing w (fun () ->
      let refusal = refused ~verbatim:(Json (change (settings_of w))) w in
      says_invalid (reason "other" [ ("member", member) ]) refusal;
      not_said "the value it found" refusal "hunter2-not-a-name";
      not_said "the URL it found" refusal "http://dispatcher")

let domain_twice w =
  dialling_nothing w (fun () ->
      let second = W.second w in
      let settings = settings_of w in
      let own = first_kb settings in
      let other =
        W.configuration second (World.origin second.world) (engines_of own)
      in
      (* The third is the first's domain at another gateway: the domain is
         what makes a knowledge base the same. *)
      let refusal =
        refused w
          ~settings:
            (Edit.set_at [ "knowledgeBases" ]
               (`List
                  [
                    own;
                    other;
                    Edit.set_at [ "domain" ] (Check.at own [ "domain" ]) other;
                  ])
               settings)
      in
      says_once "domain-twice" [ ("later", "2"); ("earlier", "0") ] refusal;
      not_said "the domain" refusal (W.domain w.own))

let provider_key_unset w =
  dialling_nothing w (fun () ->
      let second = W.second w in
      let settings = settings_of w in
      let own = first_kb settings in
      (* The engine is the second knowledge base's, so the line says which
         knowledge base named the variable. *)
      let named =
        Edit.set_at [ "knowledgeBases" ]
          (`List
             [
               own;
               W.configuration second
                 (World.origin second.world)
                 [
                   Edit.set_at [ "apiKeyEnv" ]
                     (str "WORKER_SERVICE_PROVIDER_KEY")
                     (List.hd (engines_of own));
                 ];
             ])
          settings
      in
      List.iter
        (fun env ->
          says_once "api-key-unset"
            [ ("knowledgeBase", "1"); ("engine", "0") ]
            (refused ~settings:named ~env w))
        [ w.env; w.env @ [ ("WORKER_SERVICE_PROVIDER_KEY", "") ] ])

let key_value_for_name w =
  dialling_nothing w (fun () ->
      let value = "sk-conformance-0123456789abcdef" in
      let refusal =
        refused w
          ~settings:
            (in_first_engine
               (Edit.set_at [ "apiKeyEnv" ] (str value))
               (settings_of w))
      in
      says_once "api-key-unset"
        [ ("knowledgeBase", "0"); ("engine", "0") ]
        refusal;
      not_said "the key" refusal value)

let without_key w =
  dialling_nothing w (fun () ->
      List.iter
        (fun env ->
          let refusal = refused ~env w in
          says_once "key-unset" [] refusal;
          not_said "the key" refusal (key w))
        [
          List.remove_assoc "XYNOLOG_OIDC_CLIENT_KEY" w.env;
          [ ("XYNOLOG_OIDC_CLIENT_KEY", "") ];
        ])

let pem key = X509.Private_key.encode_pem key

let unreadable_keys w =
  dialling_nothing w (fun () ->
      Lazy.force Fresh.seeded;
      List.iter
        (fun (held, name) ->
          let refusal = refused ~env:[ ("XYNOLOG_OIDC_CLIENT_KEY", held) ] w in
          says_once name [] refusal;
          not_said "the key" refusal held)
        [
          ("not a key at all", "key-unreadable");
          (pem (X509.Private_key.generate `P256), "key-not-rsa");
        ])

(* The [fatal] line a worker every knowledge base of which refused it says
   as it ends, when none was confirmed first (R5). *)
let none_admits_line () =
  match
    List.find_map
      (function
        | M.Own_boot_refusal { name = "none-admits"; refusal; _ } ->
            Some refusal
        | _ -> None)
      (messages ()).boot
  with
  | Some refusal ->
      Protocol.fill_template (Boot_refusals.table ()).line
        [ ("refusal", refusal) ]
  | None -> failwith "worker/messages.json states no none-admits refusal"

let refused_at (refusal : Worker_service.refusal) =
  logged refusal.out (kb_log "refused")

let logs_refused domains refusal =
  let lines = refused_at refusal in
  Check.equal "the knowledge bases its log says refused it"
    (strs (List.sort compare domains))
    (strs
       (List.sort compare
          (List.map (fun line -> text_of (Check.at line [ "domain" ])) lines)));
  List.iter
    (fun line ->
      Check.equal "the level it logs a refusal at" (str "error")
        (Check.at line [ "level" ]);
      Check.that "it says why it was refused"
        (match Protocol.at line [ "reason" ] with
        | Some (`String _) -> true
        | _ -> false))
    lines

let issuer_refuses w =
  Lazy.force Fresh.seeded;
  (* A key the issuer holds no public half of: its assertion is refused. *)
  let unregistered = pem (X509.Private_key.generate ~bits:2048 `RSA) in
  let refusal = refused ~env:[ ("XYNOLOG_OIDC_CLIENT_KEY", unregistered) ] w in
  let none_admits = none_admits_line () in
  Check.equal "its [fatal] lines" (strs [ none_admits ])
    (strs (fatal refusal.err));
  logs_refused [ W.domain w.own ] refusal;
  (* The refusal is in its log before it ends. *)
  let lines = String.split_on_char '\n' refusal.written in
  let index_of found =
    let rec at i = function
      | [] -> max_int
      | line :: rest -> if found line then i else at (i + 1) rest
    in
    at 0 lines
  in
  Check.that "the refusal is in its log before it ends"
    (index_of (fun line -> logged [ line ] (kb_log "refused") <> [])
    < index_of (String.equal none_admits));
  not_said "the key" refusal unregistered;
  Check.equal "what it claimed" (`Int 0) (`Int (Dynarray.length w.own.claims))

let each_refuses w =
  let second = W.second w in
  (* The second answers as the first: a gateway that is another knowledge
     base's. *)
  second.hooks <-
    {
      W.no_hooks with
      confirm = Some (fun () -> Some (W.Domain (W.domain w.own)));
    };
  let settings = settings_of w in
  let engines = engines_of (first_kb settings) in
  (* The first names a client its issuer does not hold. *)
  let refusal =
    refused w
      ~settings:
        (Edit.set_at [ "knowledgeBases" ]
           (`List
              [
                W.configuration ~client:"xynolog-worker-unregistered" w.own
                  (World.origin w.own.world) engines;
                W.configuration second (World.origin second.world) engines;
              ])
           settings)
  in
  Check.equal "its [fatal] lines"
    (strs [ none_admits_line () ])
    (strs (fatal refusal.err));
  logs_refused [ W.domain w.own; W.domain second ] refusal;
  not_said "its key" refusal (key w);
  Check.equal "what it claimed"
    (`List [ `Int 0; `Int 0 ])
    (`List
       [
         `Int (Dynarray.length w.own.claims);
         `Int (Dynarray.length second.claims);
       ])

let cannot_listen w =
  (* The port is taken: something else listens on it, and answers nobody. *)
  let holder = Serve.start (fun _ -> Serve.Dropped) in
  Fun.protect ~finally:holder.stop (fun () ->
      let refusal =
        refused w
          ~settings:(Edit.set_at [ "port" ] (`Int holder.port) (settings_of w))
      in
      says_once "cannot-listen" [ ("port", string_of_int holder.port) ] refusal;
      not_said "its key" refusal (key w))

let config_with_equals w =
  let worker =
    Worker_service.start
      {
        (launch w) with
        args = Some (fun document -> [ "--config=" ^ document ]);
      }
  in
  let states =
    Fun.protect
      ~finally:(fun () ->
        Check.equal "its exit code on SIGTERM" (`Int 0)
          (match Worker_service.stop worker with
          | Some c -> `Int c
          | None -> `Null))
      (fun () ->
        List.map
          (fun kb -> Check.at kb [ "state" ])
          (knowledge_bases_of (health worker)))
  in
  Check.equal "the states of its knowledge bases" (strs [ "working" ])
    (`List states)

let tests implementations =
  in_world implementations "boot" ~category:[ "the worker's boot" ]
    ([
       case "is configured by a document the spec accepts" accepted;
       case
         "refuses to start without --config, saying so once on stderr, and \
          exits 1"
         without_config;
       case "refuses a --config that names no file, naming the path"
         no_such_file;
       case "refuses a document that is not JSON, naming the file" not_json;
     ]
    @ List.map
        (fun (what, change, member) ->
          case
            (Printf.sprintf "refuses a document missing %s, naming the member"
               what)
            (lacking change member))
        missing
    @ List.map
        (fun (what, change, member) ->
          case
            (Printf.sprintf
               "refuses a document with %s, naming the member and no value \
                found in it"
               what)
            (having change member))
        wrong
    @ [
        case
          "refuses a document that names one knowledge base twice, naming the \
           later entry and the earlier, and not the domain"
          domain_twice;
        case
          "refuses to start when a variable a knowledge base's engine names \
           for a provider's key is not set, or is empty, naming the member"
          provider_key_unset;
        case
          "refuses a document that has a key's value where the name of its \
           variable belongs, and does not repeat it"
          key_value_for_name;
        case
          "refuses to start without XYNOLOG_OIDC_CLIENT_KEY, or with it empty"
          without_key;
        case
          "refuses to start with a key it cannot read, or one that is not RSA, \
           and does not repeat it"
          unreadable_keys;
        case
          "exits 1, having served nothing, when the issuer of its one \
           knowledge base refuses its sign-in: saying so in its log, and then \
           once on stderr that no knowledge base admits it"
          issuer_refuses;
        case
          "exits 1, having served nothing, when each of its knowledge bases \
           refuses it before any is confirmed: one whose issuer refuses its \
           sign-in, one that answers another domain"
          each_refuses;
        case
          "exits 1 when it cannot listen on its port, saying why once on stderr"
          cannot_listen;
        case
          "takes the path of its document joined to the flag by an equals sign"
          config_with_equals;
      ])
