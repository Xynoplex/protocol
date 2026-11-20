(* What a gateway needs before it serves, and its refusal to serve without
   it: it exits non-zero and never answers on its port, and its output names
   what is missing. Then the signing key ring, across restarts. *)

open Xynolog_conformance
open Support
open Case

type shared = {
  command : string list;
  kb : Protocol.kb;
  secret : string;
  issuer : Issuer.t;
  archivist : Archivist_standin.t;
}

let make implementations () =
  let kb = Protocol.kb_identity Protocol.suite_kb in
  let secret = Fresh.hex 24 in
  let issuer =
    Issuer.start kb.resource
      [
        ( World.gateway_client,
          { Issuer.secret; roles = [ Protocol.service_role () ] } );
      ]
  in
  {
    command = implementations.Implementations.gateway;
    kb;
    secret;
    issuer;
    archivist = Archivist_standin.start issuer;
  }

let close { issuer; archivist; _ } =
  Archivist_standin.close archivist;
  Issuer.close issuer

(* The environment a gateway is started with, changed by [changes]: a
   variable with a value is set, one with none withheld. *)
let env ?(changes = []) shared =
  List.filter
    (fun (name, _) -> not (List.mem_assoc name changes))
    [
      ("JWT_SECRET", Fresh.hex 32);
      ("XYNOLOG_OIDC_CLIENT_ID", World.gateway_client);
      ("XYNOLOG_OIDC_CLIENT_SECRET", shared.secret);
    ]
  @ List.filter_map
      (fun (name, value) -> Option.map (fun v -> (name, v)) value)
      changes

(* The settings a gateway on the in-process plane runs with, changed by
   [change]. *)
let settings ?(change = Fun.id) shared =
  change
    (Gateway.document ~kb:shared.kb ~issuer:shared.issuer.origin
       ~archivist:(shared.archivist.host, shared.archivist.port)
       ~plane:In_process ())

let signal ?(named = []) servers =
  Edit.set_at [ "signal" ]
    (`Assoc
       ([
          ( "type",
            `String (Protocol.Service_rows.gateway_config_type_value Nats) );
        ]
       @ (match servers with
         | Some s -> [ ("servers", `String s) ]
         | None -> [])
       @ List.map (fun (k, v) -> (k, `String v)) named))

let started ?unlisted shared document env =
  Gateway.start_launch
    (Gateway.launched ?unlisted ~command:shared.command ~document env)

(* [body] on a gateway started for it, stopped after. *)
let with_gateway ?unlisted shared document env body =
  let gateway = started ?unlisted shared document env in
  Fun.protect ~finally:(fun () -> Gateway.stop gateway) (fun () -> body gateway)

let refused ?args shared document env =
  Gateway.refused_boot
    (Gateway.launched ?args ~command:shared.command ~document env)

let exited_non_zero why (code, output) =
  match code with
  | Some 0 -> failwith (Printf.sprintf "%s: it exited 0:\n%s" why output)
  | None -> failwith (Printf.sprintf "%s: a signal ended it:\n%s" why output)
  | Some _ -> ()

let output_of (code, lines) = (code, String.concat "\n" lines)

(* What a refusal's output must hold: the refusal boot-refusals.json names,
   filled, with its {members} when it has them; or, for a refusal the table
   does not state, a pattern. *)
type said =
  | Table of
      string * (string * string) list * (string * (string * string) list) list
  | Pattern of string

(* A case that refuses: why, what it is started with, and what its output
   must hold. *)
type refusal = {
  why : string;
  start :
    shared ->
    Yojson.Safe.t * (string * string) list * (string -> string list) option;
  said : said;
}

let refusal ?args ?(change = Fun.id) ?changes why said =
  { why; said; start = (fun s -> (settings ~change s, env ?changes s, args)) }

let missing pointer member =
  Table
    ( "invalid",
      [],
      [ ("missing", [ ("pointer", pointer); ("member", member) ]) ] )

let schema = "GatewayConfig"

(* Fail unless [output] holds what [said] says. *)
let holds said (code, output) =
  match said with
  | Pattern pattern ->
      Check.that
        (Printf.sprintf "its output matches /%s/:\n%s" pattern output)
        (Check.matches_re pattern output)
  | Table (name, fill, members) ->
      Check.equal
        (Printf.sprintf "its exit code\n%s" output)
        (`Int (Boot_refusals.exit_code ()))
        (match code with Some c -> `Int c | None -> `Null);
      Boot_refusals.holds ~members ~service:Gateway ~schema name fill output

let temporary name = Filename.concat (Filename.get_temp_dir_name ()) name

let absent =
  lazy
    (temporary
       (Printf.sprintf "gateway-conformance-no-document-%s.json" (Fresh.uuid ())))

let refusals =
  [
    refusal "no JWT_SECRET"
      ~changes:[ ("JWT_SECRET", None) ]
      (Pattern "JWT_SECRET");
    refusal "a JWT_SECRET key shorter than 32 characters"
      ~changes:[ ("JWT_SECRET", Some (String.make 40 'a' ^ ",short")) ]
      (Pattern "JWT_SECRET");
    refusal "no service-account client id"
      ~changes:[ ("XYNOLOG_OIDC_CLIENT_ID", None) ]
      (Table
         ("no-service-account", [ ("missing", "XYNOLOG_OIDC_CLIENT_ID") ], []));
    refusal "no service-account secret"
      ~changes:[ ("XYNOLOG_OIDC_CLIENT_SECRET", None) ]
      (Table
         ( "no-service-account",
           [ ("missing", "XYNOLOG_OIDC_CLIENT_SECRET") ],
           [] ));
    refusal "no knowledge-base domain"
      ~change:(Edit.remove_at [ "kb"; "domain" ])
      (missing "/kb" "domain");
    refusal "no Archivist address"
      ~change:(Edit.remove_at [ "archivist"; "host" ])
      (missing "/archivist" "host");
    refusal "no issuer"
      ~change:(Edit.remove_at [ "identity"; "issuer" ])
      (missing "/identity" "issuer");
    refusal "no subject claim"
      ~change:(Edit.remove_at [ "identity"; "subjectClaim" ])
      (missing "/identity" "subjectClaim");
    refusal "a field the document does not declare"
      ~change:(Edit.set_at [ "corsOrigin" ] (`String "*"))
      (Table
         ( "invalid",
           [],
           [ ("undeclared", [ ("pointer", "/"); ("members", "corsOrigin") ]) ]
         ));
    refusal "a log format the document does not declare"
      ~change:(Edit.set_at [ "logFormat" ] (`String "text"))
      (Table ("invalid", [], [ ("other", [ ("pointer", "/logFormat") ]) ]));
    refusal "no capacity"
      ~change:(Edit.remove_at [ "capacity" ])
      (missing "/" "capacity");
    refusal "no --config: there is no default path to fall back on"
      ~args:(fun _ -> [])
      (Table ("unnamed", [], []));
    {
      why = "a --config naming no file";
      said = Table ("unreadable", [ ("path", Lazy.force absent) ], []);
      start =
        (fun s ->
          (settings s, env s, Some (fun _ -> [ "--config"; Lazy.force absent ])));
    };
    (let path =
       lazy
         (temporary
            (Printf.sprintf "gateway-conformance-not-json-%s.json"
               (Fresh.uuid ())))
     in
     {
       why = "a document that is not JSON, naming the file";
       said = Table ("not-json", [ ("path", Lazy.force path) ], []);
       start =
         (fun s ->
           Out_channel.with_open_bin (Lazy.force path) (fun out ->
               output_string out "gatewayUrl = \"http://gateway\"\n");
           (settings s, env s, Some (fun _ -> [ "--config"; Lazy.force path ])));
     });
    refusal "a NATS plane with no servers" ~change:(signal None)
      (missing "/signal" "servers");
    {
      why = "a broker nobody answers at";
      said = Pattern ".";
      start =
        (fun s ->
          let nobody =
            Printf.sprintf "nats://127.0.0.1:%d" (Net.free_port ())
          in
          (settings ~change:(signal (Some nobody)) s, env s, None));
    };
  ]

let refuses r shared =
  let document, env, args = r.start shared in
  let code, output = output_of (refused ?args shared document env) in
  exited_non_zero r.why (code, output);
  holds r.said (code, output);
  let secrets =
    List.filter
      (fun value -> String.length value >= 8)
      (List.map String.trim
         ((match List.assoc_opt "JWT_SECRET" env with
            | Some ring -> String.split_on_char ',' ring
            | None -> [])
         @ Option.to_list (List.assoc_opt "XYNOLOG_OIDC_CLIENT_SECRET" env)))
  in
  List.iter
    (fun secret ->
      Check.that "a secret in the output"
        (not (Re.execp (Re.compile (Re.str secret)) output)))
    secrets

(* A refusal is written as the table's line, and the gateway exits with the
   table's exit code. *)
let refusal_line shared =
  let code, output =
    output_of
      (refused ~args:(fun _ -> []) shared (settings shared) (env shared))
  in
  Check.equal
    (Printf.sprintf "its exit code\n%s" output)
    (`Int (Boot_refusals.exit_code ()))
    (match code with Some c -> `Int c | None -> `Null);
  Boot_refusals.holds ~line:true ~service:Gateway ~schema "unnamed" [] output

let health origin =
  (Call.call origin "GET" (Protocol.path Gateway "GET" "/api/health")).status

let every_address shared =
  with_gateway shared (settings shared) (env shared) (fun gateway ->
      List.iter
        (fun host ->
          Check.that (host ^ " answers")
            (health (Printf.sprintf "http://%s:%d" host gateway.port) = 200))
        [ "127.0.0.1"; "[::1]" ])

let no_unlisted_variable shared =
  with_gateway
    ~unlisted:[ ("TOKIO_WORKER_THREADS", "0") ]
    shared (settings shared) (env shared)
    (fun gateway -> Check.that "it serves" (health gateway.origin = 200))

(* A stream on [origin] carries what [token] emits on it. *)
let carries shared origin =
  let token = Issuer.person shared.issuer "ready" in
  match
    Bus_stream.subscribe origin (Some token)
      (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ())
         ~global:[ "beckon:focus" ] ())
  with
  | Refused reply ->
      failwith
        (Printf.sprintf "subscribe answered %d: %s" reply.status reply.text)
  | Stream stream ->
      ignore
        (Bus_stream.next stream "the first ping" (fun m -> m.event = "ping")
          : Bus_stream.message);
      let mark = Fresh.uuid () in
      let reply =
        Call.call ~token
          ~json:
            (`Assoc
               [
                 ("channel", `String "beckon:focus");
                 ( "payload",
                   `Assoc
                     [
                       ("annotationId", `String mark);
                       ("resourceId", `String "res-1");
                     ] );
               ])
          ~headers:[ ("idempotency-key", Fresh.uuid ()) ]
          origin "POST"
          (Protocol.path Gateway "POST" "/bus/emit")
      in
      status "the emit" 202 reply;
      ignore
        (Bus_stream.frame stream "beckon:focus" ~matches:(fun f ->
             Protocol.at f.payload [ "annotationId" ] = Some (`String mark))
          : Bus_stream.frame);
      Bus_stream.close stream;
      Check.equal "the stream's violations" (`List [])
        (`List
           (List.map (fun v -> `String v) (Dynarray.to_list stream.violations)))

let with_broker ?jetstream ?credentials body =
  let broker = Nats.start_broker ?jetstream ?credentials () in
  Fun.protect
    ~finally:(fun () -> Nats.stop_broker broker)
    (fun () -> body broker)

let broker_credentials shared =
  let user = "CONFORMANCE_BROKER_USER"
  and password = "CONFORMANCE_BROKER_PASSWORD" in
  with_broker ~credentials:{ Nats.user = "gateway"; password = "the-password" }
    (fun broker ->
      let named =
        settings
          ~change:
            (signal (Some broker.url)
               ~named:[ ("userEnv", user); ("passwordEnv", password) ])
          shared
      in
      List.iter
        (fun (why, document, env, said) ->
          let code, output = output_of (refused shared document env) in
          exited_non_zero why (code, output);
          holds said (code, output);
          Check.that
            (why ^ ": the password is not in its output")
            (not (Re.execp (Re.compile (Re.str "the-password")) output)))
        [
          ( "no credentials",
            settings ~change:(signal (Some broker.url)) shared,
            env shared,
            Pattern "." );
          ( "the wrong password",
            named,
            env
              ~changes:
                [ (user, Some "gateway"); (password, Some "not-the-password") ]
              shared,
            Pattern "." );
          ( "a named variable that is unset",
            named,
            env ~changes:[ (password, Some "the-password") ] shared,
            Table ("unset", [ ("field", "/signal/userEnv"); ("name", user) ], [])
          );
        ];
      with_gateway shared named
        (env
           ~changes:[ (user, Some "gateway"); (password, Some "the-password") ]
           shared)
        (fun admitted -> carries shared admitted.origin))

let no_jetstream shared =
  with_broker ~jetstream:false (fun broker ->
      exited_non_zero "a broker with no JetStream"
        (output_of
           (refused shared
              (settings ~change:(signal (Some broker.url)) shared)
              (env shared))))

let held_until_confirmed shared =
  with_broker (fun broker ->
      let proxy = Proxy.start broker.port in
      Fun.protect ~finally:(fun () -> Proxy.close proxy) @@ fun () ->
      Proxy.hold proxy;
      let document = settings ~change:(signal (Some proxy.url)) shared in
      let port = Yojson.Safe.Util.(member "port" document |> to_int) in
      let starting, start = Eio.Promise.create () in
      Eio.Fiber.fork ~sw:(Run.sw ()) (fun () ->
          Eio.Promise.resolve start
            (match started shared document (env shared) with
            | gateway -> Ok gateway
            | exception failure -> Error failure));
      Run.sleep 3.0;
      let early =
        Call.reach
          (Printf.sprintf "http://127.0.0.1:%d" port)
          "GET"
          (Protocol.path Gateway "GET" "/api/health")
      in
      Check.that
        "the gateway answered before the broker had seen its subscriptions"
        (early = None);
      Proxy.release proxy;
      match Eio.Promise.await starting with
      | Error failure -> raise failure
      | Ok gateway ->
          Fun.protect
            ~finally:(fun () -> Gateway.stop gateway)
            (fun () -> carries shared gateway.origin))

let key_ring shared =
  let old_key = Fresh.hex 32 and new_key = Fresh.hex 32 in
  let resource = "res-ring" in
  Hashtbl.replace shared.archivist.resources resource
    { storage_uri = "file://" ^ resource; media_type = "text/plain" };
  Hashtbl.replace shared.archivist.content ("file://" ^ resource) "ring bytes";
  let person = Issuer.person shared.issuer "ring-viewer" in
  let mint origin =
    let reply =
      Call.call ~token:person
        ~json:(`Assoc [ ("resourceId", `String resource) ])
        origin "POST"
        (Protocol.path Gateway "POST" "/api/tokens/media")
    in
    text_member "token" reply
  in
  let opens origin media =
    (Call.call origin "GET"
       (Protocol.path_of Gateway "GET" "/api/resources/{id}"
          [ ("id", resource) ]
       ^ "?token=" ^ media))
      .status = 200
  in
  let keyed ring = env ~changes:[ ("JWT_SECRET", Some ring) ] shared in
  let under_old =
    with_gateway shared (settings shared) (keyed old_key) (fun first ->
        mint first.origin)
  in
  let under_new =
    with_gateway shared (settings shared)
      (keyed (Printf.sprintf " %s , %s " new_key old_key))
      (fun ring ->
        let under_new = mint ring.origin in
        Check.that "the ring opens a token of its second key"
          (opens ring.origin under_old);
        Check.that "the ring opens a token of its first key"
          (opens ring.origin under_new);
        under_new)
  in
  with_gateway shared (settings shared) (keyed new_key) (fun only_new ->
      Check.that "the new key opens its own token"
        (opens only_new.origin under_new);
      Check.that "the new key alone refuses the old key's token"
        (not (opens only_new.origin under_old)))

let tests implementations =
  let shared = Suite.world "gateway/boot" ~make:(make implementations) ~close in
  List.map (fun c ->
      Case.test ~category:[ "starting a gateway" ] c (fun () ->
          c.run (shared ())))
  @@ List.map
       (fun r ->
         case
           (Printf.sprintf
              "refuses to serve with %s, and prints no secret it was given"
              r.why)
           (refuses r))
       refusals
  @ [
      case
        ~known:
          "the gateway at images bb38209 writes its refusal bare, without the \
           line's [fatal] prefix"
        "writes a refusal as boot-refusals.json's line, [fatal] {refusal}, and \
         exits with its exit code"
        refusal_line;
      case
        "listens on every address the host has: the IPv4 and the IPv6 loopback \
         both answer"
        every_address;
      case
        "reads no variable the table does not list, even one its runtime would \
         read for itself: TOKIO_WORKER_THREADS=0, which tokio refuses, changes \
         nothing"
        no_unlisted_variable;
      case
        "reaches a broker that requires credentials through the variables the \
         document names — and refuses without them"
        broker_credentials;
      case
        "refuses to serve with a broker that has no JetStream: the claims \
         table lives there"
        no_jetstream;
      case
        "serves nothing until the broker has confirmed what it registered: \
         held after its handshake, it does not answer; released, it does"
        held_until_confirmed;
      case
        "signs a media token with the first key of JWT_SECRET and accepts one \
         signed by any key of it, whitespace around the keys ignored"
        key_ring;
    ]
