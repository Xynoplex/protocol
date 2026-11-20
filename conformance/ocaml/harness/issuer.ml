(* The knowledge base's trusted issuer, served over HTTP: OIDC discovery,
   the key set at the path discovery names, and a token endpoint answering
   the client-credentials grant for the service accounts it is given, each
   proving itself by its client secret. Tokens are RS256, signed with the
   newest key the key set publishes, under its key id. A case can add a key,
   and can have the key set answered 500, or accepted and never answered. *)

type account = { secret : string; roles : string list }

(* What a request of the key set is answered. *)
type keys_answer = Published | Failing | Silent
type key = { kid : string; priv : Jose.Jwk.priv Jose.Jwk.t }

type t = {
  origin : string;
  audience : string;
  mutable keys : key list;  (** Oldest first; the newest signs. *)
  mutable keys_answer : keys_answer;
  mutable key_fetches : int;  (** Requests of the key set. *)
  grants : (string, int) Hashtbl.t;  (** Grants answered, per client id. *)
  server : Serve.t;
}

let subject_of client = "service-account-" ^ client

let rsa_key kid =
  Lazy.force Fresh.seeded;
  {
    kid;
    priv =
      Jose.Jwk.make_priv_rsa ~use:`Sig
        (Mirage_crypto_pk.Rsa.generate ~bits:2048 ());
  }

(* A key the key set does not publish: what a forger signs with. *)
let unpublished_key () = (rsa_key "unpublished").priv

let sign_with ~kid priv payload =
  match
    Jose.Jwt.sign
      ~header:
        (Jose.Header.make_header ~typ:"JWT" ~alg:`RS256
           ~extra:[ ("kid", `String kid) ]
           priv)
      ~payload priv
  with
  | Ok jwt -> Jose.Jwt.to_string jwt
  | Error (`Msg why) -> failwith ("the issuer could not sign a token: " ^ why)

let payload_of claims =
  List.fold_left
    (fun payload (name, value) -> Jose.Jwt.add_claim name value payload)
    Jose.Jwt.empty_payload claims

(* A token of this issuer, for its audience, issued now and expiring
   [expires_in] seconds from now (a negative number: in the past): [claims]
   over those. Signed with the newest key, or with the published key [kid],
   or with [key] under [kid]. *)
let token t ?kid ?key ?(expires_in = 600) claims =
  let now = int_of_float (Unix.time ()) in
  let newest = List.nth t.keys (List.length t.keys - 1) in
  let kid = Option.value kid ~default:newest.kid in
  let priv =
    match key with
    | Some key -> key
    | None -> (
        match List.find_opt (fun k -> k.kid = kid) t.keys with
        | Some k -> k.priv
        | None -> failwith ("the issuer publishes no key " ^ kid))
  in
  let defaults =
    [
      ("iss", `String t.origin);
      ("aud", `String t.audience);
      ("iat", `Int now);
      ("exp", `Int (now + expires_in));
    ]
  in
  sign_with ~kid priv
    (payload_of
       (List.filter (fun (name, _) -> not (List.mem_assoc name claims)) defaults
       @ claims))

(* A person's token: [sub] names them, with a verified address and a name,
   which [claims] may change. *)
let person t ?(claims = []) sub =
  let given =
    [
      ("sub", `String sub);
      ("email", `String (sub ^ "@people.example"));
      ("email_verified", `Bool true);
      ("name", `String ("Person " ^ sub));
    ]
  in
  token t
    (List.filter (fun (name, _) -> not (List.mem_assoc name claims)) given
    @ claims)

let service_claims ~roles client =
  [
    ("sub", `String (subject_of client));
    ("azp", `String client);
    ("roles", `List (List.map (fun r -> `String r) roles));
    (Protocol.kind_claim (), `String (Protocol.agent_kind ()));
  ]

(* A service account's token, as the token endpoint issues it: an agent's
   account, which says so. *)
let service t ?(roles = [ Protocol.service_role () ]) client =
  token t (service_claims ~roles client)

(* Publish a new key, which signs every token from now on. *)
let add_key t kid = t.keys <- t.keys @ [ rsa_key kid ]

let jwks t =
  `Assoc
    [
      ( "keys",
        `List
          (List.map
             (fun { kid; priv } ->
               match Jose.Jwk.to_pub_json (Jose.Jwk.pub_of_priv priv) with
               | `Assoc members ->
                   `Assoc
                     (List.filter (fun (k, _) -> k <> "kid") members
                     @ [ ("kid", `String kid); ("alg", `String "RS256") ])
               | _ -> failwith "a public key is a JSON object")
             t.keys) );
    ]

(* The claims of a token this issuer signed for [audience], still in date;
   None for any other. *)
let verified t token =
  List.find_map
    (fun { priv; _ } ->
      match
        Jose.Jwt.of_string
          ~jwk:(Jose.Jwk.pub_of_priv priv)
          ~now:(Ptime_clock.now ()) token
      with
      | Ok jwt
        when Jose.Jwt.get_string_claim jwt "iss" = Some t.origin
             && Jose.Jwt.get_string_claim jwt "aud" = Some t.audience ->
          Some jwt
      | _ -> None)
    t.keys

let start audience accounts =
  let grants = Hashtbl.create 8 in
  let self = ref None in
  let issuer () = Option.get !self in
  let handler (request : Serve.request) =
    let t = issuer () in
    let json status body = Serve.Reply (Serve.json_reply status body) in
    match (request.meth, request.path) with
    | "GET", "/.well-known/openid-configuration" ->
        json 200
          (`Assoc
             [
               ("issuer", `String t.origin);
               ("jwks_uri", `String (t.origin ^ "/keys"));
               ("token_endpoint", `String (t.origin ^ "/token"));
             ])
    | "GET", "/keys" -> (
        t.key_fetches <- t.key_fetches + 1;
        match t.keys_answer with
        | Published -> json 200 (jwks t)
        | Failing -> json 500 (`Assoc [ ("error", `String "unavailable") ])
        | Silent -> Serve.Held (fst (Eio.Promise.create ())))
    | "POST", "/token" -> (
        let form = Uri.query_of_encoded request.body in
        let field name =
          Option.bind (List.assoc_opt name form) (function
            | v :: _ -> Some v
            | [] -> None)
        in
        let client = Option.value ~default:"" (field "client_id") in
        match List.assoc_opt client accounts with
        | Some account
          when field "grant_type" = Some "client_credentials"
               && field "client_secret" = Some account.secret ->
            Hashtbl.replace grants client
              (1 + Option.value ~default:0 (Hashtbl.find_opt grants client));
            json 200
              (`Assoc
                 [
                   ( "access_token",
                     `String
                       (token t (service_claims ~roles:account.roles client)) );
                   ("token_type", `String "Bearer");
                   ("expires_in", `Int 300);
                 ])
        | _ -> json 401 (`Assoc [ ("error", `String "invalid_client") ]))
    | _ -> json 404 (`Assoc [ ("error", `String "not found") ])
  in
  let server = Serve.start handler in
  let t =
    {
      origin = server.origin;
      audience;
      keys = [ rsa_key "k1" ];
      keys_answer = Published;
      key_fetches = 0;
      grants;
      server;
    }
  in
  self := Some t;
  t

let close t = t.server.stop ()
