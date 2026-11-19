(* The knowledge base's trusted issuer, served over HTTP: OIDC discovery,
   the key set at the path discovery names, and a token endpoint answering
   the client-credentials grant for the service accounts it is given, each
   proving itself by its client secret. Tokens are RS256, signed with the
   one key the key set publishes. *)

type account = { secret : string; roles : string list }

type t = {
  origin : string;
  audience : string;
  key : Jose.Jwk.priv Jose.Jwk.t;
  grants : (string, int) Hashtbl.t;  (** Grants answered, per client id. *)
  server : Serve.t;
}

let subject_of client = "service-account-" ^ client

let sign key ~issuer ~audience claims =
  let now = int_of_float (Unix.time ()) in
  let payload =
    List.fold_left
      (fun payload (name, value) -> Jose.Jwt.add_claim name value payload)
      Jose.Jwt.empty_payload
      ([
         ("iss", `String issuer);
         ("aud", `String audience);
         ("iat", `Int now);
         ("exp", `Int (now + 600));
       ]
      @ claims)
  in
  match
    Jose.Jwt.sign ~header:(Jose.Header.make_header ~alg:`RS256 key) ~payload key
  with
  | Ok jwt -> Jose.Jwt.to_string jwt
  | Error (`Msg why) -> failwith ("the issuer could not sign a token: " ^ why)

let token t claims = sign t.key ~issuer:t.origin ~audience:t.audience claims

(* A person's token: [sub] names them, with a verified address and a name. *)
let person t ?(claims = []) sub =
  token t
    ([
       ("sub", `String sub);
       ("email", `String (sub ^ "@people.example"));
       ("email_verified", `Bool true);
       ("name", `String ("Person " ^ sub));
     ]
    @ claims)

(* A service account's token, as the token endpoint issues it: an agent's
   account, which says so. *)
let service t ?(roles = [ Protocol.service_role () ]) client =
  token t
    [
      ("sub", `String (subject_of client));
      ("azp", `String client);
      ("roles", `List (List.map (fun r -> `String r) roles));
      (Protocol.kind_claim (), `String (Protocol.agent_kind ()));
    ]

let jwks key =
  match Jose.Jwk.to_pub_json (Jose.Jwk.pub_of_priv key) with
  | `Assoc members ->
      `Assoc
        [ ("keys", `List [ `Assoc (members @ [ ("alg", `String "RS256") ]) ]) ]
  | _ -> failwith "a public key is a JSON object"

(* The claims of a token this issuer signed for [audience], still in date;
   None for any other. *)
let verified t token =
  match
    Jose.Jwt.of_string
      ~jwk:(Jose.Jwk.pub_of_priv t.key)
      ~now:(Ptime_clock.now ()) token
  with
  | Ok jwt
    when Jose.Jwt.get_string_claim jwt "iss" = Some t.origin
         && Jose.Jwt.get_string_claim jwt "aud" = Some t.audience ->
      Some jwt
  | _ -> None

let start audience accounts =
  Lazy.force Fresh.seeded;
  let key =
    Jose.Jwk.make_priv_rsa ~use:`Sig
      (Mirage_crypto_pk.Rsa.generate ~bits:2048 ())
  in
  let grants = Hashtbl.create 8 in
  let origin = ref "" in
  let handler (request : Serve.request) =
    let json status body = Serve.Reply (Serve.json_reply status body) in
    match (request.meth, request.path) with
    | "GET", "/.well-known/openid-configuration" ->
        json 200
          (`Assoc
             [
               ("issuer", `String !origin);
               ("jwks_uri", `String (!origin ^ "/keys"));
               ("token_endpoint", `String (!origin ^ "/token"));
             ])
    | "GET", "/keys" -> json 200 (jwks key)
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
                       (sign key ~issuer:!origin ~audience
                          [
                            ("sub", `String (subject_of client));
                            ("azp", `String client);
                            ( "roles",
                              `List
                                (List.map (fun r -> `String r) account.roles) );
                            ( Protocol.kind_claim (),
                              `String (Protocol.agent_kind ()) );
                          ]) );
                   ("token_type", `String "Bearer");
                   ("expires_in", `Int 300);
                 ])
        | _ -> json 401 (`Assoc [ ("error", `String "invalid_client") ]))
    | _ -> json 404 (`Assoc [ ("error", `String "not found") ])
  in
  let server = Serve.start handler in
  origin := server.origin;
  { origin = server.origin; audience; key; grants; server }

let close t = t.server.stop ()
