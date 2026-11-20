(* The harness's issuer admits a key account as an issuer does: an
   assertion signed with the registered key, for this token endpoint and not
   expired, is answered once; one signed by another key, expired, addressed
   elsewhere or sent a second time is refused
   (specs/src/session/client-assertion-cases.json). A grant a case tells it
   to fail is answered 503, or not answered at all. *)

open Xynolog_conformance
open Expect

let client = "xynolog-worker-alice-laptop"

let generated () =
  Lazy.force Fresh.seeded;
  Mirage_crypto_pk.Rsa.generate ~bits:2048 ()

(* The account's key, and another. *)
let keys = lazy (generated (), generated ())
let encoded = Base64.encode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet

let assertion key claims =
  let input =
    encoded {|{"alg":"RS256"}|}
    ^ "."
    ^ encoded (Yojson.Safe.to_string (`Assoc claims))
  in
  input ^ "."
  ^ encoded
      (Mirage_crypto_pk.Rsa.PKCS1.sign ~hash:`SHA256 ~key (`Message input))

let now () = int_of_float (Unix.time ())

(* An issuer with the account registered by its key's public half, and its
   token endpoint. *)
let started () =
  let key, _ = Lazy.force keys in
  let issuer =
    Issuer.start "https://kb.example"
      [
        ( client,
          {
            Issuer.proof = Key (Mirage_crypto_pk.Rsa.pub_of_priv key);
            roles = [ Protocol.service_role (); Protocol.worker_role () ];
          } );
      ]
  in
  (issuer, issuer.origin ^ "/token")

(* The assertion's claims for [endpoint], with [changed] over them. *)
let claims ?(changed = []) endpoint =
  List.map
    (fun (name, value) ->
      (name, Option.value ~default:value (List.assoc_opt name changed)))
    [
      ("iss", `String client);
      ("sub", `String client);
      ("aud", `String endpoint);
      ("jti", `String (Fresh.uuid ()));
      ("iat", `Int (now ()));
      ("exp", `Int (now () + 60));
    ]

let posted (issuer : Issuer.t) form =
  Call.call
    ~headers:[ ("content-type", "application/x-www-form-urlencoded") ]
    ~body:(Uri.encoded_of_query (List.map (fun (k, v) -> (k, [ v ])) form))
    issuer.origin "POST" "/token"

let grant ?(assertion_type = Issuer.assertion_type ()) issuer signed =
  posted issuer
    [
      ("grant_type", "client_credentials");
      ("client_assertion_type", assertion_type);
      ("client_assertion", signed);
    ]

let with_issuer f =
  let issuer, endpoint = started () in
  Fun.protect ~finally:(fun () -> Issuer.close issuer) @@ fun () ->
  f issuer endpoint

let grants_by_key () =
  with_issuer @@ fun issuer endpoint ->
  let key, _ = Lazy.force keys in
  let answer = grant issuer (assertion key (claims endpoint)) in
  check
    (Printf.sprintf "it is answered 200, not %d" answer.status)
    (answer.status = 200);
  let token =
    match Option.map (Yojson.Safe.Util.member "access_token") answer.json with
    | Some (`String token) -> token
    | _ -> failwith ("the answer carries no access_token: " ^ answer.text)
  in
  let body =
    match String.split_on_char '.' token with
    | [ _; body; _ ] ->
        Yojson.Safe.from_string
          (Base64.decode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet body)
    | _ -> failwith ("the token is no JWT: " ^ token)
  in
  check "the token names the account"
    (Yojson.Safe.Util.member "azp" body = `String client);
  check "and carries its roles"
    (Yojson.Safe.Util.member "roles" body
    = `List
        [
          `String (Protocol.service_role ()); `String (Protocol.worker_role ());
        ]);
  check "the grant is counted" (Issuer.granted issuer.grants client = 1)

let refuses made () =
  with_issuer @@ fun issuer endpoint ->
  let answer = grant issuer (made endpoint) in
  check
    (Printf.sprintf "it is answered 401, not %d" answer.status)
    (answer.status = 401)

let refused_by_key =
  refuses (fun endpoint -> assertion (snd (Lazy.force keys)) (claims endpoint))

let mine changed endpoint =
  assertion (fst (Lazy.force keys)) (claims ~changed endpoint)

let refused_expired = refuses (mine [ ("exp", `Int (now () - 1)) ])

let refused_elsewhere =
  refuses (mine [ ("aud", `String "https://elsewhere.example/token") ])

let refused_subject = refuses (mine [ ("sub", `String "xynolog-gateway") ])

let refuses_a_second_time () =
  with_issuer @@ fun issuer endpoint ->
  let once = mine [] endpoint in
  check "the first is answered 200" ((grant issuer once).status = 200);
  check "the second is answered 401" ((grant issuer once).status = 401)

let refuses_another_type () =
  with_issuer @@ fun issuer endpoint ->
  check "an assertion of another type is answered 401"
    ((grant
        ~assertion_type:
          "urn:ietf:params:oauth:client-assertion-type:saml2-bearer"
        issuer (mine [] endpoint))
       .status = 401);
  check "a secret for a key account is answered 401"
    ((posted issuer
        [
          ("grant_type", "client_credentials");
          ("client_id", client);
          ("client_secret", "anything");
        ])
       .status = 401)

let fails_as_told () =
  with_issuer @@ fun issuer endpoint ->
  Queue.add Issuer.Unavailable issuer.failing;
  Queue.add Issuer.Unanswered issuer.failing;
  check "the first is answered 503"
    ((grant issuer (mine [] endpoint)).status = 503);
  (match grant issuer (mine [] endpoint) with
  | answer ->
      failwith
        (Printf.sprintf "the second was answered %d: %s" answer.status
           answer.text)
  | exception (Eio.Io _ | End_of_file) -> ());
  check "the third is granted" ((grant issuer (mine [] endpoint)).status = 200);
  check "it met each failure, in order"
    (List.map fst (Dynarray.to_list issuer.met)
    = [ Issuer.Unavailable; Unanswered ]);
  check "and has none left" (Queue.is_empty issuer.failing);
  check "only the grant is counted" (Issuer.granted issuer.grants client = 1)

let tests =
  List.map
    (fun (name, case) -> Testo.create ~category:[ "issuer" ] name case)
    [
      ( "answers an assertion signed with the registered key with a token \
         carrying the account and its roles",
        grants_by_key );
      ("refuses one signed by another key", refused_by_key);
      ("refuses one expired", refused_expired);
      ("refuses one addressed to another token endpoint", refused_elsewhere);
      ("refuses one naming another subject than its issuer", refused_subject);
      ("refuses an assertion sent a second time", refuses_a_second_time);
      ( "refuses an assertion of another type, and a secret for a key account",
        refuses_another_type );
      ( "fails the grants it is told to fail, in order, and grants again after \
         them",
        fails_as_told );
    ]
