(* The credentials the gateway accepts and mints — the bearerAuth and
   mediaToken schemes in specs/src/openapi.json. One kind of bearer, the
   issuer's, for people and agents alike; the one token the gateway mints is
   a media token. *)

open Xynolog_conformance
open Support
module Tables = Xynolog_protocol.Tables

let users_me = "/api/users/me"
let media_route = "/api/resources/{id}"
let mint_route = "/api/tokens/media"

(* A token's header and payload, as JSON. *)
let decode token =
  let part text =
    Yojson.Safe.from_string
      (Base64.decode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet text)
  in
  match String.split_on_char '.' token with
  | header :: payload :: _ -> (part header, part payload)
  | _ -> failwith (token ^ " is no JWT")

let challenge (reply : Call.reply) =
  match Http.Header.get reply.headers "www-authenticate" with
  | Some challenge -> challenge
  | None -> failwith ("the 401 states no challenge: " ^ Call.shown reply.text)

(* A credential the gateway cannot verify: the challenge names
   invalid_token. *)
let refused_as_invalid (w : World.t) meth path (reply : Call.reply) =
  status "a refused token" 401 reply;
  conforms meth path reply;
  Check.equal "its challenge"
    (`String (Protocol.challenge_refused (World.origin w)))
    (`String (challenge reply))

(* No credential at all: the challenge names no error, and the body a hint
   naming the header. *)
let no_credential (w : World.t) meth path why (reply : Call.reply) =
  status why 401 reply;
  conforms meth path reply;
  Check.equal (why ^ ": its challenge")
    (`String (Protocol.challenge_no_credential (World.origin w)))
    (`String (challenge reply));
  Check.equal (why ^ ": its hint")
    (`String (Option.get (Protocol.message "no-credential").hint))
    (`String (text_member "hint" reply))

let now () = int_of_float (Unix.time ())

let who_am_i ?token ?(headers = []) ?(query = "") (w : World.t) =
  Call.call ?token ~headers (World.origin w) "GET"
    (Protocol.path Gateway "GET" users_me ^ query)

let did_of (reply : Call.reply) = text_member "did" reply

(* The media route of [id], with [query]. *)
let media_path id query =
  Protocol.path_of Gateway "GET" media_route [ ("id", id) ] ^ query

let mint (w : World.t) token json =
  Call.call ~token ~json (World.origin w) "POST"
    (Protocol.path Gateway "POST" mint_route)

let minted (w : World.t) token id =
  text_member "token" (mint w token (`Assoc [ ("resourceId", `String id) ]))

(* A token for the media route, signed with the gateway's own key, or with
   [key]: [claims] alone. *)
let media_claims ?iss ~purpose ~sub ~iat ~exp () =
  [ ("purpose", `String purpose) ]
  @ (match iss with Some iss -> [ ("iss", `String iss) ] | None -> [])
  @ [ ("sub", `String sub); ("iat", `Int iat); ("exp", `Int exp) ]

let names_a_person (w : World.t) =
  let token =
    World.person w
      ~claims:
        [
          ("name", `String "Alice Liddell");
          ("picture", `String "https://pictures.example/alice.png");
        ]
      "alice"
  in
  let reply = who_am_i ~token w in
  status "who am I" 200 reply;
  conforms "GET" users_me reply;
  Check.equal "who am I"
    (`Assoc
       [
         ("did", `String (World.person_did w "alice"));
         ("email", `String "alice@people.example");
         ("name", `String "Alice Liddell");
         ("image", `String "https://pictures.example/alice.png");
         ("domain", `String w.kb.domain);
       ])
    (Option.get reply.json)

let wrong_issuer_tokens (w : World.t) =
  let claims =
    [ ("sub", `String "x"); ("email", `String "x@people.example") ]
  in
  List.iter
    (fun (why, token) ->
      let reply = who_am_i ~token w in
      status why 401 reply;
      conforms "GET" users_me reply;
      Check.that
        (why ^ ": its challenge names invalid_token")
        (Check.matches_re {|^Bearer error="invalid_token"|} (challenge reply)))
    [
      ( "another audience",
        Issuer.token w.issuer
          (("aud", `String "https://elsewhere.example/kb") :: claims) );
      ( "another issuer",
        Issuer.token w.issuer
          (("iss", `String "http://elsewhere.example") :: claims) );
      ("expired", Issuer.token w.issuer ~expires_in:(-60) claims);
      ( "a key the issuer does not publish",
        Issuer.token w.issuer ~kid:"k1" ~key:(Issuer.unpublished_key ()) claims
      );
      ( "an unverified email",
        World.person w ~claims:[ ("email_verified", `Bool false) ] "x" );
      ("an empty subject", World.person w "");
    ]

(* The cases of principals/cases.json under the suite's knowledge base. *)
let principal_cases () =
  let table = (Protocol.tables ()).principals_cases in
  let domain = (Protocol.kb_identity Protocol.suite_kb).domain in
  let under =
    List.filter (fun (c : Tables.Principals_cases.people_item) ->
        c.domain = domain)
  in
  match (under table.people, under table.agents) with
  | [], _ | _, [] ->
      failwith
        ("principals/cases.json needs person and agent cases under " ^ domain
       ^ ", the suite's knowledge base")
  | people, agents -> (people, agents)

let person_named (c : Tables.Principals_cases.people_item) (w : World.t) =
  let me = who_am_i ~token:(World.person w c.subject) w in
  status "who am I" 200 me;
  Check.equal "the person's DID" (`String c.did) (`String (did_of me))

let agent_named (c : Tables.Principals_cases.people_item) (w : World.t) =
  let token =
    Issuer.token w.issuer
      [
        ("sub", `String c.subject);
        (Protocol.kind_claim (), `String (Protocol.agent_kind ()));
        ("roles", `List [ `String (Protocol.service_role ()) ]);
      ]
  in
  let me = who_am_i ~token w in
  status "who am I" 200 me;
  Check.equal "the agent's DID" (`String c.did) (`String (did_of me))

let names_an_agent (w : World.t) =
  let token, did =
    World.agent w
      ~roles:[ Protocol.service_role (); Protocol.worker_role () ]
      "named-agent"
  in
  Check.equal "the token's kind"
    (`String (Protocol.agent_kind ()))
    (Check.at (snd (decode token)) [ Protocol.kind_claim () ]);
  let me = who_am_i ~token w in
  status "who am I" 200 me;
  conforms "GET" users_me me;
  Check.equal "who am I"
    (`Assoc
       [
         ("did", `String did);
         ("email", `Null);
         ("name", `Null);
         ("image", `Null);
         ("domain", `String w.kb.domain);
       ])
    (Option.get me.json);
  Check.contains_text "the agent's DID" ":agents:" did

let unmarked_is_a_person (w : World.t) =
  List.iter
    (fun (why, claims) ->
      let me = who_am_i ~token:(Issuer.token w.issuer claims) w in
      status why 200 me;
      Check.equal why
        (`String (World.person_did w "service-account-unmarked"))
        (`String (did_of me)))
    [
      ( "no kind",
        [
          ("sub", `String "service-account-unmarked");
          ( "roles",
            `List
              [
                `String (Protocol.service_role ());
                `String (Protocol.worker_role ());
              ] );
        ] );
      ( "another kind",
        [
          ("sub", `String "service-account-unmarked");
          ("roles", `List [ `String (Protocol.service_role ()) ]);
          (Protocol.kind_claim (), `String "person");
        ] );
    ]

let no_address (w : World.t) =
  let me =
    who_am_i ~token:(Issuer.token w.issuer [ ("sub", `String "no-address") ]) w
  in
  status "who am I" 200 me;
  conforms "GET" users_me me;
  Check.matching "who am I"
    [ ("did", `String (World.person_did w "no-address")); ("email", `Null) ]
    (Option.get me.json)

let no_agent_route (w : World.t) =
  let service =
    Issuer.service w.issuer ~roles:[ Protocol.service_role () ] "a-sidecar"
  in
  status "POST /api/tokens/agent" 404
    (Call.call ~token:service
       ~json:(`Assoc [ ("provider", `String "p"); ("model", `String "m") ])
       (World.origin w) "POST" "/api/tokens/agent")

let cookie_is_none (w : World.t) =
  let token, _ = World.agent w "cookie" in
  let person = World.person w "cookie-eater" in
  List.iter
    (fun cookie ->
      no_credential w "GET" users_me
        (String.sub cookie 0 (min 30 (String.length cookie)))
        (who_am_i ~headers:[ ("cookie", cookie) ] w))
    [ "xynolog-token=" ^ token; "xynolog-token=" ^ person ];
  status "a bearer beside a cookie" 200
    (who_am_i ~token:person ~headers:[ ("cookie", "xynolog-token=junk") ] w)

let header_shapes (w : World.t) =
  let person = World.person w "header-shapes" in
  List.iter
    (fun (why, authorization) ->
      no_credential w "GET" users_me why
        (who_am_i ~headers:[ ("authorization", authorization) ] w))
    [
      ("another scheme", "Basic dXNlcjpwYXNz");
      ("a token with no scheme", person);
      ("the scheme alone", "Bearer");
      ("the scheme and blanks", "Bearer   ");
    ];
  List.iter
    (fun scheme ->
      status scheme 200
        (who_am_i ~headers:[ ("authorization", scheme ^ " " ^ person) ] w))
    [ "bearer"; "BEARER"; "bEaReR" ];
  List.iter
    (fun (why, token) ->
      let reply =
        who_am_i ~headers:[ ("authorization", "Bearer " ^ token) ] w
      in
      status why 401 reply;
      conforms "GET" users_me reply;
      Check.equal why
        (`Assoc [ ("error", `String (Protocol.message_text "refused-token")) ])
        (Option.get reply.json))
    [
      ("a 10,000-character token", String.make 10_000 'a');
      ("a token with a space in it", person ^ " extra");
      ("a token with quotes and backslashes", "\"" ^ person ^ "\\\"");
    ]

let self_signed_is_no_bearer (w : World.t) =
  let forged iss =
    [
      ("did", `String (w.kb.did ^ ":agents:forged"));
      ("email", `String "forged@agents.example");
      ("name", `String "forged");
      ("domain", `String w.kb.domain);
      ("iss", `String iss);
      ("sub", `String "forged");
      (Protocol.kind_claim (), `String (Protocol.agent_kind ()));
      ("iat", `Int (now ()));
      ("exp", `Int (now () + 600));
    ]
  in
  List.iter
    (fun (why, token) ->
      let reply = who_am_i ~token w in
      status why 401 reply;
      refused_as_invalid w "GET" users_me reply)
    [
      ( "naming the knowledge base as its issuer",
        World.signed w (forged w.kb.domain) );
      ("naming the trusted issuer", World.signed w (forged w.issuer.origin));
    ]

let media_refusals (w : World.t) =
  let id = "res-media-edges" in
  Hashtbl.replace w.archivist.resources id
    { storage_uri = "file://" ^ id; media_type = "text/plain" };
  Hashtbl.replace w.archivist.content ("file://" ^ id) "media bytes";
  let person = World.person w "viewer" in
  let media = minted w person id in
  let opened =
    Call.call (World.origin w) "GET" (media_path id ("?token=" ^ media))
  in
  status "the media route" 200 opened;
  Check.equal "the bytes" (`String "media bytes") (`String opened.text);
  let table = (Protocol.tables ()).gateway_media_token in
  let iss =
    Protocol.fill_template table.claims.iss [ ("domain", w.kb.domain) ]
  in
  let purpose = table.claims.purpose in
  let refused =
    [
      ("garbage", "garbage");
      ( "expired",
        World.signed w
          (media_claims ~iss ~purpose ~sub:id
             ~iat:(now () - 600)
             ~exp:(now () - 300)
             ()) );
      ( "another key",
        World.signed ~key:(Fresh.hex 32) w
          (media_claims ~iss ~purpose ~sub:id ~iat:(now ())
             ~exp:(now () + 300)
             ()) );
      ( "not a media token",
        World.signed w
          (media_claims ~iss ~purpose:"download" ~sub:id ~iat:(now ())
             ~exp:(now () + 300)
             ()) );
      ( "another knowledge base, on the same key ring",
        World.signed w
          (media_claims ~iss:("another." ^ iss) ~purpose ~sub:id ~iat:(now ())
             ~exp:(now () + 300)
             ()) );
      ( "no knowledge base",
        World.signed w
          (media_claims ~purpose ~sub:id ~iat:(now ()) ~exp:(now () + 300) ())
      );
    ]
  in
  List.iter
    (fun (why, token) ->
      let reply =
        Call.call (World.origin w) "GET" (media_path id ("?token=" ^ token))
      in
      status why 401 reply;
      conforms "GET" media_route reply)
    refused;
  (* Anywhere but that GET, a media token is no credential at all. *)
  List.iter
    (fun meth ->
      let reply =
        Call.call (World.origin w) meth (media_path id ("?token=" ^ media))
      in
      status meth 401 reply;
      Check.that
        (meth ^ ": its challenge names no error")
        (not (Re.execp (Re.compile (Re.str "error=")) (challenge reply))))
    [ "POST"; "PUT"; "DELETE" ];
  no_credential w "GET" users_me "a media token on another route"
    (who_am_i ~query:("?token=" ^ media) w)

let changed_address (w : World.t) =
  let before =
    who_am_i
      ~token:
        (World.person w
           ~claims:[ ("email", `String "carol@old.example") ]
           "carol")
      w
  in
  let after =
    who_am_i
      ~token:
        (World.person w
           ~claims:[ ("email", `String "carol@new.example") ]
           "carol")
      w
  in
  Check.equal "the DID before"
    (`String (World.person_did w "carol"))
    (`String (did_of before));
  Check.matching "after"
    [
      ("did", `String (World.person_did w "carol"));
      ("email", `String "carol@new.example");
    ]
    (Option.get after.json);
  (* A person whose token carries no name and no picture. *)
  let plain =
    who_am_i
      ~token:
        (Issuer.token w.issuer
           [
             ("sub", `String "dave");
             ("email", `String "dave@people.example");
             ("email_verified", `Bool true);
           ])
      w
  in
  status "who am I" 200 plain;
  conforms "GET" users_me plain;
  Check.matching "who am I"
    [
      ("did", `String (World.person_did w "dave"));
      ("name", `Null);
      ("image", `Null);
    ]
    (Option.get plain.json)

let agent_other_signature (w : World.t) =
  let token, _ = World.agent w "m" in
  match String.split_on_char '.' token with
  | header :: payload :: _ ->
      let forged =
        String.concat "."
          [
            header;
            payload;
            Base64.encode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet
              "not the signature";
          ]
      in
      refused_as_invalid w "GET" users_me (who_am_i ~token:forged w)
  | _ -> failwith (token ^ " is no JWT")

let media_token_shape (w : World.t) =
  let token = World.person w "viewer" in
  let reply = mint w token (`Assoc [ ("resourceId", `String "res-media-1") ]) in
  status "the mint" 200 reply;
  conforms "POST" mint_route reply;
  let header, payload = decode (text_member "token" reply) in
  let table = (Protocol.tables ()).gateway_media_token in
  Check.equal "its alg" (`String table.header.alg) (Check.at header [ "alg" ]);
  Check.matching "its claims"
    [
      ("purpose", `String table.claims.purpose);
      ( "iss",
        `String
          (Protocol.fill_template table.claims.iss [ ("domain", w.kb.domain) ])
      );
      ( "sub",
        `String
          (Protocol.fill_template table.claims.sub
             [ ("resourceId", "res-media-1") ]) );
    ]
    payload;
  let seconds name =
    match Check.at payload [ name ] with
    | `Int n -> n
    | other -> failwith (name ^ " is " ^ Yojson.Safe.to_string other)
  in
  Check.that "it lives its lifetime"
    (seconds "exp" - seconds "iat" = table.lifetime_seconds);
  let missing = mint w token (`Assoc []) in
  status "a mint naming nothing" 400 missing;
  conforms "POST" mint_route missing

let media_not_for_non_ids (w : World.t) =
  let token = World.person w "viewer" in
  let kind =
    List.find
      (fun (k : Tables.Identifiers_kinds.kinds_item) -> k.schema = "ResourceId")
      (Protocol.tables ()).identifiers_kinds.kinds
  in
  List.iter
    (fun ({ id; why } : Tables.Identifiers_kinds.accepts_item) ->
      let reply = mint w token (`Assoc [ ("resourceId", `String id) ]) in
      status (Printf.sprintf "%S: %s" id why) 400 reply;
      conforms "POST" mint_route reply)
    kind.refuses

let media_opens_one (w : World.t) =
  let person = World.person w "viewer" in
  let media = minted w person "res-media-2" in
  let other =
    Call.call (World.origin w) "GET"
      (media_path "res-media-3" ("?token=" ^ media))
  in
  status "another resource" 401 other;
  conforms "GET" media_route other;
  status "the bearer route" 401
    (Call.call (World.origin w) "GET"
       (Protocol.path_of Gateway "GET" "/resources/{id}"
          [ ("id", "res-media-2") ]
       ^ "?token=" ^ media));
  status "as a bearer" 401 (who_am_i ~token:media w)

let other_subject_claim (w : World.t) =
  let named =
    who_am_i
      ~token:
        (World.person w
           ~claims:[ ("preferred_username", `String "erin") ]
           "sub-is-ignored")
      w
  in
  status "named by preferred_username" 200 named;
  Check.equal "the DID"
    (`String (World.person_did w "erin"))
    (`String (did_of named));
  refused_as_invalid w "GET" users_me
    (who_am_i ~token:(World.person w "frank") w)

let unreachable_issuer (w : World.t) =
  let token = World.person w "stranded" in
  Issuer.close w.issuer;
  refused_as_invalid w "GET" users_me (who_am_i ~token w)

let key_rotation (w : World.t) =
  status "before" 200 (who_am_i ~token:(World.person w "before") w);
  Issuer.add_key w.issuer "k-rotated";
  Run.sleep (key_refetch_cooldown () +. 1.0);
  status "after" 200 (who_am_i ~token:(World.person w "after") w)

let tests implementations =
  let people, agents = principal_cases () in
  let make = plain implementations in
  let credentials =
    [
      case
        "an issuer token names a person under the knowledge base's domain, by \
         the subject claim"
        names_a_person;
      case
        "an issuer token is refused when its audience, issuer, expiry, \
         signature or claims are wrong"
        wrong_issuer_tokens;
    ]
    @ List.map
        (fun (c : Tables.Principals_cases.people_item) ->
          case
            ("a person is named exactly by their subject \
              (principals/cases.json): " ^ c.why)
            (person_named c))
        people
    @ List.map
        (fun (c : Tables.Principals_cases.people_item) ->
          case
            ("an agent is named exactly by its subject \
              (principals/cases.json): " ^ c.why)
            (agent_named c))
        agents
    @ [
        case
          "an issuer token whose kind is agent names an agent, with what its \
           token carries and no address it lacks"
          names_an_agent;
        case
          "a token that does not say it is an agent's names a person, whatever \
           roles it carries"
          unmarked_is_a_person;
        case
          "a token without an email is admitted: an account need not have an \
           address"
          no_address;
        case
          "the gateway mints no agent token: POST /api/tokens/agent is no route"
          no_agent_route;
        case
          "a cookie is not a credential: alone it is none, and beside a bearer \
           token it changes nothing"
          cookie_is_none;
        case
          "an Authorization header that is not a bearer credential is none; \
           the scheme is matched in any case, and whatever follows it is the \
           token"
          header_shapes;
        case
          "a token the gateway signed itself is no bearer: only the issuer's \
           are"
          self_signed_is_no_bearer;
        case
          "a media token is refused when it is not one, has expired, was \
           signed with a key the gateway does not hold, or names another \
           knowledge base or none; and it never opens a write"
          media_refusals;
        case
          "a person is named by their subject: a changed address keeps the \
           DID, and absent name and picture are null"
          changed_address;
        case "an agent's token signed with another key is refused"
          agent_other_signature;
        case
          "a media token names its knowledge base and one resource, for five \
           minutes"
          media_token_shape;
        case
          "a media token is not minted for what is not a resource's id \
           (identifiers/kinds.json)"
          media_not_for_non_ids;
        case
          "a media token opens only the resource it names, and only on the \
           media route"
          media_opens_one;
      ]
  in
  each_plane ~make ~world:Fun.id ~close:World.close "credentials" credentials
  (* Plane-independent: which claim names a person is the knowledge base's
     choice. *)
  @ each_plane ~planes:[ In_process ]
      ~make:(fun plane ->
        World.create ~command:implementations.Implementations.gateway
          ~document:
            (Edit.set_at
               [ "identity"; "subjectClaim" ]
               (`String "preferred_username"))
          plane)
      ~world:Fun.id ~close:World.close
      "a knowledge base that names people by another claim"
      [
        case
          "names a person by the claim identity.subjectClaim selects, and \
           refuses a token without it"
          other_subject_claim;
      ]
  (* Plane-independent: the issuer's keys are fetched when first needed, so
     an issuer gone before then leaves nothing to verify with. *)
  @ each_plane ~planes:[ In_process ] ~make ~world:Fun.id ~close:World.close
      "an issuer that cannot be reached"
      [
        case "refuses its tokens as unverifiable — a 401, never a 5xx"
          unreachable_issuer;
      ]
  (* Plane-independent, and slow: the verifier refetches the issuer's keys
     on a kid it has not seen at most once per cooldown (bearerAuth's
     x-xynolog-limits.keyRefetchCooldownSeconds), so the case waits one out.
     Its own world, because every token signed after it uses the new key. *)
  @ each_plane ~planes:[ In_process ] ~make ~world:Fun.id ~close:World.close
      "issuer key rotation"
      [
        case
          "a key the issuer adds is accepted without restarting the gateway, \
           once the key-fetch cooldown has passed"
          key_rotation;
      ]
