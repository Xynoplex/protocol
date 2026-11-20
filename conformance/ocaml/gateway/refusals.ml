(* What bounds a request whose credential is refused (the bearer scheme's
   x-xynolog-limits: refusalsPerSecond, refusalBurst, refusalHolders).

   A refused request is counted against the holder its token names, read
   without verifying it: the token's iss, and the claim the gateway names a
   holder by — the subject claim for the trusted issuer's tokens, did for any
   other. Past its holder's allowance, a refused request is answered 429
   refusal-rate and told when to return. A request that names no holder is
   counted in one count of its own. A token that verifies is never counted,
   and never limited, whatever its holder's count.

   The cases run in order on one gateway, and only the last fills the count
   of requests that name no holder: the ones before it rely on its being
   empty. *)

open Xynolog_conformance
open Support
open Case

let me = "/api/users/me"
let burst () = Protocol.scheme_limit "bearerAuth" "refusalBurst"
let holders () = Protocol.scheme_limit "bearerAuth" "refusalHolders"

(* An issuer token for [sub] that has expired: refused, and naming its
   holder. *)
let expired (w : World.t) sub =
  Issuer.token w.issuer ~expires_in:(-60)
    [
      ("sub", `String sub);
      ("email", `String (sub ^ "@people.example"));
      ("jti", `String (Fresh.uuid ()));
    ]

(* An agent's issuer token for [sub] that has expired: refused, and naming
   its holder. *)
let expired_agent (w : World.t) sub =
  Issuer.token w.issuer ~expires_in:(-60)
    [
      ("sub", `String sub);
      (Protocol.kind_claim (), `String (Protocol.agent_kind ()));
      ("jti", `String (Fresh.uuid ()));
    ]

(* A token naming an issuer the gateway does not trust: refused, and naming
   no holder it counts. *)
let elsewhere (w : World.t) =
  World.signed w ~key:"a key the gateway does not hold"
    [
      ("sub", `String ("elsewhere-" ^ Fresh.uuid ()));
      ("iss", `String "http://elsewhere.example");
      ("jti", `String (Fresh.uuid ()));
    ]

let agent_sub () = "refused-agent-" ^ Fresh.uuid ()

(* [count] tokens, made before any is sent, so sending them takes no longer
   than the sending. *)
let tokens count make = List.init count (fun _ -> make ())

type asked = Token of string | Authorization of string | Nothing

let ask (w : World.t) asked =
  let path = Protocol.path Gateway "GET" me in
  match asked with
  | Token token -> Call.call ~token (World.origin w) "GET" path
  | Authorization header ->
      Call.call
        ~headers:[ ("authorization", header) ]
        (World.origin w) "GET" path
  | Nothing -> Call.call (World.origin w) "GET" path

(* Every token sent at once; what each was answered. *)
let send_all w sent = Eio.Fiber.List.map (fun token -> ask w (Token token)) sent
let statuses replies = List.map (fun (r : Call.reply) -> r.status) replies

let none_but expected why replies =
  match List.filter (fun s -> s <> expected) (statuses replies) with
  | [] -> ()
  | others ->
      failwith
        (Printf.sprintf "%s: answered %s" why
           (String.concat ", " (List.map string_of_int others)))

(* The refusals past the allowance: each a conforming 429 refusal-rate that
   says when to return. *)
let past_the_allowance why replies =
  (match List.filter (fun s -> s <> 401 && s <> 429) (statuses replies) with
  | [] -> ()
  | others ->
      failwith
        (Printf.sprintf "%s: every request refused, 401 or 429, not %s" why
           (String.concat ", " (List.map string_of_int others))));
  let limited = List.filter (fun (r : Call.reply) -> r.status = 429) replies in
  List.iteri
    (fun at (reply : Call.reply) ->
      if at < 3 then (
        conforms "GET" me reply;
        Check.that
          (why ^ ": its code is " ^ Protocol.message_code "refusal-rate")
          (text_member "code" reply = Protocol.message_code "refusal-rate");
        Check.that (why ^ ": Retry-After")
          (match Http.Header.get reply.headers "retry-after" with
          | Some after -> (
              match float_of_string_opt after with
              | Some s -> s >= 1.0
              | None -> false)
          | None -> false)))
    limited;
  limited

let refused_past why replies =
  Check.that
    (why ^ ": refused past the allowance")
    (past_the_allowance why replies <> [])

let one_holder (w : World.t) =
  let sub = "refused-" ^ Fresh.uuid () in
  let replies = send_all w (tokens (burst () + 20) (fun () -> expired w sub)) in
  refused_past "one holder" replies;
  Check.that "a holder new to the gateway starts with its whole allowance"
    (List.length (List.filter (fun s -> s = 401) (statuses replies)) >= burst ())

let verified_served (w : World.t) =
  let sub = "refused-" ^ Fresh.uuid () in
  let draining = tokens (burst () + 20) (fun () -> expired w sub) in
  let meanwhile = tokens 20 (fun () -> expired w sub) in
  let good = World.person w sub
  and other = World.person w ("another-" ^ Fresh.uuid ()) in
  refused_past "one holder" (send_all w draining);
  match
    Eio.Fiber.List.map (ask w)
      (Token good :: Token other :: List.map (fun t -> Token t) meanwhile)
  with
  | served :: also_served :: refused ->
      refused_past "the same holder, meanwhile" refused;
      status "its good token is served" 200 served;
      status "another holder is served" 200 also_served
  | _ -> assert false

let agents_by_subject (w : World.t) =
  let a = agent_sub () in
  let draining = tokens (burst () + 20) (fun () -> expired_agent w a) in
  let another = expired_agent w (agent_sub ()) in
  refused_past "one agent" (send_all w draining);
  match Eio.Fiber.List.map (ask w) [ Token another; Nothing ] with
  | [ b; unnamed ] ->
      status "another subject is counted apart" 401 b;
      status
        "the agent's refusals are not in the count of requests that name no \
         holder"
        401 unnamed
  | _ -> assert false

let at_most_holders (w : World.t) =
  let oldest = agent_sub () in
  let draining = tokens (burst () + 20) (fun () -> expired_agent w oldest) in
  let newcomers =
    tokens (holders ()) (fun () -> expired_agent w (agent_sub ()))
  in
  let again = tokens (burst ()) (fun () -> expired_agent w oldest) in
  refused_past "the oldest" (send_all w draining);
  (* As many new holders as the gateway counts, each refused once: every
     holder it counted before is dropped. *)
  let rec in_batches = function
    | [] -> ()
    | rest ->
        let batch = List.filteri (fun i _ -> i < 64) rest
        and later = List.filteri (fun i _ -> i >= 64) rest in
        none_but 401 "a newcomer starts with its whole allowance"
          (send_all w batch);
        in_batches later
  in
  in_batches newcomers;
  (* Dropped, the oldest starts again with its whole allowance: a whole
     burst at once is refused 401, none 429. Kept, it would hold only what
     it earned back since it was drained: less than a burst, while that took
     under refusalBurst ÷ refusalsPerSecond seconds. *)
  none_but 401 "the oldest was dropped, and starts again" (send_all w again)

let unnamed_share_a_count (w : World.t) =
  let named = expired w ("refused-" ^ Fresh.uuid ())
  and agent = expired_agent w (agent_sub ()) in
  let foreign =
    Array.of_list (tokens (burst () + 20) (fun () -> elsewhere w))
  in
  let unnamed =
    Eio.Fiber.List.map (ask w)
      (List.init
         (burst () + 20)
         (fun i ->
           match i mod 3 with
           | 0 -> Nothing
           | 1 -> Authorization "Bearer not-a-token"
           | _ -> Token foreign.(i)))
  in
  refused_past "no holder" unnamed;
  match Eio.Fiber.List.map (ask w) [ Token named; Token agent ] with
  | [ person; software ] ->
      status "a refused token naming a person is its holder's to count" 401
        person;
      status "so is a refused agent token, named by its subject" 401 software
  | _ -> assert false

let tests implementations =
  each_plane ~make:(plain implementations) ~world:Fun.id ~close:World.close
    "what bounds a refused request"
    [
      case
        "refused requests naming one holder, each with a different token, are \
         answered 401 up to the allowance and 429 refusal-rate past it"
        one_holder;
      case
        "a token that verifies is served whatever its holder's count: the same \
         subject's, and another's"
        verified_served;
      case
        "refused agent tokens are counted by their subject, as a person's are: \
         two apart, and neither in the count of requests that name no holder"
        agents_by_subject;
      case
        "the gateway counts at most refusalHolders holders: a new one drops \
         the oldest, which starts again with its whole allowance"
        at_most_holders;
      case
        "a request with no token, one whose bearer is not a token, and one \
         naming another issuer share one count; filling it limits no request \
         whose token names a holder"
        unnamed_share_a_count;
    ]
