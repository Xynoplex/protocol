(* The trusted issuer's key set as the gateway fetches it: the deadline one
   fetch is given, and what a fetch that fails costs the requests after it.
   The timings are the bearerAuth scheme's x-xynolog-limits in
   specs/src/openapi.json, whose description states the rule.

   Plane-independent, and slow: each world waits out a deadline or a
   cooldown. Each has a world of its own, because the gateway holds the keys
   it fetched. *)

open Xynolog_conformance
open Support
open Case

let deadline () =
  float_of_int (Protocol.scheme_limit "bearerAuth" "keyFetchDeadlineSeconds")

let who_am_i (w : World.t) token =
  Call.call ~token (World.origin w) "GET"
    (Protocol.path Gateway "GET" "/api/users/me")

let refused reply =
  status "a token the gateway cannot verify" 401 reply;
  conforms "GET" "/api/users/me" reply

let silent (w : World.t) =
  w.issuer.keys_answer <- Silent;
  let started = Run.now () in
  let replies =
    Eio.Fiber.List.map
      (fun (wait, sub) ->
        Run.sleep wait;
        who_am_i w (World.person w sub))
      [ (0.0, "stalled"); (deadline () /. 2.0, "behind") ]
  in
  List.iter refused replies;
  (* Both by the first fetch's deadline: a second fetch, or a wait behind
     the first, would have taken half a deadline longer at the least. *)
  Check.that "both were refused by the first fetch's deadline"
    (Run.now () -. started < deadline () *. 1.3);
  Check.that
    (Printf.sprintf "the key set was fetched once, not %d times"
       w.issuer.key_fetches)
    (w.issuer.key_fetches = 1)

let failing (w : World.t) =
  w.issuer.keys_answer <- Failing;
  let token = World.person w "waiting" in
  List.iter refused
    (Eio.Fiber.List.map (fun () -> who_am_i w token) (List.init 5 ignore));
  for _ = 1 to 5 do
    refused (who_am_i w token)
  done;
  Check.that "the key set was fetched once" (w.issuer.key_fetches = 1);
  w.issuer.keys_answer <- Published;
  refused (who_am_i w token);
  Run.sleep (key_refetch_cooldown () +. 1.0);
  status "a token once a fetch succeeds" 200 (who_am_i w token);
  Check.that
    (Printf.sprintf "the key set was fetched twice, not %d times"
       w.issuer.key_fetches)
    (w.issuer.key_fetches = 2)

let tests implementations =
  let make = plain implementations in
  World.each_plane ~planes:[ In_process ] ~make ~world:Fun.id ~close:World.close
    "an issuer that accepts a key fetch and never answers it"
    [
      case
        ~timeout:(deadline () *. 3.0)
        "refuses its tokens once the fetch's deadline passes, and a request \
         made meanwhile waits for that fetch, not one of its own"
        silent;
    ]
  @ World.each_plane ~planes:[ In_process ] ~make ~world:Fun.id
      ~close:World.close "an issuer whose key set cannot be fetched"
      [
        case
          ~timeout:((key_refetch_cooldown () *. 2.0) +. 10.0)
          "is asked once per cooldown, however many of its tokens arrive, and \
           its tokens are accepted once a fetch succeeds"
          failing;
      ]
