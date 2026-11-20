(* What one principal may take (x-xynolog-limits on /bus/subscribe and
   /bus/emit): a baseline, and a coefficient per role. A person and an agent
   are indistinguishable here — the same case runs for each, and each must
   behave alike; only a role changes the coefficient, whoever holds it. *)

open Xynolog_conformance
open Support
open Case
module Openapi = Xynolog_protocol_schema.Openapi

(* The emit bucket's burst, and its limit per role. *)
let emit_limit () =
  match
    Protocol.principal_limit Gateway "POST" "/bus/emit" "emitsPerPrincipal"
  with
  | Openapi.Limits baseline, roles -> (
      match List.assoc_opt "burst" baseline with
      | Some burst -> (Protocol.count burst, roles)
      | None -> failwith "emitsPerPrincipal's baseline states no burst")
  | _ -> failwith "emitsPerPrincipal's baseline is not a bucket"

let stream_limit () =
  let baseline, roles =
    Protocol.principal_limit Gateway "POST" "/bus/subscribe"
      "streamsPerPrincipal"
  in
  (Protocol.count baseline, roles)

(* A subscription body, a fresh client each time. *)
let stream () =
  Bus_stream.subscribe_body ~client_id:(Fresh.uuid ())
    ~global:[ "beckon:focus" ] ()

let unique prefix = prefix ^ Fresh.hex 6

(* A principal holding no role, of each kind. *)
let unroled (w : World.t) =
  [
    ("a person", World.person w (unique "limits-person-"));
    ("an agent", fst (World.agent w ~roles:[] (unique "limits-model-")));
  ]

let focus =
  `Assoc
    [
      ("channel", `String "beckon:focus");
      ("payload", `Assoc [ ("resourceId", `String "res-1") ]);
    ]

(* [count] emits at once. *)
let burst (w : World.t) token count =
  Eio.Fiber.List.map
    (fun () -> World.emit w token focus)
    (List.init count ignore)

let unlimited roles what =
  List.iter
    (fun role ->
      Check.that
        (Printf.sprintf "%s: %s is unlimited" what role)
        (List.assoc_opt role roles = Some Openapi.Unlimited))
    [ Protocol.service_role (); Protocol.worker_role () ]

let emit_bucket (w : World.t) =
  let burst_size, _ = emit_limit () in
  List.iter
    (fun (kind, token) ->
      (* Rounds of a full burst each, until one is refused: the bucket refills
         while a round is in flight, so no single count is sure to empty it. *)
      let rec rounds round replies =
        if
          round >= 5
          || List.exists (fun (r : Call.reply) -> r.status = 429) replies
        then replies
        else rounds (round + 1) (replies @ burst w token burst_size)
      in
      let replies = rounds 0 [] in
      let accepted =
        List.filter (fun (r : Call.reply) -> r.status = 202) replies
      in
      let refused =
        List.filter (fun (r : Call.reply) -> r.status = 429) replies
      in
      Check.that
        (kind ^ ": every emit accepted or refused")
        (List.length accepted + List.length refused = List.length replies);
      Check.that
        (kind ^ ": the bucket starts full")
        (List.length accepted >= burst_size);
      Check.that (kind ^ ": refused past the burst") (refused <> []);
      List.iteri
        (fun i reply ->
          if i < 3 then (
            conforms "POST" "/bus/emit" reply;
            Check.that
              (kind ^ ": the refusal's code is emit-rate")
              (text_member "code" reply = Protocol.message_code "emit-rate")))
        refused;
      let last = List.nth refused (List.length refused - 1) in
      let wait =
        match Http.Header.get last.headers "retry-after" with
        | Some v -> ( match int_of_string_opt v with Some n -> n | None -> 0)
        | None -> 0
      in
      Check.that (kind ^ ": Retry-After") (wait >= 1);
      Run.sleep (float_of_int wait);
      status
        (kind ^ ": admitted after Retry-After")
        202 (World.emit w token focus))
    (unroled w)

let stream_baseline (w : World.t) =
  let baseline, _ = stream_limit () in
  List.iter
    (fun (kind, token) ->
      let held =
        List.init baseline (fun _ -> World.subscribe w token (stream ()))
      in
      (match Bus_stream.subscribe (World.origin w) (Some token) (stream ()) with
      | Stream over ->
          Bus_stream.close over;
          failwith (kind ^ ": a stream past the baseline was admitted")
      | Refused over ->
          status kind 429 over;
          conforms "POST" "/bus/subscribe" over;
          Check.that
            (kind ^ ": the refusal's code is streams")
            (text_member "code" over = Protocol.message_code "streams"));
      Bus_stream.close (List.hd held);
      Run.eventually (kind ^ ": a stream admitted once one closed") 10.0
        (fun () ->
          match
            Bus_stream.subscribe (World.origin w) (Some token) (stream ())
          with
          | Stream again ->
              Bus_stream.close again;
              Some ()
          | Refused _ -> None);
      List.iter Bus_stream.close held)
    (unroled w)

let role_holders (w : World.t) prefix =
  [
    ( "a person holding the service role",
      World.person w
        ~claims:[ ("roles", `List [ `String (Protocol.service_role ()) ]) ]
        (unique (prefix ^ "-service-")) );
    ( "an agent holding the worker role",
      fst
        (World.agent w
           ~roles:[ Protocol.service_role (); Protocol.worker_role () ]
           (unique (prefix ^ "-worker-"))) );
  ]

let unlimited_streams (w : World.t) =
  let baseline, roles = stream_limit () in
  unlimited roles "streamsPerPrincipal";
  List.iter
    (fun (kind, token) ->
      let held =
        List.init (baseline + 1) (fun _ -> World.subscribe w token (stream ()))
      in
      Check.that kind (List.length held = baseline + 1);
      List.iter Bus_stream.close held)
    (role_holders w "streams")

let unlimited_emits (w : World.t) =
  let burst_size, roles = emit_limit () in
  unlimited roles "emitsPerPrincipal";
  List.iter
    (fun (kind, token) ->
      let replies = burst w token burst_size @ burst w token burst_size in
      Check.equal kind (`List [])
        (`List
           (List.filter_map
              (fun (r : Call.reply) ->
                if r.status <> 202 then Some (`Int r.status) else None)
              replies)))
    (role_holders w "limits")

let tests implementations =
  World.each_plane ~make:(plain implementations) ~world:Fun.id
    ~close:World.close "what one principal may take"
    [
      case
        "past its emit bucket a principal holding no role is refused 429 \
         emit-rate, told when to return, and admitted then — a person and an \
         agent alike"
        emit_bucket;
      case
        "a principal holding no role holds at most its baseline of streams: \
         the next is refused 429 streams, told when to return, and closing one \
         admits one — a person and an agent alike"
        stream_baseline;
      case
        "a principal holding an unlimited role holds more streams than the \
         baseline, whatever its kind"
        unlimited_streams;
      case
        "a principal holding an unlimited role is not limited by its emit \
         bucket, whatever its kind"
        unlimited_emits;
    ]
