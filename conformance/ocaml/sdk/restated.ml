(* A case states what a table already states. These hold each to its table,
   so a table that changes fails the case that restates it rather than
   leaving it to agree with a client about a stale value. And each exemption
   an SDK claims names a case of the corpus, and says why. *)

open Xynolog_conformance
open Xynolog_protocol
module Case = Tables.Conformance_case
module Codes = Tables.Errors_codes

let failf format = Printf.ksprintf failwith format
let shown = Yojson.Safe.to_string

let named corpus name =
  match List.find_opt (fun (c : Corpus.case) -> c.name = name) corpus with
  | Some c -> c.case.steps
  | None -> failf "the corpus has no case %s" name

let member name = function
  | `Assoc members -> List.assoc_opt name members
  | _ -> None

let failure_codes corpus () =
  let { Codes.bus_request; transport; _ } = (Protocol.tables ()).errors_codes in
  let wire_codes =
    List.map Codes.command_error_code_value Codes.command_error_code_all
  in
  (* A failure with no words is not its channel's, and is refused as a
     result with a fault is. *)
  let expected failure =
    match (member "message" failure, member "code" failure) with
    | Some (`String _), code -> (
        match
          List.find_opt
            (fun (entry : Codes.codes_item) ->
              match (entry.wire, code) with
              | Some wire, Some (`String code) ->
                  Codes.command_error_code_value wire = code
              | _ -> false)
            bus_request.codes
        with
        | Some entry -> entry.code
        | None -> bus_request.unrecognized_failure)
    | _ -> transport.unclassified
  in
  let answered = ref [] and wordless = ref 0 and last = ref None in
  List.iter
    (fun (step : Case.conformance_step) ->
      match step with
      | Backend { backend = Emit; with_ = Some w } -> (
          match List.assoc_opt "payload" w with
          | Some (`Assoc _ as payload) -> (
              last := Some payload;
              match member "message" payload with
              | Some (`String _) ->
                  answered := member "code" payload :: !answered
              | _ -> incr wordless)
          | _ -> ())
      | Settles { fails; _ } -> (
          match !last with
          | None -> failwith "a settle with no failure before it"
          | Some failure ->
              Check.equal
                ("the failure answering " ^ shown failure)
                (`Assoc [ ("code", `String (expected failure)) ])
                (Option.value ~default:`Null fails);
              last := None)
      | _ -> ())
    (named corpus "failure-codes");
  (match
     List.filter
       (fun code -> not (List.mem (Some (`String code)) !answered))
       wire_codes
   with
  | [] -> ()
  | never ->
      failf "wire codes the case never answers with: %s"
        (String.concat ", " never));
  Check.that "a failure stating no code" (List.mem None !answered);
  Check.that "a failure stating a code the wire does not declare"
    (List.exists
       (function
         | Some (`String code) -> not (List.mem code wire_codes) | _ -> false)
       !answered);
  Check.that "a failure with no words" (!wordless > 0)

let timing name =
  match
    List.find_opt
      (fun (row : Tables.Client_timing.timing_item) -> row.name = name)
      (Protocol.tables ()).client_timing.timing
  with
  | Some row -> row.value
  | None -> failf "client/timing.json states no %s" name

let backend_with directive (step : Case.conformance_step) =
  match step with
  | Backend { backend; with_ } when backend = directive -> Some with_
  | _ -> None

let emit_budget_spent corpus () =
  let attempts =
    match timing "emitRetry" with
    | Object { attempts; _ } -> attempts
    | Integer _ -> failwith "client/timing.json states no emitRetry budget"
  in
  let steps = named corpus "emit-budget-spent" in
  let refused =
    List.filter
      (fun (step : Case.conformance_step) ->
        match step with
        | Wire { wire = "POST /bus/emit"; status = Some 429; _ } -> true
        | _ -> false)
      steps
  in
  Check.that
    (Printf.sprintf "%d refused emits, not %d" (List.length refused) attempts)
    (List.length refused = attempts);
  let scripted = List.find_map (backend_with Refuse) steps in
  Check.equal "the refusal's times" (`Int attempts)
    (Option.value ~default:`Null
       (Option.bind (Option.join scripted) (List.assoc_opt "times")))

let dedup_window corpus () =
  let remembered =
    match timing "seenEventIdsCount" with
    | Integer n -> n
    | Object _ -> failwith "client/timing.json states no seenEventIdsCount"
  in
  let steps = named corpus "dedup-window" in
  let replayed =
    List.find_map
      (fun step ->
        match backend_with Record step with
        | Some (Some w) -> List.assoc_opt "count" w
        | _ -> None)
      steps
  in
  Check.equal "the events replayed" (`Int remembered)
    (Option.value ~default:`Null replayed);
  let delivered =
    List.find_map
      (fun (step : Case.conformance_step) ->
        match step with Frames { count; _ } -> Some count | _ -> None)
      steps
  in
  Check.that "the frames delivered" (delivered = Some remembered)

let exemptions corpus (implementations : Implementations.t) () =
  List.iter
    (fun (sdk, (drivers : Implementations.sdk)) ->
      List.iter
        (fun (name, why) ->
          Check.that
            (Printf.sprintf
               "%s is exempt from %s, which the corpus has no case of" sdk name)
            (List.exists (fun (c : Corpus.case) -> c.name = name) corpus);
          Check.that
            (Printf.sprintf "%s says nothing of why it is exempt from %s" sdk
               name)
            (String.trim why <> ""))
        (Option.value ~default:[] drivers.exempt))
    implementations.sdks

let tests implementations corpus =
  let test category name body =
    let c = Xynolog_conformance.Case.case name body in
    Xynolog_conformance.Case.test ~category:[ category ] c (fun () -> c.run ())
  in
  let restating = "the corpus against the tables it restates" in
  [
    test restating
      "failure-codes answers with every code a failure can carry, and expects \
       of each the client code specs/src/errors/codes.json gives it"
      (failure_codes corpus);
    test restating
      "emit-budget-spent refuses as many attempts as \
       specs/src/client/timing.json gives one emit"
      (emit_budget_spent corpus);
    test restating
      "dedup-window has the new stream replay as many events as \
       specs/src/client/timing.json has a client remember"
      (dedup_window corpus);
    test "the exemptions" "each names a case of the corpus, and says why"
      (exemptions corpus implementations);
  ]
