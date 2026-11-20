(* What the dispatcher files share: a file's cases run against a dispatcher
   world of their own, and what a case reads of a job. *)

open Xynolog_conformance
module D = Dispatcher_world

(* The end of a case on [w]: its pending jobs cancelled, and the streams it
   opened held to carrying nothing the spec does not allow. *)
let ended w body =
  World.end_case
    ~account:(fun () ->
      "dispatcher output:\n" ^ String.concat "\n" (D.output w))
    ~settle:(fun () ->
      D.clear_pending w;
      World.drain w.world)
    body

(* [cases] against one dispatcher world, under [title]. The world's options
   are [D.create]'s. *)
let with_dispatcher (implementations : Implementations.t) ?timing ?vocabulary
    ?credentials ?env title cases =
  let made =
    Suite.world ("dispatcher/" ^ title)
      ~make:(fun () ->
        D.start ~gateway:implementations.gateway
          ~command:implementations.dispatcher ?timing ?vocabulary ?credentials
          ?env ())
      ~close:D.close
  in
  List.map
    (fun c ->
      Case.test ~category:[ title ] c (fun () ->
          let w = made () in
          ended w (fun () -> c.run w)))
    cases

(* The retry budget jobs/budgets.json gives [job_type]. *)
let max_retries job_type =
  match
    List.find_opt
      (fun (row : Xynolog_protocol.Tables.Jobs_budgets.budgets_item) ->
        Xynolog_protocol.Tables.Jobs_budgets.job_type_value row.job_type
        = job_type)
      (Protocol.tables ()).jobs_budgets.budgets
  with
  | Some row -> `Int row.max_retries
  | None -> failwith ("jobs/budgets.json gives no budget for " ^ job_type)

let is_time what json path =
  match Protocol.at json path with
  | Some (`String moment) ->
      Check.that
        (Printf.sprintf "%s, %s, is a time" what moment)
        (Result.is_ok (Ptime.of_rfc3339 moment))
  | _ ->
      failwith (Printf.sprintf "%s: %s holds no time" what (Check.shown json))

let has what json path =
  Check.that
    (Printf.sprintf "%s: %s holds %s" what (Check.shown json)
       (String.concat "." path))
    (Protocol.at json path <> None)

let lacks what json path =
  Check.that
    (Printf.sprintf "%s: %s holds %s" what (Check.shown json)
       (String.concat "." path))
    (Protocol.at json path = None)

let str s = `String s
let motivation m = ("motivation", str m)

(* A mark job of [params], its motivation among them, created by [c] on a
   fresh resource unless [resource_id] names one: its id. *)
let mark ?(resource_id = D.resource_id ()) c params =
  D.created ~resource_id c "mark" (`Assoc params)

(* A linking job on the two entity types the Browser registers. *)
let linking =
  D.Mark
    [
      motivation "linking"; ("entityTypes", `List [ str "Person"; str "Place" ]);
    ]

(* A unit's cursor (UnitCursor). *)
let cursor next size found emitted errors =
  `Assoc
    [
      ("next", `Int next);
      ("size", `Int size);
      ("found", `Int found);
      ("emitted", `Int emitted);
      ("errors", `Int errors);
    ]

let units names = `List (List.map str names)

let sorted = function
  | `List items -> `List (List.sort compare items)
  | other -> failwith (Check.shown other ^ " is not a list")

let shown_member = function Some json -> Check.shown json | None -> "nothing"

(* Fail unless [actual] is [expected]: both nothing, or equal values. *)
let member_is what expected actual =
  if Option.map Check.canonical expected <> Option.map Check.canonical actual
  then
    failwith
      (Printf.sprintf "%s:\nexpected %s\nreceived %s" what
         (shown_member expected) (shown_member actual))

(* Fail unless [answer] declines: nothing is pending that the claim takes. *)
let declined what answer =
  member_is what (Some (str (D.code_word None_pending))) (D.code_of answer)

(* Nothing is pending: a claim that takes every job is told so. *)
let nothing_queued w =
  declined "the code of a claim for every job"
    (D.claim (D.worker w "idle") (D.every_job ()))

(* The gateway refused [payload] on [channel] at its door. *)
let refused_at_door c channel payload =
  let reply = D.offered c channel payload in
  Check.equal
    (Printf.sprintf "the gateway's answer to %s: %s" channel
       (Call.shown reply.text))
    (`Int 400) (`Int reply.status)
