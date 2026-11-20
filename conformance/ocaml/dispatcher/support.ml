(* What the dispatcher files share: a file's cases run against a dispatcher
   world of their own, and what a case reads of a job. *)

open Xynolog_conformance
module D = Dispatcher_world

type case = {
  name : string;
  timeout : float;
  solo : string option;
  known : string option;
  run : D.t -> unit;
}

(* A case: [timeout] is its own bound, in seconds, as vitest's; its world's
   start is given a bound of its own on top. [solo], the reason, runs it in
   order with the run's other solo cases, in one process, before any other.
   [known] is why the dispatcher this suite is held to fails it: the case
   is expected to fail, and fails the run once it passes. *)
let case ?(timeout = 60.0) ?solo ?known name run =
  { name; timeout; solo; known; run }

let testo ~category ~timeout ?solo ?known name body =
  Testo.create ~category ~max_duration:(timeout +. 60.0) ?solo
    ?expected_outcome:(Option.map (fun why -> Testo.Should_fail why) known)
    name body

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
        D.create ~gateway:implementations.gateway
          ~command:implementations.dispatcher ?timing ?vocabulary ?credentials
          ?env ())
      ~close:D.close
  in
  List.map
    (fun c ->
      testo ~category:[ title ] ~timeout:c.timeout ?solo:c.solo ?known:c.known
        c.name (fun () ->
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
