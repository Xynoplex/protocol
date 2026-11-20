(* What a harness case expects of what it calls. *)

let check what condition = if not condition then failwith what

(* [f ()] fails, with a message [pattern] finds. *)
let fails what pattern f =
  match f () with
  | _ -> failwith (what ^ ": it did not fail")
  | exception Failure message ->
      check
        (Printf.sprintf "%s: it failed with %S" what message)
        (Re.execp (Re.compile (Re.Pcre.re pattern)) message)

(* [result] is an [Error] whose reason [pattern] finds. *)
let refused what pattern = function
  | Ok _ -> failwith (what ^ ": it was not refused")
  | Error why ->
      check
        (Printf.sprintf "%s: it was refused with %S" what why)
        (Re.execp (Re.compile (Re.Pcre.re pattern)) why)
