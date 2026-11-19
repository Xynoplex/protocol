(* What a case expects, said as the TypeScript cases say it with vitest's
   expect: JSON compared as values, so members are equal in any order. *)

let that what condition = if not condition then failwith what

(* JSON as a value: an object's members in key order, and a number as the
   same number however it was written. *)
let rec canonical : Yojson.Safe.t -> Yojson.Safe.t = function
  | `Assoc members ->
      `Assoc
        (List.sort
           (fun (a, _) (b, _) -> compare a b)
           (List.map (fun (k, v) -> (k, canonical v)) members))
  | `List items -> `List (List.map canonical items)
  | `Int n -> `Float (float_of_int n)
  | `Intlit digits -> `Float (float_of_string digits)
  | other -> other

let shown json = Yojson.Safe.to_string json

let equal what (expected : Yojson.Safe.t) (actual : Yojson.Safe.t) =
  if canonical expected <> canonical actual then
    failwith
      (Printf.sprintf "%s:\nexpected %s\nreceived %s" what (shown expected)
         (shown actual))

(* Whether [actual] holds every member [partial] states, each equal: what
   toMatchObject and objectContaining hold. *)
let matches (partial : (string * Yojson.Safe.t) list) (actual : Yojson.Safe.t) =
  List.for_all
    (fun (key, value) ->
      Option.map canonical (Protocol.at actual [ key ]) = Some (canonical value))
    partial

let matching what partial actual =
  if not (matches partial actual) then
    failwith
      (Printf.sprintf "%s: %s does not hold %s" what (shown actual)
         (shown (`Assoc partial)))

let containing what partial items =
  if not (List.exists (matches partial) items) then
    failwith
      (Printf.sprintf "%s: none of %s holds %s" what
         (Call.shown (shown (`List items)))
         (shown (`Assoc partial)))

let contains_text what needle text =
  if not (Re.execp (Re.compile (Re.str needle)) text) then
    failwith
      (Printf.sprintf "%s: %S does not contain %S" what (Call.shown text) needle)

(* The value at [path] of [json], which must be there. *)
let at json path =
  match Protocol.at json path with
  | Some value -> value
  | None ->
      failwith
        (Printf.sprintf "%s holds nothing at %s"
           (Call.shown (shown json))
           (String.concat "." path))

let list json path =
  match at json path with
  | `List items -> items
  | other -> failwith (shown other ^ " is not a list")
