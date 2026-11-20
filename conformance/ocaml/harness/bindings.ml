(* How a case states a value it expects (conformance/sdk/README.md,
   "Values"). A pattern is JSON, compared exactly: the same scalars, the
   same list, the same keys. The one operator is {"$var": "name"}: the first
   time a name is met it takes the value found there, and every later use
   must equal it, so a case can say "the same client id" or "the correlation
   id that request carried" without knowing either. _ takes anything and
   keeps nothing. {"$fresh": "name"} takes a value as $var does, and only one
   no variable already holds: "a key of its own, not the last request's".
   {"$join": [...]} is the text its parts make end to end, each a string or a
   variable already bound: a watermark is p-, a scope, - and a sequence
   number.

   JSON's values are compared as JSON means them: a number by its value,
   whether written 1 or 1.0, and an object's members in any order. *)

type t = { values : (string * Yojson.Safe.t) Dynarray.t }

let create () = { values = Dynarray.create () }

let number = function
  | `Int i -> Some (float_of_int i)
  | `Float f -> Some f
  | `Intlit digits -> Some (float_of_string digits)
  | _ -> None

let rec same (a : Yojson.Safe.t) (b : Yojson.Safe.t) =
  match (number a, number b) with
  | Some x, Some y -> x = y
  | _ -> (
      match (a, b) with
      | `Assoc x, `Assoc y ->
          List.length x = List.length y
          && List.for_all
               (fun (key, v) ->
                 match List.assoc_opt key y with
                 | Some w -> same v w
                 | None -> false)
               x
      | `List x, `List y ->
          List.length x = List.length y && List.for_all2 same x y
      | _ -> a = b)

let same_option a b =
  match (a, b) with
  | None, None -> true
  | Some a, Some b -> same a b
  | _ -> false

(* A value as a difference shows it: JSON, cut at 400 characters. *)
let show = function
  | None -> "nothing"
  | Some value ->
      let text = Yojson.Safe.to_string value in
      if String.length text > 400 then String.sub text 0 400 ^ "…" else text

let operator name = function
  | `Assoc [ (key, `String value) ] when key = name -> Some value
  | _ -> None

let joined = function
  | `Assoc [ ("$join", `List parts) ] -> Some parts
  | _ -> None

let find t name = Dynarray.find_opt (fun (n, _) -> n = name) t.values
let bound t name = Option.is_some (find t name)

let bind t name value =
  if bound t name then failwith (name ^ " is already bound");
  Dynarray.add_last t.values (name, value)

let get t name =
  match find t name with
  | Some (_, value) -> value
  | None -> failwith (name ^ " is not bound yet")

(* A value as text, as $join writes it end to end. *)
let text_of : Yojson.Safe.t -> string = function
  | `String s -> s
  | `Float f when Float.is_integer f -> Printf.sprintf "%.0f" f
  | other -> Yojson.Safe.to_string other

(* [template] with every variable replaced by its value. A variable not yet
   bound is the case's mistake. *)
let rec resolve t (template : Yojson.Safe.t) : Yojson.Safe.t =
  match (operator "$var" template, joined template, template) with
  | Some name, _, _ -> get t name
  | _, Some parts, _ ->
      `String
        (String.concat ""
           (List.map (fun part -> text_of (resolve t part)) parts))
  | _, _, `List items -> `List (List.map (resolve t) items)
  | _, _, `Assoc members ->
      `Assoc (List.map (fun (key, value) -> (key, resolve t value)) members)
  | _ -> template

let rec compare t met (pattern : Yojson.Safe.t) (value : Yojson.Safe.t option)
    at =
  let where = if at = "" then "the value" else at in
  let known name =
    match find t name with
    | Some (_, v) -> Some v
    | None -> List.assoc_opt name !met
  in
  match
    (operator "$var" pattern, operator "$fresh" pattern, joined pattern)
  with
  | Some "_", _, _ ->
      if value = None then Some (where ^ " is missing") else None
  | Some name, _, _ -> (
      match (known name, value) with
      | Some held, _ ->
          if same_option (Some held) value then None
          else
            Some
              (Printf.sprintf "%s is %s, not %s (%s)" where (show value) name
                 (show (Some held)))
      | None, None ->
          Some
            (Printf.sprintf "%s is missing, so %s has nothing to take" where
               name)
      | None, Some v ->
          met := (name, v) :: !met;
          None)
  | _, Some name, _ -> (
      if Option.is_some (known name) then
        Some
          (Printf.sprintf
             "%s is already bound: $fresh takes a name not yet used" name)
      else
        match value with
        | None ->
            Some
              (Printf.sprintf "%s is missing, so %s has nothing to take" where
                 name)
        | Some v -> (
            match
              List.find_opt
                (fun (_, held) -> same held v)
                (Dynarray.to_list t.values @ List.rev !met)
            with
            | Some (holder, _) ->
                Some
                  (Printf.sprintf "%s is %s, which %s already holds" where
                     (show value) holder)
            | None ->
                met := (name, v) :: !met;
                None))
  | _, _, Some _ ->
      let text = resolve t pattern in
      if same_option (Some text) value then None
      else
        Some
          (Printf.sprintf "%s is %s, not %s" where (show value)
             (show (Some text)))
  | _ -> (
      match (pattern, value) with
      | `List items, Some (`List found) ->
          if List.length found <> List.length items then
            Some
              (Printf.sprintf "%s has %d entries, not %d: %s" where
                 (List.length found) (List.length items) (show value))
          else
            let rec each index = function
              | [] -> None
              | item :: rest -> (
                  match
                    compare t met item
                      (Some (List.nth found index))
                      (Printf.sprintf "%s[%d]" at index)
                  with
                  | Some difference -> Some difference
                  | None -> each (index + 1) rest)
            in
            each 0 items
      | `List _, _ ->
          Some (Printf.sprintf "%s is %s, not a list" where (show value))
      | `Assoc expected, Some (`Assoc given) -> (
          match
            List.find_opt
              (fun (key, _) -> not (List.mem_assoc key expected))
              given
          with
          | Some (key, extra) ->
              Some
                (Printf.sprintf "%s carries %s (%s), which was not expected"
                   where key (show (Some extra)))
          | None ->
              let rec each = function
                | [] -> None
                | (key, wanted) :: rest -> (
                    match
                      compare t met wanted (List.assoc_opt key given)
                        (if at = "" then key else at ^ "." ^ key)
                    with
                    | Some difference -> Some difference
                    | None -> each rest)
              in
              each expected)
      | `Assoc _, _ ->
          Some (Printf.sprintf "%s is %s, not an object" where (show value))
      | _ ->
          if same_option (Some pattern) value then None
          else
            Some
              (Printf.sprintf "%s is %s, not %s" where (show value)
                 (show (Some pattern))))

(* Whether [value] is what [pattern] states: the first difference, or None
   when there is none; only then are the variables it met bound. *)
let matches t pattern value =
  let met = ref [] in
  let difference = compare t met pattern value "" in
  if difference = None then
    List.iter
      (fun (name, v) -> Dynarray.add_last t.values (name, v))
      (List.rev !met);
  difference
