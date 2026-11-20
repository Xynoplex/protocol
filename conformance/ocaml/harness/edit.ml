(* Changing a JSON document as a case means to: a member set or removed at
   a path. *)

(* [json] with the member at [path] set to [value], each object on the way
   there made when it is missing. *)
let rec set_at path value json =
  match (path, json) with
  | [], _ -> value
  | name :: rest, `Assoc members ->
      let inner =
        match List.assoc_opt name members with
        | Some inner -> inner
        | None -> `Assoc []
      in
      `Assoc
        (List.filter (fun (k, _) -> k <> name) members
        @ [ (name, set_at rest value inner) ])
  | _ :: _, other ->
      failwith (Yojson.Safe.to_string other ^ " is not an object to set in")

(* [json] without the member at [path], which must be there. *)
let rec remove_at path json =
  match (path, json) with
  | [ name ], `Assoc members when List.mem_assoc name members ->
      `Assoc (List.filter (fun (k, _) -> k <> name) members)
  | name :: rest, `Assoc members when List.mem_assoc name members ->
      `Assoc
        (List.map
           (fun (k, v) -> if k = name then (k, remove_at rest v) else (k, v))
           members)
  | _ ->
      failwith
        (Printf.sprintf "%s holds nothing at %s"
           (Yojson.Safe.to_string json)
           (String.concat "." path))
