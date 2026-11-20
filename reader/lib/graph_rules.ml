(* The graph's schema agrees with what it names.

   - Each relationship of graph/schema.json goes from and to a kind of node
     it names, and its stub is a kind of node it names.
   - A node's row is a part of graph/codec-cases.json that stores a row. Its
     key is a property that row requires, and each of its lookups a property
     of the row. A node without a row has no lookups.
   - The stub's marker is no property of its node's row. *)

let schema = "graph/schema.json"
let codec = "graph/GraphCodecCases.json"
let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token
let at pointer json = Decode.at_pointer pointer json
let text = function Some (`String s) -> Some s | _ -> None
let keys = function Some (`Assoc members) -> List.map fst members | _ -> []

let texts = function
  | Some (`List items) ->
      List.filter_map (function `String s -> Some s | _ -> None) items
  | _ -> []

(* The schema of a codec part's stored case: the part itself, or the branch
   of its oneOf titled Stored. *)
let stored part =
  match at "/oneOf" part with
  | Some (`List branches) ->
      List.find_opt
        (fun branch -> text (at "/title" branch) = Some "Stored")
        branches
  | _ -> Some part

(* A stored row's properties, plain and JSON, and those it requires. *)
let row_of part =
  Option.map
    (fun stored ->
      let both member = keys (at ("/properties" / member / "properties") stored)
      and required member =
        texts (at ("/properties" / member / "required") stored)
      in
      (both "properties" @ both "json", required "properties" @ required "json"))
    (Option.bind part stored)

let refusals ~(read_json : string -> Yojson.Safe.t option) =
  let found = ref [] in
  let refuse pointer fmt =
    Printf.ksprintf
      (fun message ->
        found := { Decode.file = schema; pointer; message } :: !found)
      fmt
  in
  let table = read_json schema in
  let parts =
    Option.bind (read_json codec) (at "/properties/cases/items/properties")
  in
  let nodes = keys (Option.bind table (at "/nodes")) in
  let named pointer =
    match Option.bind table (fun t -> text (at pointer t)) with
    | Some kind when not (List.mem kind nodes) ->
        refuse pointer "%s is no kind of node under /nodes" kind
    | _ -> ()
  in
  List.iter
    (fun relationship ->
      named ("/relationships" / relationship / "from");
      named ("/relationships" / relationship / "to"))
    (keys (Option.bind table (at "/relationships")));
  named "/stub/node";
  let row_of_node node =
    Option.bind
      (Option.bind table (fun t -> text (at ("/nodes" / node / "row") t)))
      (fun row -> Some (row, row_of (Option.bind parts (at ("" / row)))))
  in
  List.iter
    (fun node ->
      let pointer = "/nodes" / node in
      let key = Option.bind table (fun t -> text (at (pointer / "key") t))
      and lookups = texts (Option.bind table (at (pointer / "lookups"))) in
      match row_of_node node with
      | None ->
          if lookups <> [] then
            refuse (pointer / "lookups")
              "a node without a row holds its key alone, and is found by \
               nothing else"
      | Some (row, None) ->
          refuse (pointer / "row")
            "%s is no part of graph/codec-cases.json that stores a row" row
      | Some (row, Some (properties, required)) ->
          (match key with
          | Some key when not (List.mem key required) ->
              refuse (pointer / "key") "%s is no property %s's row requires" key
                row
          | _ -> ());
          List.iteri
            (fun i lookup ->
              if not (List.mem lookup properties) then
                refuse
                  (pointer / "lookups" / string_of_int i)
                  "%s is no property of %s's row" lookup row)
            lookups)
    nodes;
  (match
     ( Option.bind table (fun t -> text (at "/stub/node" t)),
       Option.bind table (fun t -> text (at "/stub/marker" t)) )
   with
  | Some node, Some marker -> (
      match row_of_node node with
      | Some (row, Some (properties, _)) when List.mem marker properties ->
          refuse "/stub/marker" "%s is a property of %s's row" marker row
      | _ -> ())
  | _ -> ());
  List.rev !found
