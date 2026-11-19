(* Every id is checked. Protocol names four kinds of id
   (identifiers/kinds.json), each a component whose [pattern] is the rule a
   value of that kind is held to. A gateway validates a payload against its
   schema, so a property that carries an id and does not refer to its kind is
   an id nothing checks.

   - A kind's schema is exactly a string with a pattern and a description,
     beside the dialect and the [$id] every schema states, and openapi.json
     declares it among its schemas.
   - Each id a kind accepts matches its pattern, and none it refuses does.
   - A property the table names for a kind is, in every file of specs/src,
     a reference to that kind, a list of them, or a union of one of them and
     null. A property is named for one kind at most.
   - A property whose name ends in [Id] or [Ids] is named for a kind or is
     among [others], and none of [others] refers to a kind: a new id is
     classified before it is merged.
   - Each property the table names is a property of some schema.
   - A path parameter refers to a kind. *)

let file = "identifiers/kinds.json"
let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token
let text = function Some (`String s) -> Some s | _ -> None

let member name = function
  | `Assoc members -> List.assoc_opt name members
  | _ -> None

let named_like_an_id name =
  String.ends_with ~suffix:"Id" name || String.ends_with ~suffix:"Ids" name

let refusals ~ids ~(read_json : string -> Yojson.Safe.t option) files
    (table : Tables.Identifiers_kinds.t) =
  let found = ref [] in
  let refuse file pointer fmt =
    Printf.ksprintf
      (fun message -> found := { Decode.file; pointer; message } :: !found)
      fmt
  in
  let kind_file name = Places.components ^ name ^ ".json" in
  let declared =
    Option.bind (read_json "openapi.json")
      (Decode.at_pointer "/components/schemas")
  in
  List.iteri
    (fun i (kind : Tables.Identifiers_kinds.kinds_item) ->
      let at = "/kinds" / string_of_int i in
      match read_json (kind_file kind.schema) with
      | None ->
          refuse file (at / "schema") "%s has no schema: %s" kind.schema
            (kind_file kind.schema)
      | Some schema -> (
          let keys =
            match schema with
            | `Assoc members -> List.sort compare (List.map fst members)
            | _ -> []
          in
          match
            ( text (member "type" schema),
              text (member "pattern" schema),
              text (member "description" schema) )
          with
          | Some "string", Some pattern, Some _
            when keys = [ "$id"; "$schema"; "description"; "pattern"; "type" ]
            ->
              if
                Option.bind declared (member kind.schema)
                |> Option.map (member "$ref")
                <> Some (Some (`String (Decode.base ^ kind.schema ^ ".json")))
              then
                refuse "openapi.json" "/components/schemas"
                  "does not declare %s among its schemas" kind.schema;
              let held side accepted cases =
                List.iteri
                  (fun j (case : Tables.Identifiers_kinds.accepts_item) ->
                    if Pattern.matches pattern case.id <> accepted then
                      refuse file
                        (at / side / string_of_int j / "id")
                        "%s %s %s (%s), and its pattern %s %s" kind.schema side
                        (Yojson.Safe.to_string (`String case.id))
                        case.why pattern
                        (if accepted then "does not" else "accepts it"))
                  cases
              in
              (* A pattern [Pattern] does not read is the schema's refusal. *)
              if Option.is_none (Pattern.refusal pattern) then (
                held "accepts" true kind.accepts;
                held "refuses" false kind.refuses)
          | _ ->
              refuse (kind_file kind.schema) ""
                "%s must be exactly a string with a pattern and a description; \
                 it has [%s]"
                kind.schema (String.concat "," keys)))
    table.kinds;
  let kind_of_property = Hashtbl.create 64 in
  List.iteri
    (fun i (kind : Tables.Identifiers_kinds.kinds_item) ->
      List.iteri
        (fun j property ->
          match Hashtbl.find_opt kind_of_property property with
          | Some other ->
              refuse file
                ("/kinds" / string_of_int i / "properties" / string_of_int j)
                "%s is named for two kinds: %s and %s" property other
                kind.schema
          | None -> Hashtbl.replace kind_of_property property kind.schema)
        kind.properties)
    table.kinds;
  let others =
    List.map
      (fun (o : Tables.Identifiers_kinds.others_item) -> o.property)
      table.others
  in
  List.iteri
    (fun i property ->
      Option.iter
        (fun kind ->
          refuse file
            ("/others" / string_of_int i / "property")
            "%s is among `others` and is named for %s" property kind)
        (Hashtbl.find_opt kind_of_property property))
    others;
  let kinds =
    List.map
      (fun (k : Tables.Identifiers_kinds.kinds_item) ->
        (kind_file k.schema, k.schema))
      table.kinds
  in
  (* The kind [schema], written in [file], refers to: directly, as the items
     of a list, or as every member of a [oneOf] or an [anyOf] but its
     [{ "type": "null" }]. *)
  let rec referred file schema =
    let union =
      match member "oneOf" schema with
      | Some _ as u -> u
      | None -> member "anyOf" schema
    in
    match union with
    | Some (`List members) -> (
        match
          members
          |> List.filter (fun m -> text (member "type" m) <> Some "null")
          |> List.map (referred file)
          |> List.sort_uniq compare
        with
        | [ kind ] -> kind
        | _ -> None)
    | _ -> (
        let target =
          match text (member "$ref" schema) with
          | Some _ as t -> t
          | None ->
              Option.bind (member "items" schema) (fun i ->
                  text (member "$ref" i))
        in
        match target with
        | None -> None
        | Some target -> (
            match Decode.resolve ids { target; file; pointer = "" } with
            | named, "" -> List.assoc_opt named kinds
            | _ -> None))
  in
  let seen = Hashtbl.create 1024 in
  let rec walk file pointer (node : Yojson.Safe.t) =
    match node with
    | `List items ->
        List.iteri (fun i m -> walk file (pointer / string_of_int i) m) items
    | `Assoc members ->
        (match List.assoc_opt "properties" members with
        | Some (`Assoc properties) ->
            List.iter
              (fun (name, schema) ->
                match schema with
                | `Assoc _ | `List _ -> (
                    Hashtbl.replace seen name ();
                    let at = pointer / "properties" / name in
                    let actual = referred file schema in
                    match Hashtbl.find_opt kind_of_property name with
                    | Some expected when actual <> Some expected ->
                        refuse file at "%s must refer to %s%s" name expected
                          (match actual with
                          | Some a -> ", and refers to " ^ a
                          | None -> "")
                    | Some _ -> ()
                    | None when List.mem name others ->
                        Option.iter
                          (fun a ->
                            refuse file at
                              "%s is among `others` and refers to %s" name a)
                          actual
                    | None when named_like_an_id name ->
                        refuse file at
                          "%s is named like an id and is classified nowhere: \
                           name it for a kind, or among `others`, in \
                           specs/src/identifiers/kinds.json"
                          name
                    | None -> ())
                | _ -> ())
              properties
        | _ -> ());
        (match List.assoc_opt "parameters" members with
        | Some (`List parameters) ->
            List.iteri
              (fun i parameter ->
                if
                  text (member "in" parameter) = Some "path"
                  && Option.bind (member "schema" parameter) (referred file)
                     = None
                then
                  refuse file
                    (pointer / "parameters" / string_of_int i)
                    "the path parameter %s must refer to a kind of id"
                    (match text (member "name" parameter) with
                    | Some name -> "{" ^ name ^ "}"
                    | None -> "with no name"))
              parameters
        | _ -> ());
        List.iter (fun (key, value) -> walk file (pointer / key) value) members
    | _ -> ()
  in
  List.iter
    (fun f ->
      if Filename.check_suffix f ".json" && f <> file then
        Option.iter (walk f "") (read_json f))
    files;
  List.iter
    (fun property ->
      if not (Hashtbl.mem seen property) then
        refuse file "" "names %s, which no schema of the spec has" property)
    (List.concat_map
       (fun (k : Tables.Identifiers_kinds.kinds_item) -> k.properties)
       table.kinds
    @ others);
  List.rev !found
