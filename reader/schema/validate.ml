(* Whether a value is what a schema says, in JSON Schema 2020-12's reading of
   the keywords Schema knows: a [$ref] holds the value to what it names, and
   to the keywords beside it. [format]
   and [discriminator] are annotations: the type and the [oneOf] decide. A
   [pattern] is matched as ECMA-262 does it ([Pattern]).

   [unevaluatedProperties] holds the members no other keyword of its schema
   evaluated: [properties] and [additionalProperties] beside it, and, through
   a [$ref], an [allOf], an [anyOf] or a [oneOf], what the schemas they apply
   in place evaluated. An [anyOf] counts each member the value is, a [oneOf]
   the one, and a [not] nothing. *)

type place = { file : string; pointer : string }

(* The schema at a place, which the caller reads from its files. *)
type schemas = place -> Schema.t option

(* A string's length in code points, as JSON Schema counts it. *)
let code_points text =
  let count = ref 0 in
  String.iter (fun c -> if Char.code c land 0xC0 <> 0x80 then incr count) text;
  !count

let rec canonical : Yojson.Safe.t -> Yojson.Safe.t = function
  | `Assoc members ->
      `Assoc
        (List.sort
           (fun (a, _) (b, _) -> compare a b)
           (List.map (fun (name, value) -> (name, canonical value)) members))
  | `List items -> `List (List.map canonical items)
  | `Int n -> `Float (float_of_int n)
  | `Intlit digits -> `Float (float_of_string digits)
  | value -> value

let equal a b = Yojson.Safe.equal (canonical a) (canonical b)
let show value = Yojson.Safe.to_string value

let is_type (t : Schema.type_name) (value : Yojson.Safe.t) =
  match (t, value) with
  | String, `String _ -> true
  | Integer, (`Int _ | `Intlit _) -> true
  | Integer, `Float f -> Float.is_integer f
  | Number, (`Int _ | `Intlit _ | `Float _) -> true
  | Boolean, `Bool _ -> true
  | Null, `Null -> true
  | Object, `Assoc _ -> true
  | Array, `List _ -> true
  | _ -> false

let number : Yojson.Safe.t -> float option = function
  | `Int n -> Some (float_of_int n)
  | `Intlit digits -> Some (float_of_string digits)
  | `Float f -> Some f
  | _ -> None

let bound f = Printf.sprintf "%g" f
let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token

(* The ways [value], at [path] within its file, is not what [schema] says,
   each as the pointer into the value and why. A [$ref] is resolved through
   [ids], from the file it is written in, which it carries. *)
let rec errors ids (schemas : schemas) (schema : Schema.t) value path =
  fst (evaluate ids schemas schema value path)

(* [errors], and the members of [value], when it is an object, that [schema]
   evaluated. *)
and evaluate ids schemas (schema : Schema.t) value path =
  match schema with
  | Boolean_schema true -> ([], [])
  | Boolean_schema false -> ([ (path, "is admitted by no schema") ], [])
  | Schema s ->
      let named, by_name =
        match s.ref_ with
        | None -> ([], [])
        | Some r -> (
            let file, pointer = Decode.resolve ids r in
            match schemas { file; pointer } with
            | Some target -> evaluate ids schemas target value path
            | None ->
                ( [
                    ( path,
                      Printf.sprintf "is held to %s, which names nothing"
                        r.target );
                  ],
                  [] ))
      in
      let own, by_layer = layer ids schemas s value path in
      let evaluated = by_name @ by_layer in
      let rest, by_rest =
        match (s.unevaluated_properties, value) with
        | Some unevaluated, `Assoc members ->
            let left =
              List.filter
                (fun (name, _) -> not (List.mem name evaluated))
                members
            in
            ( List.concat_map
                (fun (name, member) ->
                  let at = path / name in
                  match unevaluated with
                  | Boolean_schema false ->
                      [ (at, "is a member the schema does not state") ]
                  | values -> errors ids schemas values member at)
                left,
              List.map fst left )
        | _ -> ([], [])
      in
      (named @ own @ rest, evaluated @ by_rest)

and layer ids schemas (s : Schema.schema) value path =
  let found = ref [] in
  let evaluated = ref [] in
  let say message = found := (path, message) :: !found in
  let add more = found := List.rev_append more !found in
  let saw names = evaluated := !evaluated @ names in
  let typed =
    match s.type_ with
    | None -> true
    | Some types ->
        List.exists (fun t -> is_type t value) types
        ||
        (say
           (Printf.sprintf "is %s, which is no %s" (show value)
              (String.concat " or " (List.map Schema.type_word types)));
         false)
  in
  if typed then (
    Option.iter
      (fun members ->
        if
          not
            (List.exists (fun m -> equal (Schema.literal_json m) value) members)
        then
          say
            (Printf.sprintf "is %s, which is none of %s" (show value)
               (show (`List (List.map Schema.literal_json members)))))
      s.enum;
    Option.iter
      (fun c ->
        if not (equal (Schema.literal_json c) value) then
          say
            (Printf.sprintf "is %s, not %s" (show value)
               (show (Schema.literal_json c))))
      s.const;
    (match value with
    | `String text ->
        let length = code_points text in
        Option.iter
          (fun n ->
            if length < n then
              say (Printf.sprintf "is shorter than %d code points" n))
          s.min_length;
        Option.iter
          (fun n ->
            if length > n then
              say (Printf.sprintf "is longer than %d code points" n))
          s.max_length;
        Option.iter
          (fun p ->
            if not (Pattern.matches p text) then say ("does not match " ^ p))
          s.pattern
    | _ -> ());
    (match number value with
    | Some n ->
        Option.iter
          (fun m -> if n < m then say ("is less than " ^ bound m))
          s.minimum;
        Option.iter
          (fun m -> if n > m then say ("is more than " ^ bound m))
          s.maximum
    | None -> ());
    (match value with
    | `List items ->
        let count = List.length items in
        Option.iter
          (fun n ->
            if count < n then say (Printf.sprintf "has fewer than %d items" n))
          s.min_items;
        Option.iter
          (fun n ->
            if count > n then say (Printf.sprintf "has more than %d items" n))
          s.max_items;
        if
          s.unique_items = Some true
          && List.length
               (List.sort_uniq compare
                  (List.map (fun i -> show (canonical i)) items))
             <> count
        then say "repeats an item";
        Option.iter
          (fun item_schema ->
            List.iteri
              (fun i item ->
                add
                  (errors ids schemas item_schema item (path / string_of_int i)))
              items)
          s.items
    | _ -> ());
    match value with
    | `Assoc members ->
        let count = List.length members in
        Option.iter
          (fun n ->
            if count < n then say (Printf.sprintf "has fewer than %d members" n))
          s.min_properties;
        Option.iter
          (fun n ->
            if count > n then say (Printf.sprintf "has more than %d members" n))
          s.max_properties;
        List.iter
          (fun name ->
            if not (List.mem_assoc name members) then say ("lacks " ^ name))
          (Option.value ~default:[] s.required);
        let properties = Option.value ~default:[] s.properties in
        List.iter
          (fun (name, member) ->
            let at = path / name in
            Option.iter
              (fun names ->
                add
                  (List.map
                     (fun (_, message) -> (at, "is a name that " ^ message))
                     (errors ids schemas names (`String name) at)))
              s.property_names;
            match List.assoc_opt name properties with
            | Some property ->
                saw [ name ];
                add (errors ids schemas property member at)
            | None -> (
                match s.additional_properties with
                | Some (Boolean_schema false) ->
                    saw [ name ];
                    add [ (at, "is a member the schema does not state") ]
                | Some (Schema _ as values) ->
                    saw [ name ];
                    add (errors ids schemas values member at)
                | Some (Boolean_schema true) -> saw [ name ]
                | None -> ()))
          members
    | _ -> ());
  List.iter
    (fun part ->
      let wrong, names = evaluate ids schemas part value path in
      add wrong;
      saw names)
    (Option.value ~default:[] s.all_of);
  (* The members [value] is, each with what it evaluated. *)
  let fitting members =
    List.filter_map
      (fun member ->
        match evaluate ids schemas member value path with
        | [], names -> Some names
        | _ -> None)
      members
  in
  Option.iter
    (fun members ->
      match fitting members with
      | [] -> say "is none of the anyOf"
      | fits -> List.iter saw fits)
    s.any_of;
  Option.iter
    (fun members ->
      match fitting members with
      | [ names ] -> saw names
      | [] -> say "is none of the oneOf"
      | fits ->
          say
            (Printf.sprintf "is %d of the oneOf, where it must be one"
               (List.length fits)))
    s.one_of;
  Option.iter
    (fun refused ->
      if errors ids schemas refused value path = [] then
        say "is what the not refuses")
    s.not_;
  (List.rev !found, !evaluated)
