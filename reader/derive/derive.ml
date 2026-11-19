(* derive.exe SPECS: every table's model, derived from its schema, as OCaml
   source on standard output. SPECS is specs/src. derive.exe typescript
   SPECS: the same models as TypeScript, for the conformance harness, with
   every service's schema and every table's value.

   Each table is a module named as its schema (Places.model_of), of types
   and strict decoders, its own model [t] and [decode] last; then [t], every
   table's model, and [load], which decodes each table through its own.

   A table's schema is read in the table dialect: every node of it is one a
   type is derived from, and any other is refused, so the dialect is what
   this derives. A node is:
   - a [$ref], with nothing but annotations beside it: the type of what it
     names, once; or, when it names JsonSchema.json, [Schema.t];
   - [{ "anyOf": [{ "$ref": … }, { "type": "null" }] }]: that, or null;
   - [{ "$comment": … }] alone: JSON by nature, [Yojson.Safe.t];
   - a [oneOf] of closed objects, any two told apart, and at most one member
     of each other type, with [{ "type": "null" }] its last member when it
     may be null: a variant. A member is named by the value it fixes a
     property all the objects require to; else, beside other types, an object
     is [Object] and each other member its type; else by its [title];
   - a node of one [type], with only the keywords that type takes, and
     [[T, "null"]] its type when it may be null. A string, an integer, a number
     or a boolean is OCaml's; one [enum] value or a [const] is that value's
     type, holding the value, which the decoder checks; a string [enum] is a
     variant, with its values ([_value]) and its constructors in order
     ([_all]). An object with
     [properties] is closed, a record; one without is a map, in its file's
     order, of its [additionalProperties], keyed by its [propertyNames]
     when it states them. An array states its [items].
   A member a record does not require is an [option]. A string enum that
   states the values of another in the table's module is that one's type. A
   member named [$ref] is a reference, which the reader resolves.

   What a [$ref] names in another table's schema is a node of that schema,
   in the table dialect as its own table's model reads it, and is derived
   by these rules, named for that schema's file. What it names elsewhere
   outside the schema's own file (a component, or CaseTable's) is derived
   where it can be, by the same rules with its annotations and formats read
   as such; where it cannot (an open object, an [allOf], a [oneOf] the
   components state) it is [Yojson.Safe.t], which the table's validation
   against its schema holds to what it names.

   The TypeScript is of the same types, and of each service's schema
   (Places.service_schemas), read in the service dialect, which a table's
   reference out of the table dialect is read in too: the table dialect
   with an open object an open record, but one a node around it closes with
   [unevaluatedProperties: false] through a [$ref], an [allOf], a [oneOf] or
   an [anyOf], [allOf] an intersection, any [oneOf]
   or [anyOf] a union, a node of several types a union, [format], [examples]
   and [default] annotations, and an enum's constructors its values. Each
   schema is a namespace named as its file's stem, its root [T]; a string
   enum is a union of its values, and [<name>All] its values in order; and
   each table's value is a constant, [<model>], of its root's type.
   [ServiceSchemas] names each service's schema as its bundle does.

   A case table's schema extends CaseTable.json: its [allOf] is that alone,
   and it admits each of CaseTable's properties with [true] but [cases],
   which it states, or leaves out when it has no cases. Any other table's
   schema requires [$comment] as CaseTable's. A [why], wherever a table's
   schema states one, is CaseTable's. Every refusal is printed, and any stops
   the build. *)

open Xynolog_protocol_schema

type output = Ocaml | Typescript

let output, root =
  match Sys.argv with
  | [| _; root |] -> (Ocaml, root)
  | [| _; "typescript"; root |] -> (Typescript, root)
  | _ ->
      prerr_endline "usage: derive.exe [typescript] SPECS";
      exit 2

(* Each schema's [$id], and its file. *)
let ids =
  match Places.ids root with
  | Ok ids -> ids
  | Error errors ->
      List.iter (fun e -> prerr_endline (Decode.error_to_string e)) errors;
      exit 1

(* Every table's schema, from specs/src: each in the table dialect. *)
let table_schemas = List.map Places.schema_of (Places.tables root)
let refusals = ref []

(* Whether the node being derived is in the service dialect: a service's
   schema, or what it names. *)
let service = ref false

exception Stop

let refuse file pointer message =
  refusals :=
    Decode.error_to_string { Decode.file; pointer; message } :: !refusals;
  raise Stop

(* Each file's JSON, and the schema at a place in it. *)
let files = Hashtbl.create 64

let file_json file =
  match Hashtbl.find_opt files file with
  | Some json -> json
  | None -> (
      match Decode.parse_file ~root file with
      | Ok json ->
          Hashtbl.replace files file json;
          json
      | Error error ->
          refusals := Decode.error_to_string error :: !refusals;
          raise Stop)

let schema_at file pointer : Schema.t =
  match Decode.at_pointer pointer (file_json file) with
  | None -> refuse file pointer "names nothing"
  | Some json -> (
      match Decode.run ~file json Schema.decode with
      | Ok (schema, _) -> schema
      | Error error ->
          refuse file
            (pointer ^ if error.pointer = "" then "" else error.pointer)
            error.message)

let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token

(* Names. A label or type is the snake case of what it is written as; a
   constructor is that capitalized. *)
let keywords =
  [
    "and";
    "as";
    "assert";
    "begin";
    "class";
    "constraint";
    "do";
    "done";
    "downto";
    "effect";
    "else";
    "end";
    "exception";
    "external";
    "false";
    "for";
    "fun";
    "function";
    "functor";
    "if";
    "in";
    "include";
    "inherit";
    "initializer";
    "lazy";
    "let";
    "match";
    "method";
    "module";
    "mutable";
    "new";
    "nonrec";
    "object";
    "of";
    "open";
    "or";
    "private";
    "rec";
    "sig";
    "struct";
    "then";
    "to";
    "true";
    "try";
    "type";
    "val";
    "virtual";
    "when";
    "while";
    "with";
  ]

let snake text =
  let buffer = Buffer.create (String.length text + 4) in
  let last = ref '_' in
  String.iter
    (fun c ->
      let next =
        match c with
        | 'A' .. 'Z' ->
            (match !last with
            | 'a' .. 'z' | '0' .. '9' -> Buffer.add_char buffer '_'
            | _ -> ());
            Char.lowercase_ascii c
        | 'a' .. 'z' | '0' .. '9' -> c
        | _ -> '_'
      in
      if not (next = '_' && !last = '_') then Buffer.add_char buffer next;
      last := next)
    text;
  let s = Buffer.contents buffer in
  let s =
    if String.ends_with ~suffix:"_" s then String.sub s 0 (String.length s - 1)
    else s
  in
  if String.starts_with ~prefix:"_" s then String.sub s 1 (String.length s - 1)
  else s

let label text =
  let s = snake text in
  if s = "" || match s.[0] with 'a' .. 'z' -> false | _ -> true then None
  else if List.mem s keywords then Some (s ^ "_")
  else Some s

let constructor text =
  match snake text with
  | "" -> None
  | s -> (
      match s.[0] with
      | 'a' .. 'z' -> Some (String.capitalize_ascii s)
      | _ -> None)

(* Whether each of an enum's values names a constructor of its own. *)
let enum_names values =
  let names =
    List.map
      (function Schema.String_literal v -> constructor v | _ -> None)
      values
  in
  List.for_all Option.is_some names
  && List.length (List.sort_uniq compare names) = List.length names

(* The model a node derives. *)
type ty =
  | String
  | Int
  | Float
  | Bool
  | Json
  | Json_schema
  | Reference
  | Constant of Yojson.Safe.t
  | List of ty
  | Map of ty * ty
      (** Its keys' type, a string's or an enum's, and its values'. *)
  | Union of ty list
  | All of ty list
  | Nullable of ty
  | Named of string

type field = { label : string; key : string; ty : ty; required : bool }

type case = { name : string; payload : ty; dispatch : dispatch }

and dispatch =
  | Object of {
      required : string list;
      stated : string list;
      fixed : (string * Yojson.Safe.t) list;
    }
  | Kind of string list

type decl =
  | Record of string * field list * bool
      (** Its fields, and whether it admits members it does not state. *)
  | Enum of string * (string * string) list
  | Variant of string * case list

(* One table's module, as it is derived: its declarations in the order they
   are written (each after what it names), the name each place of a schema
   was given, and the names taken. *)
let decls = ref []
let named : (string * string, ty) Hashtbl.t = Hashtbl.create 64
let deriving : (string * string, unit) Hashtbl.t = Hashtbl.create 64
let taken : (string, string * string) Hashtbl.t = Hashtbl.create 64

(* The name each place was given. *)
let places : (string * string, string) Hashtbl.t = Hashtbl.create 64

(* Each string enum's type, by its values: an enum that states the values of
   another in the module is its type. *)
let enums : (string list, string) Hashtbl.t = Hashtbl.create 16

let reset () =
  decls := [];
  Hashtbl.reset enums;
  Hashtbl.reset named;
  Hashtbl.reset deriving;
  Hashtbl.reset taken;
  Hashtbl.reset places

(* The first of [candidates] no other place has, for the place [key]. *)
let name_for key candidates =
  let candidates =
    List.map (fun n -> if List.mem n keywords then n ^ "_" else n) candidates
  in
  match
    List.find_opt
      (fun name ->
        match Hashtbl.find_opt taken name with
        | None -> true
        | Some owner -> owner = key)
      candidates
  with
  | Some name ->
      Hashtbl.replace taken name key;
      Hashtbl.replace places key name;
      name
  | None ->
      refuse (fst key) (snd key)
        ("has no name its module's other types leave it: "
        ^ String.concat ", " candidates)

let declare decl = decls := decl :: !decls

(* The names a place may take, shortest first: its last property's, then
   qualified by each enclosing one (a [oneOf]'s member by the name it was
   given), and, outside the table's own schema, by its file's. A list's
   items and a map's values are named with what holds them. *)
let candidates ~own file pointer =
  let tokens =
    match String.split_on_char '/' pointer with
    | "" :: tokens -> List.map Decode.unescape tokens
    | _ -> []
  in
  let rec words at = function
    | "properties" :: name :: rest ->
        snake name :: words (at ^ "/properties/" ^ Decode.escape name) rest
    | "items" :: rest -> "item" :: words (at ^ "/items") rest
    | "additionalProperties" :: rest ->
        "value" :: words (at ^ "/additionalProperties") rest
    | (("oneOf" | "anyOf") as key) :: index :: rest -> (
        let at = at ^ "/" ^ key ^ "/" ^ index in
        match Hashtbl.find_opt places (file, at) with
        | Some name when key = "oneOf" -> name :: words at rest
        | _ when !service -> ("member_" ^ index) :: words at rest
        | _ -> words at rest)
    | token :: rest -> words (at ^ "/" ^ Decode.escape token) rest
    | [] -> []
  in
  let words = List.rev (words "" tokens) in
  let base =
    if own then []
    else [ snake (Filename.remove_extension (Filename.basename file)) ]
  in
  let rec grow acc = function
    | [] -> []
    | word :: rest ->
        let acc = word :: acc in
        if acc = [ "item" ] || acc = [ "value" ] then grow acc rest
        else String.concat "_" acc :: grow acc rest
  in
  let names =
    match grow [] words with
    | [] -> [ (match base with [] -> "t" | _ -> String.concat "_" base) ]
    | names -> List.map (fun n -> String.concat "_" (base @ [ n ])) names
  in
  (* Last, a place of the table's own schema is qualified by its model. *)
  let names =
    if own then
      names
      @ [
          snake (Filename.remove_extension (Filename.basename file))
          ^ "_"
          ^ List.nth names (List.length names - 1);
        ]
    else names
  in
  List.fold_left
    (fun kept n -> if List.mem n kept then kept else kept @ [ n ])
    [] names

let annotations = [ "$schema"; "$id"; "$comment"; "title"; "description" ]

(* What may be said beside a node: in the service dialect, its format, its
   examples and its default too, and the [unevaluatedProperties] that closes
   it and the [$defs] it holds. *)
let said () =
  if !service then
    annotations
    @ [ "format"; "examples"; "default"; "unevaluatedProperties"; "$defs" ]
  else annotations

(* Whether the node about to be derived is closed by an
   [unevaluatedProperties] of a node that applies it in place: the [$ref],
   the [allOf] or the [oneOf] or [anyOf] of a closed node. Each node reads it
   as it starts, and clears it. *)
let closing = ref false

(* Whether [s] is [{ "type": "null" }], the alternative of a node that may be
   null. *)
let is_null = function
  | Schema.Schema l -> Schema.keywords l = [ "type" ] && l.type_ = Some [ Null ]
  | Boolean_schema _ -> false

let takes = function
  | Schema.String -> [ "enum"; "const"; "minLength"; "maxLength"; "pattern" ]
  | Integer | Number -> [ "enum"; "const"; "minimum"; "maximum" ]
  | Boolean -> [ "enum"; "const" ]
  | Object ->
      [
        "properties";
        "required";
        "additionalProperties";
        "propertyNames";
        "minProperties";
        "maxProperties";
      ]
  | Array -> [ "items"; "minItems"; "maxItems"; "uniqueItems" ]
  | Null -> []

let type_words = "string, integer, number, boolean, object, array"

(* What a reference names: its file from specs/src, and its place there. *)
let target (r : Decode.reference) =
  match Decode.names ids r with
  | Some named -> named
  | None -> refuse r.file r.pointer (r.target ^ " is an $id no schema states")

(* The object or the other type a node is, following references. *)
let rec resolved file (s : Schema.t) =
  match s with
  | Schema { ref_ = Some r; _ } ->
      let file, pointer = target r in
      resolved file (schema_at file pointer)
  | s -> (file, s)

let object_of file s =
  match resolved file s with
  | _, Schema ({ type_ = Some [ Object ]; properties = Some _; _ } as l) ->
      Some l
  | _ -> None

let scalar_of file s =
  match resolved file s with
  | _, Schema { type_ = Some [ t ]; _ } when t <> Schema.Object -> Some t
  | _ -> None

let fixed (l : Schema.schema) =
  match (l.const, l.enum) with
  | Some v, _ -> Some (Schema.literal_json v)
  | None, Some [ v ] -> Some (Schema.literal_json v)
  | _ -> None

let property_fixed file (l : Schema.schema) name =
  match List.assoc_opt name (Option.value ~default:[] l.properties) with
  | None -> None
  | Some s -> ( match resolved file s with _, Schema p -> fixed p | _ -> None)

let requires (l : Schema.schema) name =
  List.mem name (Option.value ~default:[] l.required)

let states (l : Schema.schema) name =
  List.mem_assoc name (Option.value ~default:[] l.properties)

let apart file one other =
  let names =
    List.sort_uniq compare
      (List.map fst (Option.value ~default:[] one.Schema.properties)
      @ List.map fst (Option.value ~default:[] other.Schema.properties))
  in
  List.exists
    (fun name ->
      (requires one name && not (states other name))
      || (requires other name && not (states one name))
      || requires one name && requires other name
         &&
         match
           (property_fixed file one name, property_fixed file other name)
         with
         | Some a, Some b -> not (Yojson.Safe.equal a b)
         | _ -> false)
    names

let kinds = function
  | Schema.String -> [ "String" ]
  | Integer -> [ "Int" ]
  | Number -> [ "Int"; "Number" ]
  | Boolean -> [ "Bool" ]
  | Array -> [ "Array" ]
  | Null -> [ "Null" ]
  | Object -> [ "Object" ]

let is_why_of_case_table file (s : Schema.t) =
  match s with
  | Schema ({ ref_ = Some r; _ } as l) ->
      Schema.keywords l = [ "$ref" ]
      && target { r with file }
         = (Places.case_table, "/properties/cases/items/properties/why")
  | _ -> false

(* The type of the node [s] at [pointer] of [file]. [strict] holds it to the
   table dialect, as a node of a table's own schema; otherwise it is what a
   reference names, derived where it can be. *)
let rec derive ~strict ~own file pointer (s : Schema.t) : ty =
  let key = (file, pointer) in
  match Hashtbl.find_opt named key with
  | Some ty -> ty
  | None ->
      if Hashtbl.mem deriving key then
        refuse file pointer "names itself, which no type can be derived from";
      Hashtbl.replace deriving key ();
      let ty = node ~strict ~own file pointer s in
      Hashtbl.remove deriving key;
      Hashtbl.replace named key ty;
      ty

and node ~strict ~own file pointer s =
  let fail message = if strict then refuse file pointer message else Json in
  match s with
  | Boolean_schema _ ->
      fail
        "is no schema a type can be derived from: a boolean schema is only an \
         admission of CaseTable's"
  | Schema l -> (
      let present = Schema.keywords l in
      let closed_here =
        !closing
        ||
        match l.unevaluated_properties with
        | None -> false
        | Some (Boolean_schema false) -> true
        | Some _ ->
            refuse file
              (pointer / "unevaluatedProperties")
              "must be false: it closes what its node applies in place"
      in
      closing := false;
      (* [derive], of a schema this node applies in place. *)
      let in_place f =
        closing := closed_here;
        Fun.protect ~finally:(fun () -> closing := false) f
      in
      let beside allowed =
        List.iter
          (fun k ->
            if strict && not (List.mem k allowed) then
              refuse file (pointer / k) "is beside what this node is")
          present
      in
      match l with
      | { ref_ = Some r; _ } -> (
          if
            strict
            && List.exists
                 (fun k -> k <> "$ref" && not (List.mem k (said ())))
                 present
          then
            refuse file pointer
              "has keywords beside its $ref, where only $comment, title or \
               description is said of a reference";
          match target r with
          | tfile, "" when tfile = Places.json_schema -> Json_schema
          | tfile, tpointer ->
              let tabled =
                strict && (tfile = file || List.mem tfile table_schemas)
              in
              let named () =
                derive ~strict:(!service || tabled)
                  ~own:(own && tfile = file)
                  tfile tpointer (schema_at tfile tpointer)
              in
              if output = Typescript && not (!service || tabled) then (
                (* TypeScript types what a table names outside the table
                   dialect in the service dialect, where OCaml's is JSON. *)
                service := true;
                Fun.protect
                  ~finally:(fun () -> service := false)
                  (fun () -> in_place named))
              else in_place named)
      | _ when !service && List.for_all (fun k -> List.mem k (said ())) present
        ->
          Json
      | _ when present = [ "$comment" ] ->
          if strict && String.trim (Option.get l.comment) = "" then
            refuse file pointer
              "is JSON by nature, and must say what it is in $comment";
          Json
      | { all_of = Some members; _ } when !service ->
          beside ("allOf" :: said ());
          All
            (List.mapi
               (fun i m ->
                 in_place (fun () ->
                     derive ~strict ~own file
                       (pointer / "allOf" / string_of_int i)
                       m))
               members)
      | { any_of = Some members; _ } when !service ->
          beside ("anyOf" :: said ());
          union ~own ~in_place file (pointer / "anyOf") members
      | { one_of = Some members; _ } when !service ->
          beside ("oneOf" :: "discriminator" :: said ());
          union ~own ~in_place file (pointer / "oneOf") members
      | { all_of = Some _; _ } ->
          fail
            "is an allOf, which a table's schema says only where it extends \
             CaseTable.json"
      | { any_of = Some members; _ } ->
          if not strict then Json
          else (
            beside ("anyOf" :: said ());
            match members with
            | [ (Schema { ref_ = Some _; _ } as named); null ] when is_null null
              ->
                Nullable
                  (derive ~strict ~own file (pointer / "anyOf" / "0") named)
            | _ ->
                refuse file pointer
                  "must be { \"anyOf\": [{ \"$ref\": … }, { \"type\": \"null\" \
                   }] }: an anyOf in a table's schema is a $ref that may be \
                   null")
      | { one_of = Some members; _ } ->
          if not strict then Json
          else (
            beside ("oneOf" :: said ());
            match List.rev members with
            | null :: rest when is_null null ->
                Nullable (variant ~own file pointer (List.rev rest))
            | _ -> variant ~own file pointer members)
      | { type_ = Some [ Null ]; _ } when strict ->
          refuse file pointer
            "is null alone, which a table's schema says only as the last \
             member of a oneOf or the second of an anyOf"
      | { type_ = Some (([ t ] | [ t; Null ]) as types); _ } when t <> Null -> (
          let nullable ty =
            if List.mem Schema.Null types then Nullable ty else ty
          in
          if strict then (
            beside (("type" :: said ()) @ takes t);
            match (l.enum, l.const) with
            | Some values, _ ->
                List.iter
                  (fun v ->
                    let ok =
                      match (t, v) with
                      | String, Schema.String_literal _
                      | (Integer | Number), Int_literal _
                      | Boolean, Bool_literal _ ->
                          true
                      | _ -> false
                    in
                    if not ok then
                      refuse file (pointer / "enum")
                        (Printf.sprintf "holds %s, which is no %s"
                           (Yojson.Safe.to_string (Schema.literal_json v))
                           (Schema.type_word t)))
                  values
            | _ -> ());
          match t with
          | String | Integer | Number | Boolean | Null ->
              nullable (scalar ~strict ~own file pointer t l)
          | Array -> (
              match l.items with
              | Some items ->
                  nullable
                    (List (derive ~strict ~own file (pointer / "items") items))
              | None -> fail "is an array, and must state its items")
          | Object -> (
              match (l.properties, l.additional_properties) with
              | Some properties, Some (Boolean_schema false) ->
                  nullable
                    (record ~strict ~own ~open_:false file pointer l properties)
              | Some properties, (None | Some (Boolean_schema true))
                when !service ->
                  let open_ =
                    l.additional_properties = Some (Boolean_schema true)
                    || not closed_here
                  in
                  nullable
                    (record ~strict ~own ~open_ file pointer l properties)
              | Some _, _ ->
                  fail
                    "has properties, and must be closed with \
                     \"additionalProperties\": false"
              | None, Some (Schema _ as values) ->
                  let keys =
                    match l.property_names with
                    | None -> String
                    | Some names -> (
                        match
                          derive ~strict ~own file
                            (pointer / "propertyNames")
                            names
                        with
                        | (String | Named _) as keys -> keys
                        | _ ->
                            refuse file
                              (pointer / "propertyNames")
                              "must be a string or a string enum: a map's keys \
                               are names")
                  in
                  nullable
                    (Map
                       ( keys,
                         derive ~strict ~own file
                           (pointer / "additionalProperties")
                           values ))
              | None, (None | Some (Boolean_schema true)) when !service ->
                  nullable (Map (String, Json))
              | None, _ ->
                  fail
                    "is a map, and must state its values' schema in \
                     additionalProperties"))
      | { type_ = Some _; _ } when not strict -> Json
      | { type_ = Some types; _ } when !service ->
          beside
            (("type" :: said ())
            @ List.concat_map takes (List.sort_uniq compare types));
          let scalar = function
            | Schema.String -> String
            | Integer -> Int
            | Number -> Float
            | Boolean -> Bool
            | t ->
                refuse file (pointer / "type")
                  ("names " ^ Schema.type_word t
                 ^ " beside other types, where only scalars are named together"
                  )
          in
          let some = List.filter (( <> ) Schema.Null) types in
          let union = Union (List.map scalar some) in
          if List.mem Schema.Null types then Nullable union else union
      | _ ->
          if strict then
            refuse file (pointer / "type")
              ("must be one of " ^ type_words
             ^ ", or the node a $ref, a oneOf or { \"$comment\": … }")
          else Json)

and scalar ~strict ~own file pointer t (l : Schema.schema) =
  match fixed l with
  | Some v -> Constant v
  | None -> (
      match (t, l.enum) with
      | Schema.String, Some values when (not strict) && not (enum_names values)
        ->
          String
      | Schema.String, Some values -> (
          let cases =
            List.map
              (function
                | Schema.String_literal v -> (
                    (* The service dialect has no OCaml model, so no
                       constructors. *)
                    match if !service then Some v else constructor v with
                    | Some c -> (c, v)
                    | None ->
                        refuse file (pointer / "enum")
                          (Printf.sprintf "holds %S, which names no constructor"
                             v))
                | _ ->
                    refuse file (pointer / "enum")
                      "holds a value that is no string")
              values
          in
          let constructors = List.map fst cases in
          if
            List.length (List.sort_uniq compare constructors)
            <> List.length constructors
          then
            refuse file (pointer / "enum")
              "holds two values that name one constructor";
          match Hashtbl.find_opt enums (List.map snd cases) with
          | Some name -> Named name
          | None ->
              let name =
                name_for (file, pointer) (candidates ~own file pointer)
              in
              Hashtbl.replace enums (List.map snd cases) name;
              declare (Enum (name, cases));
              Named name)
      | Schema.String, None -> String
      | Integer, _ -> Int
      | Number, _ -> Float
      | Boolean, _ -> Bool
      | _ -> refuse file pointer "is no scalar")

and record ~strict ~own ~open_ file pointer (l : Schema.schema) properties =
  let required = Option.value ~default:[] l.required in
  record_of ~strict ~own ~open_ file pointer ~required
    ~names:(candidates ~own file pointer)
    properties

and record_of ~strict ~own ~open_ file pointer ~required ~names properties =
  if strict then
    List.iter
      (fun name ->
        if not (List.mem_assoc name properties) then
          refuse file (pointer / "required")
            ("requires " ^ name ^ ", which it does not state"))
      required;
  let name = name_for (file, pointer) names in
  let fields =
    List.map
      (fun (key, s) ->
        let at = pointer / "properties" / key in
        if strict && key = "why" && not (is_why_of_case_table file s) then
          refuse file at
            "is CaseTable's, and must be { \"$ref\": \
             \"…/CaseTable.json#/properties/cases/items/properties/why\" }";
        let ty =
          match s with
          | Schema.Boolean_schema true when strict && own && pointer = "" ->
              admitted key
          | s when key = "$ref" -> (
              match derive ~strict ~own file at s with
              | String -> Reference
              | _ -> refuse file at "is a $ref, and must be a string")
          | s -> derive ~strict ~own file at s
        in
        let label =
          match label key with
          | Some l -> l
          | None -> refuse file at "names no label"
        in
        { label; key; ty; required = List.mem key required })
      properties
  in
  let labels = List.map (fun f -> f.label) fields in
  if List.length (List.sort_uniq compare labels) <> List.length labels then
    refuse file pointer "states two properties that name one label";
  declare (Record (name, fields, open_));
  Named name

(* A property a case table's schema admits with [true]: CaseTable's own. *)
and admitted key =
  let pointer = "/properties" / key in
  derive ~strict:true ~own:false Places.case_table pointer
    (schema_at Places.case_table pointer)

and variant ~own file pointer members =
  if List.length members < 2 then
    refuse file (pointer / "oneOf") "must name two members or more";
  let indexed = List.mapi (fun i m -> (i, m)) members in
  let scalars =
    List.filter_map
      (fun (i, m) -> Option.map (fun t -> (i, m, t)) (scalar_of file m))
      indexed
  in
  let objects =
    List.filter
      (fun (i, _) -> not (List.exists (fun (j, _, _) -> i = j) scalars))
      indexed
  in
  let scalar_types = List.map (fun (_, _, t) -> t) scalars in
  if
    List.length (List.sort_uniq compare scalar_types)
    <> List.length scalar_types
  then
    refuse file (pointer / "oneOf")
      "has two members of one type other than an object, which nothing tells \
       apart";
  let objects =
    List.map
      (fun (i, m) ->
        match object_of file m with
        | Some o when o.additional_properties = Some (Boolean_schema false) ->
            (i, m, o)
        | _ ->
            refuse file (pointer / "oneOf")
              "must be of closed objects and of at most one member of each \
               other type, so a value is one member or none")
      objects
  in
  List.iteri
    (fun a (i, _, one) ->
      List.iteri
        (fun b (j, _, other) ->
          if b > a && not (apart file one other) then
            refuse file (pointer / "oneOf")
              (Printf.sprintf
                 "has members %d and %d, which no property tells apart" i j))
        objects)
    objects;
  let name = name_for (file, pointer) (candidates ~own file pointer) in
  (* A property every object requires and fixes, to values that differ. *)
  let tag =
    match objects with
    | [] | [ _ ] -> None
    | (_, _, first) :: _ ->
        List.find_opt
          (fun property ->
            let values =
              List.map
                (fun (_, _, o) ->
                  if requires o property then property_fixed file o property
                  else None)
                objects
            in
            List.for_all Option.is_some values
            && List.length
                 (List.sort_uniq compare
                    (List.map
                       (fun v -> Yojson.Safe.to_string (Option.get v))
                       values))
               = List.length values)
          (List.map fst (Option.value ~default:[] first.properties))
  in
  let title i m =
    match resolved file m with
    | _, Schema { title = Some t; _ } -> (
        match constructor t with
        | Some c -> c
        | None ->
            refuse file
              (pointer / "oneOf" / string_of_int i / "title")
              "names no constructor")
    | _ ->
        refuse file
          (pointer / "oneOf" / string_of_int i)
          "is told apart from the other members by no property they all fix, \
           so it must have a title, which names its constructor"
  in
  let object_cases =
    List.map
      (fun (i, m, (o : Schema.schema)) ->
        let constructor_name =
          match (tag, scalars, objects) with
          | Some property, _, _ -> (
              match property_fixed file o property with
              | Some (`String v) -> (
                  match constructor v with Some c -> c | None -> title i m)
              | _ -> title i m)
          | None, _ :: _, [ _ ] -> "Object"
          | None, _, _ -> title i m
        in
        let at = pointer / "oneOf" / string_of_int i in
        let payload =
          match m with
          | Schema.Schema { ref_ = Some _; _ } ->
              derive ~strict:true ~own file at m
          | _ -> (
              let key = (file, at) in
              match Hashtbl.find_opt named key with
              | Some ty -> ty
              | None ->
                  let payload_name = snake constructor_name in
                  let ty =
                    record_of ~strict:true ~own ~open_:false file at
                      ~required:(Option.value ~default:[] o.required)
                      ~names:[ payload_name; name ^ "_" ^ payload_name ]
                      (Option.get o.properties)
                  in
                  Hashtbl.replace named key ty;
                  ty)
        in
        let fixed =
          List.filter_map
            (fun (property, _) ->
              Option.map
                (fun v -> (property, v))
                (property_fixed file o property))
            (Option.value ~default:[] o.properties)
        in
        ( i,
          {
            name = constructor_name;
            payload;
            dispatch =
              Object
                {
                  required = Option.value ~default:[] o.required;
                  stated = List.map fst (Option.value ~default:[] o.properties);
                  fixed;
                };
          } ))
      objects
  in
  let scalar_cases =
    List.map
      (fun (i, m, t) ->
        let constructor_name = String.capitalize_ascii (Schema.type_word t) in
        let at = pointer / "oneOf" / string_of_int i in
        ( i,
          {
            name = constructor_name;
            payload = derive ~strict:true ~own file at m;
            dispatch = Kind (kinds t);
          } ))
      scalars
  in
  let cases =
    List.map snd
      (List.sort
         (fun (i, _) (j, _) -> compare i j)
         (object_cases @ scalar_cases))
  in
  let constructors = List.map (fun c -> c.name) cases in
  if
    List.length (List.sort_uniq compare constructors)
    <> List.length constructors
  then
    refuse file (pointer / "oneOf") "has two members that name one constructor";
  declare (Variant (name, cases));
  Named name

(* The service dialect's [oneOf] or [anyOf]: any one of its members, or null
   when [{ "type": "null" }] is its last. *)
and union ~own ~in_place file pointer members =
  let null = List.exists is_null members in
  let types =
    List.concat
      (List.mapi
         (fun i m ->
           if is_null m then []
           else
             [
               in_place (fun () ->
                   derive ~strict:true ~own file (pointer / string_of_int i) m);
             ])
         members)
  in
  let union = match types with [ one ] -> one | many -> Union many in
  if null then Nullable union else union

(* A table's schema at its top: a closed object, with CaseTable's admissions
   for a case table and CaseTable's [$comment] for any other. *)
let table_schema table file =
  let s = schema_at file "" in
  let to_case_table = Decode.base ^ Places.case_table in
  match s with
  | Schema ({ type_ = Some [ Object ]; properties = Some properties; _ } as l)
    ->
      let case_table = schema_at Places.case_table "" in
      let case_table_properties, case_table_required =
        match case_table with
        | Schema { properties = Some ps; required; _ } ->
            (ps, Option.value ~default:[] required)
        | _ -> refuse Places.case_table "" "must state its properties"
      in
      let names_case_table (r : Decode.reference) fragment =
        target { r with file } = (Places.case_table, fragment)
      in
      let own_required = Option.value ~default:[] l.required in
      if Places.is_case_table table then (
        (match l.all_of with
        | Some [ Schema ({ ref_ = Some r; _ } as part) ]
          when Schema.keywords part = [ "$ref" ] && names_case_table r "" ->
            ()
        | _ ->
            refuse file "/allOf"
              (Printf.sprintf
                 "must be [{ \"$ref\": \"%s\" }] and nothing else: a case \
                  table is a CaseTable"
                 to_case_table));
        List.iter
          (fun (name, _) ->
            match List.assoc_opt name properties with
            | _ when name = "cases" -> (
                match List.assoc_opt "cases" properties with
                | Some (Boolean_schema true) ->
                    refuse file "/properties/cases"
                      "must say what a case is, or be left out of a table that \
                       has no cases"
                | _ -> ())
            | Some (Boolean_schema true) -> ()
            | _ ->
                refuse file ("/properties" / name)
                  "is CaseTable's, and must be admitted with true: CaseTable \
                   states it")
          case_table_properties)
      else (
        if Option.is_some l.all_of then
          refuse file "/allOf"
            "extends nothing: only a case table's schema extends CaseTable";
        match List.assoc_opt "$comment" properties with
        | Some (Schema ({ ref_ = Some r; _ } as c))
          when Schema.keywords c = [ "$ref" ]
               && names_case_table r "/properties/$comment"
               && List.mem "$comment" own_required ->
            ()
        | _ ->
            refuse file "/properties/$comment"
              (Printf.sprintf
                 "must be required, and be { \"$ref\": \
                  \"%s#/properties/$comment\" }"
                 to_case_table));
      if l.additional_properties <> Some (Boolean_schema false) then
        refuse file "" "must be a closed object: a table is one";
      List.iter
        (fun k ->
          if
            not
              (List.mem k
                 ("type" :: "allOf" :: "properties" :: "required"
                :: "additionalProperties" :: annotations))
          then refuse file ("" / k) "is no keyword of a table's schema")
        (Schema.keywords l);
      let required =
        own_required
        @ if Places.is_case_table table then case_table_required else []
      in
      List.iter
        (fun name ->
          if not (List.mem_assoc name properties) then
            refuse file "/required"
              ("requires " ^ name ^ ", which it does not state"))
        own_required;
      ignore
        (record_of ~strict:true ~own:true ~open_:false file "" ~required
           ~names:[ "t" ] properties)
  | _ -> refuse file "" "must be a closed object: a table is one"

(* Printing. A type's decoder has its name, but a table's own, [decode]. *)
let decoder_name = function "t" -> "decode" | name -> name

(* A constant's type, which holds it. *)
let constant_type : Yojson.Safe.t -> ty = function
  | `String _ -> String
  | `Int _ -> Int
  | `Float _ -> Float
  | `Bool _ -> Bool
  | v ->
      invalid_arg
        ("a constant of no scalar type, which no table states: "
       ^ Yojson.Safe.to_string v)

(* A constant as an OCaml expression of its type. *)
let constant_expr : Yojson.Safe.t -> string = function
  | `String s -> Printf.sprintf "%S" s
  | `Int n -> Printf.sprintf "(%d)" n
  | `Float f -> Printf.sprintf "(%h)" f
  | `Bool b -> string_of_bool b
  | v -> invalid_arg (Yojson.Safe.to_string v)

(* What checks that a value is the constant, as a decoder of unit. *)
let check = function
  | `String s -> Printf.sprintf "(Decode.literal %S)" s
  | v ->
      Printf.sprintf "(Decode.constant (Yojson.Safe.from_string %S))"
        (Yojson.Safe.to_string v)

let rec type_expr = function
  | String -> "string"
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Json -> "Yojson.Safe.t"
  | Json_schema -> "Schema.t"
  | Reference -> "Decode.reference"
  | Constant v -> type_expr (constant_type v)
  | List t -> "(" ^ type_expr t ^ ") list"
  | Map (keys, values) ->
      "(" ^ type_expr keys ^ " * " ^ type_expr values ^ ") list"
  | Nullable t -> "(" ^ type_expr t ^ ") option"
  | Named n -> n
  | Union _ | All _ -> invalid_arg "the service dialect has no OCaml model"

let rec decoder = function
  | String -> "Decode.string"
  | Int -> "Decode.int"
  | Float -> "Decode.number"
  | Bool -> "Decode.bool"
  | Json -> "Decode.json"
  | Json_schema -> "Schema.decode"
  | Reference -> "Decode.reference"
  | Constant v ->
      Printf.sprintf "(fun cursor -> %s cursor; %s)" (check v) (constant_expr v)
  | List t -> "(Decode.list " ^ decoder t ^ ")"
  | Map (String, values) -> "(Decode.map " ^ decoder values ^ ")"
  | Map (keys, values) ->
      "(Decode.map_by " ^ decoder keys ^ " " ^ decoder values ^ ")"
  | Nullable t -> "(Decode.nullable " ^ decoder t ^ ")"
  | Named n -> decoder_name n
  | Union _ | All _ -> invalid_arg "the service dialect has no OCaml model"

let strings xs =
  "[" ^ String.concat "; " (List.map (Printf.sprintf "%S") xs) ^ "]"

let print_decl buffer = function
  | Record (_, _, true) -> invalid_arg "the service dialect has no OCaml model"
  | Record (name, [], false) ->
      Printf.bprintf buffer
        "type %s = unit\n\nlet %s = Decode.obj (fun _ -> ())\n\n" name
        (decoder_name name)
  | Record (name, fields, false) ->
      Printf.bprintf buffer "type %s = {\n" name;
      List.iter
        (fun f ->
          Printf.bprintf buffer "  %s : %s;\n" f.label
            (if f.required then type_expr f.ty
             else "(" ^ type_expr f.ty ^ ") option"))
        fields;
      Printf.bprintf buffer
        "}\n\nlet %s =\n  Decode.obj (fun o : %s ->\n    {\n"
        (decoder_name name) name;
      List.iter
        (fun f ->
          Printf.bprintf buffer "      %s = Decode.%s o %S %s;\n" f.label
            (if f.required then "required" else "optional")
            f.key (decoder f.ty))
        fields;
      Printf.bprintf buffer "    })\n\n"
  | Enum (name, cases) ->
      Printf.bprintf buffer "type %s = %s\n\n" name
        (String.concat " | " (List.map fst cases));
      Printf.bprintf buffer "let %s = Decode.enum [ %s ]\n\n" name
        (String.concat "; "
           (List.map (fun (c, v) -> Printf.sprintf "(%S, %s)" v c) cases));
      Printf.bprintf buffer "let %s_value = function %s\n\n" name
        (String.concat " | "
           (List.map (fun (c, v) -> Printf.sprintf "%s -> %S" c v) cases));
      Printf.bprintf buffer "let %s_all = [ %s ]\n\n" name
        (String.concat "; " (List.map fst cases))
  | Variant (name, cases) ->
      Printf.bprintf buffer "type %s =\n" name;
      List.iter
        (fun c ->
          match c.payload with
          | Constant _ -> Printf.bprintf buffer "  | %s\n" c.name
          | ty -> Printf.bprintf buffer "  | %s of %s\n" c.name (type_expr ty))
        cases;
      let build c =
        match c.payload with
        | Constant v ->
            Printf.sprintf "(fun cursor -> %s cursor; %s)" (check v) c.name
        | ty ->
            Printf.sprintf "(fun cursor -> %s (%s cursor))" c.name (decoder ty)
      in
      let objects =
        List.filter
          (fun c -> match c.dispatch with Object _ -> true | Kind _ -> false)
          cases
      in
      Printf.bprintf buffer
        "\nlet %s cursor =\n  match Decode.kind cursor with\n" name;
      if objects <> [] then (
        Printf.bprintf buffer "  | Decode.Object ->\n      Decode.one_of [\n";
        List.iter
          (fun c ->
            match c.dispatch with
            | Object { required; stated; fixed } ->
                Printf.bprintf buffer
                  "        { Decode.required = %s; stated = %s; fixed = [%s]; \
                   decode = %s };\n"
                  (strings required) (strings stated)
                  (String.concat "; "
                     (List.map
                        (fun (k, v) ->
                          Printf.sprintf "(%S, Yojson.Safe.from_string %S)" k
                            (Yojson.Safe.to_string v))
                        fixed))
                  (build c)
            | Kind _ -> ())
          objects;
        Printf.bprintf buffer "      ] cursor\n");
      List.iter
        (fun c ->
          match c.dispatch with
          | Kind ks ->
              Printf.bprintf buffer "  | %s -> %s cursor\n"
                (String.concat " | " (List.map (fun k -> "Decode." ^ k) ks))
                (build c)
          | Object _ -> ())
        cases;
      Printf.bprintf buffer
        "  | _ -> Decode.refuse cursor \"is none of the oneOf's members\"\n\n"

let stop_on_refusals () =
  if !refusals <> [] then (
    List.iter prerr_endline (List.rev !refusals);
    Printf.eprintf "%d refusals\n" (List.length !refusals);
    exit 1)

let ocaml () =
  let tables = Places.tables root in
  let buffer = Buffer.create (1 lsl 20) in
  Buffer.add_string buffer
    "(* Every table's model, derived from its schema by reader/derive. *)\n\n";
  let derived =
    List.filter_map
      (fun table ->
        reset ();
        let file = Places.schema_of table in
        match
          if not (Sys.file_exists (Filename.concat root file)) then
            refuse table "" ("has no schema, which would be " ^ file);
          table_schema table file
        with
        | () ->
            let model = Places.model_of table in
            Printf.bprintf buffer "module %s = struct\n"
              (String.capitalize_ascii model);
            Printf.bprintf buffer "  (* %s, by %s *)\n\n" table file;
            List.iter (print_decl buffer) (List.rev !decls);
            Buffer.add_string buffer "end\n\n";
            Some (table, model)
        | exception Stop -> None)
      tables
  in
  stop_on_refusals ();
  Buffer.add_string buffer "type t = {\n";
  List.iter
    (fun (_, model) ->
      Printf.bprintf buffer "  %s : %s.t;\n" model
        (String.capitalize_ascii model))
    derived;
  Buffer.add_string buffer "}\n\n";
  Buffer.add_string buffer
    "type loader = { table : 'a. string -> (Decode.cursor -> 'a) -> ('a, \
     Decode.error list) result }\n\n\
     let ( let+ ) result f = Result.map f result\n\n\
     let ( and+ ) a b =\n\
    \  match (a, b) with\n\
    \  | Ok a, Ok b -> Ok (a, b)\n\
    \  | Error e, Ok _ | Ok _, Error e -> Error e\n\
    \  | Error a, Error b -> Error (a @ b)\n\n\
     let files = [ ";
  Buffer.add_string buffer
    (String.concat "; "
       (List.map (fun (table, _) -> Printf.sprintf "%S" table) derived));
  Buffer.add_string buffer " ]\n\nlet load { table } =\n";
  List.iteri
    (fun i (table, model) ->
      Printf.bprintf buffer "  %s %s = table %S %s.decode\n"
        (if i = 0 then "let+" else "and+")
        model table
        (String.capitalize_ascii model))
    derived;
  Printf.bprintf buffer "  in\n  { %s }\n"
    (String.concat "; " (List.map snd derived));
  print_string (Buffer.contents buffer)

(* TypeScript: each schema's types in a namespace named as its file, every
   type in PascalCase and the schema's own [T], and each enum's values in
   order, as [_all] is in OCaml; every service schema's [T] by its name; and
   each table, as a constant of its type. *)
let pascal name =
  String.concat ""
    (List.map String.capitalize_ascii (String.split_on_char '_' name))

let camel name = String.uncapitalize_ascii (pascal name)
let quoted text = Yojson.Safe.to_string (`String text)

(* A JSON value as TypeScript types it: what [Json] and [Json_schema] are. *)
let json_type = "JsonValue"

let rec ts_type = function
  | String | Reference -> "string"
  | Int | Float -> "number"
  | Bool -> "boolean"
  | Json | Json_schema -> json_type
  | Constant v -> Yojson.Safe.to_string v
  | List t -> "(" ^ ts_type t ^ ")[]"
  | Map (String, values) -> "{ [key: string]: " ^ ts_type values ^ " }"
  | Map (keys, values) ->
      "{ [key in " ^ ts_type keys ^ "]?: " ^ ts_type values ^ " }"
  | Nullable t -> "(" ^ ts_type t ^ " | null)"
  | Named n -> pascal n
  | Union types -> "(" ^ String.concat " | " (List.map ts_type types) ^ ")"
  | All types -> "(" ^ String.concat " & " (List.map ts_type types) ^ ")"

let ts_decl file buffer decl =
  let name =
    match decl with Record (n, _, _) | Enum (n, _) | Variant (n, _) -> n
  in
  if pascal name = json_type then
    refuse file "" ("names a type " ^ json_type ^ ", which is JSON's");
  match decl with
  | Record (name, fields, open_) ->
      Printf.bprintf buffer "  export type %s = {\n" (pascal name);
      List.iter
        (fun f ->
          Printf.bprintf buffer "    %s%s: %s;\n" (quoted f.key)
            (if f.required then "" else "?")
            (ts_type f.ty))
        fields;
      if open_ then Buffer.add_string buffer "    [member: string]: unknown;\n";
      Buffer.add_string buffer "  };\n"
  | Enum (name, cases) ->
      let values = List.map (fun (_, v) -> quoted v) cases in
      Printf.bprintf buffer "  export type %s = %s;\n" (pascal name)
        (String.concat " | " values);
      Printf.bprintf buffer "  export const %s: readonly %s[] = [%s];\n"
        (camel (name ^ "_all"))
        (pascal name)
        (String.concat ", " values)
  | Variant (name, cases) ->
      Printf.bprintf buffer "  export type %s = %s;\n" (pascal name)
        (String.concat " | " (List.map (fun c -> ts_type c.payload) cases))

let typescript () =
  let buffer = Buffer.create (1 lsl 22) in
  Buffer.add_string buffer
    "// Every table's types and value, and every service schema's types, \
     derived from their schemas by reader/derive.\n\n\
     export type JsonValue = null | boolean | number | string | JsonValue[] | \
     { [member: string]: JsonValue };\n\n";
  let names = Hashtbl.create 128 in
  let namespace file ty =
    let name = Filename.remove_extension (Filename.basename file) in
    (match Hashtbl.find_opt names name with
    | Some other -> refuse file "" ("is named as " ^ other ^ " is")
    | None -> Hashtbl.replace names name file);
    Printf.bprintf buffer "export namespace %s {\n" name;
    List.iter (ts_decl file buffer) (List.rev !decls);
    if ty <> Named "t" then
      Printf.bprintf buffer "  export type T = %s;\n" (ts_type ty);
    Buffer.add_string buffer "}\n\n";
    name
  in
  let tables =
    List.filter_map
      (fun table ->
        reset ();
        service := false;
        let file = Places.schema_of table in
        match
          if not (Sys.file_exists (Filename.concat root file)) then
            refuse table "" ("has no schema, which would be " ^ file);
          table_schema table file;
          namespace file (Named "t")
        with
        | name -> Some (table, name)
        | exception Stop -> None)
      (Places.tables root)
  in
  let services =
    List.filter_map
      (fun file ->
        reset ();
        service := true;
        (* [T] is the schema's own, whatever its top is. *)
        Hashtbl.replace taken "t" (file, "");
        match
          namespace file
            (derive ~strict:true ~own:true file "" (schema_at file ""))
        with
        | name -> Some name
        | exception Stop -> None)
      (Places.service_schemas root)
  in
  stop_on_refusals ();
  Buffer.add_string buffer "export type ServiceSchemas = {\n";
  List.iter
    (fun name -> Printf.bprintf buffer "  %s: %s.T;\n" (quoted name) name)
    services;
  Buffer.add_string buffer "};\n\n";
  List.iter
    (fun (table, name) ->
      match file_json table with
      | json ->
          Printf.bprintf buffer "export const %s: %s.T = %s;\n\n"
            (camel (Places.model_of table))
            name
            (Yojson.Safe.to_string json)
      | exception Stop -> ())
    tables;
  stop_on_refusals ();
  print_string (Buffer.contents buffer)

let () = match output with Ocaml -> ocaml () | Typescript -> typescript ()
