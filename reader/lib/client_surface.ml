(* client/surface.json's cases against their methods' parameters: a method
   takes each parameter once, and each case gives it nothing it does not take,
   everything it requires, and each argument as its parameter's schema says,
   with each [{"$fixture": name}] read as the table's fixture of that name.

   And each method's [returns] against its shape: a [signal] or [count]
   returns null, its shape saying all it gives; a promise may return null,
   resolving with nothing; any other shape gives a value. An [events] method
   returns the schema bus/registry.json states for the channel it observes. *)

let file = "client/surface.json"
let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token
let refusal pointer message = { Decode.file; pointer; message }
let indexed items = List.mapi (fun index item -> (index, item)) items

let name : Tables.Client_surface.parameters_item -> string = function
  | Value { parameter; _ } | Callback { parameter; _ } -> parameter

let rec with_fixtures fixtures at (value : Yojson.Safe.t) =
  match value with
  | `Assoc [ ("$fixture", `String fixture) ] -> (
      match Decode.at_pointer ("" / fixture) fixtures with
      | Some found -> (found, [])
      | None ->
          ( value,
            [
              refusal at
                (Printf.sprintf "names the fixture %s, which the table has not"
                   fixture);
            ] ))
  | `Assoc members ->
      let members, refusals =
        List.split
          (List.map
             (fun (key, member) ->
               let member, refusals =
                 with_fixtures fixtures (at / key) member
               in
               ((key, member), refusals))
             members)
      in
      (`Assoc members, List.concat refusals)
  | `List items ->
      let items, refusals =
        List.split
          (List.map
             (fun (index, item) ->
               with_fixtures fixtures (at / string_of_int index) item)
             (indexed items))
      in
      (`List items, List.concat refusals)
  | other -> (other, [])

(* The method at [at], which a caller calls as [called]. *)
let method_refusals ~ids ~schema_at ~fixtures ~at ~called
    (method_ : Tables.Client_surface.methods_item) =
  let parameters = indexed method_.parameters in
  let twice =
    List.filter_map
      (fun (k, parameter) ->
        if
          List.exists
            (fun (j, other) -> j < k && name other = name parameter)
            parameters
        then
          Some
            (refusal
               (at / "parameters" / string_of_int k)
               (Printf.sprintf "names %s, which %s takes already"
                  (name parameter) called))
        else None)
      parameters
  in
  let argument args_at (key, value) =
    let at_argument = args_at / key in
    match
      List.find_opt (fun (_, parameter) -> name parameter = key) parameters
    with
    | None ->
        [ refusal at_argument (Printf.sprintf "is no parameter of %s" called) ]
    | Some (_, Callback _) ->
        [ refusal at_argument "is a callback, which a case cannot give" ]
    | Some (k, Value _) -> (
        let value, unnamed = with_fixtures fixtures at_argument value in
        let place : Validate.place =
          { file; pointer = at / "parameters" / string_of_int k / "schema" }
        in
        unnamed
        @
        match schema_at place with
        | None -> [ refusal place.pointer "is no schema the reader can read" ]
        | Some schema ->
            List.map
              (fun (pointer, message) -> refusal pointer message)
              (Validate.errors ids schema_at schema value at_argument))
  in
  let case (c, (case : Tables.Client_surface.cases_item)) =
    let args_at = at / "cases" / string_of_int c / "args" in
    match case.args with
    | `Assoc args ->
        List.filter_map
          (function
            | Tables.Client_surface.Value { parameter; required = true; _ }
              when not (List.mem_assoc parameter args) ->
                Some
                  (refusal args_at
                     (Printf.sprintf "lacks %s, which %s requires" parameter
                        called))
            | _ -> None)
          method_.parameters
        @ List.concat_map (argument args_at) args
    | _ -> [ refusal args_at "is no object of arguments" ]
  in
  twice @ List.concat_map case (indexed method_.cases)

(* The schema the registry states for [name], by its [$id]. *)
let channel_schema (registry : Tables.Bus_registry.t) name =
  List.find_map
    (fun (c : Tables.Bus_registry.channels_item) ->
      match c with
      | Schema { channel; schema; _ } when channel = name ->
          Some (Decode.base ^ schema ^ ".json")
      | _ -> None)
    registry.channels

let returns_refusals ~registry ~at ~called
    (method_ : Tables.Client_surface.methods_item) =
  let via_at = at / "via" and at = at / "returns" in
  let shape = Tables.Client_surface.shape_value method_.shape in
  match (method_.shape, method_.returns) with
  | (Signal | Count), Some _ ->
      [
        refusal at
          (Printf.sprintf
             "is a value, and %s is a %s, whose shape says all it gives: it \
              returns null"
             called shape);
      ]
  | (Stream | Delegation | Upload | Cache | Events | Claims), None ->
      [
        refusal at
          (Printf.sprintf "is null, and %s is a %s, which gives a value" called
             shape);
      ]
  | Events, Some returned -> (
      let observed =
        match method_.via with
        | Observes { observes } -> Some observes
        | _ -> None
      in
      match (Option.bind observed (channel_schema registry), returned) with
      | Some stated, Reference { ref = { target; _ } } when target = stated ->
          []
      | Some stated, _ ->
          [
            refusal at
              (Printf.sprintf
                 "is not %s, the schema bus/registry.json states for the \
                  channel %s observes"
                 stated called);
          ]
      | None, _ ->
          [
            refusal via_at
              (Printf.sprintf
                 "names no channel bus/registry.json states a schema for, so \
                  what %s returns is unstated"
                 called);
          ])
  | _ -> []

let refusals ~ids ~read_json ~schema_at ~registry
    (surface : Tables.Client_surface.t) =
  let each f =
    List.concat_map
      (fun (n, (namespace : Tables.Client_surface.namespaces_item)) ->
        List.concat_map
          (fun (m, (method_ : Tables.Client_surface.methods_item)) ->
            f
              ~at:
                ("" / "namespaces" / string_of_int n / "methods"
               / string_of_int m)
              ~called:(namespace.namespace ^ "." ^ method_.method_)
              method_)
          (indexed namespace.methods))
      (indexed surface.namespaces)
  in
  each (returns_refusals ~registry)
  @
  match Option.bind (read_json file) (Decode.at_pointer "/fixtures") with
  | None -> [ refusal "" "has no fixtures" ]
  | Some fixtures -> each (method_refusals ~ids ~schema_at ~fixtures)
