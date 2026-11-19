(* How large each answer on the bus can be. A bus message is bounded by its
   question; an answer whose size follows a document or a collection is read
   over HTTP. bus/reply-bounds.json names each list or map an operation's
   reply carries that its own schema does not bound: one bounded by a member
   of the request ([by]), or one that grows.

   So every list or map a reply carries is bounded by its schema or named
   there, and every one named is carried and unbounded, by an operation of
   the registry, and bounded by an array of the request or an integer with a
   maximum. The lists inside a resource's descriptor or an annotation are the
   record's, bounded by the write that made them, and are not walked. *)

let file = "bus/reply-bounds.json"
let records = [ "ResourceDescriptor.json"; "Annotation.json" ]
let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token

(* A list or map a reply carries: by its path of member names, where its
   schema is, and whether that schema bounds it. *)
type collection = { path : string; at : Validate.place; bounded : bool }

let is_record file = List.mem (Filename.basename file) records

(* Every list and map [schema], at [at], carries, by its path from [path],
   following each [$ref] but into a file already walked or a record. *)
let rec collections ~ids ~(schema_at : Validate.schemas) ~seen
    (at : Validate.place) path (schema : Schema.t) =
  match schema with
  | Boolean_schema _ -> []
  | Schema s -> (
      match s.ref_ with
      | Some r -> (
          let file, pointer = Decode.resolve ids r in
          if List.mem file seen || is_record file then []
          else
            match schema_at { Validate.file; pointer } with
            | Some target ->
                collections ~ids ~schema_at ~seen:(file :: seen)
                  { file; pointer } path target
            | None -> [])
      | None ->
          let within key branches =
            List.concat
              (List.mapi
                 (fun index branch ->
                   collections ~ids ~schema_at ~seen
                     {
                       at with
                       pointer = at.pointer / key / string_of_int index;
                     }
                     path branch)
                 (Option.value ~default:[] branches))
          in
          let branches =
            within "allOf" s.all_of @ within "oneOf" s.one_of
            @ within "anyOf" s.any_of
          in
          let is_array =
            Option.is_some s.items
            || List.mem Schema.Array (Option.value ~default:[] s.type_)
          in
          if is_array then
            branches @ [ { path; at; bounded = Option.is_some s.max_items } ]
          else
            let map =
              match s.additional_properties with
              | Some (Schema _) ->
                  [ { path; at; bounded = Option.is_some s.max_properties } ]
              | _ -> []
            in
            let members =
              List.concat_map
                (fun (name, member) ->
                  collections ~ids ~schema_at ~seen
                    { at with pointer = at.pointer / "properties" / name }
                    (if path = "" then name else path ^ "." ^ name)
                    member)
                (Option.value ~default:[] s.properties)
            in
            branches @ map @ members)

(* One collection for each path, in the order the paths are first found: the
   last place it is found at, as a later branch states it again. *)
let by_path collections =
  let paths =
    List.fold_left
      (fun paths c -> if List.mem c.path paths then paths else c.path :: paths)
      [] collections
  in
  List.filter_map
    (fun path -> List.find_opt (fun c -> c.path = path) (List.rev collections))
    (List.rev paths)

(* The schema of [request]'s member [name], its [$ref] followed. *)
let member ~ids ~(schema_at : Validate.schemas) request name =
  let at : Validate.place =
    { file = Places.components ^ request ^ ".json"; pointer = "" }
  in
  match schema_at at with
  | Some (Schema { properties = Some properties; _ }) -> (
      match List.assoc_opt name properties with
      | Some (Schema { ref_ = Some r; _ }) ->
          let file, pointer = Decode.resolve ids r in
          schema_at { file; pointer }
      | other -> other)
  | _ -> None

let bounds (schema : Schema.t) =
  match schema with
  | Schema s ->
      let types = Option.value ~default:[] s.type_ in
      List.mem Schema.Array types
      || (List.mem Schema.Integer types && Option.is_some s.maximum)
  | Boolean_schema _ -> false

let refusals ~ids ~(schema_at : Validate.schemas)
    (registry : Tables.Bus_registry.t) (table : Tables.Bus_reply_bounds.t) =
  let refusal file pointer message = { Decode.file; pointer; message } in
  let channel name =
    List.find_opt
      (fun (c : Tables.Bus_registry.channels_item) ->
        match c with
        | Schema { channel; _ }
        | Envelope { channel; _ }
        | Stored_event { channel; _ }
        | Void { channel; _ }
        | Empty { channel; _ } ->
            channel = name)
      registry.channels
  in
  let operations =
    List.map
      (fun (o : Tables.Bus_registry.operations_item) -> o.request)
      registry.operations
  in
  let stray =
    List.filter_map
      (fun (request, _) ->
        if List.mem request operations then None
        else
          Some
            (refusal file
               ("" / "replies" / request)
               (Printf.sprintf "names %s, which is no operation of the registry"
                  request)))
      table.replies
  in
  let operation (o : Tables.Bus_registry.operations_item) =
    let entries =
      Option.value ~default:[] (List.assoc_opt o.request table.replies)
    in
    let carried =
      match channel o.result with
      | Some (Schema { schema; _ }) ->
          let at : Validate.place =
            { file = Places.components ^ schema ^ ".json"; pointer = "" }
          in
          Option.fold ~none:[]
            ~some:(fun schema ->
              by_path
                (collections ~ids ~schema_at ~seen:[ at.file ] at "" schema))
            (schema_at at)
      | _ -> []
    in
    let found path = List.find_opt (fun c -> c.path = path) carried in
    let unnamed =
      List.filter_map
        (fun c ->
          if c.bounded || List.mem_assoc c.path entries then None
          else
            Some
              (refusal c.at.file c.at.pointer
                 (Printf.sprintf
                    "is %s of %s, a list with no bound that %s does not name: \
                     bound it in its schema, or say what bounds it"
                    c.path o.result file)))
        carried
    in
    let named (path, (entry : Tables.Bus_reply_bounds.value_value)) =
      let at = "" / "replies" / o.request / path in
      let carriage =
        match found path with
        | None ->
            [
              refusal file at
                (Printf.sprintf "names %s of %s, which it does not carry" path
                   o.result);
            ]
        | Some { bounded = true; _ } ->
            [
              refusal file at
                (Printf.sprintf "names %s of %s, which its schema bounds" path
                   o.result);
            ]
        | Some _ -> []
      in
      let by =
        match entry with
        | Grows _ -> []
        | By { by } -> (
            let asked =
              match channel o.request with
              | Some (Schema { schema; _ } | Envelope { schema; _ }) ->
                  member ~ids ~schema_at schema by
              | _ -> None
            in
            match asked with
            | None ->
                [
                  refusal file (at / "by")
                    (Printf.sprintf "is %s, which is no member of %s" by
                       o.request);
                ]
            | Some asked when not (bounds asked) ->
                [
                  refusal file (at / "by")
                    (Printf.sprintf
                       "is %s of %s, which is neither an array nor an integer \
                        with a maximum"
                       by o.request);
                ]
            | Some _ -> [])
      in
      carriage @ by
    in
    unnamed @ List.concat_map named entries
  in
  stray @ List.concat_map operation registry.operations
