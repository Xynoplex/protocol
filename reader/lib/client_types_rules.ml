(* The client types agree with what restates them. client/types states the
   shapes every SDK hands its callers, and the client and session tables'
   schemas what an SDK reads from them; each restates a fact stated
   elsewhere.

   - Telemetry's [anchor.method] values are AnchorMethod's words.
   - A reconciled span names a search exactly when its method is
     [fuzzy-match].
   - Some case of bus/frame-cases.json is refused for each reason
     FrameRefusalReason names.
   - annotations/reader-cases.json's [readers] are, in order, what its
     schema says a case [reads], and each is a reader of client/surface.json's
     annotations module or one of its functions that reads an annotation
     ([is…]); annotations/builder-cases.json's [builders] are, in order, the
     module's other functions.
   - DeviceCode is what control/UserAction.json carries less its
     [requestId], its [expiresInSeconds] named [expiresIn].
   - A method's [shape] in client/ClientSurface.json is, in order, one of the
     shapes it states.
   - session/SessionCacheCases.json's loads are, in order, of
     client/refresh.json's persisted queries, each keeping its query's
     value. *)

let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token
let at = Decode.at_pointer
let text = function Some (`String s) -> Some s | _ -> None

let texts = function
  | Some (`List items) ->
      List.filter_map (function `String s -> Some s | _ -> None) items
  | _ -> []

let items = function Some (`List items) -> items | _ -> []
let keys = function Some (`Assoc members) -> List.map fst members | _ -> []
let shown json = Yojson.Safe.to_string json
let words list = shown (`List (List.map (fun s -> `String s) list))

let reads_an_annotation name =
  String.length name > 2
  && String.starts_with ~prefix:"is" name
  && match name.[2] with 'A' .. 'Z' -> true | _ -> false

let refusals ~(read_json : string -> Yojson.Safe.t option) =
  let found = ref [] in
  let refuse file pointer fmt =
    Printf.ksprintf
      (fun message -> found := { Decode.file; pointer; message } :: !found)
      fmt
  in
  let files = Hashtbl.create 16 in
  let read file =
    match read_json file with
    | Some json -> Some json
    | None ->
        if not (Hashtbl.mem files file) then (
          Hashtbl.replace files file ();
          refuse file "" "is missing: the client types are checked against it");
        None
  in
  let with_files names f =
    match List.map read names with
    | jsons when List.for_all Option.is_some jsons ->
        f (List.map Option.get jsons)
    | _ -> ()
  in
  let telemetry = "service-telemetry/telemetry.json" in
  let anchor_method = "client/types/AnchorMethod.json" in
  with_files [ anchor_method; telemetry ] (function
    | [ anchor; table ] ->
        let methods = at "/enum" anchor in
        let attributes =
          List.concat_map
            (fun rows ->
              List.concat
                (List.mapi
                   (fun i row ->
                     List.concat
                       (List.mapi
                          (fun j attribute ->
                            if text (at "/key" attribute) = Some "anchor.method"
                            then
                              [
                                ( "/"
                                  ^ rows / string_of_int i / "attributes"
                                    / string_of_int j / "values",
                                  at "/values" attribute );
                              ]
                            else [])
                          (items (at "/attributes" row))))
                   (items (at ("/" ^ rows) table))))
            [ "spans"; "metrics" ]
        in
        if attributes = [] then
          refuse telemetry "" "has no `anchor.method` attribute";
        List.iter
          (fun (pointer, values) ->
            if values <> methods then
              refuse telemetry pointer
                "lists anchor.method values %s, not AnchorMethod's %s"
                (Option.fold ~none:"none" ~some:shown values)
                (Option.fold ~none:"none" ~some:shown methods))
          attributes
    | _ -> ());
  let reconcile = "annotations/reconcile-cases.json" in
  with_files [ reconcile ] (function
    | [ table ] ->
        List.iteri
          (fun i case ->
            match at "/reconciled" case with
            | None | Some `Null -> ()
            | Some reconciled -> (
                let pointer = "/cases" / string_of_int i / "reconciled" in
                let fuzzy =
                  text (at "/anchorMethod" reconciled) = Some "fuzzy-match"
                in
                match at "/matchQuality" reconciled with
                | None when fuzzy ->
                    refuse reconcile pointer
                      "is a fuzzy match, and names no search"
                | Some quality when not fuzzy ->
                    refuse reconcile (pointer / "matchQuality")
                      "names a search, %s, and is not a fuzzy match"
                      (shown quality)
                | _ -> ()))
          (items (at "/cases" table))
    | _ -> ());
  let reasons = "client/types/FrameRefusalReason.json" in
  with_files [ reasons; "bus/frame-cases.json" ] (function
    | [ reason; frames ] ->
        let refused_for =
          List.filter_map
            (fun case ->
              match at "/refused" case with
              | Some (`Bool true) -> text (at "/reason" case)
              | _ -> None)
            (items (at "/cases" frames))
        in
        List.iteri
          (fun i name ->
            if not (List.mem name refused_for) then
              refuse reasons
                ("/enum" / string_of_int i)
                "FrameRefusalReason names %S, and no case of frame-cases.json \
                 is refused for it"
                name)
          (texts (at "/enum" reason))
    | _ -> ());
  let reader_cases = "annotations/reader-cases.json" in
  let builder_cases = "annotations/builder-cases.json" in
  with_files
    [
      "client/surface.json";
      reader_cases;
      "annotations/AnnotationsReaderCases.json";
      builder_cases;
    ] (function
    | [ surface; readers_table; readers_schema; builders_table ] ->
        let members =
          List.concat_map
            (fun m ->
              if text (at "/module" m) = Some "annotations" then
                items (at "/members" m)
              else [])
            (items (at "/modules" surface))
        in
        let member_of kinds m =
          match (text (at "/kind" m), text (at "/member" m)) with
          | Some kind, Some name -> kinds kind name
          | _ -> None
        in
        let readers = texts (at "/readers" readers_table) in
        let reads =
          keys
            (at "/properties/cases/items/properties/reads/properties"
               readers_schema)
        in
        if readers <> reads then
          refuse reader_cases "/readers"
            "readers %s are not, in order, what its schema says a case reads %s"
            (words readers) (words reads);
        let surface_readers =
          List.filter_map
            (member_of (fun kind name ->
                 if
                   kind = "reader"
                   || (kind = "function" && reads_an_annotation name)
                 then Some name
                 else None))
            members
        in
        List.iteri
          (fun i reader ->
            if not (List.mem reader surface_readers) then
              refuse reader_cases
                ("/readers" / string_of_int i)
                "names the reader %S, which client/surface.json's annotations \
                 module does not have"
                reader)
          readers;
        let builders = texts (at "/builders" builders_table) in
        let surface_builders =
          List.filter_map
            (member_of (fun kind name ->
                 if kind = "function" && not (reads_an_annotation name) then
                   Some name
                 else None))
            members
        in
        if builders <> surface_builders then
          refuse builder_cases "/builders"
            "builders %s are not client/surface.json's other annotations \
             functions %s"
            (words builders) (words surface_builders)
    | _ -> ());
  let device_code = "client/types/DeviceCode.json" in
  with_files [ "control/UserAction.json"; device_code ] (function
    | [ action; device ] ->
        let as_device name =
          if name = "expiresInSeconds" then "expiresIn" else name
        in
        let carried =
          match at "/properties" action with
          | Some (`Assoc members) ->
              List.filter (fun (n, _) -> n <> "requestId") members
          | _ -> []
        in
        let device_members = keys (at "/properties" device) in
        let carried_members = List.map (fun (n, _) -> as_device n) carried in
        if List.sort compare device_members <> List.sort compare carried_members
        then
          refuse device_code "/properties"
            "DeviceCode's members %s are not UserAction's less requestId %s"
            (words device_members) (words carried_members);
        List.iter
          (fun (name, property) ->
            let own = at ("/properties" / as_device name / "type") device in
            let carried_type = at "/type" property in
            let typed = Option.fold ~none:"untyped" ~some:shown in
            if own <> carried_type then
              refuse device_code
                ("/properties" / as_device name)
                "DeviceCode's %s is %s, and UserAction's %s is %s"
                (as_device name) (typed own) name (typed carried_type))
          carried;
        let required = List.sort compare (texts (at "/required" device)) in
        let carried_required =
          texts (at "/required" action)
          |> List.filter (( <> ) "requestId")
          |> List.map as_device |> List.sort compare
        in
        if required <> carried_required then
          refuse device_code "/required"
            "DeviceCode requires %s, and UserAction, less requestId, requires \
             %s"
            (words (texts (at "/required" device)))
            (words carried_required)
    | _ -> ());
  let client_surface = "client/ClientSurface.json" in
  with_files [ client_surface ] (function
    | [ schema ] ->
        let shapes = keys (at "/properties/shapes/properties" schema) in
        let pointer =
          "/properties/namespaces/items/properties/methods/items/properties/shape/enum"
        in
        let method_shapes = texts (at pointer schema) in
        if method_shapes <> shapes then
          refuse client_surface pointer
            "the method shapes %s are not, in order, the shapes it states %s"
            (words method_shapes) (words shapes)
    | _ -> ());
  let cache_cases = "session/SessionCacheCases.json" in
  with_files [ "client/refresh.json"; cache_cases ] (function
    | [ refresh; cases ] ->
        let persisted =
          List.filter
            (fun q -> at "/persisted" q = Some (`Bool true))
            (items (at "/queries" refresh))
        in
        let pointer = "/properties/cases/items/oneOf" in
        let loads =
          List.map
            (fun load ->
              ( at "/properties/query/enum" load,
                at "/properties/kept/items/properties/value" load ))
            (items (at pointer cases))
        in
        let names =
          List.map
            (fun q -> Option.map (fun n -> `List [ n ]) (at "/name" q))
            persisted
        in
        let queries = List.map fst loads in
        if queries <> names then
          refuse cache_cases pointer
            "loads %s, not, in order, refresh.json's persisted queries %s"
            (shown (`List (List.filter_map Fun.id queries)))
            (shown
               (`List
                  (List.concat_map
                     (fun q -> Option.to_list (at "/name" q))
                     persisted)));
        List.iteri
          (fun i query ->
            match List.nth_opt loads i with
            | Some (_, kept) when kept <> at "/value" query ->
                refuse cache_cases
                  ((pointer / string_of_int i)
                  ^ "/properties/kept/items/properties/value")
                  "keeps %s for %s, and refresh.json's value is %s"
                  (Option.fold ~none:"nothing" ~some:shown kept)
                  (Option.fold ~none:"an unnamed query" ~some:shown
                     (at "/name" query))
                  (Option.fold ~none:"nothing" ~some:shown (at "/value" query))
            | _ -> ())
          persisted
    | _ -> ());
  List.rev !found
