(* The conformance corpus (Places.corpus): the case files the SDK and worker
   suites put every SDK's drivers through, outside specs/src. Each is read
   through the case's model and validated against its schema, and held to
   what it names of the spec:
   - its file's name is kebab case, as every name of Protocol's (D5);
   - each operation of the gateway's API it names, a wire step's and a
     scripted answer's, is one the gateway's document declares, and each
     read a live case expects is a GET of it;
   - each request on the bus a live case expects is a request of the bus
     registry, and each channel a step listens on, sends on, records or
     reads a frame of is a channel of it;
   - a case of the live directory states its tier, and no other case does;
   - each step is one of its directory's layer: a live case accounts for the
     wire with fetches and the stream's scopes, never a wire step, and alone
     observes and fetches; a worker case alone hears what a worker reports.

   A harness reads the corpus with [load]; the reader's check holds it with
   [refusals]. *)

module Case = Tables.Conformance_case

type case = { name : string; file : string; case : Case.t }

(* The case files of [directory], from the repository's [root], in order. *)
let files ~root directory =
  let path = Filename.concat root directory in
  if not (Sys.file_exists path && Sys.is_directory path) then []
  else
    Sys.readdir path |> Array.to_list |> List.sort compare
    |> List.filter (fun name -> Filename.check_suffix name ".json")
    |> List.map (fun name -> directory ^ "/" ^ name)

let parsed ~root file =
  match
    Decode.parse_file
      ~root:(Filename.dirname (Filename.concat root file))
      (Filename.basename file)
  with
  | Ok json -> Ok json
  | Error error -> Error [ { error with file } ]

let decoded file json =
  match Decode.run ~file json Case.decode with
  | Ok (case, _) ->
      Ok
        {
          name = Filename.remove_extension (Filename.basename file);
          file;
          case;
        }
  | Error error -> Error [ error ]

(* Every case of [directory], read through the case's model. *)
let load ~root directory =
  List.fold_right
    (fun file rest ->
      match (Result.bind (parsed ~root file) (decoded file), rest) with
      | Ok case, Ok cases -> Ok (case :: cases)
      | Error e, Ok _ | Ok _, Error e -> Error e
      | Error a, Error b -> Error (a @ b))
    (files ~root directory) (Ok [])

let refusal file pointer message = { Decode.file; pointer; message }
let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token

(* The operations of the gateway's document, each "METHOD /path". *)
let gateway_operations (spec : Spec.t) =
  List.concat_map
    (fun (path, reference) ->
      let file = fst (Decode.resolve spec.ids reference) in
      match List.assoc_opt file spec.path_items with
      | None -> []
      | Some (item : Openapi.path_item) ->
          List.filter_map
            (fun (meth, operation) ->
              Option.map (fun _ -> meth ^ " " ^ path) operation)
            [ ("GET", item.get); ("PUT", item.put); ("POST", item.post) ])
    spec.gateway.paths

let channels (registry : Tables.Bus_registry.t) =
  List.map
    (fun (channel : Tables.Bus_registry.channels_item) ->
      match channel with
      | Schema { channel; _ }
      | Envelope { channel; _ }
      | Stored_event { channel; _ }
      | Void { channel; _ }
      | Empty { channel; _ } ->
          channel)
    registry.channels

let requests (registry : Tables.Bus_registry.t) =
  List.map
    (fun (operation : Tables.Bus_registry.operations_item) -> operation.request)
    registry.operations

(* The layer a directory's cases are of: what its runner reads. *)
type layer = Wire | Live | Worker

let layer_of directory =
  match Filename.basename directory with
  | "live" -> Live
  | "wire" -> Wire
  | _ -> Worker

(* What [case] names of the spec that the spec must have, and how its steps
   fit its layer, at [file]. *)
let held ~operations ~channels ~requests ~layer { file; case; _ } =
  let refuse pointer message = refusal file pointer message in
  let operation pointer what named =
    if List.mem named operations then []
    else
      [
        refuse pointer
          (Printf.sprintf
             "names %s %s, which the gateway's document does not declare" what
             named);
      ]
  in
  let channel pointer named =
    if List.mem named channels then []
    else
      [
        refuse pointer
          (Printf.sprintf
             "names the channel %s, which the bus registry does not have" named);
      ]
  in
  let request pointer named =
    if List.mem named requests then []
    else
      [
        refuse pointer
          (Printf.sprintf
             "names the request %s, which no operation of the bus registry asks"
             named);
      ]
  in
  let literal pointer arguments key check =
    match List.assoc_opt key (Option.value ~default:[] arguments) with
    | Some (`String named) -> check (pointer / "with" / key) named
    | _ -> []
  in
  let fetched pointer :
      Tables.Conformance_case.conformance_step_fetches_item -> _ = function
    | Bus_fetch { fetch; _ } -> request (pointer / "fetch") fetch
    | Http_fetch { fetch; _ } -> operation (pointer / "fetch") "the read" fetch
  in
  let tier =
    match (layer, case.tier) with
    | Live, None -> [ refuse "" "is a live case, and states no tier" ]
    | (Wire | Worker), Some _ ->
        [ refuse "/tier" "states a tier, which only a live case has" ]
    | _ -> []
  in
  let outside pointer what =
    [
      refuse pointer
        (Printf.sprintf "is %s, which this directory's cases have none of" what);
    ]
  in
  tier
  @ List.concat
      (List.mapi
         (fun i (step : Case.conformance_step) ->
           let at = "/steps" / string_of_int i in
           let only_live what = if layer = Live then [] else outside at what in
           let only_worker what =
             if layer = Worker then [] else outside at what
           in
           match step with
           | Wire { wire; _ } ->
               (if layer = Live then
                  [
                    refuse at
                      "is a wire step: a live case accounts for the wire with \
                       fetch and scopes";
                  ]
                else [])
               @ operation (at / "wire") "the operation" wire
           | Backend { backend; with_ } -> (
               match backend with
               | Refuse | Unanswered | Answer_lost | Hold ->
                   literal at with_ "wire" (fun pointer named ->
                       operation pointer "the operation" named)
               | Emit | Record -> literal at with_ "channel" channel
               | Listen -> (
                   match
                     List.assoc_opt "channels" (Option.value ~default:[] with_)
                   with
                   | Some (`List named) ->
                       List.concat
                         (List.mapi
                            (fun j -> function
                              | `String name ->
                                  channel
                                    (at / "with" / "channels" / string_of_int j)
                                    name
                              | _ -> [])
                            named)
                   | _ -> [])
               | _ -> [])
           | Driver { driver = "listen"; with_; _ } ->
               literal at with_ "channel" channel
           | Frame { frame; _ } -> channel (at / "frame") frame
           | Frames { frames; _ } -> channel (at / "frames") frames
           | Bus_fetch { fetch; _ } -> request (at / "fetch") fetch
           | Http_fetch { fetch; _ } ->
               only_live "a fetch" @ operation (at / "fetch") "the read" fetch
           | Fetches { fetches } ->
               only_live "a fetch"
               @ List.concat
                   (List.mapi
                      (fun j fetch ->
                        fetched (at / "fetches" / string_of_int j) fetch)
                      fetches)
           | Observe _ -> only_live "an observation"
           | Leave _ | Reaches _ | Holds _ | Completes _ ->
               only_live "an observer's"
           | Claimed _ | Refused _ | Signalled _ | Stalled _ ->
               only_worker "what a worker reports"
           | _ -> [])
         case.steps)

(* How the corpus at the repository's [root] is not what the spec says. *)
let refusals ~root (spec : Spec.t) =
  let specs = Filename.concat root "specs/src" in
  let schema_cache = Hashtbl.create 64 in
  let schema_at ({ file; pointer } : Validate.place) =
    match Hashtbl.find_opt schema_cache (file, pointer) with
    | Some found -> found
    | None ->
        let found =
          match Decode.parse_file ~root:specs file with
          | Error _ -> None
          | Ok json ->
              Option.bind (Decode.at_pointer pointer json) (fun node ->
                  Result.to_option
                    (Result.map fst (Decode.run ~file node Schema.decode)))
        in
        Hashtbl.replace schema_cache (file, pointer) found;
        found
  in
  let schema =
    match schema_at { file = Places.corpus_case; pointer = "" } with
    | Some schema -> schema
    | None -> invalid_arg (Places.corpus_case ^ " is no schema")
  in
  let operations = gateway_operations spec in
  let channels = channels spec.tables.bus_registry in
  let requests = requests spec.tables.bus_registry in
  List.concat_map
    (fun directory ->
      let layer = layer_of directory in
      match files ~root directory with
      | [] -> [ refusal directory "" "holds no case" ]
      | listed ->
          List.concat_map
            (fun file ->
              let stem = Filename.remove_extension (Filename.basename file) in
              let named =
                if Places.is_kebab stem then []
                else [ refusal file "" "is not named in kebab case" ]
              in
              named
              @
              match parsed ~root file with
              | Error e -> e
              | Ok json -> (
                  match
                    List.map
                      (fun (pointer, message) -> refusal file pointer message)
                      (Validate.errors spec.ids schema_at schema json "")
                  with
                  | _ :: _ as invalid -> invalid
                  | [] -> (
                      match decoded file json with
                      | Error e -> e
                      | Ok case ->
                          held ~operations ~channels ~requests ~layer case)))
            listed)
    Places.corpus
