(* The Smelter's tables agree with what they name.

   - Each refusal of smelter/messages.json is of an operation the registry
     says the Smelter answers.
   - embedding/requests.json states how each kind of provider is asked: the
     kinds EmbeddingConfig tells apart, each once, and no other. *)

let messages = "smelter/messages.json"
let requests = "embedding/requests.json"
let config = "embedding/EmbeddingConfig.json"
let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token
let at pointer json = Decode.at_pointer pointer json
let text = function Some (`String s) -> Some s | _ -> None
let keys = function Some (`Assoc members) -> List.map fst members | _ -> []
let shown words = "[" ^ String.concat "," words ^ "]"

let refusals ~(read_json : string -> Yojson.Safe.t option) =
  let found = ref [] in
  let refuse file pointer fmt =
    Printf.ksprintf
      (fun message -> found := { Decode.file; pointer; message } :: !found)
      fmt
  in
  let answered_by_smelter =
    match Option.bind (read_json "bus/registry.json") (at "/operations") with
    | Some (`List operations) ->
        List.filter_map
          (fun op ->
            match text (at "/answeredBy" op) with
            | Some "smelter" -> text (at "/request" op)
            | _ -> None)
          operations
    | _ -> []
  in
  (match Option.bind (read_json messages) (at "/refusals") with
  | Some (`List rows) ->
      List.iteri
        (fun i row ->
          match text (at "/operation" row) with
          | Some op when not (List.mem op answered_by_smelter) ->
              refuse messages
                ("/refusals" / string_of_int i / "operation")
                "%s is no operation the registry says the Smelter answers" op
          | _ -> ())
        rows
  | _ -> ());
  let kinds =
    List.sort compare
      (keys (Option.bind (read_json config) (at "/discriminator/mapping")))
  in
  let stated =
    List.sort compare
      (keys (Option.bind (read_json requests) (at "/providers")))
  in
  if stated <> kinds then
    refuse requests "/providers"
      "the providers are %s, and EmbeddingConfig's are %s" (shown stated)
      (shown kinds);
  List.rev !found
