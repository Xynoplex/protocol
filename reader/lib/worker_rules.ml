(* The Worker service's tables agree with what they name.

   - worker/answer-formats.json has a row for each motivation a mark job
     takes (MarkJobParams) and one for the count, each once, and no other.
   - Each case of worker/prompt-cases.json has a name no other case has. A
     mark case asks for a row of answer-formats.json: its motivation's, or,
     for a linking job, the count; it is handed a tag schema when it is a
     tagging job's, and only then. It is about one of a linking job's
     entityTypes, or one of a tagging job's categories, and a job of any
     other motivation is about none. Each prompt carries its piece.
   - worker/messages.json names a job by each JobType, and no other.
   - A refusal of worker/messages.json that is a shared one names a refusal of
     service-environment/boot-refusals.json. *)

let formats = "worker/answer-formats.json"
let prompts = "worker/prompt-cases.json"
let messages = "worker/messages.json"
let boot_refusals = "service-environment/boot-refusals.json"
let mark_params = "components/schemas/MarkJobParams.json"
let job_type = "components/schemas/JobType.json"
let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token
let at pointer json = Decode.at_pointer pointer json
let text = function Some (`String s) -> Some s | _ -> None
let keys = function Some (`Assoc members) -> List.map fst members | _ -> []
let shown words = "[" ^ String.concat "," words ^ "]"

let rows file member read_json =
  match Option.bind (read_json file) (at member) with
  | Some (`List rows) -> rows
  | _ -> []

let names rows = List.filter_map (fun row -> text (at "/name" row)) rows

let contains text ~within =
  let n = String.length text and m = String.length within in
  let rec from i =
    i + n <= m && (String.sub within i n = text || from (i + 1))
  in
  from 0

let refusals ~(read_json : string -> Yojson.Safe.t option) =
  let found = ref [] in
  let refuse file pointer fmt =
    Printf.ksprintf
      (fun message -> found := { Decode.file; pointer; message } :: !found)
      fmt
  in
  let motivations =
    keys (Option.bind (read_json mark_params) (at "/discriminator/mapping"))
  in
  let stated = names (rows formats "/formats" read_json) in
  let expected = List.sort compare ("count" :: motivations) in
  if List.sort compare stated <> expected then
    refuse formats "/formats"
      "the formats are %s, and MarkJobParams' motivations and the count are %s"
      (shown (List.sort compare stated))
      (shown expected);
  let cases = rows prompts "/cases" read_json in
  List.iteri
    (fun i case ->
      let pointer = "/cases" / string_of_int i in
      let name = text (at "/name" case) in
      (match name with
      | Some name
        when List.length
               (List.filter (( = ) (Some name))
                  (List.map (fun c -> text (at "/name" c)) cases))
             > 1 ->
          refuse prompts (pointer / "name") "%s names another case too" name
      | _ -> ());
      (match (text (at "/piece" case), text (at "/prompt" case)) with
      | Some piece, Some prompt when not (contains piece ~within:prompt) ->
          refuse prompts (pointer / "piece")
            "the prompt does not carry its piece"
      | _ -> ());
      match (text (at "/job" case), text (at "/params/motivation" case)) with
      | Some "mark", Some motivation -> (
          (match (at "/schema" case, motivation) with
          | None, "tagging" ->
              refuse prompts pointer
                "a tagging job is handed the tag schema its schemaId names, \
                 and this one is not"
          | Some _, motivation when motivation <> "tagging" ->
              refuse prompts (pointer / "schema")
                "a %s job is handed no tag schema" motivation
          | _ -> ());
          (let among member =
             match at member case with
             | Some (`List items) ->
                 List.filter_map (fun item -> text (Some item)) items
             | _ -> []
           in
           match (text (at "/about" case), motivation) with
           | None, ("linking" | "tagging") ->
               refuse prompts pointer
                 "a %s prompt is about one of what its job names, and this one \
                  is about none"
                 motivation
           | Some about, "linking"
             when not (List.mem about (among "/params/entityTypes")) ->
               refuse prompts (pointer / "about")
                 "%s is none of the job's entityTypes" about
           | Some about, "tagging"
             when not (List.mem about (among "/params/categories")) ->
               refuse prompts (pointer / "about")
                 "%s is none of the job's categories" about
           | Some _, motivation
             when motivation <> "linking" && motivation <> "tagging" ->
               refuse prompts (pointer / "about")
                 "a %s prompt is about no entity type or category" motivation
           | _ -> ());
          match text (at "/asks" case) with
          | Some asks when not (List.mem asks stated) ->
              refuse prompts (pointer / "asks") "%s is no row of %s" asks
                formats
          | Some asks
            when asks <> motivation
                 && not (asks = "count" && motivation = "linking") ->
              refuse prompts (pointer / "asks") "a %s job's prompt asks for %s"
                motivation asks
          | _ -> ())
      | _ -> ())
    cases;
  let shared = names (rows boot_refusals "/refusals" read_json) in
  List.iteri
    (fun i row ->
      match text (at "/bootRefusal" row) with
      | Some name when not (List.mem name shared) ->
          refuse messages
            ("/boot" / string_of_int i / "bootRefusal")
            "%s is no refusal of %s" name boot_refusals
      | _ -> ())
    (rows messages "/boot" read_json);
  let types =
    match Option.bind (read_json job_type) (at "/enum") with
    | Some (`List types) -> List.filter_map (fun t -> text (Some t)) types
    | _ -> []
  in
  let named = keys (Option.bind (read_json messages) (at "/job")) in
  if List.sort compare named <> List.sort compare types then
    refuse messages "/job" "it names jobs of %s, and JobType is %s"
      (shown (List.sort compare named))
      (shown (List.sort compare types));
  List.rev !found
