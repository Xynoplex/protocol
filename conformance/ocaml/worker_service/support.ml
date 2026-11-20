(* What the worker-service files share: a file's world, the text their jobs
   are about, the requests worker/prompt-cases.json states a worker makes of
   its provider, the words worker/messages.json states it says, and the
   reading of a job's messages off a worker's transcript. *)

open Xynolog_conformance
module W = Worker_service_world
module P = Xynolog_protocol.Tables.Worker_prompt_cases
module M = Xynolog_protocol.Tables.Worker_messages

let str s = `String s
let strs items = `List (List.map str items)
let member = Yojson.Safe.Util.member
let text_of json = Yojson.Safe.Util.to_string json

(* [cases] against one worker world, under [category]: the file's, made by
   its first case. A case fails, whatever it was about, if a worker asked a
   gateway for something it refused, unless [allow_refused]. *)
let in_world ?allow_refused implementations file =
  let made =
    Suite.world ("worker-service/" ^ file)
      ~make:(fun () -> W.create implementations)
      ~close:W.close
  in
  fun ~category cases ->
    List.map
      (fun (c : W.t Case.t) ->
        Case.test ~category c (fun () ->
            let w = made () in
            W.end_case ?allow_refused w (fun () -> c.run w)))
      cases

(* ── The texts the jobs are about ───────────────────────────────────── *)

(* The text the short jobs are about: three paragraphs, 391 characters,
   every one of the Basic Multilingual Plane. It has a dash and an accented
   letter ahead of most of its spans, so an offset counted in bytes is not
   an offset counted in code points; one name three times, so a span must be
   told from its repeats; and a final line end, which a prompt does not
   carry. *)
let text =
  "Ada Lovelace published the first program in 1843 \u{2014} a method for \
   computing Bernoulli numbers on the Analytical Engine.\n\n\
   Charles Babbage designed the engine in London, but it was never built. \
   Lovelace argued that the engine could manipulate symbols as well as \
   numbers; nobody tested that claim for a century.\n\n\
   The program was na\u{00ef}ve by later standards, and London ignored it. \
   London was wrong.\n"

(* The text's length in code points, which is what a worker counts. *)
let code_points s =
  let rec count at n =
    if at >= String.length s then n
    else
      count (at + Uchar.utf_decode_length (String.get_utf_8_uchar s at)) (n + 1)
  in
  count 0 0

(* The text of [s] from code point [start] up to code point [stop], as a
   selector counts them. *)
let code_point_slice s start stop =
  let rec offset at n =
    if n = 0 then at
    else
      offset (at + Uchar.utf_decode_length (String.get_utf_8_uchar s at)) (n - 1)
  in
  let from = offset 0 start in
  String.sub s from (offset from (stop - start) - from)

(* A text too long for one request of a model with a small window: eight
   paragraphs of six sentences, 3847 characters, each sentence naming a
   marker no other names. *)
let long_text =
  String.concat "\n\n"
    (List.init 8 (fun p ->
         String.concat " "
           (List.init 6 (fun s ->
                Printf.sprintf
                  "Paragraph %d, sentence %d: the survey party recorded marker \
                   %d beside the Rh\u{00f4}ne."
                  (p + 1) (s + 1)
                  (((p + 1) * 100) + s + 1)))))
  ^ "\n"

(* A context length small enough that [long_text] is cut into pieces. *)
let small_context_length = 1200

(* A context length at which a linking job and a tagging job each cut
   [long_text] into two pieces. *)
let middling_context_length = 2400

(* The context length the stand-in Ollama reports unless a case sets
   another. *)
let context_length = 8192

(* The sizes, in tokens, a job asked with no more than its motivation, its
   entity types or its categories cuts [text] at in that window: what its
   units' cursors state. *)
let highlighting_piece = 2641
let linking_piece = 2658
let claim_piece = 2621
let evidence_piece = 2618

(* The size a highlighting job cuts a text too long for one request at in a
   window of [small_context_length], its provider reporting no usage. *)
let small_highlighting_piece = 311

(* ── The worker's health ─────────────────────────────────────────────── *)

(* The worker's health: answered 200, as JSON, and held to the spec's
   WorkerHealth before a case reads it. *)
let health (worker : Worker_service.t) =
  match Worker_service.http worker "GET" "/health" with
  | None -> failwith "the worker's health did not answer"
  | Some reply -> (
      Check.equal "its health's status" (`Int 200) (`Int reply.status);
      Check.that "its health is JSON"
        (match Http.Header.get reply.headers "content-type" with
        | Some t -> String.starts_with ~prefix:"application/json" t
        | None -> false);
      let json = Option.get reply.json in
      match Protocol.refusals (Protocol.service_schema "WorkerHealth") json with
      | [] -> json
      | refusals ->
          failwith
            ("the worker's health answer is no WorkerHealth: "
            ^ String.concat "; " refusals))

let knowledge_bases_of health = Check.list health [ "knowledgeBases" ]

(* What [engine], used for [jobs], says of itself in the worker's health
   when it has done nothing. *)
let idle_engine (engine : W.engine) jobs =
  `Assoc
    [
      ( "engine",
        `Assoc
          [ ("provider", str engine.provider); ("model", str engine.model) ] );
      ("jobs", jobs);
      ("lastQueuedEventAt", `Null);
      ("lastClaimAt", `Null);
      ("lastFinishedAt", `Null);
      ("lastActivityAt", `Null);
      ("activeJob", `Null);
      ("jobsCompleted", `Int 0);
    ]

(* ── What a worker asks its model ───────────────────────────────────── *)

(* The model of the world's first Ollama engine: the one a worker runs when
   a case names no engines. *)
let model = (List.hd W.ollama_engines).model

(* The case [name] of worker/prompt-cases.json. *)
let prompt_case name =
  match
    List.find_opt
      (function P.Mark { name = n; _ } | P.Yield { name = n; _ } -> n = name)
      (Protocol.tables ()).worker_prompt_cases.cases
  with
  | Some case -> case
  | None -> failwith ("worker/prompt-cases.json has no case " ^ name)

(* The prompt of the case [name]: what a worker sends its model. *)
let prompt name =
  match prompt_case name with
  | Mark { prompt; _ } | Yield { prompt; _ } -> prompt

(* The parameters of the case [name]'s job, with the tag schema a tagging
   job is handed beside them. *)
let params name =
  match prompt_case name with
  | Mark { params = `Assoc members; schema = Some schema; _ } ->
      `Assoc (members @ [ ("schema", schema) ])
  | Mark { params; _ } | Yield { params; _ } -> params

let requests name =
  match prompt_case name with
  | Mark { requests; _ } | Yield { requests; _ } -> requests

(* What a detection asks the model to answer in: an array of the element
   worker/answer-formats.json states for [name], written as it is there. *)
let answer_format name =
  match Protocol.json_of "worker/answer-formats.json" with
  | Some formats -> (
      match
        List.find_opt
          (fun row -> member "name" row = str name)
          (Check.list formats [ "formats" ])
      with
      | Some row ->
          `Assoc [ ("type", str "array"); ("items", member "element" row) ]
      | None -> failwith ("worker/answer-formats.json has no format " ^ name))
  | None -> failwith "specs/src has no worker/answer-formats.json"

(* What the case [name] asks the model to answer in: none for a yield
   job's. *)
let format_of name =
  match prompt_case name with
  | Mark { asks; _ } -> Some (answer_format asks)
  | Yield _ -> None

(* The options of the case [name]'s request of an Ollama whose window is
   [context_length]. *)
let ollama_options ~context_length name =
  match
    List.find_map
      (function
        | P.Ollama { context_length = window; options; _ }
          when window = context_length ->
            Some options
        | _ -> None)
      (requests name)
  with
  | Some options ->
      `Assoc
        [
          ("num_predict", `Int options.num_predict);
          ("num_ctx", `Int options.num_ctx);
          ("temperature", `Float options.temperature);
        ]
  | None ->
      failwith
        (Printf.sprintf
           "worker/prompt-cases.json's %s states no request of an Ollama of \
            context length %d"
           name context_length)

(* The request a worker makes of its Ollama for the case [name]: nothing
   more, and nothing less. *)
let generation ?(context_length = context_length) model name =
  `Assoc
    ([
       ("model", str model);
       ("prompt", str (prompt name));
       ("stream", `Bool false);
       ("think", `Bool false);
       ("options", ollama_options ~context_length name);
     ]
    @
    match format_of name with
    | Some format -> [ ("format", format) ]
    | None -> [])

(* Hold what the Ollama was asked to [expected], request by request. The
   prompt is compared first and apart, so a difference reads as text. *)
let expect_generations (ollama : Ollama.t) expected =
  let asked = List.map fst (Dynarray.to_list ollama.generations) in
  Check.equal "the prompts its model was sent"
    (`List (List.map (fun e -> member "prompt" e) expected))
    (`List (List.map (fun g -> member "prompt" g) asked));
  Check.equal "what its model was asked" (`List expected) (`List asked)

let expect_generations_count (w : W.t) n =
  Check.equal "the generations its model was asked for" (`Int n)
    (`Int (Dynarray.length w.ollama.generations))

(* Hold what a worker asked of its Ollama's limits to a request for each of
   [models], in the order asked. *)
let expect_shows (w : W.t) models =
  Check.equal "what it asked of its model's limits"
    (`List (List.map (fun m -> `Assoc [ ("model", str m) ]) models))
    (`List (Dynarray.to_list w.ollama.shows))

(* The request the case [name] states a worker makes of an Anthropic model
   of [facts]: the row of its requests for that model's two ceilings, and
   streamed or not as [stream] says. *)
let anthropic_request ?(facts = W.anthropic_model) ?stream name =
  match
    List.find_map
      (function
        | P.Anthropic row
          when row.max_input_tokens = facts.max_input_tokens
               && row.max_output_tokens = facts.max_output_tokens
               && (stream = None || stream = Some row.stream) ->
            Some row
        | _ -> None)
      (requests name)
  with
  | Some row -> row
  | None ->
      failwith
        (Printf.sprintf
           "worker/prompt-cases.json's %s states no request of an Anthropic \
            model that reads %d tokens and writes %d"
           name facts.max_input_tokens facts.max_output_tokens)

(* What a detection asks of a model the stand-in Anthropic describes as it
   does unless a case says otherwise: the tokens of answer the highlighting
   case states. *)
let anthropic_output () = (anthropic_request "highlighting").max_tokens

(* The size, in tokens, a detection on that model cuts its text at: what its
   unit's cursor states. *)
let anthropic_piece = 5333

(* The request a worker makes of Anthropic for the case [name], on a model
   of [facts]: nothing more, and nothing less. A detection asks for its
   answer format when the model answers in a schema. *)
let message ?(facts = W.anthropic_model) ?stream model name =
  let row = anthropic_request ~facts ?stream name in
  `Assoc
    ([ ("model", str model); ("max_tokens", `Int row.max_tokens) ]
    @ (if row.stream then [ ("stream", `Bool true) ] else [])
    @ [
        ( "messages",
          `List
            [ `Assoc [ ("role", str "user"); ("content", str (prompt name)) ] ]
        );
      ]
    @
    match format_of name with
    | Some schema when facts.structured_outputs ->
        [
          ( "output_config",
            `Assoc
              [
                ( "format",
                  `Assoc [ ("type", str "json_schema"); ("schema", schema) ] );
              ] );
        ]
    | _ -> [])

(* Hold what Anthropic was asked to [expected], request by request. What was
   said to the model is compared first and apart, so a difference reads as
   text. *)
let expect_messages (anthropic : Anthropic.t) expected =
  let asked =
    List.map
      (fun (r : Anthropic.request) -> Option.value r.body ~default:`Null)
      (Dynarray.to_list anthropic.generations)
  in
  Check.equal "what was said to its model"
    (`List (List.map (fun e -> member "messages" e) expected))
    (`List (List.map (fun g -> member "messages" g) asked));
  Check.equal "what its model was asked" (`List expected) (`List asked)

(* What the worker asked the stand-in Anthropic, request by request: each
   its method and its path. *)
let anthropic_requests (w : W.t) =
  List.map
    (fun (r : Anthropic.request) -> r.meth ^ " " ^ r.path)
    (Dynarray.to_list w.anthropic.requests)

(* What a model says when it proposes [items]: their array, as JSON. *)
let proposing items = Yojson.Safe.to_string (`List items)

(* A stand-in Ollama's answer of [response], its model done. *)
let answer response = Ollama.Answer { response; stop = Done; usage = None }

(* A stand-in Ollama's answer of [response], its model cut off. *)
let cut_off response = Ollama.Answer { response; stop = Cut_off; usage = None }

(* A stand-in Ollama's answer that proposes [items]. *)
let proposed items = answer (proposing items)

(* A proposal of the span [text], with nothing said of it. *)
let exact text = `Assoc [ ("exact", str text) ]

(* A proposal of marker [n] of [long_text]. *)
let marker n = exact (Printf.sprintf "marker %d" n)

(* A stand-in Ollama's answer that proposes each marker of [markers]. *)
let finding markers = proposed (List.map marker markers)

(* A proposal of a mention of the entity type [kind]. *)
let mention ?prefix ?suffix text kind =
  `Assoc
    ([ ("exact", str text); ("entityType", str kind) ]
    @ (match prefix with Some p -> [ ("prefix", str p) ] | None -> [])
    @ match suffix with Some s -> [ ("suffix", str s) ] | None -> [])

(* A stand-in Ollama's answer to a count: the one element a count is asked
   for. *)
let counting n = proposed [ `Assoc [ ("count", `Int n) ] ]

(* Have the stand-in Ollama answer by which model asked, as engines that work
   at once need: [model] finds the first program, any other Charles
   Babbage. *)
let answer_by_model (w : W.t) =
  w.ollama.choose <-
    Some
      (fun body ->
        Some
          (proposed
             [
               exact
                 (if member "model" body = str model then "the first program"
                  else "Charles Babbage");
             ]))

(* ── What a worker commits ──────────────────────────────────────────── *)

let is_instant text =
  Check.matches_re {|^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$|} text
  && Result.is_ok (Ptime.of_rfc3339 text)

(* The annotations of a batch, each with a created that is an instant, and
   that member taken away: when is the worker's own. *)
let without_created annotations =
  match annotations with
  | `List items ->
      `List
        (List.map
           (function
             | `Assoc members as annotation ->
                 (match List.assoc_opt "created" members with
                 | Some (`String created) when is_instant created -> ()
                 | _ ->
                     failwith
                       ("an annotation whose created is no instant: "
                      ^ Check.shown annotation));
                 `Assoc (List.remove_assoc "created" members)
             | other -> failwith ("not an annotation: " ^ Check.shown other))
           items)
  | other -> failwith ("not a batch of annotations: " ^ Check.shown other)

(* An annotation on a span of text, as a worker commits one: without
   created. *)
let text_annotation ?prefix ?suffix ?body generator resource_id motivation id
    ~start ~stop exact =
  `Assoc
    ([
       ("@context", str "http://www.w3.org/ns/anno.jsonld");
       ("type", str "Annotation");
       ("id", str id);
       ("motivation", str motivation);
       ("generator", generator);
       ( "target",
         `Assoc
           [
             ("type", str "SpecificResource");
             ("source", str resource_id);
             ( "selector",
               `List
                 [
                   `Assoc
                     [
                       ("type", str "TextPositionSelector");
                       ("start", `Int start);
                       ("end", `Int stop);
                     ];
                   `Assoc
                     ([
                        ("type", str "TextQuoteSelector"); ("exact", str exact);
                      ]
                     @ (match prefix with
                       | Some p -> [ ("prefix", str p) ]
                       | None -> [])
                     @
                     match suffix with
                     | Some s -> [ ("suffix", str s) ]
                     | None -> []);
                 ] );
           ] );
     ]
    @ match body with Some b -> [ ("body", b) ] | None -> [])

(* The highlight of marker [n] in [long_text], at [start]. What stands
   before and after a marker is the same for every sentence but a
   paragraph's first, which follows a paragraph break; none of the markers
   the cases use is in a paragraph's last sentence. *)
let marker_highlight generator resource_id id n start =
  let exact = Printf.sprintf "marker %d" n in
  let paragraph = n / 100 and sentence = n mod 100 in
  let before =
    if sentence = 1 then "beside the Rh\u{00f4}ne.\n\n"
    else "beside the Rh\u{00f4}ne. "
  in
  text_annotation generator resource_id "highlighting" id ~start
    ~stop:(start + String.length exact)
    ~prefix:
      (Printf.sprintf "%sParagraph %d, sentence %d: the survey party recorded "
         before paragraph sentence)
    ~suffix:
      (Printf.sprintf
         " beside the Rh\u{00f4}ne. Paragraph %d, sentence %d: the survey \
          party recorded"
         paragraph (sentence + 1))
    exact

(* A string in the canonical form specs/src/annotations/id-cases.json
   states: a double quote and a backslash escaped, the five control
   characters that have a short escape written so, every other below U+0020
   as \u and four lower-case hexadecimal digits, and every other character
   as itself. *)
let canonical_string text =
  let out = Buffer.create (String.length text + 2) in
  Buffer.add_char out '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string out "\\\""
      | '\\' -> Buffer.add_string out "\\\\"
      | '\b' -> Buffer.add_string out "\\b"
      | '\t' -> Buffer.add_string out "\\t"
      | '\n' -> Buffer.add_string out "\\n"
      | '\012' -> Buffer.add_string out "\\f"
      | '\r' -> Buffer.add_string out "\\r"
      | c when Char.code c < 0x20 ->
          Buffer.add_string out (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char out c)
    text;
  Buffer.add_char out '"';
  Buffer.contents out

(* What the annotation an id is worked out from is, in the canonical form
   specs/src/annotations/id-cases.json states: no white space, the members of
   every object in order of their names by code point (their UTF-8 bytes) at
   every depth, arrays in their own order, whole numbers as their digits. *)
let rec canonical : Yojson.Safe.t -> string = function
  | `Assoc members ->
      "{"
      ^ String.concat ","
          (List.map
             (fun (name, value) ->
               canonical_string name ^ ":" ^ canonical value)
             (List.sort (fun (a, _) (b, _) -> String.compare a b) members))
      ^ "}"
  | `List items -> "[" ^ String.concat "," (List.map canonical items) ^ "]"
  | `String text -> canonical_string text
  | `Int n -> string_of_int n
  | `Float f when Float.is_integer f -> Printf.sprintf "%.0f" f
  | `Bool b -> string_of_bool b
  | `Null -> "null"
  | other ->
      failwith
        ("specs/src/annotations/id-cases.json states no canonical form of "
        ^ Yojson.Safe.to_string other)

(* The id of an annotation, derived from what it is as
   specs/src/annotations/id-cases.json states: for an annotation whose
   resource a case learns only as it runs, and so cannot state its id
   beforehand. [body] is left out for an annotation that has none. The
   yield file holds this to every case of that table. *)
let annotation_id_of ?body resource_id motivation anchor =
  let identity =
    `Assoc
      ([
         ("resourceId", str resource_id);
         ("motivation", str motivation);
         ("anchor", anchor);
       ]
      @ match body with Some b -> [ ("body", b) ] | None -> [])
  in
  String.sub
    (Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet
       (Digestif.SHA256.to_raw_string
          (Digestif.SHA256.digest_string (canonical identity))))
    0 21

(* ── A job's messages ───────────────────────────────────────────────── *)

(* What every lifecycle message of [job] but a cancel carries. *)
let identity_members (job : W.job) =
  [
    ("resourceId", str job.resource_id);
    ("jobId", str job.id);
    ("jobType", str job.job_type);
    ("attempt", `Int (job.retry_count + 1));
  ]

let identity job = `Assoc (identity_members job)

(* [identity] with [members] after it. *)
let identified job members = `Assoc (identity_members job @ members)

(* A progress report of [job]. *)
let report ?(extra = []) job percentage message =
  identified job
    [
      ("percentage", `Int percentage);
      ( "progress",
        `Assoc
          ([ ("percentage", `Int percentage); ("message", message) ] @ extra) );
    ]

let code ?(members = []) name = `Assoc (("code", str name) :: members)

(* Hold the progress reports of [job] to [expected]: every one, each as
   often as it was said, in no order. *)
let expect_progress served (job : W.job) expected =
  Check.equal "its progress reports"
    (`List (W.sorted_progress expected))
    (`List (W.progress served job.id))

let about_job (job : W.job) (e : W.emitted) =
  member "jobId" e.payload = str job.id

(* Wait until [job] is settled, by [channel], and the worker has claimed
   again: the settle is a worker's last word on a job, and the claim after
   it is what shows the worker went on. The settle's payload. *)
let settled ?(channel = "job:complete") served job =
  let settle = W.emitted ~matches:(about_job job) served channel in
  let emits = W.emits served in
  let rec index at = function
    | [] -> List.length emits
    | (e : W.emitted) :: rest ->
        if e.channel = channel && about_job job e then at
        else index (at + 1) rest
  in
  let settled_at = index 0 emits in
  Client_proxy.until ~timeout:20.0 served.W.proxy "the claim after the settle"
    (fun () ->
      if
        List.exists
          (fun (e : W.emitted) -> e.channel = "job:claim" && e.status <> None)
          (List.filteri (fun at _ -> at > settled_at) (W.emits served))
      then Some ()
      else None);
  settle.payload

(* Ask for the limits on [kb]'s bus as a client does, and wait for the one
   reply. *)
let limits_asked kb =
  let listener = W.listen kb [ "job:limits-result"; "job:limits-failed" ] in
  let correlation_id = Fresh.uuid () in
  let reply =
    World.emit kb.W.world listener.token
      (`Assoc
         [
           ("channel", str "job:limits-requested");
           ("payload", `Assoc []);
           ("correlationId", str correlation_id);
           ("clientId", str listener.client_id);
         ])
  in
  Check.equal "the gateway's answer to the suite's limits request" (`Int 202)
    (`Int reply.status);
  Bus_stream.next ~timeout:15.0 listener.stream "the limits" (fun m ->
      match m.frame with
      | Some f -> f.correlation_id = Some correlation_id
      | None -> false)

(* The cancel of [job], naming the units it finished when it finished
   any. *)
let cancel_of ?completed (job : W.job) =
  `Assoc
    ([
       ("resourceId", str job.resource_id);
       ("jobId", str job.id);
       ("jobType", str job.job_type);
     ]
    @
    match completed with
    | Some units -> [ ("completedUnits", strs units) ]
    | None -> [])

(* Ask for [job] to be cancelled, as the gateway relays a cancellation to
   every worker, and wait until the worker has had it. Its one stream
   carries frames in the order they were sent, so the worker's answer to a
   request sent after the cancellation (its limits) says the cancellation
   has arrived. *)
let cancel_requested kb (job : W.job) =
  W.emit kb "job:cancel-requested" (`Assoc [ ("jobId", str job.id) ]);
  ignore (limits_asked kb : Bus_stream.message)

(* A text resource about [text], and a mark job on it, queued for the
   worker's next claim that takes it. *)
let mark_job ?metadata ?(about = text) kb name params =
  let resource_id = "res-ws-" ^ name in
  W.resource kb resource_id about;
  W.queued ?metadata kb "mark"
    (match params with
    | `Assoc members -> `Assoc (("resourceId", str resource_id) :: members)
    | other ->
        failwith ("a job's parameters are an object: " ^ Check.shown other))

(* The case [name] of worker/prompt-cases.json as a mark job, queued. *)
let case_job ?metadata ?about kb name =
  mark_job ?metadata ?about kb name (params name)

(* A mark job's parameters that name its [motivation] and nothing more. *)
let motivated motivation = `Assoc [ ("motivation", str motivation) ]

(* The jobs of an engine used for highlighting alone. *)
let highlighting_only = [ Dispatcher_world.marks "highlighting" ]

(* A highlighting job of nothing more, queued. *)
let highlighting_job ?metadata ?about kb name =
  mark_job ?metadata ?about kb name (motivated "highlighting")

(* A linking job's parameters, asking for the entity types [types]. *)
let linking types =
  `Assoc [ ("motivation", str "linking"); ("entityTypes", strs types) ]

(* The schema worker/prompt-cases.json's tagging cases are asked with, as
   the dispatcher adds it to the job it hands over: two categories. *)
let argument () = Check.at (params "tagging-claim") [ "schema" ]

(* That schema with its first category, Claim, alone. *)
let argument_of_claims () =
  match argument () with
  | `Assoc members ->
      `Assoc
        (List.map
           (fun (key, value) ->
             match (key, value) with
             | "tags", `List (claim :: _) -> (key, `List [ claim ])
             | "tags", _ ->
                 failwith
                   "worker/prompt-cases.json's tagging schema has no tags"
             | _ -> (key, value))
           members)
  | other -> failwith ("a tagging schema is an object: " ^ Check.shown other)

(* A tagging job's parameters, asking for [categories] of [schema]. *)
let tagging schema categories =
  `Assoc
    [
      ("motivation", str "tagging");
      ("schemaId", Check.at schema [ "id" ]);
      ("categories", strs categories);
      ("schema", schema);
    ]

(* The resource worker/prompt-cases.json's yield cases are focused on. *)
let case_source =
  text_of
    (Check.at (params "yield-markdown")
       [ "context"; "focus"; "resource"; "@id" ])

(* The description of a text resource [id] a yield job is focused on. *)
let source_descriptor id =
  `Assoc
    [
      ("@context", str "https://schema.org");
      ("@id", str id);
      ("name", str "Notes on the engine");
      ( "representations",
        `List
          [
            `Assoc
              [
                ("mediaType", str "text/markdown");
                ("storageUri", str (W.storage_uri id));
                ("rel", str "original");
              ];
          ] );
    ]

(* A yield job of [params] on the resource [source], queued, and held to
   the spec's GenerationJobParams first. What it makes is stored where
   [params] says, or under [stored_as] when a case names it. *)
let yield_job ?stored_as kb ~source params =
  let members =
    match params with
    | `Assoc members -> members
    | other ->
        failwith ("a yield job's parameters are an object: " ^ Check.shown other)
  in
  let members =
    match stored_as with
    | None -> members
    | Some name ->
        let storage_uri = str (Printf.sprintf "file://generated/%s.md" name) in
        if List.mem_assoc "storageUri" members then
          List.map
            (fun (k, v) ->
              if k = "storageUri" then (k, storage_uri) else (k, v))
            members
        else members @ [ ("storageUri", storage_uri) ]
  in
  let params = `Assoc (("resourceId", str source) :: members) in
  (match Protocol.against "GenerationJobParams" params with
  | [] -> ()
  | refusals ->
      failwith
        ("the case's yield job is no GenerationJobParams: "
        ^ String.concat "; " refusals));
  W.queued kb "yield" params

(* A yield job focused on the resource [source] of the short text, asking
   for a title only, and stored as [name]; queued. *)
let titled_yield kb ~source name =
  yield_job ~stored_as:name kb ~source
    (`Assoc
       [
         ("title", str "The Analytical Engine");
         ( "context",
           `Assoc
             [
               ( "focus",
                 `Assoc
                   [
                     ("kind", str "resource");
                     ("resource", source_descriptor source);
                     ( "content",
                       `Assoc [ ("main", str text); ("related", `Assoc []) ] );
                   ] );
               ("graph", `Assoc [ ("nodes", `List []); ("edges", `List []) ]);
               ("metadata", `Assoc []);
             ] );
       ])

(* ── What a job asked, made and said ────────────────────────────────── *)

(* Hold what a worker asked the gateway for, in order, to [expected]. *)
let expect_sequence served expected =
  Check.equal "what it asked the gateway for" (strs expected)
    (strs (W.sequence served))

(* What a mark job that made nothing asked of the gateway, in order. *)
let made_nothing (job : W.job) =
  [
    "emit job:claim";
    "emit job:start";
    "emit browse:resource-requested";
    "GET /resources/" ^ job.resource_id;
    "emit job:fail";
    "emit job:claim";
  ]

(* What a yield job that cited nothing asked of the gateway, in order: its
   upload, and the link from its source. *)
let yielded =
  [
    "emit job:claim";
    "emit job:start";
    "POST /resources";
    "emit mark:commit";
    "emit job:complete";
    "emit job:claim";
  ]

let all_but_last items =
  List.filteri (fun at _ -> at < List.length items - 1) items

(* Hold that the worker said nothing on any of [channels]. *)
let said_nothing served channels =
  List.iter
    (fun channel ->
      Check.equal ("its " ^ channel) (`List [])
        (`List (W.payloads served channel)))
    channels

(* What the worker committed: each batch's payload, its annotations without
   created. *)
let committed served =
  `List
    (List.map
       (function
         | `Assoc members ->
             `Assoc
               (List.map
                  (fun (k, v) ->
                    if k = "annotations" then (k, without_created v) else (k, v))
                  members)
         | other -> other)
       (W.payloads served "mark:commit"))

(* What the worker committed: each batch's annotations, without created. *)
let batches served =
  `List
    (List.map
       (fun p -> without_created (member "annotations" p))
       (W.payloads served "mark:commit"))

(* How many annotations each batch the worker committed carried. *)
let sizes_of_batches (w : W.t) =
  `List
    (List.map
       (fun (c : W.commit) -> `Int (List.length c.annotations))
       (Dynarray.to_list w.own.commits))

let expect_commits_count (w : W.t) n =
  Check.equal "the batches it committed" (`Int n)
    (`Int (Dynarray.length w.own.commits))

let tag_body category =
  `Assoc
    [
      ("type", str "TextualBody");
      ("value", str category);
      ("purpose", str "tagging");
      ("format", str "text/plain");
      ("language", str "en");
    ]

(* An unresolved reference's body: the entity type, as a tag. *)
let entity kind = `List [ tag_body kind ]

(* A tag's bodies: the category, and the schema it is a category of. *)
let tagged category =
  `List
    [
      tag_body category;
      `Assoc
        [
          ("type", str "TextualBody");
          ("value", Check.at (argument ()) [ "id" ]);
          ("purpose", str "classifying");
          ("format", str "text/plain");
        ];
    ]

(* A comment's body: what the model wrote, as plain text, in the language
   the job asked for. *)
let comment ~language value =
  `List
    [
      `Assoc
        [
          ("type", str "TextualBody");
          ("value", str value);
          ("purpose", str "commenting");
          ("format", str "text/plain");
          ("language", str language);
        ];
    ]

(* Where a unit stands, as a checkpoint says it. *)
let cursor ~next ~size ~found ~emitted ~errors =
  `Assoc
    [
      ("next", `Int next);
      ("size", `Int size);
      ("found", `Int found);
      ("emitted", `Int emitted);
      ("errors", `Int errors);
    ]

(* A checkpoint of [job]: the units it names finished, and where each unit
   of [cursors] stands. *)
let checkpoint ?(completed = []) (job : W.job) cursors =
  `Assoc
    [
      ("jobId", str job.id);
      ("completedUnits", strs completed);
      ("unitCursors", `Assoc cursors);
    ]

let checkpoints served = `List (W.payloads served "job:checkpoint")

(* ── What a mark job reports ────────────────────────────────────────── *)

(* The report of a mark job that has made [count] so far. *)
let creating ?extra job count =
  report ?extra job 60
    (code ~members:[ ("count", `Int count) ] "creating-annotations")

(* The report of a mark job of [motivation] that finished, having made
   [count]. *)
let complete_created ?extra job motivation count =
  report ?extra job 100
    (code
       ~members:[ ("count", `Int count); ("motivation", str motivation) ]
       "complete-created")

(* The completion of a mark job of [result] whose commits the record
   acknowledged. *)
let completed job result =
  identified job
    [ ("result", `Assoc result); ("durability", str "acknowledged") ]

(* What a linking job of the entity types [types] echoes of what it was
   asked with, on every report. *)
let echoed_entity_types types =
  ( "requestParams",
    `List
      [
        `Assoc
          [
            ("label", str "entity-types");
            ("value", str (String.concat ", " types));
          ];
      ] )

(* An entity type among the finished, with what it found and recorded, and
   what a count said was missed when it said so: the pieces, what they
   found, and what was counted. *)
let finished_type ?under_reported kind ~found ~persisted =
  `Assoc
    ([
       ("value", str kind);
       ("foundCount", `Int found);
       ("persistedCount", `Int persisted);
     ]
    @
    match under_reported with
    | Some (pieces, found, counted) ->
        [
          ( "underReported",
            `Assoc
              [
                ("pieces", `Int pieces);
                ("found", `Int found);
                ("counted", `Int counted);
              ] );
        ]
    | None -> [])

(* A report of where a linking job of [types] stands while it works on the
   entity type [kind]. [expected] is left out until a count has said it. *)
let detecting ~types job kind percentage ~processed ~found ?expected ~emitted
    completed =
  report job percentage
    (code ~members:[ ("entityType", str kind) ] "detecting-entities")
    ~extra:
      ([
         ("current", `Assoc [ ("kind", str "entity-type"); ("value", str kind) ]);
         ("processed", `Int processed);
         ("total", `Int (List.length types));
         ("entitiesFound", `Int found);
       ]
      @ (match expected with
        | Some n -> [ ("entitiesExpected", `Int n) ]
        | None -> [])
      @ [
          ("entitiesEmitted", `Int emitted);
          ("completedItems", `List completed);
          echoed_entity_types types;
        ])

(* The report of a linking job of [types] that finished, having made
   [count]. *)
let linking_complete ~types job ~count ~expected completed =
  complete_created job "linking" count
    ~extra:
      [
        ("entitiesExpected", `Int expected);
        ("completedItems", `List completed);
        echoed_entity_types types;
      ]

(* Where a tagging job of two categories stands as it begins [category],
   with [processed] of them behind it. *)
let on_category category processed completed =
  [
    ("current", `Assoc [ ("kind", str "category"); ("value", str category) ]);
    ("processed", `Int processed);
    ("total", `Int 2);
    ("completedItems", `List completed);
  ]

(* A category among a tagging job's finished, with what it found. *)
let found_in category count =
  `Assoc [ ("value", str category); ("foundCount", `Int count) ]

let analyzing_tags ?extra job percentage =
  report ?extra job percentage (code "analyzing-tags")

(* The report of a tagging job that has made [count] so far. *)
let creating_tags job count =
  report job 60
    (code ~members:[ ("count", `Int count) ] "creating-tag-annotations")

(* A failure's payload, its error taken away, and the error. *)
let failure_of payload =
  match payload with
  | `Assoc members ->
      let error =
        match List.assoc_opt "error" members with
        | Some (`String error) -> error
        | _ ->
            failwith ("a failure whose error is no text: " ^ Check.shown payload)
      in
      (`Assoc (List.remove_assoc "error" members), error)
  | other -> failwith ("a failure is an object: " ^ Check.shown other)

(* A failure of [job], without its error. *)
let failed ?failure_class ~will_retry job =
  identified job
    ((match failure_class with
       | Some c -> [ ("failureClass", str c) ]
       | None -> [])
    @ [ ("willRetry", `Bool will_retry) ])

(* Hold that [said] carries [words]. *)
let says what said words =
  Check.that
    (Printf.sprintf "%s says %S: %s" what words said)
    (Re.execp (Re.compile (Re.str words)) said)

let uploads (w : W.t) = Dynarray.to_list w.own.world.archivist.uploads

(* The one resource the worker uploaded. *)
let upload_of (w : W.t) =
  match uploads w with
  | [ upload ] -> upload
  | uploads ->
      failwith
        (Printf.sprintf "it uploaded %d resources, not one"
           (List.length uploads))

(* The completion of a yield job that made [upload], named by the job's
   title. *)
let generated (job : W.job) (upload : Archivist_standin.upload) ~truncated =
  identified job
    [
      ( "result",
        `Assoc
          [
            ("resourceId", str upload.resource_id);
            ("resourceName", Check.at job.running [ "params"; "title" ]);
            ("truncated", `Bool truncated);
          ] );
      ("durability", str "acknowledged");
    ]

(* ── What a worker says ─────────────────────────────────────────────── *)

let messages () = (Protocol.tables ()).worker_messages

(* What a worker's log says of a knowledge base it serves, as
   worker/messages.json's [logged] row [name] states it. *)
let kb_log name =
  match
    List.find_opt
      (fun (row : M.smelter_messages_logged_item) -> row.name = name)
      (messages ()).logged
  with
  | Some row -> row.message
  | None -> failwith ("worker/messages.json states no logged message " ^ name)

(* The lines of a worker's log, written in the json form, whose message is
   [message]: each the one JSON object it is, its facts members of it. *)
let logged lines message =
  List.filter_map
    (fun line ->
      match Yojson.Safe.from_string line with
      | `Assoc _ as entry when member "message" entry = str message ->
          Some entry
      | _ -> None
      | exception Yojson.Json_error _ -> None)
    lines

(* The lines the worker's log wrote of a knowledge base, as
   worker/messages.json's [logged] row [name] states it. *)
let kb_logged (served : W.served) name =
  logged (Worker_service.stdout served.worker) (kb_log name)

(* Hold a line its log wrote of a knowledge base: at [level], of [domain],
   with each of [said] a text. *)
let logs_of ~level ~domain ~said line =
  Check.like "the line its log wrote"
    (`Assoc [ ("level", str level); ("domain", str domain) ])
    line;
  List.iter
    (fun name ->
      Check.that
        (Printf.sprintf "the line says %s" name)
        (match Protocol.at line [ name ] with
        | Some (`String _) -> true
        | _ -> false))
    said

(* Hold that its log says once that a knowledge base refused it, and of
   [kb], with its reason. *)
let logs_refusal served (kb : W.kb) =
  match kb_logged served "refused" with
  | [ line ] ->
      logs_of ~level:"error" ~domain:(W.domain kb) ~said:[ "reason" ] line
  | lines ->
      failwith
        (Printf.sprintf "its log says %d times that a knowledge base refused it"
           (List.length lines))

(* What worker/messages.json states a worker calls a mark job of
   [motivation] in what it says. *)
let mark_named motivation =
  Protocol.fill_template (messages ()).job.mark [ ("motivation", motivation) ]

(* The detail worker/messages.json states for [name], filled. *)
let detail name fill =
  match
    List.find_opt
      (fun (row : M.details_item) -> row.name = name)
      (messages ()).details
  with
  | Some row -> Protocol.fill_template row.detail fill
  | None -> failwith ("worker/messages.json states no detail " ^ name)

(* What worker/messages.json states a worker says of a wait, for [name],
   filled. *)
let wait name fill =
  match
    List.find_opt
      (fun (row : M.waits_item) -> row.name = name)
      (messages ()).waits
  with
  | Some row -> Protocol.fill_template row.said fill
  | None -> failwith ("worker/messages.json states no wait " ^ name)

(* The error worker/messages.json states for [name], filled. *)
let error name fill =
  match
    List.find_opt
      (fun (row : M.errors_item) -> row.name = name)
      (messages ()).errors
  with
  | Some row -> Protocol.fill_template row.error fill
  | None -> failwith ("worker/messages.json states no error " ^ name)

(* ── Who it is, and who said what ────────────────────────────────────── *)

(* The grants [kb]'s issuer has answered the worker's account there. An
   issuer holds a count for an account only once it has granted it one, so
   no count is none. *)
let grants (kb : W.kb) =
  Option.value ~default:0 (Hashtbl.find_opt kb.world.issuer.grants kb.client)

(* Who the gateway named the party that did a thing, or null for none. *)
let by_of = function Some did -> str did | None -> `Null

(* What the worker said on a channel: who the gateway named it, what it
   said, to which scope, and what the gateway answered. *)
let said_on (e : W.emitted) =
  `Assoc
    [
      ("by", by_of e.emitted_by);
      ("payload", e.payload);
      ("scope", match e.scope with Some s -> str s | None -> `Null);
      ("status", match e.status with Some s -> `Int s | None -> `Null);
    ]

(* The answer to the worker's question [asked] of which knowledge base this
   is, as its stream carried it. *)
let answer_to served (asked : W.emitted) =
  let correlation_id =
    match asked.correlation_id with
    | Some id -> str id
    | None -> failwith "it asked which knowledge base this is uncorrelated"
  in
  match
    List.find_opt
      (fun (c : W.carried) -> member "correlationId" c.frame = correlation_id)
      (W.carried served "browse:kb-result")
  with
  | Some answer -> answer
  | None -> failwith "its question was answered on no stream it read"

(* ── Stopping ───────────────────────────────────────────────────────── *)

let exit_code code =
  match code with Some code -> `Int code | None -> `String "a signal"

(* Stop the worker with SIGTERM, and its exit code. *)
let stopped (served : W.served) = exit_code (Worker_service.stop served.worker)

(* Stop the worker while it holds [job], and hold what it does: it exits 0,
   having failed the job, with no class and as the gateway took it, saying
   the worker stopped. *)
let stopped_holding served job =
  Check.equal "its exit code" (`Int 0) (stopped served);
  let failures = W.emits ~channel:"job:fail" served in
  Check.equal "the gateway's answers to its failures"
    (`List [ `Int 202 ])
    (`List
       (List.map
          (fun (e : W.emitted) ->
            match e.status with Some s -> `Int s | None -> `Null)
          failures));
  let failure, error = failure_of (List.hd failures).payload in
  (* No class is stated: the job's retry budget decides. *)
  Check.equal "its failure" (failed ~will_retry:true job) failure;
  says "its error" error "stopped"

(* ── What a worker exports ──────────────────────────────────────────── *)

(* Where a worker exports, and how soon it does. *)
let exporting (otlp : Otlp.t) =
  [
    ("OTEL_EXPORTER_OTLP_ENDPOINT", Some otlp.endpoint);
    ("OTEL_METRIC_EXPORT_INTERVAL", Some "500");
    ("OTEL_BSP_SCHEDULE_DELAY", Some "100");
  ]

let otlp_json = function
  | Otlp.Text s -> str s
  | Flag b -> `Bool b
  | Number n -> `Float n
  | Unreadable -> str "(unreadable)"

(* Every value [metric] carried under [key], if it carried [key]. *)
let carried_under (metric : Otlp.metric) key =
  Option.map
    (fun (_, values) -> Dynarray.to_list values)
    (Dynarray.find_opt (fun (k, _) -> k = key) metric.attributes)

(* Every value [metric] carried under [key], which it must have carried. *)
let values_of metric key =
  match carried_under metric key with
  | Some values -> values
  | None -> failwith ("the metric carried no " ^ key)

(* The span a worker exported under [name] for [job]. *)
let span_of (otlp : Otlp.t) name (job : W.job) =
  Run.eventually ("the span of " ^ job.id) 15.0 (fun () ->
      Dynarray.find_opt
        (fun (s : Otlp.span) ->
          s.name = name
          && List.assoc_opt "job.id" s.attributes = Some (Text job.id))
        otlp.spans)
