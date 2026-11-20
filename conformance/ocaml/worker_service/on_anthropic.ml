(* anthropic.test.ts, named on_anthropic beside the harness's Anthropic: a
   worker whose engine is an Anthropic one (WORKER-SERVICE.md § Limits, § The request, § The five kinds of mark job,
   § Failures, § Cancellation, § Generation, § Telemetry, § Stopping): what it
   asks its provider before its first generation, the request each kind of
   generation is, how many times a request the provider refuses or drops is
   made, what a job fails as then, the answer it takes as a stream, the
   tokens it counts, and what a cancellation and a stop do to a request that
   is under way. *)

open Xynolog_conformance
open Support
open Case

(* The engine the worker of these cases runs: the first Anthropic one. *)
let claude = List.hd W.anthropic_engines

(* Start a worker with one engine, the Anthropic one, serving every job. *)
let started ?env (w : W.t) =
  W.start ~engines:[ W.entry w claude (Dispatcher_world.every_job ()) ] ?env w

(* An answer of the model: its text, and the tokens the provider says it
   read and wrote. *)
let said ?stop text =
  Anthropic.Answer
    { text; usage = { input = 412; output = 57 }; stop_reason = stop }

let json_text json = Yojson.Safe.to_string json

(* A refusal, as the provider words one. *)
let refused ?(headers = []) status kind words =
  Anthropic.Fails
    { status; body = json_text (Anthropic.refusal kind words); headers }

(* The request a highlighting job over the short text makes of a model the
   stand-in describes as it does unless told otherwise. *)
let highlighting () = message claude.model "highlighting"

(* A yield job focused on [case_source], queued, asking as the case [asked]
   of worker/prompt-cases.json does. *)
let yield_job kb name asked =
  yield_job ~stored_as:name kb ~source:case_source (params asked)

(* The highlight of "the first program" in the short text, on
   [resource_id]. *)
let first_program generator resource_id =
  text_annotation generator resource_id "highlighting"
    (annotation_id_of resource_id "highlighting"
       (str "23:40:the first program"))
    ~start:23 ~stop:40 ~prefix:"Ada Lovelace published "
    ~suffix:
      " in 1843 \u{2014} a method for computing Bernoulli numbers on the \
       Analytical"
    "the first program"

let generation (w : W.t) at = Dynarray.get w.anthropic.generations at

let early_reports job =
  [ report job 10 (code "loading"); report job 30 (code "analyzing") ]

let asks_once_then_each (w : W.t) =
  let first = highlighting_job w.own "anthropic-highlighting" in
  let second = highlighting_job w.own "anthropic-highlighting-again" in
  let resource_id = first.resource_id in
  Anthropic.script w.anthropic
    [
      said
        (proposing
           [
             `Assoc
               [
                 ("exact", str "the first program");
                 ("prefix", str "Ada Lovelace published ");
                 ("suffix", str " in 1843");
               ];
             (* Three times in the text: what the model says is around it
                tells which. *)
             `Assoc
               [
                 ("exact", str "London");
                 ("prefix", str "standards, and ");
                 ("suffix", str " ignored it");
               ];
             (* Nowhere in the text. *)
             exact "a steam locomotive crossing the Alps";
           ]);
      said "[]";
    ];
  let served = started w in
  let completion = settled served first in
  ignore (settled served second : Yojson.Safe.t);
  (* The model's ceilings, then a generation for each job: the first is
     asked once, and kept. *)
  Check.equal "what it asked its provider"
    (strs
       [
         "GET /v1/models/" ^ claude.model;
         "POST /v1/messages";
         "POST /v1/messages";
       ])
    (strs (anthropic_requests w));
  Check.equal "what it asked of its model's ceilings" (`List [ `Null ])
    (`List
       (List.map
          (fun (r : Anthropic.request) -> Option.value r.body ~default:`Null)
          (Dynarray.to_list w.anthropic.described)));
  (* A generation: the model, the budget, the prompt as the one message, and
     the schema of the answer. No temperature, though a detection's is 0. *)
  expect_messages w.anthropic [ highlighting (); highlighting () ];
  (* Every request carries the key the variable holds, as a bearer token and
     in no other header, and the version of the API it speaks. *)
  let header name =
    `List
      (List.map
         (fun (r : Anthropic.request) ->
           match List.assoc_opt name r.headers with
           | Some value -> str value
           | None -> `Null)
         (Dynarray.to_list w.anthropic.requests))
  in
  Check.equal "the bearer each request carries"
    (`List (List.init 3 (fun _ -> str ("Bearer " ^ w.anthropic.api_key))))
    (header "authorization");
  Check.equal "the x-api-key each request carries"
    (`List (List.init 3 (fun _ -> `Null)))
    (header "x-api-key");
  Check.equal "the version each request speaks"
    (`List (List.init 3 (fun _ -> str "2026-10-21")))
    (header "anthropic-version");
  (* And nothing the worker wrote carries the key. *)
  Check.that "what it wrote carries no key"
    (not
       (Re.execp
          (Re.compile (Re.str w.anthropic.api_key))
          (String.concat "\n" (Worker_service.output served.worker))));
  (* What it made of the answer: the two spans that are in the text, by the
     engine that asked. *)
  let generator = W.generator ~engine:claude w.own in
  Check.equal "what it committed"
    (`List
       [
         `Assoc
           [
             ("resourceId", str resource_id);
             ("jobId", str first.id);
             ( "annotations",
               `List
                 [
                   first_program generator resource_id;
                   text_annotation generator resource_id "highlighting"
                     (annotation_id_of resource_id "highlighting"
                        (str "354:360:London"))
                     ~start:354 ~stop:360
                     ~prefix:
                       "claim for a century.\n\n\
                        The program was na\u{00ef}ve by later standards, and "
                     ~suffix:" ignored it. London was wrong.\n" "London";
                 ] );
           ];
       ])
    (committed served);
  Check.equal "who committed"
    (strs [ W.did w.own ])
    (`List
       (List.map
          (fun (c : W.commit) -> c.committed_by)
          (Dynarray.to_list w.own.commits)));
  Check.equal "its first checkpoint"
    (checkpoint first
       [
         ( "highlighting",
           cursor ~next:(code_points text) ~size:anthropic_piece ~found:3
             ~emitted:2 ~errors:1 );
       ])
    (List.hd (W.payloads served "job:checkpoint"));
  Check.equal "its completion"
    (completed first
       [ ("found", `Int 3); ("persisted", `Int 2); ("errors", `Int 1) ])
    completion

let budget (max_input_tokens, max_output_tokens, size) (w : W.t) =
  let facts =
    { W.anthropic_model with Anthropic.max_input_tokens; max_output_tokens }
  in
  Hashtbl.replace w.anthropic.models claude.model facts;
  let job = highlighting_job w.own "anthropic-budget" in
  Anthropic.script w.anthropic [ said "[]" ];
  let served = started w in
  ignore (settled served job : Yojson.Safe.t);
  expect_messages w.anthropic [ message ~facts claude.model "highlighting" ];
  Check.equal "its checkpoints"
    (`List
       [
         checkpoint job
           [
             ( "highlighting",
               cursor ~next:(code_points text) ~size ~found:0 ~emitted:0
                 ~errors:0 );
           ];
       ])
    (checkpoints served)

let no_temperature (w : W.t) =
  let mark = highlighting_job w.own "anthropic-no-temperature" in
  let made =
    yield_job w.own "anthropic-no-temperature-yield" "yield-markdown"
  in
  Anthropic.script w.anthropic
    [
      said (proposing [ exact "the first program" ]); said "It was never built.";
    ];
  let served = started w in
  (* The stand-in refuses a temperature with 400 on every model, as the
     provider does: a job that sent one would fail here. *)
  ignore (settled served mark : Yojson.Safe.t);
  ignore (settled served made : Yojson.Safe.t);
  expect_messages w.anthropic
    [ highlighting (); message claude.model "yield-markdown" ];
  said_nothing served [ "job:fail" ]

let no_schema (w : W.t) =
  let facts = { W.anthropic_model with Anthropic.structured_outputs = false } in
  Hashtbl.replace w.anthropic.models claude.model facts;
  let mark = highlighting_job w.own "anthropic-no-schema" in
  let made = yield_job w.own "anthropic-no-schema-yield" "yield-markdown" in
  Anthropic.script w.anthropic [ said "It was never built." ];
  let served = started w in
  let failure, error = failure_of (settled ~channel:"job:fail" served mark) in
  ignore (settled served made : Yojson.Safe.t);
  (* No attempt changes what the model can do: the job says it will not be
     retried, whatever budget it has left. *)
  Check.equal "its failure"
    (failed ~failure_class:"deterministic" ~will_retry:false mark)
    failure;
  says "its error" error
    (Support.error "no-structured-outputs" [ ("model", claude.model) ]);
  expect_progress served mark (early_reports mark);
  (* It learned of the model once, and asked it for the one generation that
     wants no schema. *)
  Check.equal "how often it asked of its model" (`Int 1)
    (`Int (Dynarray.length w.anthropic.described));
  expect_messages w.anthropic [ message ~facts claude.model "yield-markdown" ];
  expect_sequence served (all_but_last (made_nothing mark) @ yielded);
  Check.equal "what it committed to" (strs [ case_source ])
    (strs
       (List.map
          (fun (c : W.commit) -> c.committed_to)
          (Dynarray.to_list w.own.commits)))

(* [text] with its one [from] replaced by [into]. *)
let replaced ~from ~into text =
  Re.replace_string ~all:false (Re.compile (Re.str from)) ~by:into text

let with_message body content =
  match body with
  | `Assoc members ->
      `Assoc
        (List.map
           (fun (k, v) ->
             if k = "messages" then
               ( k,
                 `List
                   [ `Assoc [ ("role", str "user"); ("content", str content) ] ]
               )
             else (k, v))
           members)
  | other -> other

let linking_four_at_once (w : W.t) =
  let types = [ "Person"; "Place"; "Date"; "Machine"; "Number" ] in
  let job = mark_job w.own "anthropic-linking" (linking types) in
  let resource_id = job.resource_id in
  (* What is asked of a type is what is kept as asked of Person, with the
     type's own name. *)
  let extraction kind =
    with_message
      (message claude.model "linking-person")
      (replaced ~from:"mentions of: Person."
         ~into:("mentions of: " ^ kind ^ ".")
         (prompt "linking-person"))
  in
  let count kind =
    with_message
      (message claude.model "linking-person-count")
      (replaced ~from:"mention of: Person in"
         ~into:("mention of: " ^ kind ^ " in")
         (prompt "linking-person-count"))
  in
  Check.equal "what is asked of Place"
    (`List
       [
         message claude.model "linking-place";
         message claude.model "linking-place-count";
       ])
    (`List [ extraction "Place"; count "Place" ]);
  let said_in body = Check.canonical (member "messages" body) in
  let sorted bodies =
    `List
      (List.sort
         (fun a b -> compare (json_text (said_in a)) (json_text (said_in b)))
         bodies)
  in
  let asked () =
    List.map
      (fun (r : Anthropic.request) -> Option.value r.body ~default:`Null)
      (Dynarray.to_list w.anthropic.generations)
  in
  (* Each extraction is held until the case lets it go; each count is
     answered as it is asked, with what its extraction is to find. *)
  let found =
    [
      ("Person", [ mention "Charles Babbage" "Person" ]);
      ( "Place",
        [ mention "London" "Place" ~prefix:"engine in " ~suffix:", but" ] );
      ("Date", []);
      ("Machine", []);
      ("Number", []);
    ]
  in
  let type_of body request =
    List.find_opt (fun kind -> said_in body = said_in (request kind)) types
  in
  (* Both ask for structure: a count is told by the schema it asks in. *)
  let count_format = Option.get (format_of "linking-person-count") in
  let asks_for_a_count body =
    Check.canonical (member "output_config" body)
    = Check.canonical
        (`Assoc
           [
             ( "format",
               `Assoc [ ("type", str "json_schema"); ("schema", count_format) ]
             );
           ])
  in
  w.anthropic.choose <-
    Some
      (fun body ->
        if asks_for_a_count body then
          Option.map
            (fun kind ->
              said
                (proposing
                   [
                     `Assoc
                       [ ("count", `Int (List.length (List.assoc kind found))) ];
                   ]))
            (type_of body count)
        else Some Anthropic.Hold);
  let extracted body =
    match type_of body extraction with
    | Some kind -> said (proposing (List.assoc kind found))
    | None ->
        failwith ("an extraction of no type the job names: " ^ Check.shown body)
  in
  let served = started w in
  Anthropic.asked w.anthropic 4;
  Run.sleep 0.25;
  (* Four types are with the model, the job's first four, and none has been
     answered: the fifth is not asked about while they are. *)
  Check.equal "what is with its model"
    (sorted (List.map extraction (List.filteri (fun at _ -> at < 4) types)))
    (sorted (asked ()));
  expect_commits_count w 0;
  Anthropic.release w.anthropic extracted;
  (* Once one of the four is done, the fifth is asked about. *)
  Run.eventually "the fifth type to be asked about" 15.0 (fun () ->
      if
        List.exists
          (fun body -> said_in body = said_in (extraction "Number"))
          (asked ())
      then Some ()
      else None);
  Anthropic.release w.anthropic extracted;
  let completion = settled served job in
  (* Ten requests, in no fixed order across the types: an extraction and a
     count for each. *)
  Check.equal "what its model was asked"
    (sorted (List.map extraction types @ List.map count types))
    (sorted (asked ()));
  let reference kind ~start ~stop ~prefix ~suffix exact =
    text_annotation
      (W.generator ~engine:claude w.own)
      resource_id "linking"
      (annotation_id_of ~body:(entity kind) resource_id "linking"
         (str (Printf.sprintf "%d:%d:%s" start stop exact)))
      ~start ~stop ~prefix ~suffix ~body:(entity kind) exact
  in
  let by_text = function
    | `List items ->
        `List
          (List.sort
             (fun a b ->
               compare
                 (json_text (Check.canonical a))
                 (json_text (Check.canonical b)))
             items)
    | other -> other
  in
  Check.equal "what it committed"
    (by_text
       (`List
          [
            `List
              [
                reference "Person" ~start:119 ~stop:134
                  ~prefix:
                    "method for computing Bernoulli numbers on the Analytical \
                     Engine.\n\n"
                  ~suffix:
                    " designed the engine in London, but it was never built. \
                     Lovelace"
                  "Charles Babbage";
              ];
            `List
              [
                reference "Place" ~start:158 ~stop:164
                  ~prefix:
                    "on the Analytical Engine.\n\n\
                     Charles Babbage designed the engine in "
                  ~suffix:
                    ", but it was never built. Lovelace argued that the engine \
                     could manipulate"
                  "London";
              ];
          ]))
    (by_text (batches served));
  Check.equal "its completion"
    (completed job [ ("found", `Int 2); ("persisted", `Int 2) ])
    completion

let refused_thrice (status, kind, words) (w : W.t) =
  let job =
    highlighting_job w.own ("anthropic-refused-" ^ string_of_int status)
  in
  let refusal = refused status kind words in
  Anthropic.script w.anthropic [ refusal; refusal; refusal ];
  let served = started w in
  let failure, error = failure_of (settled ~channel:"job:fail" served job) in
  (* The same request, three times: the first asking, and two more. *)
  expect_messages w.anthropic
    [ highlighting (); highlighting (); highlighting () ];
  (* It waited before each: three eighths of a second at the least, and then
     twice that, less the grain of a timer. *)
  let at n = (generation w n).at in
  Check.that
    (Printf.sprintf "it waited %g ms before its second asking" (at 1 -. at 0))
    (at 1 -. at 0 >= 370.0);
  Check.that
    (Printf.sprintf "it waited %g ms before its third asking" (at 2 -. at 1))
    (at 2 -. at 1 >= 745.0);
  Check.equal "its failure"
    (failed ~failure_class:"transient" ~will_retry:true job)
    failure;
  (* The error says what the provider said the last time: its status and
     its body. *)
  says "its error" error (string_of_int status);
  says "its error" error words;
  expect_progress served job (early_reports job);
  expect_sequence served (made_nothing job)

let refused_as_its_fault (w : W.t) =
  let job = highlighting_job w.own "anthropic-refused-400" in
  Anthropic.script w.anthropic
    [ refused 400 "invalid_request_error" "max_tokens: the model writes fewer" ];
  let served = started w in
  let failure, error = failure_of (settled ~channel:"job:fail" served job) in
  expect_messages w.anthropic [ highlighting () ];
  Check.equal "its failure"
    (failed ~failure_class:"deterministic" ~will_retry:false job)
    failure;
  says "its error" error "400";
  says "its error" error "max_tokens: the model writes fewer";
  expect_sequence served (made_nothing job)

let over_its_rate = "the account is over its rate"

let refused_then_answered (w : W.t) =
  let job = highlighting_job w.own "anthropic-refused-once" in
  Anthropic.script w.anthropic
    [
      refused
        ~headers:[ ("retry-after", "2") ]
        429 "rate_limit_error" over_its_rate;
      said (proposing [ exact "the first program" ]);
    ];
  let served = started w in
  let completion = settled served job in
  expect_messages w.anthropic [ highlighting (); highlighting () ];
  (* Two seconds, less the grain of a timer: far over the second it waits
     when it is told nothing. *)
  let waited = (generation w 1).at -. (generation w 0).at in
  Check.that
    (Printf.sprintf "it waited %g ms before asking again" waited)
    (waited >= 1990.0);
  Check.equal "its completion"
    (completed job [ ("found", `Int 1); ("persisted", `Int 1) ])
    completion;
  said_nothing served [ "job:fail" ]

let told_to_wait (w : W.t) =
  let job = highlighting_job w.own "anthropic-told-to-wait" in
  (* Two minutes and a second. A second asking is scripted no answer: one
     made would fail the case as a request nothing scripted. *)
  Anthropic.script w.anthropic
    [
      refused
        ~headers:[ ("retry-after", "121") ]
        429 "rate_limit_error" over_its_rate;
    ];
  let served = started w in
  let failure, error = failure_of (settled ~channel:"job:fail" served job) in
  expect_messages w.anthropic [ highlighting () ];
  (* The refusal's own class: the provider said not now, and nothing about
     the request was judged. *)
  Check.equal "its failure"
    (failed ~failure_class:"transient" ~will_retry:true job)
    failure;
  (* What the provider said, and the wait it stated, which the worker did not
     wait. *)
  says "its error" error "429";
  says "its error" error over_its_rate;
  says "its error" error (wait "seconds" [ ("seconds", "121") ]);
  expect_sequence served (made_nothing job)

let unlearned_told_to_wait (w : W.t) =
  w.anthropic.models_refusal <-
    Some { Anthropic.status = 429; headers = [ ("retry-after-ms", "121000") ] };
  let job = highlighting_job w.own "anthropic-unlearned-told-to-wait" in
  let served = started w in
  let failure, error = failure_of (settled ~channel:"job:fail" served job) in
  Check.equal "its failure"
    (failed ~failure_class:"transient" ~will_retry:true job)
    failure;
  says "its error" error claude.model;
  says "its error" error "429";
  says "its error" error (wait "milliseconds" [ ("milliseconds", "121000") ]);
  (* Asked once, and nothing else was: no generation. *)
  Check.equal "what it asked its provider"
    (strs [ "GET /v1/models/" ^ claude.model ])
    (strs (anthropic_requests w));
  expect_sequence served (made_nothing job)

let dropped (w : W.t) =
  let job = highlighting_job w.own "anthropic-dropped" in
  Anthropic.script w.anthropic
    [ Anthropic.Drop; Anthropic.Drop; Anthropic.Drop ];
  let served = started w in
  let failure, _ = failure_of (settled ~channel:"job:fail" served job) in
  expect_messages w.anthropic
    [ highlighting (); highlighting (); highlighting () ];
  Check.equal "its failure" (failed ~will_retry:true job) failure;
  expect_sequence served (made_nothing job)

let unlearned (status, failure_class, askings) (w : W.t) =
  w.anthropic.models_refusal <- Some { Anthropic.status; headers = [] };
  let job =
    highlighting_job w.own ("anthropic-unlearned-" ^ string_of_int status)
  in
  let served = started w in
  let failure, error = failure_of (settled ~channel:"job:fail" served job) in
  (* The class is the status's, as a refused generation's is, and the error
     says the status. *)
  Check.equal "its failure"
    (failed ~failure_class ~will_retry:(failure_class = "transient") job)
    failure;
  says "its error" error claude.model;
  says "its error" error (string_of_int status);
  Check.equal "how often it asked of its model" (`Int askings)
    (`Int (Dynarray.length w.anthropic.described));
  Check.equal "the generations it asked for" (`Int 0)
    (`Int (Dynarray.length w.anthropic.generations));
  expect_sequence served (made_nothing job);
  (* What it could not learn it did not keep: the next job asks again. *)
  w.anthropic.models_refusal <- None;
  let next = highlighting_job w.own "anthropic-learned" in
  Anthropic.script w.anthropic [ said "[]" ];
  W.announce w.own next;
  ignore (settled served next : Yojson.Safe.t);
  Check.equal "how often it asked of its model"
    (`Int (askings + 1))
    (`Int (Dynarray.length w.anthropic.described));
  expect_messages w.anthropic [ highlighting () ]

let cut_off_twice (w : W.t) =
  let job = highlighting_job w.own "anthropic-cut-off" in
  let cut_off =
    said ~stop:"max_tokens" (proposing [ exact "the first program" ])
  in
  Anthropic.script w.anthropic [ cut_off; cut_off ];
  let served = started w in
  let failure, _ = failure_of (settled ~channel:"job:fail" served job) in
  expect_messages w.anthropic [ highlighting (); highlighting () ];
  Check.equal "its failure"
    (failed ~failure_class:"deterministic" ~will_retry:false job)
    failure;
  (* What a cut-off answer did carry is not committed. *)
  expect_commits_count w 0

let two_uploads (w : W.t) =
  match uploads w with
  | [ first; second ] -> (first, second)
  | uploads ->
      failwith
        (Printf.sprintf "it uploaded %d resources, not two"
           (List.length uploads))

let yield_own_length (w : W.t) =
  (* The model reads 400 tokens: the prompt, 394 of them, and the 300 asked
     for are together over that, and the generation is asked for all the
     same. *)
  let facts =
    {
      W.anthropic_model with
      Anthropic.max_input_tokens = 400;
      max_output_tokens = 300;
    }
  in
  Hashtbl.replace w.anthropic.models claude.model facts;
  let whole = yield_job w.own "anthropic-yield" "yield-markdown" in
  let cut_off = yield_job w.own "anthropic-yield-cut-off" "yield-markdown" in
  let written =
    "Charles Babbage designed the engine in London. It was never built."
  in
  let partly = "Charles Babbage designed the engine in" in
  Anthropic.script w.anthropic [ said written; said ~stop:"max_tokens" partly ];
  let served = started w in
  let completion = settled served whole in
  let truncated = settled served cut_off in
  let request = message ~facts claude.model "yield-markdown" in
  expect_messages w.anthropic [ request; request ];
  let first, second = two_uploads w in
  Check.equal "what it uploaded"
    (strs [ written; partly ])
    (strs [ first.file; second.file ]);
  Check.equal "the generator of what it uploaded"
    (W.generator ~engine:claude w.own)
    (match List.assoc_opt "generator" first.fields with
    | Some generator -> Yojson.Safe.from_string generator
    | None -> `Null);
  Check.equal "its completion"
    (generated whole first ~truncated:false)
    completion;
  Check.equal "its completion of the one cut off"
    (generated cut_off second ~truncated:true)
    truncated;
  expect_sequence served (all_but_last yielded @ yielded)

let yield_empty (w : W.t) =
  let job = yield_job w.own "anthropic-yield-empty" "yield-markdown" in
  Anthropic.script w.anthropic [ said "" ];
  let served = started w in
  let failure, error = failure_of (settled ~channel:"job:fail" served job) in
  expect_messages w.anthropic [ message claude.model "yield-markdown" ];
  Check.equal "its failure" (failed ~will_retry:true job) failure;
  says "its error" error (detail "empty" []);
  Check.equal "its uploads" (`Int 0) (`Int (List.length (uploads w)));
  said_nothing served [ "job:complete" ]

let withheld (w : W.t) =
  let mark = highlighting_job w.own "anthropic-withheld" in
  let generation =
    yield_job w.own "anthropic-yield-withheld" "yield-markdown"
  in
  (* Each refusal carries what would have been an answer: a passage to
     highlight, and the start of a document. *)
  Anthropic.script w.anthropic
    [
      said ~stop:"refusal" (proposing [ exact "the first program" ]);
      said ~stop:"refusal" "Charles Babbage designed the engine in";
    ];
  let served = started w in
  let mark_failure, mark_error =
    failure_of (settled ~channel:"job:fail" served mark)
  in
  let yield_failure, yield_error =
    failure_of (settled ~channel:"job:fail" served generation)
  in
  expect_messages w.anthropic
    [ highlighting (); message claude.model "yield-markdown" ];
  Check.equal "the mark job's failure"
    (failed ~failure_class:"withheld" ~will_retry:false mark)
    mark_failure;
  Check.equal "the yield job's failure"
    (failed ~failure_class:"withheld" ~will_retry:false generation)
    yield_failure;
  List.iter
    (fun error ->
      says "its error" error (Support.error "withheld" [ ("why", "") ]))
    [ mark_error; yield_error ];
  expect_commits_count w 0;
  Check.equal "its uploads" (`Int 0) (`Int (List.length (uploads w)));
  said_nothing served [ "job:complete" ]

let streamed (w : W.t) =
  let written =
    "Ada Lovelace published the first program in 1843 \u{2014} a method for \
     computing Bernoulli numbers. Charles Babbage designed the engine in \
     London. It was na\u{00ef}ve by later standards, and it was never built."
  in
  let whole = yield_job w.own "anthropic-yield-whole" "yield-21333" in
  let streamed = yield_job w.own "anthropic-yield-streamed" "yield-21334" in
  (* Each answer says the model was cut off: the one message in its
     stop_reason, the stream in its last event but one. *)
  Anthropic.script w.anthropic
    [ said ~stop:"max_tokens" written; said ~stop:"max_tokens" written ];
  let served = started w in
  let first = settled served whole in
  let second = settled served streamed in
  (* Neither carries the 0.7 a job that states no temperature takes, and the
     second asks for its answer as a stream of events. *)
  expect_messages w.anthropic
    [
      message ~stream:false claude.model "yield-21333";
      message ~stream:true claude.model "yield-21334";
    ];
  let one, other = two_uploads w in
  Check.equal "what it uploaded"
    (strs [ written; written ])
    (strs [ one.file; other.file ]);
  Check.equal "its completion of the one answer"
    (generated whole one ~truncated:true)
    first;
  Check.equal "its completion of the stream"
    (generated streamed other ~truncated:true)
    second;
  expect_sequence served (all_but_last yielded @ yielded)

(* Each attribute a metric carried, with every value it carried. *)
let carried (metric : Otlp.metric) =
  `Assoc
    (List.map
       (fun (key, values) ->
         (key, `List (List.map otlp_json (Dynarray.to_list values))))
       (Dynarray.to_list metric.attributes))

let counted otlp (w : W.t) =
  let job = highlighting_job w.own "anthropic-telemetry" in
  (* The generation is refused once and then answered: it is one generation
     all the same. *)
  Anthropic.script w.anthropic
    [
      refused 429 "rate_limit_error" over_its_rate;
      Anthropic.Answer
        {
          text = proposing [ exact "the first program" ];
          usage = { input = 4127; output = 571 };
          stop_reason = None;
        };
    ];
  let served = started ~env:(exporting otlp) w in
  ignore (settled served job : Yojson.Safe.t);
  (* The generation's span names who was asked, and for how much, at which
     knowledge base. *)
  let name = Protocol.span_name "inference:structured" [] in
  let span =
    Run.eventually "the span of the generation" 15.0 (fun () ->
        Dynarray.find_opt (fun (s : Otlp.span) -> s.name = name) otlp.spans)
  in
  Check.equal "what the span of the generation carries"
    (`Assoc
       [
         ("inference.provider", str "anthropic");
         ("inference.model", str claude.model);
         ("inference.max_tokens", `Float (float_of_int (anthropic_output ())));
         ("xynolog.kb", str w.own.world.kb.did);
       ])
    (`Assoc (List.map (fun (k, v) -> (k, otlp_json v)) span.attributes));
  (* The two counts the provider stated, and no other. *)
  let tokens =
    Run.eventually "the tokens of the generation, read and written" 15.0
      (fun () ->
        match
          Otlp.metric otlp (Protocol.metric_name "xynolog.inference.tokens")
        with
        | Some metric -> (
            match carried_under metric "inference.direction" with
            | Some directions
              when List.length (List.sort_uniq compare directions) = 2 ->
                Some metric
            | _ -> None)
        | None -> None)
  in
  let distinct key =
    `List (List.map otlp_json (List.sort_uniq compare (values_of tokens key)))
  in
  Check.equal "the providers its tokens are counted under"
    (strs [ "anthropic" ])
    (distinct "inference.provider");
  Check.equal "the models its tokens are counted under" (strs [ claude.model ])
    (distinct "inference.model");
  Check.equal "the directions its tokens are counted in"
    (strs [ "input"; "output" ])
    (distinct "inference.direction");
  Check.equal "the tokens it counted"
    (`List [ `Float 571.0; `Float 4127.0 ])
    (`List
       (List.map
          (fun v -> `Float v)
          (List.sort_uniq compare (Dynarray.to_list tokens.values))));
  (* What learned the model's limits is no generation: of the three
     requests, the two askings of the one generation are counted, as one. *)
  Check.equal "what it asked its provider"
    (strs
       [
         "GET /v1/models/" ^ claude.model;
         "POST /v1/messages";
         "POST /v1/messages";
       ])
    (strs (anthropic_requests w));
  let ended =
    `Assoc
      [
        ("inference.provider", strs [ "anthropic" ]);
        ("inference.model", strs [ claude.model ]);
        ("inference.outcome", strs [ "success" ]);
      ]
  in
  let metric what name =
    Run.eventually what 15.0 (fun () ->
        Otlp.metric otlp (Protocol.metric_name name))
  in
  let calls = metric "the count of generations" "xynolog.inference.calls" in
  Check.equal "what its count of generations carries" ended (carried calls);
  Check.equal "its count of generations"
    (`List [ `Float 1.0 ])
    (`List
       (List.map
          (fun v -> `Float v)
          (List.sort_uniq compare (Dynarray.to_list calls.values))));
  let durations =
    metric "the time the generation took" "xynolog.inference.duration"
  in
  Check.equal "what the time of its generation carries" ended
    (carried durations)

let cancelled (w : W.t) =
  let job = highlighting_job w.own "anthropic-cancelled" in
  let resource_id = job.resource_id in
  Anthropic.script w.anthropic [ Anthropic.Hold ];
  let served = started w in
  Anthropic.asked w.anthropic 1;
  cancel_requested w.own job;
  (* The worker has the cancellation, and still waits on its provider. *)
  Check.that "the cancellation did not end its request"
    (not (Anthropic.abandoned (generation w 0)));
  said_nothing served [ "job:cancel" ];
  Anthropic.release w.anthropic (fun _ ->
      said (proposing [ exact "the first program" ]));
  let cancel = settled ~channel:"job:cancel" served job in
  expect_messages w.anthropic [ highlighting () ];
  Check.that "its request was answered, not ended"
    (not (Anthropic.abandoned (generation w 0)));
  (* The piece it was on is committed, and the job is settled as
     cancelled. *)
  Check.equal "what it committed"
    (`List
       [
         `List [ first_program (W.generator ~engine:claude w.own) resource_id ];
       ])
    (batches served);
  Check.equal "its cancel" (cancel_of ~completed:[ "highlighting" ] job) cancel;
  said_nothing served [ "job:complete" ]

let stopped (w : W.t) =
  let job = highlighting_job w.own "anthropic-stopped" in
  (* The model never answers: the worker holds the job when it is told to
     stop. *)
  Anthropic.script w.anthropic [ Anthropic.Hold ];
  let served = started w in
  Anthropic.asked w.anthropic 1;
  Check.that "its request is under way"
    (not (Anthropic.abandoned (generation w 0)));
  stopped_holding served job;
  (* The provider saw the connection close with no answer sent. *)
  Run.eventually "the provider to see the request ended" 5.0 (fun () ->
      if Anthropic.abandoned (generation w 0) then Some () else None);
  Check.equal "the generations it asked for" (`Int 1)
    (`Int (Dynarray.length w.anthropic.generations))

let tests implementations =
  let otlp = lazy (Otlp.start ()) in
  in_world implementations "anthropic"
    ~category:[ "a worker on Anthropic" ]
    ([
       case
         "asks of its model once, for its ceilings, and then for each \
          generation, every request carrying its engine's key as a bearer \
          token"
         asks_once_then_each;
     ]
    @ List.map
        (fun (what, ceilings) ->
          case
            (Printf.sprintf "asks %s for the budget its two ceilings make" what)
            (budget ceilings))
        [
          (* What it writes is under the bound of a generation's time: all of
             it is asked for, and a piece is half of it. *)
          ("a model that writes little", (200_000, 4096, 2048));
          (* What is left of the window beside what it writes is less than
             half of that: a piece is what is left. *)
          ("a model whose window is mostly what it writes", (8192, 6000, 1925));
          (* What it writes is over the bound: what a generation has time for
             is asked for, and what is left of the window is cut by as
             much. *)
          ( "a model that writes more than a generation has time for",
            (30_000, 20_000, 5190) );
        ]
    @ [
        case
          "sends no temperature to any model: not the detection's 0, and not \
           the 0.2 a yield job asks for"
          no_temperature;
        case
          "fails a mark job as deterministic, having asked for no generation, \
           on a model that does not answer in a schema, and does a yield job \
           on it all the same"
          no_schema;
        case
          "takes four of the entity types of a linking job at once, in the \
           job's order, each an extraction and then a count"
          linking_four_at_once;
      ]
    @ List.map
        (fun ((status, _, _) as refusal) ->
          case
            (Printf.sprintf
               "asks three times in all for a generation refused with %d, and \
                then fails the job as transient"
               status)
            (refused_thrice refusal))
        [
          (429, "rate_limit_error", over_its_rate);
          (500, "api_error", "the provider failed");
          (529, "overloaded_error", "the provider is overloaded");
        ]
    @ [
        case
          "asks once for a generation refused as the request's own fault, and \
           fails the job as deterministic"
          refused_as_its_fault;
        case
          "completes a job whose generation is refused and then, asked again, \
           answered: having waited as long as the provider said to"
          refused_then_answered;
        case
          "asks once for a generation refused with a wait of over two minutes, \
           and fails the job as transient, saying the wait"
          told_to_wait;
        case
          "asks once for its model's ceilings when the refusal states a wait \
           of over two minutes, and fails the job as transient, saying the \
           wait"
          unlearned_told_to_wait;
        case
          "asks three times in all for a generation whose connection ends \
           unanswered, and then fails the job with no class"
          dropped;
      ]
    @ List.map
        (fun ((status, failure_class, _) as refusal) ->
          case
            (Printf.sprintf
               "fails a job, when its provider will not say its model's \
                ceilings and refuses with %d, as %s, having asked for no \
                generation; and asks again for the next job"
               status failure_class)
            (unlearned refusal))
        [
          (* A wrong key is refused again: asked once, and the job is not
             retried. *)
          (401, "deterministic", 1);
          (503, "transient", 3);
        ]
    @ [
        case
          "asks a second time, and no more, for an answer the model was cut \
           off in, and fails the job as deterministic"
          cut_off_twice;
        case
          "asks for a generation with the job's own length, no temperature and \
           no schema, uploads the answer, and says when the model was cut off"
          yield_own_length;
        case
          "fails a yield job whose model answers nothing, with no class, and \
           uploads nothing"
          yield_empty;
        case
          "fails a job as withheld, having asked once, when the provider \
           withholds its answer, and uses nothing the answer carried"
          withheld;
        case
          "asks for a generation of more than 21333 tokens as a stream, and \
           for one of 21333 as one answer; and makes the same of either, cut \
           off as each says it was"
          streamed;
        case
          "counts a generation once, however many times it was asked for, and \
           the tokens its provider reported for it, under its provider and its \
           model" (fun w -> counted (Lazy.force otlp) w);
        case
          "does not end a generation that is under way when its job is \
           cancelled: the request stays open until the model answers"
          cancelled;
        case
          "fails the job it holds and exits 0 on SIGTERM, and its request to \
           the provider ends with it"
          stopped;
      ])
