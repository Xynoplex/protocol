(* How a failure becomes a job:fail (WORKER-SERVICE.md
   § Failures): a provider that refuses, a connection that ends, an answer
   that cannot be read, an answer cut off, and a commit the record refuses;
   the class each is given, whether the job says it will be retried, and the
   checkpoint it carries. *)

open Xynolog_conformance
open Support
open Case

(* Run a highlighting job over the short text against [replies], and return
   it, its failure, and the worker that failed it. *)
let run_failing ?metadata (w : W.t) name replies =
  let job = highlighting_job ?metadata w.own name in
  Ollama.script w.ollama replies;
  let served = W.start w in
  let failure = settled ~channel:"job:fail" served job in
  (* A job that failed before it made anything says it loaded and began, and
     no more. *)
  expect_progress served job
    [ report job 10 (code "loading"); report job 30 (code "analyzing") ];
  said_nothing served [ "job:complete" ];
  (job, failure, served)

(* A failure of [failure_class], of a job that made nothing: its identity,
   its class, and whether it will be retried. Its error, which is text. *)
let classed job failure failure_class will_retry =
  let rest, error = failure_of failure in
  Check.equal "its failure" (failed ~failure_class ~will_retry job) rest;
  error

(* A failure of no class, of a job that made nothing: its identity, and
   whether it will be retried. Its error, which is text. *)
let unclassed job failure will_retry =
  let rest, error = failure_of failure in
  Check.equal "its failure" (failed ~will_retry job) rest;
  error

let overloaded = Ollama.Fails (500, {|{"error":"overloaded"}|})

let refused_transient (w : W.t) =
  let job, failure, served =
    run_failing w "refused"
      [ Ollama.Fails (500, {|{"error":"the model is still loading"}|}) ]
  in
  let error = classed job failure "transient" true in
  (* The error says what the provider said: its status and its body. *)
  says "its error" error "500";
  says "its error" error "the model is still loading";
  (* One request: a refusal is not asked again within the attempt. *)
  expect_generations_count w 1;
  expect_sequence served (made_nothing job)

let refused_as_its_fault (w : W.t) =
  let job, failure, _ =
    run_failing w "refused-400"
      [ Ollama.Fails (400, {|{"error":"invalid options"}|}) ]
  in
  let error = classed job failure "deterministic" false in
  says "its error" error "400";
  says "its error" error "invalid options";
  (* Asked once: a request refused as its own fault is not sent again. *)
  expect_generations_count w 1

let spent (w : W.t) =
  let job, failure, _ =
    run_failing w "spent" [ overloaded ]
      ~metadata:[ ("retryCount", `Int 3); ("maxRetries", `Int 3) ]
  in
  Check.equal "its attempt" (`Int 4) (`Int (job.retry_count + 1));
  ignore (classed job failure "transient" false : string)

let dropped (w : W.t) =
  let job, failure, _ = run_failing w "dropped" [ Ollama.Drop ] in
  ignore (unclassed job failure true : string);
  expect_generations_count w 1

let limits_refused (status, failure_class, will_retry) (w : W.t) =
  w.ollama.show <- Ollama.Refuses status;
  let job, failure, _ =
    run_failing w ("no-limits-" ^ string_of_int status) []
  in
  let error = classed job failure failure_class will_retry in
  (* The error says what was being learned, and the status it was refused
     with. What the provider said is the stand-in's own, and left out: the
     rest of the error stands before it. *)
  says "its error" error
    (Support.error "ollama-limits-refused"
       [ ("model", model); ("status", string_of_int status); ("said", "") ]);
  expect_generations_count w 0

let no_limits_stated (w : W.t) =
  (* An answer, and no refusal: there is no status to class it by. *)
  w.ollama.show <- Ollama.Context 0;
  let job, failure, _ = run_failing w "no-limits-stated" [] in
  says "its error"
    (unclassed job failure true)
    (Support.error "no-context-length" [ ("model", model) ]);
  expect_generations_count w 0

let limits_dropped (w : W.t) =
  w.ollama.show <- Ollama.Drops;
  let job, failure, _ = run_failing w "no-limits-dropped" [] in
  ignore (unclassed job failure true : string);
  Check.equal "how often it asked of its model's limits" (`Int 1)
    (`Int (Dynarray.length w.ollama.shows));
  expect_generations_count w 0

let window_too_small (w : W.t) =
  (* 461 tokens, less the 267 of the prompt around the text, leave 194: a
     third is 64, one overlap, and a piece of it reads nothing new. *)
  w.ollama.show <- Ollama.Context 461;
  let job, failure, served = run_failing w "window-too-small" [] in
  let error = classed job failure "deterministic" false in
  says "its error" error "461";
  (* It learned its model's window, and asked the model nothing. *)
  expect_shows w [ model ];
  expect_generations_count w 0;
  expect_sequence served (made_nothing job)

let unreadable response (w : W.t) =
  let job, failure, _ =
    run_failing w "unreadable"
      [ answer response; answer response; answer response ]
  in
  ignore (unclassed job failure true : string);
  (* A text that fits one piece is still asked about in halves of its own
     size, two levels down: 98 tokens, its first paragraph of 30, and the
     first 15 of that. *)
  expect_generations w.ollama
    [
      generation model "highlighting";
      generation model "unreadable-half";
      generation model "unreadable-quarter";
    ];
  expect_commits_count w 0

let first_program = proposing [ exact "the first program" ]

let cut_off_twice (w : W.t) =
  let job = highlighting_job w.own "truncated" in
  Ollama.script w.ollama [ cut_off first_program; cut_off first_program ];
  let served = W.start w in
  let failure = settled ~channel:"job:fail" served job in
  (* Asked a second time, the same: once, and no more. *)
  let request = generation model "highlighting" in
  expect_generations w.ollama [ request; request ];
  ignore (classed job failure "deterministic" false : string);
  (* What a cut-off answer did carry is not committed. *)
  expect_commits_count w 0

let cut_off_once (w : W.t) =
  let job = highlighting_job w.own "truncated-once" in
  Ollama.script w.ollama
    [
      cut_off first_program;
      proposed [ exact "the first program"; exact "Charles Babbage" ];
    ];
  let served = W.start w in
  let completion = settled served job in
  expect_generations_count w 2;
  Check.equal "its completion"
    (completed job [ ("found", `Int 2); ("persisted", `Int 2) ])
    completion

let failed_partway (w : W.t) =
  w.ollama.show <- Ollama.Context small_context_length;
  let job = highlighting_job ~about:long_text w.own "failed-partway" in
  Ollama.script w.ollama [ finding [ 103; 205 ]; overloaded ];
  let served = W.start w in
  let rest, error = failure_of (settled ~channel:"job:fail" served job) in
  expect_commits_count w 1;
  (* The first piece is on the record, and a later attempt begins after
     it. *)
  Check.equal "its failure"
    (identified job
       [
         ( "unitCursors",
           `Assoc
             [
               ( "highlighting",
                 cursor ~next:704 ~size:small_highlighting_piece ~found:2
                   ~emitted:2 ~errors:0 );
             ] );
         ("failureClass", str "transient");
         ("willRetry", `Bool true);
       ])
    rest;
  says "its error" error "500";
  expect_sequence served
    [
      "emit job:claim";
      "emit job:start";
      "emit browse:resource-requested";
      "GET /resources/" ^ job.resource_id;
      "emit mark:commit";
      "emit job:checkpoint";
      "emit job:fail";
      "emit job:claim";
    ]

let failed_after_a_unit (w : W.t) =
  let job = mark_job w.own "failed-unit" (linking [ "Person"; "Place" ]) in
  Ollama.script w.ollama
    [ proposed [ mention "Ada Lovelace" "Person" ]; counting 1; overloaded ];
  let served = W.start w in
  let rest, error = failure_of (settled ~channel:"job:fail" served job) in
  expect_commits_count w 1;
  Check.equal "its failure"
    (identified job
       [
         ("completedUnits", strs [ "Person" ]);
         ( "unitCursors",
           `Assoc
             [
               ( "Person",
                 cursor ~next:(code_points text) ~size:linking_piece ~found:1
                   ~emitted:1 ~errors:0 );
             ] );
         ("failureClass", str "transient");
         ("willRetry", `Bool true);
       ])
    rest;
  says "its error" error "500"

let commit_refused (w : W.t) =
  let reason = "the record refuses this batch" in
  let job = highlighting_job w.own "commit-refused" in
  Ollama.script w.ollama [ answer first_program ];
  w.own.hooks <- { w.own.hooks with commit = Some (fun _ -> Some reason) };
  let served = W.start w in
  let failure = settled ~channel:"job:fail" served job in
  Check.equal "its failure"
    (identified job [ ("error", str reason); ("willRetry", `Bool true) ])
    failure;
  (* The batch was not established, so its unit's cursor was not said. *)
  said_nothing served [ "job:checkpoint" ]

let tests implementations =
  in_world implementations "failures" ~category:[ "a job that fails" ]
    ([
       case
         "fails as transient, and says it will be retried, when its provider \
          refuses the request with a status the job rule retries"
         refused_transient;
       case
         "fails as deterministic, and says it will not be retried, when its \
          provider refuses the request as the request's own fault"
         refused_as_its_fault;
       case
         "says a failure will not be retried when the job has no attempt left"
         spent;
       case
         "fails, with no class, when the connection to its provider ends \
          unanswered"
         dropped;
     ]
    @ List.map
        (fun ((status, _, _) as refusal) ->
          case
            (Printf.sprintf
               "fails in the class of the status, when its provider refuses \
                with %d to say its model's limits, and asks for no generation"
               status)
            (limits_refused refusal))
        [
          (* A wrong key, for one: refused again however often it is
             asked. *)
          (401, "deterministic", false);
          (503, "transient", true);
        ]
    @ [
        case
          "fails, with no class, when its provider answers and states no \
           limits for its model, and asks for no generation"
          no_limits_stated;
        case
          "fails, with no class, when the connection to its provider ends \
           unanswered as it asks its model's limits, and asks for no \
           generation"
          limits_dropped;
        case
          "fails as deterministic, having asked for no generation, when its \
           model's window leaves a piece 64 tokens or fewer"
          window_too_small;
      ]
    @ List.map
        (fun (what, response) ->
          case
            (Printf.sprintf
               "fails, with no class, on an answer that %s, having asked about \
                the text, its first half, and that half's first half"
               what)
            (unreadable response))
        [
          ("is not JSON", "I found three passages worth highlighting.");
          ( "is JSON and not an array",
            {|{"highlights":[{"exact":"the first program"}]}|} );
          ("has nothing in it", "");
        ]
    @ [
        case
          "fails as deterministic, and says it will not be retried, when an \
           answer over the smallest piece is cut off twice"
          cut_off_twice;
        case
          "completes when the second asking of a cut-off piece is answered \
           whole"
          cut_off_once;
        case "carries on its failure the cursor of what it had committed"
          failed_partway;
        case
          "carries on its failure the units it had finished, each with the \
           cursor it ended at"
          failed_after_a_unit;
        case
          "fails with the record's reason when the record refuses a commit, \
           and says nothing of how its commits were established"
          commit_refused;
      ])
