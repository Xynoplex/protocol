(* A cancellation of the job a worker holds (WORKER-SERVICE.md
   § Cancellation): every job stops at its next stopping place, a mark job
   after the piece it is on and a yield job before it uploads; it says
   job:cancel with the units it finished, and reports no completion.

   A cancellation does not end a generation that is under way. So most cases
   here hold the provider's answer, ask for the cancellation, wait until the
   worker has had it, and only then let the provider answer: what the worker
   does next is what it does with a cancellation in hand. *)

open Xynolog_conformance
open Support
open Case

let source = "res-ws-cancel-source"

(* What a mark job that stopped after one piece asked of the gateway, in
   order: its one batch, its checkpoints, and its cancel. [held] is a case
   that held the provider's answer until the worker had the cancellation:
   the worker's answer to the request that told the case so falls there. *)
let stopped_after_one_piece ?(held = true) resource_id checkpoints =
  [
    "emit job:claim";
    "emit job:start";
    "emit browse:resource-requested";
    "GET /resources/" ^ resource_id;
  ]
  @ (if held then [ "emit job:limits-result" ] else [])
  @ [ "emit mark:commit" ]
  @ List.init checkpoints (fun _ -> "emit job:checkpoint")
  @ [ "emit job:cancel"; "emit job:claim" ]

let first_not_abandoned (w : W.t) =
  Check.that "the cancellation did not end the request it waits on"
    (not (snd (Dynarray.get w.ollama.generations 0)).abandoned)

(* Hold the first generation with the model, start the worker, and once it
   has asked, ask for [job] to be cancelled and wait until it has had it. *)
let cancelled_while_held (w : W.t) job =
  let served = W.start w in
  Ollama.asked w.ollama 1;
  cancel_requested w.own job;
  served

let reports_say served (job : W.job) code =
  List.filter
    (fun p -> member "code" (member "message" (member "progress" p)) = str code)
    (W.progress served job.id)

(* The cancellation is asked for while the first batch is being committed,
   before the record acknowledges it. *)
let cancel_on_first_commit (w : W.t) (job : W.job) =
  let asked = ref false in
  w.own.hooks <-
    {
      w.own.hooks with
      commit =
        Some
          (fun _ ->
            if not !asked then (
              asked := true;
              W.emit w.own "job:cancel-requested"
                (`Assoc [ ("jobId", str job.id) ]));
            None);
    }

let highlighting_partway (w : W.t) =
  w.ollama.show <- Ollama.Context small_context_length;
  let job = highlighting_job ~about:long_text w.own "cancel-highlighting" in
  let resource_id = job.resource_id in
  (* The text is four pieces. The first is held with the model while the
     cancellation arrives. *)
  Ollama.script w.ollama [ Hold ];
  let served = cancelled_while_held w job in
  (* The cancellation did not end the request: the worker still waits on it,
     and has said nothing of the job since. *)
  first_not_abandoned w;
  said_nothing served [ "job:cancel" ];
  Ollama.release w.ollama (proposing [ marker 103; marker 205 ]);
  let cancel = settled ~channel:"job:cancel" served job in
  (* One generation: the second piece was never asked about. *)
  expect_generations_count w 1;
  let generator = W.generator w.own in
  Check.equal "what it committed"
    (`List
       [
         `List
           [
             marker_highlight generator resource_id "VetijqYBmNe9ReSC41dJL" 103
               211;
             marker_highlight generator resource_id "WKYaFWYQ1X6WqqdM94o2-" 205
               852;
           ];
       ])
    (batches served);
  Check.equal "its checkpoints"
    (`List
       [
         checkpoint job
           [
             ( "highlighting",
               cursor ~next:704 ~size:small_highlighting_piece ~found:2
                 ~emitted:2 ~errors:0 );
           ];
       ])
    (checkpoints served);
  expect_sequence served (stopped_after_one_piece resource_id 1);
  (* It was partway through its one unit: the cancel names none, and states
     no attempt. *)
  Check.equal "its cancel" (cancel_of job) cancel;
  (* What the piece made is reported; where the next piece would start, and
     a completion, are not. *)
  expect_progress served job
    [
      report job 10 (code "loading");
      report job 30 (code "analyzing");
      creating job 2;
    ];
  said_nothing served [ "job:complete"; "job:fail" ]

let commenting_partway (w : W.t) =
  w.ollama.show <- Ollama.Context small_context_length;
  let job =
    mark_job ~about:long_text w.own "cancel-commenting" (motivated "commenting")
  in
  let resource_id = job.resource_id in
  Ollama.script w.ollama [ Hold ];
  let served = cancelled_while_held w job in
  Ollama.release w.ollama
    (proposing
       [
         `Assoc
           [
             ("exact", str "marker 103");
             ("comment", str "The third marker of the first paragraph.");
           ];
       ]);
  let cancel = settled ~channel:"job:cancel" served job in
  expect_generations_count w 1;
  Check.equal "what it committed"
    (`List
       [
         `List
           [
             text_annotation (W.generator w.own) resource_id "commenting"
               "C16Xzw7e77tX-n7CHJ4ey" ~start:211 ~stop:221
               ~prefix:
                 "beside the Rh\u{00f4}ne. Paragraph 1, sentence 3: the survey \
                  party recorded "
               ~suffix:
                 " beside the Rh\u{00f4}ne. Paragraph 1, sentence 4: the \
                  survey party recorded"
               ~body:
                 (comment ~language:"en"
                    "The third marker of the first paragraph.")
               "marker 103";
           ];
       ])
    (batches served);
  Check.equal "its checkpoints"
    (`List
       [
         checkpoint job
           [
             ( "commenting",
               cursor ~next:704 ~size:292 ~found:1 ~emitted:1 ~errors:0 );
           ];
       ])
    (checkpoints served);
  expect_sequence served (stopped_after_one_piece resource_id 1);
  Check.equal "its cancel" (cancel_of job) cancel;
  expect_progress served job
    [
      report job 10 (code "loading");
      report job 30 (code "analyzing");
      creating job 1;
    ];
  said_nothing served [ "job:complete" ]

let tagging_partway (w : W.t) =
  let job =
    mark_job w.own "cancel-tagging"
      (tagging (argument ()) [ "Claim"; "Evidence" ])
  in
  let resource_id = job.resource_id in
  (* The text is one piece for each category. The first category's is held
     with the model. *)
  Ollama.script w.ollama [ Hold ];
  let served = cancelled_while_held w job in
  Ollama.release w.ollama (proposing [ exact "London was wrong." ]);
  let cancel = settled ~channel:"job:cancel" served job in
  expect_generations_count w 1;
  Check.equal "what it committed"
    (`List
       [
         `List
           [
             text_annotation (W.generator w.own) resource_id "tagging"
               "uQhBVKeO0xajjHhBxV3fv" ~start:373 ~stop:390
               ~prefix:
                 "The program was na\u{00ef}ve by later standards, and London \
                  ignored it. "
               ~suffix:"\n" ~body:(tagged "Claim") "London was wrong.";
           ];
       ])
    (batches served);
  Check.equal "its checkpoints"
    (`List
       [
         checkpoint job
           [
             ( "Claim",
               cursor ~next:(code_points text) ~size:claim_piece ~found:1
                 ~emitted:1 ~errors:0 );
           ];
       ])
    (checkpoints served);
  expect_sequence served (stopped_after_one_piece resource_id 1);
  (* The category it finished is named; the one it never began is not. *)
  Check.equal "its cancel" (cancel_of ~completed:[ "Claim" ] job) cancel;
  expect_progress served job
    [
      report job 10 (code "loading");
      analyzing_tags job 30;
      analyzing_tags job 30 ~extra:(on_category "Claim" 0 []);
      creating_tags job 1;
    ];
  said_nothing served [ "job:complete" ]

let linking_partway (w : W.t) =
  w.ollama.show <- Ollama.Context small_context_length;
  let job =
    mark_job ~about:long_text w.own "cancel-linking-partway"
      (linking [ "Person"; "Place" ])
  in
  let resource_id = job.resource_id in
  (* The text is four pieces for each type. The first piece's extraction is
     held with the model; its count is answered when asked. *)
  Ollama.script w.ollama [ Hold; counting 1 ];
  let served = cancelled_while_held w job in
  Ollama.release w.ollama (proposing [ mention "marker 103" "Person" ]);
  let cancel = settled ~channel:"job:cancel" served job in
  (* The piece's two generations, its extraction and its count, and no more:
     not the type's next piece, and not the next type. *)
  expect_generations_count w 2;
  Check.equal "what it committed"
    (`List
       [
         `List
           [
             text_annotation (W.generator w.own) resource_id "linking"
               "UiVoMJKkMBGb-pq3TP5e1" ~start:211 ~stop:221
               ~prefix:
                 "beside the Rh\u{00f4}ne. Paragraph 1, sentence 3: the survey \
                  party recorded "
               ~suffix:
                 " beside the Rh\u{00f4}ne. Paragraph 1, sentence 4: the \
                  survey party recorded"
               ~body:(entity "Person") "marker 103";
           ];
       ])
    (batches served);
  (* The type is partway: its cursor is said, and it is not among the
     finished. *)
  Check.equal "its checkpoints"
    (`List
       [
         checkpoint job
           [
             ("Person", cursor ~next:704 ~size:328 ~found:1 ~emitted:1 ~errors:0);
           ];
       ])
    (checkpoints served);
  expect_sequence served (stopped_after_one_piece resource_id 1);
  Check.equal "its cancel" (cancel_of job) cancel;
  let types = [ "Person"; "Place" ] in
  (* The type begun, and its mentions counted: both said while the piece was
     being asked about. Nothing after its checkpoint. *)
  expect_progress served job
    [
      report job 10 (code "loading") ~extra:[ echoed_entity_types types ];
      detecting ~types job "Person" 20 ~processed:0 ~found:0 ~emitted:0 [];
      detecting ~types job "Person" 20 ~processed:0 ~found:0 ~expected:1
        ~emitted:0 [];
    ];
  said_nothing served [ "job:complete" ]

let linking_last (w : W.t) =
  let job = mark_job w.own "cancel-linking-last" (linking [ "Person" ]) in
  let resource_id = job.resource_id in
  Ollama.script w.ollama [ Hold; counting 1 ];
  let served = cancelled_while_held w job in
  Ollama.release w.ollama (proposing [ mention "Ada Lovelace" "Person" ]);
  let cancel = settled ~channel:"job:cancel" served job in
  expect_generations_count w 2;
  Check.equal "what it committed"
    (`List
       [
         `List
           [
             text_annotation (W.generator w.own) resource_id "linking"
               "fmelsDjChhC9dkXYrCyEE" ~start:0 ~stop:12
               ~suffix:
                 " published the first program in 1843 \u{2014} a method for \
                  computing Bernoulli"
               ~body:(entity "Person") "Ada Lovelace";
           ];
       ])
    (batches served);
  (* The piece was the type's last: its cursor is said, and then the type
     among the finished with its cursor still, as for any type. *)
  let person =
    ( "Person",
      cursor ~next:(code_points text) ~size:linking_piece ~found:1 ~emitted:1
        ~errors:0 )
  in
  Check.equal "its checkpoints"
    (`List
       [
         checkpoint job [ person ];
         checkpoint job ~completed:[ "Person" ] [ person ];
       ])
    (checkpoints served);
  expect_sequence served (stopped_after_one_piece resource_id 2);
  (* Nothing was left to do but settle, and it is settled as cancelled all
     the same. *)
  Check.equal "its cancel" (cancel_of ~completed:[ "Person" ] job) cancel;
  Check.equal "its reports of a completion" (`List [])
    (`List (reports_say served job "complete-created"));
  said_nothing served [ "job:complete" ]

let linking_between (w : W.t) =
  let job = mark_job w.own "cancel" (linking [ "Person"; "Place" ]) in
  let resource_id = job.resource_id in
  Ollama.script w.ollama
    [ proposed [ mention "Ada Lovelace" "Person" ]; counting 1 ];
  cancel_on_first_commit w job;
  let served = W.start w in
  let cancel = settled ~channel:"job:cancel" served job in
  (* The second type was never asked for. *)
  expect_generations_count w 2;
  (* What the first type found is on the record, and the type is
     checkpointed as finished, with the cursor it ended at. *)
  Check.equal "the sizes of its batches" (`List [ `Int 1 ]) (sizes_of_batches w);
  let person =
    ( "Person",
      cursor ~next:(code_points text) ~size:linking_piece ~found:1 ~emitted:1
        ~errors:0 )
  in
  Check.equal "its checkpoints"
    (`List
       [
         checkpoint job [ person ];
         checkpoint job ~completed:[ "Person" ] [ person ];
       ])
    (checkpoints served);
  expect_sequence served (stopped_after_one_piece ~held:false resource_id 2);
  (* A cancel names the job and what it finished, and states no attempt. *)
  Check.equal "its cancel" (cancel_of ~completed:[ "Person" ] job) cancel;
  Check.equal "its reports of a completion" (`List [])
    (`List (reports_say served job "complete-created"));
  said_nothing served [ "job:complete"; "job:fail" ]

let linking_resumed (w : W.t) =
  (* An earlier attempt finished Person. This one is cancelled as the last
     batch of Place is committed. *)
  let person =
    ( "Person",
      cursor ~next:(code_points text) ~size:linking_piece ~found:2 ~emitted:2
        ~errors:0 )
  in
  let job =
    mark_job w.own "cancel-resumed"
      ~metadata:
        [
          ("retryCount", `Int 1);
          ("completedUnits", strs [ "Person" ]);
          ("unitCursors", `Assoc [ person ]);
        ]
      (linking [ "Person"; "Place" ])
  in
  let resource_id = job.resource_id in
  Ollama.script w.ollama
    [
      proposed [ mention "London" "Place" ~prefix:"engine in " ~suffix:", but" ];
      counting 1;
    ];
  cancel_on_first_commit w job;
  let served = W.start w in
  let cancel = settled ~channel:"job:cancel" served job in
  expect_generations_count w 2;
  Check.equal "the sizes of its batches" (`List [ `Int 1 ]) (sizes_of_batches w);
  let place =
    ( "Place",
      cursor ~next:(code_points text) ~size:linking_piece ~found:1 ~emitted:1
        ~errors:0 )
  in
  Check.equal "its checkpoints"
    (`List
       [
         checkpoint job [ person; place ];
         checkpoint job ~completed:[ "Place" ] [ person; place ];
       ])
    (checkpoints served);
  expect_sequence served (stopped_after_one_piece ~held:false resource_id 2);
  Check.equal "its cancel" (cancel_of ~completed:[ "Place" ] job) cancel;
  said_nothing served [ "job:complete" ]

let highlighting_last (w : W.t) =
  let job = highlighting_job w.own "cancel-highlighting-last" in
  let resource_id = job.resource_id in
  Ollama.script w.ollama [ Hold ];
  let served = cancelled_while_held w job in
  Ollama.release w.ollama (proposing [ exact "the first program" ]);
  let cancel = settled ~channel:"job:cancel" served job in
  Check.equal "what it committed"
    (`List
       [
         `List
           [
             text_annotation (W.generator w.own) resource_id "highlighting"
               "luRRYETclpP6f4x3jG5pZ" ~start:23 ~stop:40
               ~prefix:"Ada Lovelace published "
               ~suffix:
                 " in 1843 \u{2014} a method for computing Bernoulli numbers \
                  on the Analytical"
               "the first program";
           ];
       ])
    (batches served);
  Check.equal "its checkpoints"
    (`List
       [
         checkpoint job
           [
             ( "highlighting",
               cursor ~next:(code_points text) ~size:highlighting_piece ~found:1
                 ~emitted:1 ~errors:0 );
           ];
       ])
    (checkpoints served);
  expect_sequence served (stopped_after_one_piece resource_id 1);
  Check.equal "its cancel" (cancel_of ~completed:[ "highlighting" ] job) cancel;
  expect_progress served job
    [
      report job 10 (code "loading");
      report job 30 (code "analyzing");
      creating job 1;
    ];
  said_nothing served [ "job:complete" ]

let before_first_piece (w : W.t) =
  let job = highlighting_job w.own "cancel-before" in
  let resource_id = job.resource_id in
  (* The cancellation is asked for while the job reads its resource's
     description, before that is answered. *)
  w.own.hooks <-
    {
      w.own.hooks with
      described =
        Some
          (fun _ ->
            W.emit w.own "job:cancel-requested"
              (`Assoc [ ("jobId", str job.id) ]));
    };
  let served = W.start w in
  let cancel = settled ~channel:"job:cancel" served job in
  expect_shows w [];
  expect_generations_count w 0;
  expect_commits_count w 0;
  expect_sequence served
    [
      "emit job:claim";
      "emit job:start";
      "emit browse:resource-requested";
      "GET /resources/" ^ resource_id;
      "emit job:cancel";
      "emit job:claim";
    ];
  Check.equal "its cancel" (cancel_of job) cancel;
  expect_progress served job []

let generated_text = "The engine was designed in London and never built."

let yield_before_upload (w : W.t) =
  let job = titled_yield w.own ~source "cancel-yield" in
  Ollama.script w.ollama [ Hold ];
  let served = cancelled_while_held w job in
  first_not_abandoned w;
  Ollama.release w.ollama generated_text;
  let cancel = settled ~channel:"job:cancel" served job in
  (* Nothing reached the gateway but the job's own lifecycle: no upload, and
     no commit. *)
  expect_generations_count w 1;
  Check.equal "its uploads" (`Int 0) (`Int (List.length (uploads w)));
  expect_commits_count w 0;
  (* (The limits it answered are the request that told this case the
     cancellation had arrived.) *)
  expect_sequence served
    [
      "emit job:claim";
      "emit job:start";
      "emit job:limits-result";
      "emit job:cancel";
      "emit job:claim";
    ];
  Check.equal "its cancel" (cancel_of job) cancel;
  (* It said it was generating, and nothing after its model answered. *)
  expect_progress served job [ report job 5 (code "generating-resource") ];
  said_nothing served [ "job:complete"; "job:fail" ]

let yield_after_upload (w : W.t) =
  let job = titled_yield w.own ~source "cancel-yield-late" in
  Ollama.script w.ollama [ answer generated_text ];
  (* The cancellation is asked for while the link from the source is being
     committed: the upload has been sent. *)
  w.own.hooks <-
    {
      w.own.hooks with
      commit =
        Some
          (fun _ ->
            W.emit w.own "job:cancel-requested"
              (`Assoc [ ("jobId", str job.id) ]);
            None);
    };
  let served = W.start w in
  let completion = settled served job in
  expect_sequence served yielded;
  Check.equal "its completion"
    (generated job (upload_of w) ~truncated:false)
    completion;
  said_nothing served [ "job:cancel" ];
  expect_progress served job
    [
      report job 5 (code "generating-resource");
      report job 95 (code "creating-resource");
      report job 100
        (code ~members:[ ("truncated", `Bool false) ] "complete-generated");
    ]

let tests implementations =
  in_world implementations "cancel"
    ~category:[ "a job that is cancelled" ]
    [
      case
        "stops a highlighting job after the piece it is on: that piece is \
         committed and checkpointed, no other is asked about, and no \
         completion is reported"
        highlighting_partway;
      case "stops a commenting job after the piece it is on" commenting_partway;
      case
        "stops a tagging job partway through its categories: the category it \
         is on is finished and named, and the next is never asked about"
        tagging_partway;
      case
        "stops a linking job partway through an entity type of several pieces: \
         after the piece, with the type not named as finished"
        linking_partway;
      case
        "settles as cancelled a linking job cancelled on the last piece of its \
         last entity type, with every type named"
        linking_last;
      case
        "stops a linking job between entity types when the cancellation \
         arrives as a type's last batch is committed"
        linking_between;
      case
        "names on its cancel the entity type this attempt finished, and not \
         one the record it was claimed with already names"
        linking_resumed;
      case
        "settles as cancelled a highlighting job cancelled on its only piece, \
         with its one unit named"
        highlighting_last;
      case
        "asks its model nothing, and reports nothing, for a mark job cancelled \
         before its first piece"
        before_first_piece;
      case
        "stops a yield job before it uploads, when the cancellation has \
         arrived by the time its model has answered"
        yield_before_upload;
      case
        "runs a yield job to its end when the cancellation arrives after its \
         upload"
        yield_after_upload;
    ]
