(* chunks.test.ts: a text too long for one request (WORKER-SERVICE.md
   § Pieces, § Committing, and where a job stands): cut into pieces that
   overlap, each asked for, committed and checkpointed in turn, with a span
   two pieces both see recorded once. *)

open Xynolog_conformance
open Support
open Case

let piece_by_piece (w : W.t) =
  w.ollama.show <- Ollama.Context small_context_length;
  let job = mark_job ~about:long_text w.own "chunks" (params "chunks-1") in
  let resource_id = job.resource_id in
  Ollama.script w.ollama
    [
      (* The first piece ends where the second paragraph does. Marker 205 is
         in the overlap, so the second piece sees it too. *)
      finding [ 103; 205 ];
      finding [ 205; 304 ];
      proposed
        [
          marker 502;
          exact "marker 999 beside the Rh\u{00f4}ne was never recorded";
        ];
      finding [];
    ];
  let served = W.start w in
  let completion = settled served job in
  Check.equal "the length of the text" (`Int 3847)
    (`Int (code_points long_text));
  (* Each request is given the model's whole window, and asks to be
     answered in less. *)
  expect_generations w.ollama
    (List.map
       (fun name -> generation ~context_length:small_context_length model name)
       [ "chunks-1"; "chunks-2"; "chunks-3"; "chunks-4" ]);
  expect_shows w [ model ];
  (* Each piece's spans are on the record, and its cursor said, before the
     next piece is asked for. *)
  expect_sequence served
    [
      "emit job:claim";
      "emit job:start";
      "emit browse:resource-requested";
      "GET /resources/" ^ resource_id;
      "emit mark:commit";
      "emit job:checkpoint";
      "emit mark:commit";
      "emit job:checkpoint";
      "emit mark:commit";
      "emit job:checkpoint";
      "emit job:checkpoint";
      "emit job:complete";
      "emit job:claim";
    ];
  let generator = W.generator w.own in
  Check.equal "what it committed"
    (`List
       [
         `List
           [
             marker_highlight generator resource_id "Odt5ohBuSHGwbCQ-QEybG" 103
               211;
             marker_highlight generator resource_id "IILAtGKw8fjiHrcvYGCeC" 205
               852;
           ];
         (* Marker 205 was committed with the first piece: it is not
            committed again. *)
         `List
           [
             marker_highlight generator resource_id "CM_HlyAOTLyS0O_23RZzq" 304
               1253;
           ];
         `List
           [
             marker_highlight generator resource_id "K3T1OrrNtETEnBM5baLBC" 502
               2055;
           ];
       ])
    (batches served);
  (* The cursor is where the next piece starts: the end of this one, less the
     overlap. The counts are the job's so far. *)
  let at next ~found ~emitted ~errors =
    checkpoint job
      [
        ( "highlighting",
          cursor ~next ~size:small_highlighting_piece ~found ~emitted ~errors );
      ]
  in
  Check.equal "its checkpoints"
    (`List
       [
         at 704 ~found:2 ~emitted:2 ~errors:0;
         at 1666 ~found:4 ~emitted:3 ~errors:0;
         at 2628 ~found:6 ~emitted:4 ~errors:1;
         at (code_points long_text) ~found:6 ~emitted:4 ~errors:1;
       ])
    (checkpoints served);
  expect_progress served job
    [
      report job 10 (code "loading");
      report job 30 (code "analyzing");
      (* After each piece: what the job has made so far, and then how far
         through the text the next piece starts. *)
      creating job 2;
      report job 35 (code "analyzing");
      creating job 3;
      report job 43 (code "analyzing");
      creating job 4;
      report job 50 (code "analyzing");
      creating job 4;
      complete_created job "highlighting" 4;
    ];
  (* Six proposed, the repeat among them; four recorded; one that is nowhere
     in the text. *)
  Check.equal "its completion"
    (completed job
       [ ("found", `Int 6); ("persisted", `Int 4); ("errors", `Int 1) ])
    completion

let tests implementations =
  in_world implementations "chunks"
    ~category:[ "a job over a long text" ]
    [
      case
        "cuts the text into overlapping pieces at paragraph ends, and asks, \
         commits and checkpoints piece by piece"
        piece_by_piece;
    ]
