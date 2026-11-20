(* chunks.test.ts: a text too long for one request (WORKER-SERVICE.md
   § Pieces, § Committing, and where a job stands): cut into pieces that
   overlap, each asked for, committed and checkpointed in turn, with a span
   two pieces both see recorded once. *)

open Xynolog_conformance
open Support
open Case

let model = (List.hd W.ollama_engines).model

(* The size, in tokens, every piece is cut at: the provider reports no
   usage, so the size never moves. *)
let chunk_size = 311

(* An answer of [spans], as JSON. *)
let spans items = answer (Yojson.Safe.to_string (`List items))
let marker n = `Assoc [ ("exact", str (Printf.sprintf "marker %d" n)) ]

(* An answer that finds each marker of [markers]. *)
let found markers = spans (List.map marker markers)

let piece_by_piece (w : W.t) =
  w.ollama.show <- Ollama.Context small_context_length;
  let job = mark_job ~about:long_text w.own "chunks" (params "chunks-1") in
  let resource_id = job.resource_id in
  Ollama.script w.ollama
    [
      (* The first piece ends where the second paragraph does. Marker 205 is
         in the overlap, so the second piece sees it too. *)
      found [ 103; 205 ];
      found [ 205; 304 ];
      spans
        [
          marker 502;
          `Assoc
            [
              ( "exact",
                str "marker 999 beside the Rh\u{00f4}ne was never recorded" );
            ];
        ];
      found [];
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
  Check.equal "what it asked of its model's limits"
    (`List [ `Assoc [ ("model", str model) ] ])
    (`List (Dynarray.to_list w.ollama.shows));
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
        ("highlighting", cursor ~next ~size:chunk_size ~found ~emitted ~errors);
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
  let creating n =
    report job 60 (code ~members:[ ("count", `Int n) ] "creating-annotations")
  in
  expect_progress served job
    [
      report job 10 (code "loading");
      report job 30 (code "analyzing");
      (* After each piece: what the job has made so far, and then how far
         through the text the next piece starts. *)
      creating 2;
      report job 35 (code "analyzing");
      creating 3;
      report job 43 (code "analyzing");
      creating 4;
      report job 50 (code "analyzing");
      creating 4;
      report job 100
        (code
           ~members:[ ("count", `Int 4); ("motivation", str "highlighting") ]
           "complete-created");
    ];
  (* Six proposed, the repeat among them; four recorded; one that is nowhere
     in the text. *)
  Check.equal "its completion"
    (identified job
       [
         ( "result",
           `Assoc
             [ ("found", `Int 6); ("persisted", `Int 4); ("errors", `Int 1) ] );
         ("durability", str "acknowledged");
       ])
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
