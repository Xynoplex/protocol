(* code-points.test.ts: what a job counts in a text with characters outside
   the Basic Multilingual Plane (WORKER-SERVICE.md § The request, § Pieces,
   § Anchoring, § The annotation, § Committing, and where a job stands,
   § Progress): Unicode code points. Such a character is one code point,
   where a string of UTF-16 code units has two for it, so every number a job
   states of such a text (a piece's size, a cursor, a selector's offsets, how
   far through the text it stands, the tokens of a prompt) tells the one
   count from the other. *)

open Xynolog_conformance
open Support
open Case

(* Characters outside the Basic Multilingual Plane. *)
let compass = "\u{1F9ED}"

(* A mathematical letter. *)
let script_c = "\u{1D49E}"
let tally_one = "\u{1D377}"
let tally_five = "\u{1D378}"
let scroll = "\u{1F4DC}"

(* A mathematical letter. *)
let bold_b = "\u{1D435}"
let tools = "\u{1F6E0}"

(* The length of [s] in UTF-16 code units, as a JavaScript string counts
   it. *)
let utf_16_length s =
  List.fold_left (fun n (_, _, units) -> n + units) 0 (Anthropic.code_points s)

(* A text too long for one request of a model with a small window: a
   paragraph that ends in a tally of sixty marks, and eight paragraphs of six
   sentences, each naming a marker no other names, with a compass in every
   sentence. 4,140 code points, of which 110 are outside the Basic
   Multilingual Plane: 4,250 UTF-16 code units. *)
let survey =
  Printf.sprintf
    "Field notes of the Rh\u{00f4}ne survey %s, kept by the clerk %s of the \
     second party. "
    compass script_c
  ^ Printf.sprintf "The clerk kept a tally of the chains measured: %s."
      (String.concat " "
         (List.init 12 (fun _ ->
              String.concat "" (List.init 4 (Fun.const tally_one)) ^ tally_five)))
  ^ "\n\n"
  ^ String.concat "\n\n"
      (List.init 8 (fun p ->
           String.concat " "
             (List.init 6 (fun s ->
                  Printf.sprintf
                    "Paragraph %d, sentence %d: the survey party %s recorded \
                     marker %d beside the Rh\u{00f4}ne."
                    (p + 1) (s + 1) compass
                    (((p + 1) * 100) + s + 1)))))
  ^ "\n"

(* Two paragraphs, 317 code points, of which three are outside the Basic
   Multilingual Plane: 320 UTF-16 code units. *)
let engine =
  Printf.sprintf
    "%s Ada Lovelace published the first program in 1843 \u{2014} a method for \
     computing the Bernoulli numbers %s on the Analytical Engine.\n"
    scroll bold_b
  ^ "\n"
  ^ Printf.sprintf
      "Charles Babbage %s designed the engine in London, but it was never \
       built. Lovelace argued that the engine could manipulate symbols as well \
       as numbers; nobody tested that claim for a century.\n"
      tools

(* What stands either side of a marker's sentence in [survey]. *)
let before paragraph sentence =
  Printf.sprintf
    "the Rh\u{00f4}ne. Paragraph %d, sentence %d: the survey party %s recorded "
    paragraph sentence compass

let after paragraph sentence =
  Printf.sprintf
    " beside the Rh\u{00f4}ne. Paragraph %d, sentence %d: the survey party %s \
     recorded"
    paragraph sentence compass

let by_code_points (w : W.t) =
  w.ollama.show <- Ollama.Context small_context_length;
  let job =
    mark_job ~about:survey w.own "code-points" (params "code-points-1")
  in
  let resource_id = job.resource_id in
  let marker_205 =
    Printf.sprintf "survey party %s recorded marker 205" compass
  in
  Ollama.script w.ollama
    [
      (* The first piece ends where the second paragraph of entries does.
         Marker 205 is in the overlap, so the second piece sees it too. *)
      proposed [ exact ("the clerk " ^ script_c); marker 103; exact marker_205 ];
      proposed
        [
          exact marker_205;
          `Assoc
            [
              ("exact", str "marker 304");
              ("prefix", str (Printf.sprintf "party %s recorded " compass));
              ("suffix", str " beside");
            ];
        ];
      proposed
        [
          marker 502;
          exact
            (Printf.sprintf
               "marker 999 beside the Rh\u{00f4}ne %s was never recorded"
               compass);
        ];
      finding [ 806 ];
    ];
  let served = W.start w in
  let completion = settled served job in
  Check.equal "the text's code points" (`Int 4140) (`Int (code_points survey));
  Check.equal "the text's UTF-16 code units" (`Int 4250)
    (`Int (utf_16_length survey));
  (* Four pieces, each of at most 311 tokens, which is 1,244 code points,
     ended at a paragraph's end. Cut by UTF-16 code units the same text is
     eight pieces: its first window would end short of the second
     paragraph's end. The tokens of a prompt are its code points over
     four. *)
  expect_generations w.ollama
    (List.map
       (generation ~context_length:small_context_length model)
       [ "code-points-1"; "code-points-2"; "code-points-3"; "code-points-4" ]);
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
      "emit mark:commit";
      "emit job:checkpoint";
      "emit job:complete";
      "emit job:claim";
    ];
  (* A selector's offsets count code points from the start of the text, and
     so do the 64 of a prefix and of a suffix. An id is of those offsets. *)
  let highlight id ~start ~stop ~prefix ~suffix exact =
    text_annotation (W.generator w.own) resource_id "highlighting" id ~start
      ~stop ~prefix ~suffix exact
  in
  Check.equal "what it committed"
    (`List
       [
         `List
           [
             (* Before it: one character outside the plane, of the 43 code
                points there are. *)
             highlight "HrGCdvWg6RJ-CgSBGphSo" ~start:43 ~stop:54
               ~prefix:
                 (Printf.sprintf
                    "Field notes of the Rh\u{00f4}ne survey %s, kept by "
                    compass)
               ~suffix:
                 " of the second party. The clerk kept a tally of the chains \
                  measured"
               ("the clerk " ^ script_c);
             (* Before it: sixty-five. A count of UTF-16 code units would put
                it at 479. *)
             highlight "7Gw0hABVWzTOpG0l8DZKO" ~start:414 ~stop:424
               ~prefix:(before 1 3) ~suffix:(after 1 4) "marker 103";
             highlight "LDef15sL7YpN2Qsjx5gS5" ~start:1047 ~stop:1081
               ~prefix:
                 "recorded marker 204 beside the Rh\u{00f4}ne. Paragraph 2, \
                  sentence 5: the "
               ~suffix:(after 2 6) marker_205;
           ];
         (* Marker 205 was committed with the first piece: it is not
            committed again. *)
         `List
           [
             highlight "p63nAU8defkhsuTVHrY6K" ~start:1482 ~stop:1492
               ~prefix:(before 3 4) ~suffix:(after 3 5) "marker 304";
           ];
         `List
           [
             highlight "humTQV3RLd7uou_gxWvu9" ~start:2304 ~stop:2314
               ~prefix:(before 5 2) ~suffix:(after 5 3) "marker 502";
           ];
         (* The last sentence of the text: what follows it is the text's
            final line end. *)
         `List
           [
             highlight "EBOSgHXfROpcBzjqbZcNm" ~start:4111 ~stop:4121
               ~prefix:(before 8 6) ~suffix:" beside the Rh\u{00f4}ne.\n"
               "marker 806";
           ];
       ])
    (batches served);
  Check.equal "the text from code point 4111 to 4121" (str "marker 806")
    (str (code_point_slice survey 4111 4121));
  (* The cursor is an offset: where the next piece starts, in code points,
     and after the last piece the text's length in them. *)
  let at_piece ~next ~found ~emitted ~errors =
    checkpoint job
      [
        ( "highlighting",
          cursor ~next ~size:small_highlighting_piece ~found ~emitted ~errors );
      ]
  in
  Check.equal "its checkpoints"
    (`List
       [
         at_piece ~next:925 ~found:3 ~emitted:3 ~errors:0;
         at_piece ~next:1911 ~found:5 ~emitted:4 ~errors:0;
         at_piece ~next:2897 ~found:7 ~emitted:5 ~errors:1;
         at_piece ~next:4140 ~found:8 ~emitted:6 ~errors:1;
       ])
    (checkpoints served);
  expect_progress served job
    [
      report job 10 (code "loading");
      report job 30 (code "analyzing");
      (* How far through the text the next piece starts: the cursor over the
         text's length, both in code points. Over 4,250 the last two would be
         43 and 50. *)
      creating job 3;
      report job 37 (code "analyzing");
      creating job 4;
      report job 44 (code "analyzing");
      creating job 5;
      report job 51 (code "analyzing");
      creating job 6;
      complete_created job "highlighting" 6;
    ];
  (* Eight proposed, the repeat among them; six recorded; one that is
     nowhere in the text. *)
  Check.equal "its completion"
    (completed job
       [ ("found", `Int 8); ("persisted", `Int 6); ("errors", `Int 1) ])
    completion

let comments_by_code_points (w : W.t) =
  let job = case_job ~about:engine w.own "code-points-commenting" in
  let resource_id = job.resource_id in
  Ollama.script w.ollama
    [
      proposed
        [
          `Assoc
            [
              ("exact", str "Charles Babbage");
              ("comment", str "Designed the Analytical Engine.");
            ];
          (* One code point long. *)
          `Assoc
            [
              ("exact", str bold_b);
              ("prefix", str "numbers ");
              ("comment", str "The Bernoulli numbers.");
            ];
          `Assoc
            [
              ("exact", str "London");
              ("prefix", str "engine in ");
              ("comment", str "Where Babbage worked.");
            ];
          (* Nowhere in the text. *)
          `Assoc
            [
              ( "exact",
                str (Printf.sprintf "a difference engine %s of brass" tools) );
              ("comment", str "Nowhere in the text.");
            ];
        ];
    ];
  let served = W.start w in
  let completion = settled served job in
  Check.equal "the text's code points" (`Int 317) (`Int (code_points engine));
  Check.equal "the text's UTF-16 code units" (`Int 320)
    (`Int (utf_16_length engine));
  (* The prompt is 402 tokens, its 1,607 code points over four rounded up,
     and the window is sized from that. Its 1,610 UTF-16 code units would be
     one token more, and num_ctx 5794. *)
  expect_generations w.ollama [ generation model "code-points-commenting" ];
  expect_shows w [ model ];
  let commented id ~start ~stop ~prefix ~suffix exact said =
    text_annotation (W.generator w.own) resource_id "commenting" id ~start ~stop
      ~prefix ~suffix
      ~body:(comment ~language:"en" said)
      exact
  in
  Check.equal "what it committed"
    (`List
       [
         `List
           [
             (* Two characters outside the plane stand before it: a string's
                own position for it is 129. *)
             commented "4J8akSbrbEyFdTzVXKTDl" ~start:127 ~stop:142
               ~prefix:
                 (Printf.sprintf
                    "for computing the Bernoulli numbers %s on the Analytical \
                     Engine.\n\n"
                    bold_b)
               ~suffix:
                 (Printf.sprintf
                    " %s designed the engine in London, but it was never \
                     built. Lovelace"
                    tools)
               "Charles Babbage" "Designed the Analytical Engine.";
             (* The span is the character itself: one code point, from 98 to
                99. *)
             commented "3O5sj9Qhk99Y9-ZgOt9kI" ~start:98 ~stop:99
               ~prefix:
                 "first program in 1843 \u{2014} a method for computing the \
                  Bernoulli numbers "
               ~suffix:
                 (Printf.sprintf
                    " on the Analytical Engine.\n\n\
                     Charles Babbage %s designed the engine"
                    tools)
               bold_b "The Bernoulli numbers.";
             commented "lGrJCeVCNoP5O3pJU2XQ8" ~start:168 ~stop:174
               ~prefix:
                 (Printf.sprintf
                    "the Analytical Engine.\n\n\
                     Charles Babbage %s designed the engine in "
                    tools)
               ~suffix:
                 ", but it was never built. Lovelace argued that the engine \
                  could manipulate"
               "London" "Where Babbage worked.";
           ];
       ])
    (batches served);
  (* The unit's cursor is the text's length in code points. *)
  Check.equal "its checkpoints"
    (`List
       [
         checkpoint job
           [
             ( "commenting",
               cursor ~next:317 ~size:2623 ~found:4 ~emitted:3 ~errors:1 );
           ];
       ])
    (checkpoints served);
  expect_progress served job
    [
      report job 10 (code "loading");
      report job 30 (code "analyzing");
      creating job 3;
      complete_created job "commenting" 3;
    ];
  Check.equal "its completion"
    (completed job
       [ ("found", `Int 4); ("persisted", `Int 3); ("errors", `Int 1) ])
    completion

let tests implementations =
  in_world implementations "code-points"
    ~category:
      [
        "a job over a text with characters outside the Basic Multilingual Plane";
      ]
    [
      case
        "cuts, asks, anchors, checkpoints and reports by the text's code points"
        by_code_points;
      case
        "anchors a comment after such a character, and on one, at a count of \
         code points"
        comments_by_code_points;
    ]
