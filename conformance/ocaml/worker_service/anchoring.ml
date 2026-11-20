(* Where a span a model proposes is in the text
   (WORKER-SERVICE.md § Anchoring): a quote the text does not have character
   for character is looked for again, more loosely, as
   specs/src/annotations/reconcile-cases.json states; what is found is
   committed as the text's own words at the text's offsets, and what is not
   is counted and not committed. *)

open Xynolog_conformance
open Support
open Case

let loosely (w : W.t) =
  let job = mark_job w.own "anchoring" (params "highlighting") in
  let resource_id = job.resource_id in
  Ollama.script w.ollama
    [
      proposed
        [
          (* Seventeen code points, one letter from `Bernoulli numbers`:
             fewer than twenty, so no edit is allowed, and it is nowhere. *)
          exact "Bernoulli nunbers";
          (* Twenty-one code points, one letter from `the Analytical Engine`:
             one edit is allowed, and that is the one stretch one edit
             away. *)
          exact "the Analytical Engime";
          (* The text's words but for their letter case. *)
          exact "CHARLES BABBAGE";
          (* Only white space, and nothing at all: neither is a span. *)
          exact " \n";
          exact "";
        ];
    ];
  let served = W.start w in
  let completion = settled served job in
  expect_generations_count w 1;
  (* What is committed is the text's own words, where the text has them:
     never the model's spelling. *)
  Check.equal "the text from 95 to 116"
    (str "the Analytical Engine")
    (str (code_point_slice text 95 116));
  Check.equal "the text from 119 to 134" (str "Charles Babbage")
    (str (code_point_slice text 119 134));
  let generator = W.generator w.own in
  Check.equal "what it committed"
    (`List
       [
         `List
           [
             text_annotation generator resource_id "highlighting"
               "pNILL5ANCREjMJG0MZlFy" ~start:95 ~stop:116
               ~prefix:
                 "first program in 1843 \u{2014} a method for computing \
                  Bernoulli numbers on "
               ~suffix:
                 ".\n\n\
                  Charles Babbage designed the engine in London, but it was \
                  never"
               "the Analytical Engine";
             text_annotation generator resource_id "highlighting"
               "ft_LL8CCi7gl7eQg5lnrA" ~start:119 ~stop:134
               ~prefix:
                 "method for computing Bernoulli numbers on the Analytical \
                  Engine.\n\n"
               ~suffix:
                 " designed the engine in London, but it was never built. \
                  Lovelace"
               "Charles Babbage";
           ];
       ])
    (batches served);
  (* Five proposed, two recorded, three that are no span: the one no edit is
     allowed, the white space, and the empty one. *)
  Check.equal "its checkpoints"
    (`List
       [
         checkpoint job
           [
             ( "highlighting",
               cursor ~next:(code_points text) ~size:highlighting_piece ~found:5
                 ~emitted:2 ~errors:3 );
           ];
       ])
    (checkpoints served);
  Check.equal "its completion"
    (completed job
       [ ("found", `Int 5); ("persisted", `Int 2); ("errors", `Int 3) ])
    completion

let tests implementations =
  in_world implementations "anchoring"
    ~category:[ "a span that is not in the text character for character" ]
    [
      case
        "commits a quote one edit from the text or apart from it by letter \
         case, as the text has it, and counts one the rule allows no edit"
        loosely;
    ]
