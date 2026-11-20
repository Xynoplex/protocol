(* anchoring.test.ts: where a span a model proposes is in the text
   (WORKER-SERVICE.md § Anchoring): a quote the text does not have character
   for character is looked for again, more loosely, as
   specs/src/annotations/reconcile-cases.json states; what is found is
   committed as the text's own words at the text's offsets, and what is not
   is counted and not committed. *)

open Xynolog_conformance
open Support
open Case

(* The code points of [text] from [start] up to [stop], as a selector counts
   them. *)
let between text ~start ~stop =
  let rec byte_of index at =
    if index = 0 then at
    else
      byte_of (index - 1)
        (at + Uchar.utf_decode_length (String.get_utf_8_uchar text at))
  in
  let first = byte_of start 0 in
  String.sub text first (byte_of (stop - start) first - first)

let loosely (w : W.t) =
  let job = mark_job w.own "anchoring" (params "highlighting") in
  let resource_id = job.resource_id in
  Ollama.script w.ollama
    [
      answer
        (Yojson.Safe.to_string
           (`List
              [
                (* Seventeen code points, one letter from `Bernoulli numbers`:
                   fewer than twenty, so no edit is allowed, and it is
                   nowhere. *)
                `Assoc [ ("exact", str "Bernoulli nunbers") ];
                (* Twenty-one code points, one letter from `the Analytical
                   Engine`: one edit is allowed, and that is the one stretch
                   one edit away. *)
                `Assoc [ ("exact", str "the Analytical Engime") ];
                (* The text's words but for their letter case. *)
                `Assoc [ ("exact", str "CHARLES BABBAGE") ];
                (* Only white space, and nothing at all: neither is a span. *)
                `Assoc [ ("exact", str " \n") ];
                `Assoc [ ("exact", str "") ];
              ]));
    ];
  let served = W.start w in
  let completion = settled served job in
  Check.equal "the generations its model was asked for" (`Int 1)
    (`Int (Dynarray.length w.ollama.generations));
  (* What is committed is the text's own words, where the text has them:
     never the model's spelling. *)
  Check.equal "the text from 95 to 116"
    (str "the Analytical Engine")
    (str (between text ~start:95 ~stop:116));
  Check.equal "the text from 119 to 134" (str "Charles Babbage")
    (str (between text ~start:119 ~stop:134));
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
               cursor ~next:(code_points text) ~size:2641 ~found:5 ~emitted:2
                 ~errors:3 );
           ];
       ])
    (checkpoints served);
  Check.equal "its completion"
    (identified job
       [
         ( "result",
           `Assoc
             [ ("found", `Int 5); ("persisted", `Int 2); ("errors", `Int 3) ] );
         ("durability", str "acknowledged");
       ])
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
