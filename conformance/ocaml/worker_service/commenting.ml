(* commenting.test.ts: a mark job of motivation commenting, end to end
   (WORKER-SERVICE.md § Detection, § The five kinds of mark job): asked with
   instructions, a tone, a density and both of its languages. *)

open Xynolog_conformance
open Support
open Case

let model = (List.hd W.ollama_engines).model

(* A comment's body: what the model wrote, as plain text, in the language
   the job asked for. *)
let comment value =
  `List
    [
      `Assoc
        [
          ("type", str "TextualBody");
          ("value", str value);
          ("purpose", str "commenting");
          ("format", str "text/plain");
          ("language", str "fr");
        ];
    ]

let end_to_end (w : W.t) =
  let job = case_job w.own "commenting" in
  let resource_id = job.resource_id in
  Ollama.script w.ollama
    [
      answer
        (Yojson.Safe.to_string
           (`List
              [
                `Assoc
                  [
                    ("exact", str "Ada Lovelace");
                    ("prefix", str "");
                    ("suffix", str " published");
                    ( "comment",
                      str "Math\u{00e9}maticienne anglaise (1815\u{2013}1852)."
                    );
                  ];
                `Assoc
                  [
                    ("exact", str "Charles Babbage");
                    ("comment", str "Inventeur de la machine analytique.");
                  ];
                (* A comment that says nothing is no proposal: it is neither
                   committed nor counted. *)
                `Assoc
                  [ ("exact", str "Bernoulli numbers"); ("comment", str "   ") ];
              ]));
    ];
  let served = W.start w in
  let completion = settled served job in
  expect_sequence served
    [
      "emit job:claim";
      "emit job:start";
      "emit browse:resource-requested";
      "GET /resources/" ^ resource_id;
      "emit mark:commit";
      "emit job:checkpoint";
      "emit job:complete";
      "emit job:claim";
    ];
  expect_generations w.ollama [ generation model "commenting" ];
  let generator = W.generator w.own in
  Check.equal "what it committed"
    (`List
       [
         `Assoc
           [
             ("resourceId", str resource_id);
             ("jobId", str job.id);
             ( "annotations",
               `List
                 [
                   (* At the very start of the text there is nothing before
                      it, and its selector states no prefix. *)
                   text_annotation generator resource_id "commenting"
                     "xF0gdAhKtOfhw1sOLoMFJ" ~start:0 ~stop:12
                     ~suffix:
                       " published the first program in 1843 \u{2014} a method \
                        for computing Bernoulli"
                     ~body:
                       (comment
                          "Math\u{00e9}maticienne anglaise (1815\u{2013}1852).")
                     "Ada Lovelace";
                   text_annotation generator resource_id "commenting"
                     "k-1xPhvsdrjKL9y6bdx2f" ~start:119 ~stop:134
                     ~prefix:
                       "method for computing Bernoulli numbers on the \
                        Analytical Engine.\n\n"
                     ~suffix:
                       " designed the engine in London, but it was never \
                        built. Lovelace"
                     ~body:(comment "Inventeur de la machine analytique.")
                     "Charles Babbage";
                 ] );
           ];
       ])
    (committed served);
  Check.equal "its checkpoint"
    (`List
       [
         checkpoint job
           [
             ( "commenting",
               cursor ~next:(code_points text) ~size:2659 ~found:2 ~emitted:2
                 ~errors:0 );
           ];
       ])
    (checkpoints served);
  (* What the job was asked with is echoed on every report: the
     instructions, the tone, the density. *)
  let extra =
    [
      ( "requestParams",
        `List
          [
            `Assoc
              [
                ("label", str "instructions");
                ("value", str "Explain who each person was.");
              ];
            `Assoc [ ("label", str "tone"); ("value", str "scholarly") ];
            `Assoc [ ("label", str "density"); ("value", str "4") ];
          ] );
    ]
  in
  expect_progress served job
    [
      report ~extra job 10 (code "loading");
      report ~extra job 30 (code "analyzing");
      report ~extra job 60
        (code ~members:[ ("count", `Int 2) ] "creating-annotations");
      report ~extra job 100
        (code
           ~members:[ ("count", `Int 2); ("motivation", str "commenting") ]
           "complete-created");
    ];
  Check.equal "its completion"
    (identified job
       [
         ("result", `Assoc [ ("found", `Int 2); ("persisted", `Int 2) ]);
         ("durability", str "acknowledged");
       ])
    completion

let tests implementations =
  in_world implementations "commenting" ~category:[ "a commenting job" ]
    [
      case
        "asks as its instructions, tone, density and languages say, and \
         commits each comment as the body of its span"
        end_to_end;
    ]
