(* pdf.test.ts, named on_pdf beside the harness's Pdf: a mark job on a PDF
   (WORKER-SERVICE.md § What a job reads, § Anchoring, § Declines): the text
   is the Smelter's, asked of the gateway, and the worker reads no bytes; a
   span is anchored by where its text is on the page; and each answer the
   Smelter can give that is not a text ends the job its own way. *)

open Xynolog_conformance
open Support
open Case

(* The text the Smelter read out of the PDF, and each run of it on the page:
   two lines of one page. *)
let pdf_text =
  "Ada Lovelace published the first program. Charles Babbage designed the \
   engine."

let item ~start ~stop ~y ~width =
  `Assoc
    [
      ("start", `Int start);
      ("end", `Int stop);
      ("page", `Int 1);
      ("x", `Int 72);
      ("y", `Int y);
      ("width", `Int width);
      ("height", `Int 12);
    ]

let items =
  `List
    [
      (* "Ada Lovelace published the first program." and the space after it,
         on the upper line. *)
      item ~start:0 ~stop:42 ~y:700 ~width:420;
      (* "Charles Babbage designed the engine." on the lower. *)
      item ~start:42 ~stop:78 ~y:680 ~width:360;
    ]

let extracted text items =
  `Assoc
    [
      ("kind", str "extracted");
      ("method", str "pdf-text-layer");
      ("text", str text);
      ("items", items);
    ]

(* A highlighting job on a PDF, queued. [answer] is what the Archivist
   answers the gateway for its anchored text; a resource it is given no
   answer for, it does not know. *)
let pdf_job (w : W.t) name answer =
  let resource_id = "res-ws-" ^ name in
  W.describe w.own resource_id "application/pdf";
  Option.iter
    (fun answer ->
      Hashtbl.replace w.own.world.archivist.anchored_texts resource_id
        (fun () -> answer))
    answer;
  W.queued w.own "mark"
    (`Assoc
       [ ("resourceId", str resource_id); ("motivation", str "highlighting") ])

(* What a job on a PDF asks before it has its text: the description on the
   bus, the text of the gateway, and no bytes. *)
let reads_no_bytes (job : W.job) =
  [
    "emit job:claim";
    "emit job:start";
    "emit browse:resource-requested";
    "GET /resources/" ^ job.resource_id ^ "/anchored-text";
  ]

let anchored (w : W.t) =
  let job = pdf_job w "pdf" (Some (extracted pdf_text items)) in
  let resource_id = job.resource_id in
  Ollama.script w.ollama
    [
      proposed
        [ exact "Ada Lovelace"; exact "the first program. Charles Babbage" ];
    ];
  let served = W.start w in
  let completion = settled served job in
  expect_sequence served
    (reads_no_bytes job
    @ [
        "emit mark:commit";
        "emit job:checkpoint";
        "emit job:complete";
        "emit job:claim";
      ]);
  Check.equal "the anchored text it read" (strs [ resource_id ])
    (strs (W.anchored_text_reads w.own));
  Check.equal "the bytes the Archivist was asked for" (`List [])
    (strs
       (List.filter_map
          (fun (c : Archivist_standin.call) ->
            if Re.execp (Re.compile (Re.str "/content")) c.path then Some c.path
            else None)
          (Dynarray.to_list w.own.world.archivist.calls)));
  (* The model is asked about the Smelter's text. *)
  (match Dynarray.to_list w.ollama.generations with
  | [ (body, _) ] ->
      says "its prompt"
        (text_of (member "prompt" body))
        ("---\n" ^ pdf_text ^ "\n---");
      Check.equal "the format it asked its model for"
        (answer_format "highlighting")
        (member "format" body)
  | generations ->
      failwith
        (Printf.sprintf "it asked its model %d times, not once"
           (List.length generations)));
  let fragment value =
    `Assoc
      [
        ("type", str "FragmentSelector");
        ("conformsTo", str "http://tools.ietf.org/rfc/rfc3778");
        ("value", str value);
      ]
  in
  let annotation id selector =
    `Assoc
      [
        ("@context", str "http://www.w3.org/ns/anno.jsonld");
        ("type", str "Annotation");
        ("id", str id);
        ("motivation", str "highlighting");
        ("generator", W.generator w.own);
        ( "target",
          `Assoc
            [
              ("type", str "SpecificResource");
              ("source", str resource_id);
              ("selector", `List selector);
            ] );
      ]
  in
  Check.equal "what it committed"
    (`List
       [
         `List
           [
             (* Twelve of the upper line's forty-two characters, from its left
                edge: two sevenths of its width. *)
             annotation "QtseQcczJ22XiMGSSc8ub"
               [
                 fragment "page=1&viewrect=72,700,120,12";
                 `Assoc
                   [
                     ("type", str "TextQuoteSelector");
                     ("exact", str "Ada Lovelace");
                     ( "suffix",
                       str
                         " published the first program. Charles Babbage \
                          designed the engine" );
                   ];
               ];
             (* A span over both lines has a rectangle for each, the upper
                first: the end of one, and the start of the other. *)
             annotation "WAvvU5B-Pz7qbEVUMgqe4"
               [
                 fragment "page=1&viewrect=302,700,190,12";
                 fragment "page=1&viewrect=72,680,150,12";
                 `Assoc
                   [
                     ("type", str "TextQuoteSelector");
                     ("exact", str "the first program. Charles Babbage");
                     ("prefix", str "Ada Lovelace published ");
                     ("suffix", str " designed the engine.");
                   ];
               ];
           ];
       ])
    (batches served);
  Check.equal "its completion"
    (completed job [ ("found", `Int 2); ("persisted", `Int 2) ])
    completion

let declined answer reason (w : W.t) =
  let job = pdf_job w "pdf-declined" (Some answer) in
  let served = W.start w in
  let completion = settled served job in
  Check.equal "its completion"
    (identified job
       [
         ("result", `Assoc [ ("declined", `Bool true); ("reason", str reason) ]);
       ])
    completion;
  expect_sequence served
    (reads_no_bytes job @ [ "emit job:complete"; "emit job:claim" ]);
  expect_generations_count w 0;
  expect_commits_count w 0

let not_yet (w : W.t) =
  let job =
    pdf_job w "pdf-not-yet" (Some (`Assoc [ ("kind", str "not-yet") ]))
  in
  let served = W.start w in
  let failure, said = failure_of (settled ~channel:"job:fail" served job) in
  Check.equal "its failure" (failed ~will_retry:true job) failure;
  says "its error" said
    (error "not-settled" [ ("resourceId", job.resource_id) ]);
  expect_sequence served
    (reads_no_bytes job @ [ "emit job:fail"; "emit job:claim" ]);
  (* It is asked once: the worker does not wait for the text within the
     attempt. *)
  Check.equal "how often it read the anchored text" (`Int 1)
    (`Int (List.length (W.anchored_text_reads w.own)));
  expect_generations_count w 0

let absent answer kind (w : W.t) =
  let job = pdf_job w "pdf-absent" answer in
  let served = W.start w in
  let failure, said = failure_of (settled ~channel:"job:fail" served job) in
  Check.equal "its failure"
    (failed ~failure_class:"deterministic" ~will_retry:false job)
    failure;
  says "its error" said
    (error "anchored-declined"
       [
         ("job", mark_named "highlighting");
         ("resourceId", job.resource_id);
         ("declined", kind);
       ]);
  expect_generations_count w 0

let declined_kind declined =
  `Assoc [ ("kind", str "declined"); ("declined", str declined) ]

let tests implementations =
  in_world implementations "pdf" ~category:[ "a job on a PDF" ]
    ([
       case
         "asks the gateway for the Smelter's text, reads no bytes, and anchors \
          each span by its place on the page"
         anchored;
     ]
    @ List.map
        (fun (what, answer, reason) ->
          case
            ("completes, declined, a job on a PDF the Smelter says is " ^ what)
            (declined answer reason))
        [
          ("encrypted", declined_kind "encrypted", "encrypted");
          ("corrupt", declined_kind "corrupt", "corrupt");
          ( "without a text layer",
            declined_kind "no-text-layer",
            "no-text-layer" );
          ("too large", declined_kind "too-large", "too-large");
          ("read, and empty", extracted "  \n " (`List []), "empty");
        ]
    @ [
        case
          "fails, with no class, a job on a PDF whose text the Smelter has not \
           settled yet: a later attempt may find it"
          not_yet;
      ]
    @ List.map
        (fun (what, answer, kind) ->
          case
            ("fails as deterministic a job on a PDF the Smelter " ^ what)
            (absent answer kind))
        [
          ("has no map of", Some (`Assoc [ ("kind", str "no-map") ]), "no-map");
          ("does not know", None, "unknown");
        ])
