(* yield.test.ts: a yield job to a text format, end to end (WORKER-SERVICE.md
   § Generation): the one request it makes of its model, the resource it
   uploads, the link it commits from the source to what it made, the
   citations it commits on what it made, and its completion. *)

open Xynolog_conformance
open Support
open Case

(* The resource the cases' jobs are about, as worker/prompt-cases.json names
   it. *)
let source =
  text_of
    (Check.at (params "yield-markdown")
       [ "context"; "focus"; "resource"; "@id" ])

let model = (List.hd W.ollama_engines).model

(* What the first case asks for: the job whose prompt is kept as
   yield-markdown. *)
let markdown_job (w : W.t) = yield_job w.own ~source (params "yield-markdown")

let upload_of (w : W.t) =
  match uploads w with
  | [ upload ] -> upload
  | uploads ->
      failwith
        (Printf.sprintf "it uploaded %d resources, not one"
           (List.length uploads))

(* An upload's fields but [apart], and each of [apart] read as JSON. *)
let fields_of (upload : Archivist_standin.upload) apart =
  ( `Assoc
      (List.filter_map
         (fun (k, v) -> if List.mem k apart then None else Some (k, str v))
         upload.fields),
    List.map
      (fun name ->
        match List.assoc_opt name upload.fields with
        | Some value -> Yojson.Safe.from_string value
        | None -> failwith ("its upload has no field " ^ name))
      apart )

let shows_of (w : W.t) = `List (Dynarray.to_list w.ollama.shows)

(* The ids the cases below work out are worked out by the table's rule: the
   suite's own reckoning is held to every case of it. *)
let id_cases () =
  let cases = (Protocol.tables ()).annotations_id_cases.cases in
  Check.that "the table has cases" (cases <> []);
  List.iter
    (fun (c : Xynolog_protocol.Tables.Annotations_id_cases.cases_item) ->
      Check.equal
        (Check.shown
           (`Assoc
              [
                ("resourceId", str c.resource_id);
                ( "motivation",
                  str
                    (Xynolog_protocol.Tables.Annotations_id_cases
                     .motivation_value c.motivation) );
                ("anchor", str c.anchor);
              ]))
        (str c.id)
        (str
           (annotation_id_of ?body:c.body c.resource_id
              (Xynolog_protocol.Tables.Annotations_id_cases.motivation_value
                 c.motivation)
              (str c.anchor))))
    cases

let end_to_end (w : W.t) =
  let job = markdown_job w in
  Ollama.script w.ollama
    [
      (* Two claims whose words the mark wraps, one a sentence and one in the
         middle of its sentence; a bare mark, where its source's name reads;
         and one cited to a resource the context never showed. *)
      answer
        "[[res-ws-yield-source: Ada Lovelace published the first program in \
         1843]]. As [[res-ws-yield-source]] tells it, Charles Babbage designed \
         the engine in London. It was [[res-ws-yield-source: never built]]. \
         [[res-ws-not-in-the-context]]";
    ];
  let served = W.start w in
  let completion = settled served job in
  (* One generation, with the job's own temperature and length, and no
     format: the answer is the document. *)
  Check.equal "what it asked of its model's limits"
    (`List [ `Assoc [ ("model", str model) ] ])
    (shows_of w);
  expect_generations w.ollama [ generation model "yield-markdown" ];
  (* It reads nothing of the source: what it knows of it is in the job. *)
  expect_sequence served
    [
      "emit job:claim";
      "emit job:start";
      "POST /resources";
      "emit mark:commit";
      "emit mark:commit";
      "emit job:complete";
      "emit job:claim";
    ];
  Check.equal "the descriptions it read" (`List [])
    (strs (Dynarray.to_list w.own.descriptor_reads));
  (* The upload: the text without its marks, its bare mark the name the
     context gave its source, where the job said to store it, citing the job
     and its source. *)
  let content =
    "Ada Lovelace published the first program in 1843. As Notes on the engine \
     tells it, Charles Babbage designed the engine in London. It was never \
     built."
  in
  let upload = upload_of w in
  Check.equal "what it uploaded" (str content) (str upload.file);
  let fields, read = fields_of upload [ "generator"; "entityTypes" ] in
  Check.equal "its upload's fields"
    (`Assoc
       [
         ("name", str "The Analytical Engine");
         ("format", str "text/markdown");
         ("storageUri", str "file://generated/analytical-engine.md");
         ("language", str "en");
         ("sourceResourceId", str source);
         ("generationPrompt", str "Write three sentences.");
         ("jobId", str job.id);
       ])
    fields;
  Check.equal "its upload's generator and entity types"
    (`List [ W.generator w.own; strs [ "Person" ] ])
    (`List read);
  Check.equal "who the gateway named its upload's principal"
    (str (W.did w.own))
    (str upload.principal);
  (* The roles of the worker's account, as its token carries them. *)
  Check.equal "the roles its upload carries"
    (str (String.concat "," (W.worker_roles ())))
    (match upload.roles with Some roles -> str roles | None -> `Null);
  let yielded = upload.resource_id in
  (* First, on the source: a link to what was made of it, an annotation of
     the resource as a whole, and so anchored nowhere on it. Then, on what
     was made: each citation, linked to the resource it cites. A marked claim
     is the words the mark wrapped, which stay where it stood, and a bare
     mark's is the name put in its place; its annotation is a span's as a
     detection's is: a position and a quote of its words, with no context. *)
  let linked =
    `Assoc
      [
        ("type", str "SpecificResource");
        ("source", str yielded);
        ("purpose", str "linking");
      ]
  in
  let cited =
    `Assoc
      [
        ("type", str "SpecificResource");
        ("source", str source);
        ("purpose", str "linking");
      ]
  in
  let citation start stop exact =
    text_annotation (W.generator w.own) yielded "linking"
      (annotation_id_of ~body:cited yielded "linking"
         (str (Printf.sprintf "%d:%d:%s" start stop exact)))
      ~start ~stop ~body:cited exact
  in
  Check.equal "what it committed"
    (`List
       [
         `Assoc
           [
             ("resourceId", str source);
             ("jobId", str job.id);
             ( "annotations",
               `List
                 [
                   `Assoc
                     [
                       ("@context", str "http://www.w3.org/ns/anno.jsonld");
                       ("type", str "Annotation");
                       ( "id",
                         str
                           (annotation_id_of ~body:linked source "linking"
                              (str "")) );
                       ("motivation", str "linking");
                       ("generator", W.generator w.own);
                       ("target", `Assoc [ ("source", str source) ]);
                       ("body", linked);
                     ];
                 ] );
           ];
         `Assoc
           [
             ("resourceId", str yielded);
             ("jobId", str job.id);
             ( "annotations",
               `List
                 [
                   citation 0 48
                     "Ada Lovelace published the first program in 1843";
                   citation 53 72 "Notes on the engine";
                   citation 137 148 "never built";
                 ] );
           ];
       ])
    (committed served);
  Check.equal "the cited name, where its citation stands"
    (str "Notes on the engine")
    (str (String.sub content 53 19));
  Check.equal "the cited claim, where its citation stands" (str "never built")
    (str (String.sub content 137 11));
  expect_progress served job
    [
      report job 5 (code "generating-resource");
      report job 95 (code "creating-resource");
      report job 100
        (code ~members:[ ("truncated", `Bool false) ] "complete-generated");
    ];
  Check.equal "its completion"
    (identified job
       [
         ( "result",
           `Assoc
             [
               ("resourceId", str yielded);
               ("resourceName", str "The Analytical Engine");
               ("truncated", `Bool false);
             ] );
         ("durability", str "acknowledged");
       ])
    completion

let anchored (w : W.t) =
  let job = yield_job w.own ~source (params "yield-annotation") in
  let annotation_id =
    Check.at job.running [ "params"; "context"; "focus"; "annotation"; "id" ]
  in
  let written =
    "Charles Babbage\nCharles Babbage war ein englischer Mathematiker"
  in
  Ollama.script w.ollama
    [ Ollama.Answer { response = written; stop = Cut_off; usage = None } ];
  let served = W.start w in
  let completion = settled served job in
  (* Asked with no temperature or length, the job takes 0.7 and 500
     tokens. *)
  expect_generations w.ollama [ generation model "yield-annotation" ];
  let anchored members =
    identified job (("annotationId", annotation_id) :: members)
  in
  Check.equal "its start"
    (`List [ anchored [] ])
    (`List (W.payloads served "job:start"));
  (* The annotation asked for the resource, so no other link is made, and
     with nothing cited nothing is committed. *)
  expect_sequence served
    [
      "emit job:claim";
      "emit job:start";
      "POST /resources";
      "emit job:complete";
      "emit job:claim";
    ];
  Check.equal "what it committed" (`Int 0)
    (`Int (Dynarray.length w.own.commits));
  let upload = upload_of w in
  Check.equal "what it uploaded" (str written) (str upload.file);
  let fields, read = fields_of upload [ "generator" ] in
  Check.equal "its upload's fields"
    (`Assoc
       [
         ("name", str "Charles Babbage");
         ("format", str "text/plain");
         ("storageUri", str "file://generated/babbage.txt");
         ("language", str "de");
         ("sourceResourceId", str source);
         ("sourceAnnotationId", annotation_id);
         ("jobId", str job.id);
       ])
    fields;
  Check.equal "its upload's generator"
    (`List [ W.generator w.own ])
    (`List read);
  let said percentage message =
    anchored
      [
        ("percentage", `Int percentage);
        ( "progress",
          `Assoc
            [
              ("percentage", `Int percentage);
              ("message", message);
              ("annotationId", annotation_id);
            ] );
      ]
  in
  expect_progress served job
    [
      said 5 (code "generating-resource");
      said 95 (code "creating-resource");
      said 100
        (code ~members:[ ("truncated", `Bool true) ] "complete-generated");
    ];
  Check.equal "its completion"
    (anchored
       [
         ( "result",
           `Assoc
             [
               ("resourceId", str upload.resource_id);
               ("resourceName", str "Charles Babbage");
               ("truncated", `Bool true);
             ] );
       ])
    completion

(* Hold that a yield job asked its model nothing, uploaded nothing, and
   failed. *)
let made_nothing_of (w : W.t) served =
  Check.equal "its generations" (`Int 0)
    (`Int (Dynarray.length w.ollama.generations));
  Check.equal "its uploads" (`Int 0) (`Int (List.length (uploads w)));
  expect_sequence served
    [ "emit job:claim"; "emit job:start"; "emit job:fail"; "emit job:claim" ]

let unsupported_format (w : W.t) =
  let job =
    yield_job w.own ~source
      (`Assoc
         [
           ("title", str "A page");
           ("storageUri", str "file://generated/page.html");
           ("outputMediaType", str "text/html");
           ( "context",
             `Assoc
               [
                 ( "focus",
                   `Assoc
                     [
                       ("kind", str "resource");
                       ("resource", source_descriptor source);
                     ] );
                 ("graph", `Assoc [ ("nodes", `List []); ("edges", `List []) ]);
                 ("metadata", `Assoc []);
               ] );
         ])
  in
  let served = W.start w in
  let failure, error = failure_of (settled ~channel:"job:fail" served job) in
  says "its error" error "text/html";
  (* No attempt changes what the service generates: none is spent on it
     again. *)
  Check.equal "its failure"
    (failed ~failure_class:"deterministic" ~will_retry:false job)
    failure;
  Check.equal "what it asked of its model's limits" (`List []) (shows_of w);
  made_nothing_of w served

let over_the_window (w : W.t) =
  (* The prompt kept as yield-markdown is 394 tokens, and the job asks for
     300 more: 694, one over this window. *)
  w.ollama.show <- Ollama.Context 693;
  let job = markdown_job w in
  let served = W.start w in
  let failure, error = failure_of (settled ~channel:"job:fail" served job) in
  Check.equal "its failure"
    (failed ~failure_class:"deterministic" ~will_retry:false job)
    failure;
  says "its error" error "693";
  Check.equal "what it asked of its model's limits"
    (`List [ `Assoc [ ("model", str model) ] ])
    (shows_of w);
  made_nothing_of w served

let fills_the_window (w : W.t) =
  w.ollama.show <- Ollama.Context 694;
  let job = markdown_job w in
  Ollama.script w.ollama [ answer "It was never built." ];
  let served = W.start w in
  ignore (settled served job : Yojson.Safe.t);
  expect_generations w.ollama
    [ generation ~context_length:694 model "yield-markdown" ]

let tests implementations =
  Case.test ~category:[]
    (case
       "works out the id of an annotation as \
        specs/src/annotations/id-cases.json states it, case for case" (fun () ->
         ()))
    id_cases
  :: in_world implementations "yield" ~category:[ "a yield job" ]
       [
         case
           "generates from the gathered context, uploads what the model wrote \
            less its citation marks, links it from its source, and cites its \
            sources on it"
           end_to_end;
         case
           "names the annotation a job is anchored to on everything it says of \
            the job, binds what it made to that annotation, and says when the \
            model was cut off"
           anchored;
         case
           "fails as deterministic, having asked its model nothing and \
            uploaded nothing, a job for a format it does not generate"
           unsupported_format;
         case
           "fails as deterministic, having asked for no generation, a job \
            whose prompt and length are together over its model's window"
           over_the_window;
         case
           "asks for the generation of a job whose prompt and length exactly \
            fill its model's window"
           fills_the_window;
       ]
