(* The librarian suite's cases that are ported here: telemetry.test.ts, case
   for case; of search.test.ts, the first two cases of "resources by text";
   of gather.test.ts, the first two cases "for an annotation". Each file is
   a world of its own, as each TypeScript file is. *)

open Xynolog_conformance
module W = Librarian_world

let member = Yojson.Safe.Util.member

(* A text every word of which occurs once, so a case can annotate any of
   them by quoting it. *)
let fox = "The quick brown fox jumps over the lazy dog.\n"

(* A second text, which shares no word of four letters or more with the first. *)
let tide = "Twice each day high water floods a salt marsh.\n"

(* What a provider that fails answers. *)
let fails =
  Ollama.Fails (500, {|{"error":"the stand-in fails this generation"}|})

let answer response = Ollama.Answer { response; usage = None }

let world_of file implementations =
  let command =
    W.python "Librarian" implementations.Implementations.librarians
  in
  Suite.world ("librarian/" ^ file)
    ~make:(fun () -> W.create implementations command)
    ~close:W.close

(* ── telemetry.test.ts ──────────────────────────────────────────────── *)

module Telemetry_file = struct
  let options ?(content = false) ?(summary = false) () =
    `Assoc
      [
        ("depth", `Int 1);
        ("maxResources", `Int 10);
        ("includeContent", `Bool content);
        ("includeSummary", `Bool summary);
      ]

  (* A resource gather that asks for the graph and the similar passages and
     for nothing a model writes; one that asks for the texts too; and one
     that asks its model for a summary and suggestions. *)
  let plain = options ()
  let texts = options ~content:true ()
  let summarized = options ~content:true ~summary:true ()

  (* A settle bound short enough for the case to wait out. *)
  let short_settle_ms = 2_000

  (* Wait until [metric] has carried every value the table lists for the
     Librarian's [key] of it. *)
  let every_value_of otlp metric key =
    let _, metrics = Protocol.telemetry Librarian in
    let listed =
      match
        List.find_opt
          (fun (row : Protocol.metric_row) ->
            row.metric = Protocol.metric_name metric)
          metrics
      with
      | Some row -> (
          match
            List.find_opt
              (fun (a : Protocol.attribute) -> a.key = key)
              row.metric_attributes
          with
          | Some { values = Some values; _ } -> values
          | _ ->
              failwith
                (Printf.sprintf
                   "the table lists no values of %s for a Librarian's %s" key
                   metric))
      | None -> failwith ("the table lists no " ^ metric ^ " for a Librarian")
    in
    Run.eventually
      (Printf.sprintf "%s carrying every %s: %s" metric key
         (String.concat ", " (List.map Telemetry.listed_json listed)))
      30.0
      (fun () ->
        match Otlp.metric otlp metric with
        | None -> None
        | Some seen -> (
            match Dynarray.find_opt (fun (k, _) -> k = key) seen.attributes with
            | Some (_, values)
              when List.for_all
                     (fun l -> Dynarray.exists (Telemetry.carries l) values)
                     listed ->
                Some ()
            | _ -> None))

  let exports_exactly_the_table otlp (w : W.t) =
    (* The world's Librarian was started with no endpoint, and exports
       nothing. This one is told where to export, and how often, and waits
       two seconds for the Smelter. *)
    W.restart_librarian w
      ~launch:
        {
          settle_ms = short_settle_ms;
          env =
            w.base.env
            @ [
                ("OTEL_EXPORTER_OTLP_ENDPOINT", otlp.Otlp.endpoint);
                ("OTEL_METRIC_EXPORT_INTERVAL", "500");
                ("OTEL_BSP_SCHEDULE_DELAY", "100");
              ];
        };
    (* Two requests of a model that are answered, the first with the tokens its provider counted. *)
    let marsh = W.created w tide ~name:"A salt marsh" in
    W.ready w [ marsh ];
    Ollama.script w.gatherer
      [
        Answer
          {
            response = "A marsh that floods twice a day.";
            usage = Some (212, 9);
          };
        answer
          (Yojson.Safe.to_string
             (`List
                [
                  `Assoc [ ("suggestion", `String "Read the tide tables") ];
                  `Assoc [ ("suggestion", `String "Walk the sea wall") ];
                ]));
      ];
    let resource_gather id options =
      `Assoc [ ("resourceId", `String id); ("options", options) ]
    in
    ignore
      (W.answered w "gather:resource-requested"
         (resource_gather marsh summarized));
    (* A request of a model that fails: answered without its summary and suggestions. *)
    Ollama.script w.gatherer [ fails ];
    ignore
      (W.answered w "gather:resource-requested"
         (resource_gather marsh summarized));
    (* Two more that fail, in a gather around a reference bound to the
       marsh: the summary of what it links to, then the relationship
       sentence. Answered without either. *)
    let fable = W.created w fox ~name:"A fox and a dog" in
    let reference = W.referenced w fable "fox" in
    W.bound w fable reference marsh;
    W.ready w [ fable ];
    Ollama.script w.gatherer [ fails; fails ];
    ignore
      (W.answered w "gather:requested"
         (`Assoc
            [
              ("annotationId", `String reference); ("resourceId", `String fable);
            ]));
    (* A text that is derived from a resource's bytes is asked of the gateway. *)
    let pdf =
      W.created w
        (Pdf.of_pages [ [ "A snipe drums over the fen" ] ])
        ~name:"Birds of the fen" ~format:"application/pdf"
    in
    W.ready w [ pdf ];
    ignore
      (W.answered w "gather:resource-requested" (resource_gather pdf texts));
    (* A gather the Smelter does not decide in time: answered without similar passages. *)
    ignore (W.stop_smelter w : int option);
    Fun.protect
      ~finally:(fun () -> W.start_smelter w)
      (fun () ->
        let unsettled =
          W.created w "A cooper hoops the staves of an oak cask.\n" ~name:"Cask"
        in
        W.woven w [ unsettled ];
        ignore
          (W.answered w "gather:resource-requested"
             (resource_gather unsettled plain)));
    (* A gather for a resource the graph does not come to hold: failed. *)
    ignore (W.stop_weaver w : int option);
    Fun.protect
      ~finally:(fun () -> W.start_weaver w)
      (fun () ->
        let unwoven =
          W.created w "A stoat hunts along the dry stone wall.\n" ~name:"Stoat"
        in
        ignore
          (Run.eventually ("the view of " ^ unwoven) 10.0 (fun () ->
               Archivist_world.view w.kb unwoven));
        ignore
          (W.failed w "gather:resource-requested"
             (resource_gather unwoven plain)));
    let complete =
      Protocol.span_name "bus.emit:{channel}"
        [ ("channel", "gather:resource-complete") ]
    in
    let span =
      Run.eventually "the span of a gather it answered" 15.0 (fun () ->
          Dynarray.find_opt
            (fun (s : Otlp.span) -> s.name = complete)
            otlp.spans)
    in
    Check.that "the span is exported under the Librarian's name"
      (List.assoc_opt "service.name" span.resource
      = Some (Otlp.Text "xynolog-librarian"));
    (* Each way a request of a model ends, the tokens read and written, and each thing a gather went without. *)
    every_value_of otlp "xynolog.inference.calls" "inference.outcome";
    every_value_of otlp "xynolog.inference.duration" "inference.outcome";
    every_value_of otlp "xynolog.inference.tokens" "inference.direction";
    every_value_of otlp "xynolog.gather.degraded" "projection";
    match
      Telemetry.outside_the_table otlp Librarian (Some w.kb.world.kb.did)
    with
    | [] -> ()
    | unlisted ->
        failwith (String.concat "\n" (unlisted @ [ Telemetry.received otlp ]))

  let tests implementations =
    let world = world_of "telemetry" implementations in
    let otlp = lazy (Otlp.start ()) in
    [
      Testo.create ~category:[ "what a Librarian exports" ]
        ~max_duration:300.0
        "exports, under the Librarian's name, exactly the telemetry the spec \
         lists: every row it can, and nothing else" (fun () ->
          W.end_case world (exports_exactly_the_table (Lazy.force otlp)));
    ]
end

(* ── search.test.ts ─────────────────────────────────────────────────── *)

module Search_file = struct
  let category = [ "a Librarian, asked to find"; "resources by text" ]

  (* The text a hit carries beside its description: a preview of a lexical
     hit, the passage that matched of a semantic one. *)
  let text_of hit = Protocol.at hit [ "content" ]

  (* What the index holds at or above [floor] of [text]: one entry for each
     resource, with the text of its nearest chunk, nearest first. *)
  let within (w : W.t) text floor =
    let resource_id = Qdrant.payload_member "resources" "resourceId" in
    let passage_of = Qdrant.payload_member "resources" "text" in
    let nearest = Hashtbl.create 64 in
    List.iter
      (fun point ->
        let payload = Qdrant.payload point in
        let id = Yojson.Safe.Util.to_string (member resource_id payload) in
        let passage = Yojson.Safe.Util.to_string (member passage_of payload) in
        let score = Ollama_embed.nearness text passage in
        match Hashtbl.find_opt nearest id with
        | Some (_, held) when held >= score -> ()
        | _ -> if score >= floor then Hashtbl.replace nearest id (passage, score))
      (Qdrant.points w.index "resources");
    List.map
      (fun (id, (passage, _)) -> (id, passage))
      (List.sort
         (fun (_, (_, a)) (_, (_, b)) -> compare b a)
         (List.of_seq (Hashtbl.to_seq nearest)))

  (* A text recorded as a resource whose one chunk in the index is the text, whole. *)
  let recorded (w : W.t) text name =
    let id = W.created w text ~name in
    W.ready w [ id ];
    Check.equal "its one chunk is the text"
      (`List [ `String text ])
      (`List
         (List.map
            (fun p ->
              member
                (Qdrant.payload_member "resources" "text")
                (Qdrant.payload p))
            (Qdrant.points_of w.index "resources" id)));
    id

  let page_is what page ~total ~offset ~limit ~kind =
    Check.matching what
      [
        ("total", `Int total);
        ("offset", `Int offset);
        ("limit", `Int limit);
        ("matchKind", `String kind);
      ]
      page

  let lexical_page (w : W.t) =
    let exact_text = "A rosette of quills under still water.\n" in
    let longer_text = "Counted in the tarn, shore by shore.\n" in
    let exact = W.created w exact_text ~name:"Quillwort" in
    let longer = W.created w longer_text ~name:"Quillwort survey" in
    W.ready w [ exact; longer ];
    let asked = Ollama_embed.mark w.embedder in
    let search body =
      member "response" (W.answered w "match:resources-requested" (`Assoc body))
    in
    let page = search [ ("search", `String "quillwort") ] in
    page_is "the first page" page ~total:2 ~offset:0 ~limit:50 ~kind:"lexical";
    (* The page's order is the graph's own, and is no rule of a Librarian's. *)
    let hits =
      List.map
        (fun hit ->
          `Assoc
            [
              ("id", Check.at hit [ "@id" ]);
              ("name", Check.at hit [ "name" ]);
              ("preview", Option.value ~default:`Null (text_of hit));
            ])
        (Check.list page [ "resources" ])
    in
    Check.that "two hits" (List.length hits = 2);
    List.iter
      (fun (id, name, preview) ->
        Check.containing "the hits"
          [
            ("id", `String id);
            ("name", `String name);
            ("preview", `String preview);
          ]
          hits)
      [
        (exact, "Quillwort", exact_text);
        (longer, "Quillwort survey", longer_text);
      ];
    (* A later page of it: the total is still of all that match. *)
    let second =
      search
        [
          ("search", `String "quillwort"); ("offset", `Int 1); ("limit", `Int 1);
        ]
    in
    page_is "the second page" second ~total:2 ~offset:1 ~limit:1 ~kind:"lexical";
    Check.equal "the second page's one hit"
      (`List [ Check.at (List.nth hits 1) [ "id" ] ])
      (`List
         (List.map
            (fun hit -> Check.at hit [ "@id" ])
            (Check.list second [ "resources" ])));
    Check.that "no embedding was asked for"
      (Ollama_embed.since w.embedder asked = [])

  let semantic_page (w : W.t) =
    let text = "Glasswort reddens the upper marsh in autumn" in
    let sheet = recorded w text "Sheet twelve" in
    let expected = within w text W.semantic_floor in
    Check.that "the sheet is the nearest"
      (match expected with
      | (id, passage) :: _ -> id = sheet && passage = text
      | [] -> false);
    let asked = Ollama_embed.mark w.embedder in
    let page =
      member "response"
        (W.answered w "match:resources-requested"
           (`Assoc [ ("search", `String text) ]))
    in
    page_is "the page" page ~total:(List.length expected) ~offset:0 ~limit:50
      ~kind:"semantic";
    let hits = Check.list page [ "resources" ] in
    Check.equal "each hit with the passage that matched, the nearest first"
      (`List
         (List.filteri (fun at _ -> at < 50) expected
         |> List.map (fun (id, passage) ->
             `Assoc [ ("resourceId", `String id); ("passage", `String passage) ])
         ))
      (`List
         (List.map
            (fun hit ->
              `Assoc
                [
                  ("resourceId", Check.at hit [ "@id" ]);
                  ("passage", Option.value ~default:`Null (text_of hit));
                ])
            hits));
    Check.matching "the first hit"
      [ ("@id", `String sheet); ("name", `String "Sheet twelve") ]
      (List.hd hits);
    Check.that "the text is embedded once"
      (List.map
         (fun (r : Ollama_embed.recorded) -> r.texts)
         (Ollama_embed.since w.embedder asked)
      = [ [ text ] ])

  let tests implementations =
    let world = world_of "search" implementations in
    [
      Testo.create ~category ~max_duration:300.0
        "answers a page of the resources the text matches by name, labelled \
         `lexical`, each with a preview of its text; and asks for no embedding"
        (fun () -> W.end_case world lexical_page);
      Testo.create ~category ~max_duration:300.0
        "answers a search that no name matches from the index, labelled \
         `semantic`: each hit carries the passage that matched, the nearest \
         first, and the text is embedded once" (fun () ->
          W.end_case world semantic_page);
    ]
end

(* ── gather.test.ts ─────────────────────────────────────────────────── *)

module Gather_file = struct
  let category = [ "a Librarian, asked to gather"; "for an annotation" ]

  (* The focus of a context gathered for an annotation. *)
  let annotation_focus context =
    let focus = Check.at context [ "focus" ] in
    match Protocol.at focus [ "kind" ] with
    | Some (`String "annotation") -> focus
    | kind ->
        failwith
          (Printf.sprintf
             "the context's focus is a %s, and an annotation was gathered"
             (Yojson.Safe.to_string (Option.value ~default:`Null kind)))

  let prompts gatherer = Ollama.prompts gatherer

  (* A reference on "fox" in a new copy of the fox text, bound to a new
     resource that holds [target]: the three ids, once both stores hold them. *)
  let bound_reference (w : W.t) target ~name =
    let source = W.created w fox ~name:"A fox and a dog" in
    let linked = W.created w target ~name in
    let reference = W.referenced w source "fox" ~entity_types:[ "Animal" ] in
    W.bound w source reference linked;
    W.ready w [ source; linked ];
    (source, linked, reference)

  let gathered (w : W.t) annotation source =
    W.answered w "gather:requested"
      (`Assoc
         [
           ("annotationId", `String annotation); ("resourceId", `String source);
         ])

  let highlight_on_text (w : W.t) =
    let source = W.created w fox ~name:"A fox and a dog" in
    let highlight = W.highlighted w source "fox" in
    W.ready w [ source ];
    (* A highlight links nowhere: the one generation is the relationship sentence. *)
    Ollama.script w.gatherer [ answer "The passage names an animal." ];
    let reply = gathered w highlight source in
    Check.equal "the annotation it answers" (`String highlight)
      (Check.at reply [ "annotationId" ]);
    let response = Check.at reply [ "response" ] in
    let focus = annotation_focus response in
    Check.equal "the annotation, as the record holds it"
      (W.annotated w source highlight)
      (Check.at focus [ "annotation" ]);
    Check.equal "its resource" (W.described w source)
      (Check.at focus [ "sourceResource" ]);
    Check.equal "the text it selects"
      (`Assoc
         [
           ("before", `String "The quick brown ");
           ("text", `String "fox");
           ("after", `String " jumps over the lazy dog.\n");
         ])
      (Check.at focus [ "selected" ]);
    Check.that "no targetResource"
      (Protocol.at focus [ "targetResource" ] = None);
    Check.that "no targetContext" (Protocol.at focus [ "targetContext" ] = None);
    Check.equal "the sentence its model wrote"
      (`String "The passage names an animal.")
      (Check.at response [ "inferredRelationshipSummary" ]);
    (* The sentence was asked of the Gatherer's model, about the passage, and the Matcher's model was asked nothing. *)
    (match prompts w.gatherer with
    | [ prompt ] -> Check.contains_text "the prompt" "fox" prompt
    | asked ->
        failwith
          (Printf.sprintf "the Gatherer's model was asked %d times, not once"
             (List.length asked)));
    Check.equal "the model asked" (`String W.gatherer_model)
      (member "model" (fst (Dynarray.get w.gatherer.generations 0)));
    Check.that "the Matcher's model was asked nothing"
      (Dynarray.length w.matcher.generations = 0)

  let bound_reference_target (w : W.t) =
    let source, linked, reference =
      bound_reference w tide ~name:"A salt marsh"
    in
    let highlight = W.highlighted w source "dog" in
    W.ready w [ source ];
    (* Two generations, in this order: the summary of the target, then the relationship sentence. *)
    Ollama.script w.gatherer
      [
        answer "A marsh that floods twice a day.";
        answer "The passage links to a marsh.";
      ];
    let response = Check.at (gathered w reference source) [ "response" ] in
    let focus = annotation_focus response in
    Check.equal "the annotation"
      (W.annotated w source reference)
      (Check.at focus [ "annotation" ]);
    Check.equal "the target, as the record holds it" (W.described w linked)
      (Check.at focus [ "targetResource" ]);
    Check.equal "its text and its model's summary"
      (`Assoc
         [
           ("content", `String tide);
           ("summary", `String "A marsh that floods twice a day.");
         ])
      (Check.at focus [ "targetContext" ]);
    Check.equal "the sentence" (`String "The passage links to a marsh.")
      (Check.at response [ "inferredRelationshipSummary" ]);
    Check.equal "the entity types"
      (`List [ `String "Animal" ])
      (Check.at response [ "metadata"; "entityTypes" ]);
    (* The graph: the resource first, the resource the reference links to, and each annotation on the resource. *)
    let nodes = Check.list response [ "graph"; "nodes" ]
    and edges = Check.list response [ "graph"; "edges" ] in
    Check.matching "the first node"
      [
        ("id", `String source);
        ("type", `String "resource");
        ("label", `String "A fox and a dog");
      ]
      (List.hd nodes);
    Check.containing "the nodes"
      [
        ("id", `String linked);
        ("type", `String "resource");
        ("label", `String "A salt marsh");
      ]
      nodes;
    Check.containing "the edges"
      [ ("source", `String source); ("target", `String linked) ]
      edges;
    List.iter
      (fun annotation ->
        Check.containing "the nodes"
          [ ("id", `String annotation); ("type", `String "annotation") ]
          nodes;
        Check.containing "the edges"
          [
            ("source", `String annotation);
            ("target", `String source);
            ("type", `String "annotation-of");
          ]
          edges)
      [ reference; highlight ];
    (* The first prompt is about the target's text, and the second about the passage and what its resource is connected to. *)
    match prompts w.gatherer with
    | [ first; second ] ->
        Check.contains_text "the first prompt" "A salt marsh" first;
        Check.contains_text "the first prompt" (String.trim tide) first;
        Check.contains_text "the second prompt" "fox" second;
        Check.contains_text "the second prompt" "A salt marsh" second
    | asked ->
        failwith
          (Printf.sprintf "the Gatherer's model was asked %d times, not twice"
             (List.length asked))

  let tests implementations =
    let world = world_of "gather" implementations in
    [
      Testo.create ~category ~max_duration:300.0
        "answers a highlight on a text with the annotation as the record holds \
         it, its resource, the text it selects with what is before and after \
         it, and the sentence its model wrote" (fun () ->
          W.end_case world highlight_on_text);
      Testo.create ~category ~max_duration:300.0
        "answers a bound reference with what it links to: the target as the \
         record holds it, its text, its model's summary of it, and the target \
         as a resource the graph connects" (fun () ->
          W.end_case world bound_reference_target);
    ]
end

(* One world at a time: the files share one graph database and one vector
   database, each cleared when a file's world is made, and the health ports
   of a Weaver and a Smelter are constants. *)
let suite : Suite.t =
  {
    name = "librarian";
    workers = 1;
    tests =
      (fun implementations ->
        Telemetry_file.tests implementations
        @ Search_file.tests implementations
        @ Gather_file.tests implementations);
  }
