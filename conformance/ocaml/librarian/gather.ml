(* What a Librarian gathers (LIBRARIAN.md § Gathering
   around an annotation, § Gathering around a resource, § An annotation's
   summary, § The knowledge graph, § Order), case for case. *)

open Xynolog_conformance
open Support

let member = Yojson.Safe.Util.member

(* A resource gather that asks for the texts and no summary: what a case
   changes one member of. *)
let texts =
  [
    ("depth", `Int 1);
    ("maxResources", `Int 10);
    ("includeContent", `Bool true);
    ("includeSummary", `Bool false);
  ]

(* [texts] with each of [changes] in place of its member. *)
let options changes =
  `Assoc
    (List.filter (fun (key, _) -> not (List.mem_assoc key changes)) texts
    @ changes)

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

(* The focus of a context gathered for a resource. *)
let resource_focus context =
  let focus = Check.at context [ "focus" ] in
  match Protocol.at focus [ "kind" ] with
  | Some (`String "resource") -> focus
  | kind ->
      failwith
        (Printf.sprintf
           "the context's focus is an %s, and a resource was gathered"
           (Yojson.Safe.to_string (Option.value ~default:`Null kind)))

let prompts gatherer = Ollama.prompts gatherer

let absent what json path =
  Check.that ("no " ^ what) (Protocol.at json path = None)

let generations (w : W.t) = Dynarray.length w.gatherer.generations

(* Wait until the index holds the point of each annotation: the Smelter
   signals nothing when it writes one. *)
let pointed (w : W.t) annotation_ids =
  List.iter
    (fun annotation_id ->
      ignore
        (Run.eventually ("the point of the annotation " ^ annotation_id) 20.0
           (fun () -> Qdrant.annotation_point w.index annotation_id)
          : Yojson.Safe.t))
    annotation_ids

(* A reference on "fox" in a new copy of the fox text, bound to a new
   resource that holds [target]: the three ids, once both stores hold them. *)
let bound_reference ?format (w : W.t) target ~name =
  let source = W.created w fox ~name:"A fox and a dog" in
  let linked = W.created w target ~name ?format in
  let reference = W.referenced w source "fox" ~entity_types:[ "Animal" ] in
  W.bound w source reference linked;
  W.ready w [ source; linked ];
  (source, linked, reference)

let asked_of annotation source options =
  `Assoc
    ([ ("annotationId", `String annotation); ("resourceId", `String source) ]
    @ match options with Some o -> [ ("options", `Assoc o) ] | None -> [])

let gathered ?options (w : W.t) annotation source =
  W.answered w "gather:requested" (asked_of annotation source options)

let resource_gathered (w : W.t) resource_id options =
  W.answered w "gather:resource-requested"
    (`Assoc [ ("resourceId", `String resource_id); ("options", options) ])

(* What an operation out of bounds came to: the failure's message, or what
   the gateway's refusal or the wait said; "answered" when it was answered. *)
let refused_or_failed (w : W.t) operation payload =
  match W.ask w operation payload with
  | Answered _ -> "answered"
  | Failed failure -> message failure
  | exception Failure said -> said
  | exception Run.Timed_out said -> said

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
  Check.that "no targetResource" (Protocol.at focus [ "targetResource" ] = None);
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
  let source, linked, reference = bound_reference w tide ~name:"A salt marsh" in
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

(* ── for an annotation ───────────────────────────────────────────────── *)

let pdf_target (w : W.t) =
  let source, linked, reference =
    bound_reference w
      (Pdf.of_pages
         [
           [ "A grebe dives under the jetty"; "and a coot keeps to the reeds" ];
         ])
      ~name:"Birds of the jetty" ~format:"application/pdf"
  in
  Ollama.script w.gatherer
    [
      answer "Two birds by a jetty.";
      answer "The passage links to a page about birds.";
    ];
  let reply = gathered w reference source in
  (* The anchored text the Archivist serves is the text the Smelter stored:
     no byte of the PDF is read as text. *)
  let derived = member "text" (Archivist_world.anchored_text w.kb linked) in
  let focus = annotation_focus (Check.at reply [ "response" ]) in
  Check.equal "the target's text and summary"
    (`Assoc
       [ ("content", derived); ("summary", `String "Two birds by a jetty.") ])
    (Check.at focus [ "targetContext" ]);
  let content =
    Yojson.Safe.Util.to_string (Check.at focus [ "targetContext"; "content" ])
  in
  List.iter
    (fun word -> Check.contains_text "the target's text" word content)
    [ "grebe"; "jetty"; "coot"; "reeds" ];
  Check.contains_text "the first prompt" "grebe" (List.hd (prompts w.gatherer))

let entity_types (w : W.t) =
  let source = W.created w fox ~name:"A fox and a dog" in
  let other = W.created w tide ~name:"A salt marsh" in
  let reference =
    W.referenced w source "fox" ~entity_types:[ "Mammal"; "Predator" ]
  in
  (* No other case of this file gives a resource either of these two. *)
  W.tagged w source "Fable";
  W.tagged w other "Fable";
  W.tagged w other "Almanac";
  W.ready w [ source; other ];
  Ollama.script w.gatherer [ answer "The passage names a mammal." ];
  let response = Check.at (gathered w reference source) [ "response" ] in
  Check.equal "the entity types"
    (`List [ `String "Mammal"; `String "Predator" ])
    (Check.at response [ "metadata"; "entityTypes" ]);
  Check.matching "how many resources have each"
    [ ("Fable", `Int 2); ("Almanac", `Int 1) ]
    (Check.at response [ "metadata"; "entityTypeFrequencies" ])

let sentence_fails (w : W.t) =
  let source = W.created w fox ~name:"A fox and a dog" in
  let highlight = W.highlighted w source "lazy" in
  W.ready w [ source ];
  Ollama.script w.gatherer [ fails ];
  let response = Check.at (gathered w highlight source) [ "response" ] in
  absent "inferredRelationshipSummary" response
    [ "inferredRelationshipSummary" ];
  let focus = annotation_focus response in
  Check.equal "the annotation"
    (W.annotated w source highlight)
    (Check.at focus [ "annotation" ]);
  Check.equal "the selected text" (`String "lazy")
    (Check.at focus [ "selected"; "text" ]);
  Check.matching "the first node"
    [ ("id", `String source); ("type", `String "resource") ]
    (List.hd (Check.list response [ "graph"; "nodes" ]));
  (* It asked once, and did not ask again. *)
  Check.that "one generation" (generations w = 1)

let target_summary_fails (w : W.t) =
  let source, linked, reference = bound_reference w tide ~name:"A salt marsh" in
  (* Two generations, in this order: the summary of the target, which fails,
     then the relationship sentence. *)
  Ollama.script w.gatherer [ fails; answer "The passage links to a marsh." ];
  let response = Check.at (gathered w reference source) [ "response" ] in
  let focus = annotation_focus response in
  Check.equal "the target" (W.described w linked)
    (Check.at focus [ "targetResource" ]);
  Check.equal "the target's text alone"
    (`Assoc [ ("content", `String tide) ])
    (Check.at focus [ "targetContext" ]);
  Check.equal "the selected text" (`String "fox")
    (Check.at focus [ "selected"; "text" ]);
  Check.equal "the sentence" (`String "The passage links to a marsh.")
    (Check.at response [ "inferredRelationshipSummary" ]);
  Check.that "two generations" (generations w = 2)

let similar_passages (w : W.t) =
  let phrase = "a bittern booms" in
  let source =
    W.created w
      (Printf.sprintf "At dusk %s from the sedge.\n" phrase)
      ~name:"The sedge at dusk"
  in
  let elsewhere =
    W.created w
      (Printf.sprintf "Before rain %s twice, and stops.\n" phrase)
      ~name:"Signs of rain"
  in
  let focal = W.highlighted w source phrase in
  (* A second highlight of the same words on the same resource: as near as a
     passage can be, and not another resource's. *)
  let twin = W.highlighted w source phrase in
  let near = W.highlighted w elsewhere phrase in
  W.ready w [ source; elsewhere ];
  pointed w [ focal; twin; near ];
  let embedded = Ollama_embed.mark w.embedder in
  Ollama.script w.gatherer [ answer "The passage is about a bird that calls." ];
  let reply = gathered w focal source in
  let similar = listed reply [ "response"; "semanticContext"; "similar" ] in
  let found = one_of "resourceId" elsewhere similar in
  Check.matching "the passage of the other resource"
    [
      ("text", `String phrase);
      ("resourceId", `String elsewhere);
      ("resourceName", `String "Signs of rain");
      ("annotationId", `String near);
    ]
    found;
  Check.that "its score is above 0.99"
    (Yojson.Safe.Util.to_number (Check.at found [ "score" ]) > 0.99);
  (* No passage is the annotation's own resource's, though two of its
     highlights quote these very words. *)
  Check.that "no passage of its own resource"
    (not
       (List.exists
          (fun m -> Protocol.at m [ "resourceId" ] = Some (`String source))
          similar));
  (* Every passage is named as the record names its resource. *)
  List.iter
    (fun m ->
      let id = Yojson.Safe.Util.to_string (Check.at m [ "resourceId" ]) in
      Check.equal "the passage's resource name"
        (Check.at (W.described w id) [ "name" ])
        (Check.at m [ "resourceName" ]))
    similar;
  (* The text it had embedded to search by is the selected text, and that
     alone. *)
  Check.that "the selected text, embedded alone"
    (List.concat_map
       (fun (r : Ollama_embed.recorded) -> r.texts)
       (Ollama_embed.since w.embedder embedded)
    = [ phrase ])

let without_source (w : W.t) =
  let source, linked, reference = bound_reference w tide ~name:"A salt marsh" in
  let embedded = Ollama_embed.mark w.embedder in
  (* The one generation is the summary of the target. *)
  Ollama.script w.gatherer [ answer "A marsh that floods twice a day." ];
  let response =
    Check.at
      (gathered w reference source
         ~options:[ ("includeSourceContext", `Bool false) ])
      [ "response" ]
  in
  let focus = annotation_focus response in
  Check.equal "the annotation"
    (W.annotated w source reference)
    (Check.at focus [ "annotation" ]);
  Check.equal "its resource" (W.described w source)
    (Check.at focus [ "sourceResource" ]);
  absent "selected" focus [ "selected" ];
  Check.equal "the target" (W.described w linked)
    (Check.at focus [ "targetResource" ]);
  Check.equal "the target's text and summary"
    (`Assoc
       [
         ("content", `String tide);
         ("summary", `String "A marsh that floods twice a day.");
       ])
    (Check.at focus [ "targetContext" ]);
  absent "inferredRelationshipSummary" response
    [ "inferredRelationshipSummary" ];
  absent "semanticContext" response [ "semanticContext" ];
  Check.that "one prompt" (List.length (prompts w.gatherer) = 1);
  Check.that "nothing embedded" (Ollama_embed.since w.embedder embedded = [])

let without_target (w : W.t) =
  let source, linked, reference = bound_reference w tide ~name:"A salt marsh" in
  (* The one generation is the relationship sentence: no summary of the
     target is asked for. *)
  Ollama.script w.gatherer [ answer "The passage links to a marsh." ];
  let response =
    Check.at
      (gathered w reference source
         ~options:[ ("includeTargetContext", `Bool false) ])
      [ "response" ]
  in
  let focus = annotation_focus response in
  Check.equal "the target" (W.described w linked)
    (Check.at focus [ "targetResource" ]);
  absent "targetContext" focus [ "targetContext" ];
  Check.equal "the selected text" (`String "fox")
    (Check.at focus [ "selected"; "text" ]);
  Check.equal "the sentence" (`String "The passage links to a marsh.")
    (Check.at response [ "inferredRelationshipSummary" ]);
  match prompts w.gatherer with
  | [ prompt ] -> Check.contains_text "the prompt" "fox" prompt
  | asked ->
      failwith
        (Printf.sprintf "the Gatherer's model was asked %d times, not once"
           (List.length asked))

let window (w : W.t) =
  let text = String.make 150 'x' ^ " vixen " ^ String.make 150 'y' ^ "\n" in
  let at = 151 and length = String.length "vixen" in
  let end_ = at + length in
  let slice from upto = String.sub text from (upto - from) in
  let source = W.created w text ~name:"A vixen between two runs of letters" in
  let highlight = W.highlighted w source "vixen" in
  W.ready w [ source ];
  Ollama.script w.gatherer
    [
      answer "The passage names an animal.";
      answer "The passage names an animal.";
    ];
  let selected reply =
    Check.at (annotation_focus (Check.at reply [ "response" ])) [ "selected" ]
  in
  Check.equal "a hundred on each side"
    (`Assoc
       [
         ("before", `String (slice (at - 100) at));
         ("text", `String "vixen");
         ("after", `String (slice end_ (end_ + 100)));
       ])
    (selected
       (gathered w highlight source ~options:[ ("contextWindow", `Int 100) ]));
  (* With no window stated, the text on each side is shorter than the
     window, and is all taken. *)
  Check.equal "all of each side"
    (`Assoc
       [
         ("before", `String (slice 0 at));
         ("text", `String "vixen");
         ("after", `String (slice end_ (String.length text)));
       ])
    (selected (gathered w highlight source))

let window_out_of_bounds context_window (w : W.t) =
  let source = W.created w fox ~name:"A fox and a dog" in
  let highlight = W.highlighted w source "fox" in
  W.ready w [ source ];
  (* A window out of bounds is not gathered: the gateway refuses the
     request, or the Librarian fails it. *)
  let refused =
    refused_or_failed w "gather:requested"
      (asked_of highlight source
         (Some [ ("contextWindow", `Int context_window) ]))
  in
  Check.that "it was not answered" (refused <> "answered");
  Check.that
    ("what refused it names the window or the request: " ^ refused)
    (mentions "window" refused || mentions "gather:requested" refused);
  Check.that "its model was asked nothing" (generations w = 0)

let no_such_annotation (w : W.t) =
  let source = W.created w fox ~name:"A fox and a dog" in
  W.ready w [ source ];
  let failure =
    W.failed w "gather:requested" (asked_of "no-such-annotation" source None)
  in
  Check.equal "the annotation" (`String "no-such-annotation")
    (Check.at failure [ "annotationId" ]);
  Check.that "the failure is about the annotation"
    (mentions "annotation" (message failure));
  Check.that "its model was asked nothing" (generations w = 0)

let another_resource (w : W.t) =
  let source = W.created w fox ~name:"A fox and a dog" in
  let other = W.created w tide ~name:"A salt marsh" in
  let highlight = W.highlighted w source "fox" in
  W.ready w [ source; other ];
  let failure = W.failed w "gather:requested" (asked_of highlight other None) in
  Check.equal "the annotation" (`String highlight)
    (Check.at failure [ "annotationId" ]);
  Check.that "the failure is about the annotation"
    (mentions "annotation" (message failure));
  Check.that "its model was asked nothing" (generations w = 0)

(* ── for a resource ─────────────────────────────────────────────────── *)

let with_related (w : W.t) =
  let source, linked, _ = bound_reference w tide ~name:"A salt marsh" in
  let outward = resource_gathered w source (options []) in
  Check.equal "the resource" (`String source)
    (Check.at outward [ "resourceId" ]);
  let response = Check.at outward [ "response" ] in
  let focus = resource_focus response in
  Check.equal "the resource, as the record holds it" (W.described w source)
    (Check.at focus [ "resource" ]);
  Check.equal "its text and the text it relates to"
    (`Assoc
       [ ("main", `String fox); ("related", `Assoc [ (linked, `String tide) ]) ])
    (Check.at focus [ "content" ]);
  Check.matching "the first node"
    [
      ("id", `String source);
      ("type", `String "resource");
      ("label", `String "A fox and a dog");
    ]
    (List.hd (Check.list response [ "graph"; "nodes" ]));
  Check.containing "the nodes"
    [
      ("id", `String linked);
      ("type", `String "resource");
      ("label", `String "A salt marsh");
    ]
    (Check.list response [ "graph"; "nodes" ]);
  (* The resource a reference links to is related to the resource the
     reference is on. *)
  let inward = resource_gathered w linked (options []) in
  Check.equal "the target's text and the text that relates to it"
    (`Assoc
       [ ("main", `String tide); ("related", `Assoc [ (source, `String fox) ]) ])
    (Check.at (resource_focus (Check.at inward [ "response" ])) [ "content" ]);
  (* No summary was asked for, and no model was asked anything. *)
  absent "summary" focus [ "summary" ];
  absent "suggestedReferences" focus [ "suggestedReferences" ];
  Check.that "its model was asked nothing" (generations w = 0)

let no_text (w : W.t) =
  let source, linked, _ = bound_reference w tide ~name:"A salt marsh" in
  let response =
    Check.at
      (resource_gathered w source (options [ ("includeContent", `Bool false) ]))
      [ "response" ]
  in
  let focus = resource_focus response in
  Check.equal "the resource" (W.described w source)
    (Check.at focus [ "resource" ]);
  absent "content" focus [ "content" ];
  has_node response linked "resource";
  Check.that "its model was asked nothing" (generations w = 0)

let pdf_resource (w : W.t) =
  let pdf =
    W.created w
      (Pdf.of_pages
         [ [ "A snipe drums over the fen"; "and a teal sleeps on the mere" ] ])
      ~name:"Birds of the fen" ~format:"application/pdf"
  in
  W.ready w [ pdf ];
  let reply = resource_gathered w pdf (options []) in
  let main =
    Check.at
      (resource_focus (Check.at reply [ "response" ]))
      [ "content"; "main" ]
  in
  Check.equal "its text, as the Smelter derived it"
    (member "text" (Archivist_world.anchored_text w.kb pdf))
    main;
  List.iter
    (fun word ->
      Check.contains_text "its text" word (Yojson.Safe.Util.to_string main))
    [ "snipe"; "fen"; "teal"; "mere" ]

let suggestions_json items =
  Yojson.Safe.to_string
    (`List (List.map (fun s -> `Assoc [ ("suggestion", `String s) ]) items))

let summary_and_suggestions (w : W.t) =
  let source = W.created w tide ~name:"A salt marsh" in
  W.ready w [ source ];
  (* Two generations, in this order: the summary, then the suggestions,
     each an element. *)
  Ollama.script w.gatherer
    [
      answer "A marsh that floods twice a day.";
      answer (suggestions_json [ "Read the tide tables"; "Walk the sea wall" ]);
    ];
  let focus =
    resource_focus
      (Check.at
         (resource_gathered w source
            (options [ ("includeSummary", `Bool true) ]))
         [ "response" ])
  in
  Check.equal "the summary" (`String "A marsh that floods twice a day.")
    (Check.at focus [ "summary" ]);
  Check.equal "the suggestions"
    (`List [ `String "Read the tide tables"; `String "Walk the sea wall" ])
    (Check.at focus [ "suggestedReferences" ]);
  Check.equal "its text" (`String tide) (Check.at focus [ "content"; "main" ]);
  (* The first prompt is about the resource's text, and the second names the
     resource. *)
  (match prompts w.gatherer with
  | [ first; second ] ->
      Check.contains_text "the first prompt" (String.trim tide) first;
      Check.contains_text "the second prompt" "A salt marsh" second
  | asked ->
      failwith
        (Printf.sprintf "the Gatherer's model was asked %d times, not twice"
           (List.length asked)));
  Check.equal "the model asked" (`String W.gatherer_model)
    (member "model" (fst (Dynarray.get w.gatherer.generations 0)))

let summary_without_text (w : W.t) =
  let source = W.created w tide ~name:"A salt marsh" in
  W.ready w [ source ];
  Ollama.script w.gatherer
    [
      answer "A marsh that floods twice a day.";
      answer (suggestions_json [ "Read the tide tables" ]);
    ];
  let focus =
    resource_focus
      (Check.at
         (resource_gathered w source
            (options
               [
                 ("includeContent", `Bool false); ("includeSummary", `Bool true);
               ]))
         [ "response" ])
  in
  Check.equal "the summary" (`String "A marsh that floods twice a day.")
    (Check.at focus [ "summary" ]);
  Check.equal "the suggestions"
    (`List [ `String "Read the tide tables" ])
    (Check.at focus [ "suggestedReferences" ]);
  absent "content" focus [ "content" ];
  Check.contains_text "the first prompt" (String.trim tide)
    (List.hd (prompts w.gatherer))

let summary_fails (w : W.t) =
  let source = W.created w tide ~name:"A salt marsh" in
  W.ready w [ source ];
  Ollama.script w.gatherer [ fails ];
  let focus =
    resource_focus
      (Check.at
         (resource_gathered w source
            (options [ ("includeSummary", `Bool true) ]))
         [ "response" ])
  in
  absent "summary" focus [ "summary" ];
  absent "suggestedReferences" focus [ "suggestedReferences" ];
  Check.equal "the resource" (W.described w source)
    (Check.at focus [ "resource" ]);
  Check.equal "its text" (`String tide) (Check.at focus [ "content"; "main" ]);
  Check.that "one generation" (generations w = 1)

let suggestions_fail (w : W.t) =
  let source = W.created w tide ~name:"A salt marsh" in
  W.ready w [ source ];
  Ollama.script w.gatherer [ answer "A marsh that floods twice a day."; fails ];
  let focus =
    resource_focus
      (Check.at
         (resource_gathered w source
            (options [ ("includeSummary", `Bool true) ]))
         [ "response" ])
  in
  absent "summary" focus [ "summary" ];
  absent "suggestedReferences" focus [ "suggestedReferences" ];
  Check.equal "its text" (`String tide) (Check.at focus [ "content"; "main" ]);
  Check.that "two generations" (generations w = 2)

let most_resources (w : W.t) =
  let source = W.created w fox ~name:"A fox and a dog" in
  let marsh_text = "A redshank pipes over the saltings.\n"
  and shore_text = "A turnstone works along the strandline.\n" in
  let marsh = W.created w marsh_text ~name:"The saltings" in
  let shore = W.created w shore_text ~name:"The strandline" in
  W.bound w source (W.referenced w source "fox") marsh;
  W.bound w source (W.referenced w source "dog") shore;
  W.ready w [ source; marsh; shore ];
  let text_of = [ (marsh, `String marsh_text); (shore, `String shore_text) ] in
  let content most =
    let reply =
      resource_gathered w source (options [ ("maxResources", `Int most) ])
    in
    ( reply,
      Check.at (resource_focus (Check.at reply [ "response" ])) [ "content" ] )
  in
  let _, all = content 3 in
  Check.equal "every text"
    (`Assoc [ ("main", `String fox); ("related", `Assoc text_of) ])
    all;
  (* Two at most, itself and one other: which of the two others is the
     graph's to order. *)
  let two, two_content = content 2 in
  (match Check.at two_content [ "related" ] with
  | `Assoc [ (id, text) ] ->
      Check.equal "the one related text"
        (match List.assoc_opt id text_of with
        | Some expected -> expected
        | None -> failwith (id ^ " is neither of the two others"))
        text
  | related ->
      failwith ("one related text, and not " ^ Yojson.Safe.to_string related));
  (* The graph is not cut: both resources are in it. *)
  List.iter
    (fun id -> has_node (Check.at two [ "response" ]) id "resource")
    [ marsh; shore ];
  let _, one = content 1 in
  Check.equal "its own text alone"
    (`Assoc [ ("main", `String fox); ("related", `Assoc []) ])
    one

let near_by_vectors (w : W.t) =
  (* Three resources of the same words: each is as near the others as a text
     can be. *)
  let text = "A dipper bobs on a wet stone below the weir.\n" in
  let source = W.created w text ~name:"The weir, first copy" in
  let plain = W.created w text ~name:"The weir, second copy" in
  let question = W.created w text ~name:"The weir, asked about" in
  W.tagged w question "Question";
  W.ready w [ source; plain; question ];
  (* The Smelter signals nothing when it writes an entity type on a
     resource's points. *)
  let entity_types = Qdrant.payload_member "resources" "entityTypes" in
  Run.eventually
    "the entity type on the points of the resource that was given it" 20.0
    (fun () ->
      match Qdrant.points_of w.index "resources" question with
      | [] -> None
      | points ->
          if
            List.for_all
              (fun p ->
                member entity_types (Qdrant.payload p)
                = `List [ `String "Question" ])
              points
          then Some ()
          else None);
  let embedded = Ollama_embed.mark w.embedder in
  let plain_options changes =
    options (("includeContent", `Bool false) :: changes)
  in
  let every = resource_gathered w source (plain_options []) in
  let unfiltered = listed every [ "response"; "semanticContext"; "similar" ] in
  List.iter
    (fun (id, name) ->
      let found = one_of "resourceId" id unfiltered in
      Check.matching "a copy's passage"
        [
          ("text", `String text);
          ("resourceId", `String id);
          ("resourceName", `String name);
        ]
        found;
      Check.that "its score is above 0.99"
        (Yojson.Safe.Util.to_number (Check.at found [ "score" ]) > 0.99);
      (* A passage of a resource's text is no annotation's. *)
      absent "annotationId" found [ "annotationId" ])
    [ (plain, "The weir, second copy"); (question, "The weir, asked about") ];
  Check.that "no passage of its own"
    (not
       (List.exists
          (fun m -> Protocol.at m [ "resourceId" ] = Some (`String source))
          unfiltered));
  absent "excludedEntityTypes" every
    [ "response"; "semanticContext"; "excludedEntityTypes" ];
  let filtered =
    resource_gathered w source
      (plain_options [ ("excludeEntityTypes", `List [ `String "Question" ]) ])
  in
  let kept = listed filtered [ "response"; "semanticContext"; "similar" ] in
  Check.matching "the second copy's passage"
    [
      ("text", `String text); ("resourceName", `String "The weir, second copy");
    ]
    (one_of "resourceId" plain kept);
  Check.that "no passage of the excluded type"
    (not
       (List.exists
          (fun m -> Protocol.at m [ "resourceId" ] = Some (`String question))
          kept));
  Check.equal "the exclusion"
    (`List [ `String "Question" ])
    (Check.at filtered [ "response"; "semanticContext"; "excludedEntityTypes" ]);
  (* The most resources it is told is the most passages it answers with:
     one of the two copies, and nothing farther. *)
  let one =
    resource_gathered w source (plain_options [ ("maxResources", `Int 1) ])
  in
  (match listed one [ "response"; "semanticContext"; "similar" ] with
  | [ nearest ] ->
      Check.that "the nearest is one of the two copies"
        (List.mem
           (Check.at nearest [ "resourceId" ])
           [ `String plain; `String question ])
  | nearest ->
      failwith (Printf.sprintf "%d passages, and not one" (List.length nearest)));
  (* It searched by the resource's own vectors: its embedding provider was
     asked for none. *)
  Check.that "nothing embedded" (Ollama_embed.since w.embedder embedded = [])

let depth (w : W.t) =
  (* A chain: the first resource links to the second, and the second to the
     third. *)
  let first = W.created w fox ~name:"The first of a chain" in
  let second = W.created w fox ~name:"The second of a chain" in
  let third = W.created w tide ~name:"The third of a chain" in
  let to_second = W.referenced w first "fox" in
  let to_third = W.referenced w second "dog" in
  W.bound w first to_second second;
  W.bound w second to_third third;
  W.ready w [ first; second; third ];
  let at_depth n =
    resource_gathered w first
      (options [ ("includeContent", `Bool false); ("depth", `Int n) ])
  in
  let ids_of reply kind =
    `List
      (List.filter_map
         (fun node ->
           if Protocol.at node [ "type" ] = Some (`String kind) then
             Some (Check.at node [ "id" ])
           else None)
         (Check.list reply [ "response"; "graph"; "nodes" ]))
  in
  Check.equal "at depth 1, the resources"
    (`List [ `String first; `String second ])
    (ids_of (at_depth 1) "resource");
  let two = at_depth 2 in
  let nodes = Check.list two [ "response"; "graph"; "nodes" ]
  and edges = Check.list two [ "response"; "graph"; "edges" ] in
  (* The nearest first: the resource, what it links to, and what that links
     to. *)
  Check.equal "at depth 2, the resources"
    (`List [ `String first; `String second; `String third ])
    (ids_of two "resource");
  Check.containing "the nodes"
    [
      ("id", `String third);
      ("type", `String "resource");
      ("label", `String "The third of a chain");
    ]
    nodes;
  Check.containing "the edges"
    [
      ("source", `String second);
      ("target", `String third);
      ("type", `String "link");
    ]
    edges;
  (* The second resource is cited by the first, which is how it was reached:
     that link is one edge. *)
  Check.that "one edge from the first to the second"
    (List.length
       (List.filter
          (fun e ->
            Check.matches
              [ ("source", `String first); ("target", `String second) ]
              e)
          edges)
    = 1);
  (* Only the resource asked for has its annotations as nodes: the reference
     on the second resource is not one. *)
  Check.equal "the annotations"
    (`List [ `String to_second ])
    (ids_of two "annotation");
  absent "truncated" two [ "response"; "graph"; "truncated" ];
  (* A depth out of bounds is not gathered: the gateway refuses the request,
     or the Librarian fails it. *)
  let refused =
    refused_or_failed w "gather:resource-requested"
      (`Assoc
         [
           ("resourceId", `String first);
           ( "options",
             options [ ("includeContent", `Bool false); ("depth", `Int 4) ] );
         ])
  in
  Check.that "it was not answered" (refused <> "answered");
  Check.that
    ("what refused it names the depth or the request: " ^ refused)
    (mentions "depth" refused || mentions "gather:resource-requested" refused);
  Check.that "its model was asked nothing" (generations w = 0)

let no_such_resource (w : W.t) =
  let failure =
    W.failed w "gather:resource-requested"
      (`Assoc
         [ ("resourceId", `String "no-such-resource"); ("options", options []) ])
  in
  Check.equal "the resource" (`String "no-such-resource")
    (Check.at failure [ "resourceId" ]);
  Check.that "the failure is about the resource"
    (mentions "resource" (message failure));
  Check.that "its model was asked nothing" (generations w = 0)

(* ── for a summary of an annotation ─────────────────────────────────── *)

let annotation_summary (w : W.t) =
  (* More text on each side of the selection than the answer carries. *)
  let text = String.make 300 'b' ^ " heron " ^ String.make 300 'c' ^ "\n" in
  let at = 301 in
  let end_ = at + String.length "heron" in
  let slice from upto = String.sub text from (upto - from) in
  let source = W.created w text ~name:"A heron between two runs of letters" in
  let reference = W.referenced w source "heron" ~entity_types:[ "Bird" ] in
  W.ready w [ source ];
  Ollama.script w.gatherer [ answer "The passage names a wading bird." ];
  let response =
    Check.at
      (W.answered w "gather:summary-requested" (asked_of reference source None))
      [ "response" ]
  in
  Check.equal "the summary" (`String "The passage names a wading bird.")
    (Check.at response [ "summary" ]);
  Check.equal "the relevant fields"
    (`Assoc
       [
         ("resourceId", `String source);
         ("resourceName", `String "A heron between two runs of letters");
         ("entityTypes", `List [ `String "Bird" ]);
       ])
    (Check.at response [ "relevantFields" ]);
  (* Two hundred on each side: the end of what is before, and the start of
     what is after. *)
  Check.equal "the context"
    (`Assoc
       [
         ("before", `String (slice (at - 200) at));
         ("selected", `String "heron");
         ("after", `String (slice end_ (end_ + 200)));
       ])
    (Check.at response [ "context" ]);
  (* The one prompt its model saw quotes the selection, and names the
     resource. *)
  (match prompts w.gatherer with
  | [ prompt ] ->
      Check.contains_text "the prompt" "heron" prompt;
      Check.contains_text "the prompt" "A heron between two runs of letters"
        prompt
  | asked ->
      failwith
        (Printf.sprintf "the Gatherer's model was asked %d times, not once"
           (List.length asked)));
  Check.equal "the model asked" (`String W.gatherer_model)
    (member "model" (fst (Dynarray.get w.gatherer.generations 0)))

let summary_no_such_annotation (w : W.t) =
  let source = W.created w fox ~name:"A fox and a dog" in
  W.ready w [ source ];
  let failure =
    W.failed w "gather:summary-requested"
      (asked_of "no-such-annotation" source None)
  in
  Check.that "the failure is about the annotation"
    (mentions "annotation" (message failure));
  Check.that "its model was asked nothing" (generations w = 0)

(* ── for two annotations of one resource ────────────────────────────── *)

let one_after_the_other (w : W.t) =
  let source = W.created w fox ~name:"A text of nine words" in
  let other = W.created w tide ~name:"A text of nine more" in
  let first = W.highlighted w source "quick" in
  let second = W.highlighted w source "lazy" in
  let elsewhere = W.highlighted w other "marsh" in
  W.ready w [ source; other ];
  (* Each gather makes one generation, about the word its highlight selects.
     The first is held. *)
  let holds word prompt = Re.execp (Re.compile (Re.str word)) prompt in
  w.gatherer.choose <-
    Some
      (fun body ->
        let prompt = Yojson.Safe.Util.(member "prompt" body |> to_string) in
        if holds "quick" prompt then Some Ollama.Hold
        else if holds "lazy" prompt then
          Some (answer "The second gather of the resource.")
        else if holds "marsh" prompt then
          Some (answer "A gather of another resource.")
        else None);
  let about word = List.filter (holds word) (prompts w.gatherer) in
  Eio.Switch.run (fun sw ->
      let held =
        Eio.Fiber.fork_promise ~sw (fun () ->
            W.ask w "gather:requested" (asked_of first source None))
      in
      Ollama.asked w.gatherer 1;
      let behind =
        Eio.Fiber.fork_promise ~sw (fun () ->
            W.ask w "gather:requested" (asked_of second source None))
      in
      (* While the first is held, a gather for another resource is answered,
         and then another: time enough for the second gather of the first
         resource to have asked its model, were it not waiting. *)
      for _ = 1 to 2 do
        Check.equal "the gather of another resource"
          (`String "A gather of another resource.")
          (Check.at
             (gathered w elsewhere other)
             [ "response"; "inferredRelationshipSummary" ])
      done;
      Check.that "one prompt about the held gather"
        (List.length (about "quick") = 1);
      Check.that "two prompts about the other resource"
        (List.length (about "marsh") = 2);
      Check.that "none about the gather behind it" (about "lazy" = []);
      (* Released, the first is answered, and only then is the second's
         generation asked for. *)
      Ollama.release w.gatherer "The first gather of the resource.";
      let reply promise =
        match Eio.Promise.await_exn promise with
        | W.Answered reply -> reply
        | Failed failure ->
            failwith
              ("a gather failed, and both should have been answered: "
              ^ Yojson.Safe.to_string failure)
      in
      let one = reply held and two = reply behind in
      Check.equal "the first's annotation" (`String first)
        (Check.at one [ "annotationId" ]);
      Check.equal "the first's sentence"
        (`String "The first gather of the resource.")
        (Check.at one [ "response"; "inferredRelationshipSummary" ]);
      Check.equal "the second's annotation" (`String second)
        (Check.at two [ "annotationId" ]);
      Check.equal "the second's sentence"
        (`String "The second gather of the resource.")
        (Check.at two [ "response"; "inferredRelationshipSummary" ]);
      Check.that "one prompt about the second" (List.length (about "lazy") = 1);
      Check.contains_text "the last prompt" "lazy"
        (List.nth (prompts w.gatherer) (List.length (prompts w.gatherer) - 1)))

let tests implementations =
  let world = world_of "gather" implementations in
  let case group ?(max_duration = 120.0) name body =
    Testo.create ~category:[ "a Librarian, asked to gather"; group ]
      ~max_duration name (fun () -> W.end_case world body)
  in
  let annotation = case "for an annotation"
  and resource = case "for a resource"
  and summary = case "for a summary of an annotation"
  and two = case "for two annotations of one resource" in
  [
    annotation
      "answers a highlight on a text with the annotation as the record holds \
       it, its resource, the text it selects with what is before and after it, \
       and the sentence its model wrote"
      highlight_on_text;
    annotation
      "answers a bound reference with what it links to: the target as the \
       record holds it, its text, its model's summary of it, and the target as \
       a resource the graph connects"
      bound_reference_target;
    annotation
      "reads the text of a target that is a PDF from what the Smelter derived \
       of it"
      pdf_target;
    annotation
      "states the entity types of the annotation, and how many resources of \
       the knowledge base have each entity type"
      entity_types;
    annotation
      "answers without the relationship sentence when its model fails on it, \
       and with everything else"
      sentence_fails;
    annotation
      "answers a bound reference with the target's text and without a summary \
       of it when its model fails on the summary, and with everything else"
      target_summary_fails;
    annotation
      "finds the passages of other resources that are near the selected text, \
       names each by its resource, and takes none from the annotation's own \
       resource"
      similar_passages;
    annotation
      "leaves out the selected text when it is told not to include the source: \
       it then asks for no relationship sentence, and searches for no similar \
       passage"
      without_source;
    annotation
      "leaves out the target's text and its summary when it is told not to \
       include the target, and still says what the reference links to"
      without_target;
    annotation
      "takes as much text on each side of the selection as the window it is \
       given, and all there is within the window it is given none for"
      window;
  ]
  @ List.map
      (fun context_window ->
        annotation
          (Printf.sprintf
             "does not gather with a window of %d, which is out of bounds, and \
              asks its model nothing"
             context_window)
          (window_out_of_bounds context_window))
      [ 99; 5001 ]
  @ [
      annotation
        "fails a gather for an annotation the record does not hold, and names \
         the annotation it was asked for"
        no_such_annotation;
      annotation
        "fails a gather for an annotation that is on another resource than the \
         one named"
        another_resource;
      resource
        "answers with the resource as the record holds it and, when asked for \
         content, its text and the text of each resource the graph relates to \
         it, by id"
        with_related;
      resource
        "answers with no text when it is not asked for content: the resource \
         and its graph alone"
        no_text;
      resource "reads the text of a PDF from what the Smelter derived of it"
        pdf_resource;
      resource
        "asks its model for a summary of the resource and then for \
         suggestions, when it is asked for a summary, and answers with both"
        summary_and_suggestions;
      resource
        "makes a summary of the resource's text when it is asked for a summary \
         and not for the texts, and answers the summary without the text"
        summary_without_text;
      resource
        "answers without a summary or suggestions when its model fails on the \
         summary, and asks for no suggestions"
        summary_fails;
      resource
        "answers without a summary or suggestions when its model fails on the \
         suggestions"
        suggestions_fail;
      resource
        "reads the text of one resource fewer than the most it is told, \
         counting the resource itself, and leaves the graph whole"
        most_resources;
      resource
        "finds the resources near it by the vectors the index already holds of \
         it, leaves out those of an excluded entity type, and records the \
         exclusion on the reply"
        near_by_vectors;
      resource
        "builds the graph as many links out as the depth it is told: at 1 what \
         the resource links to, at 2 what that links to as well, with an edge \
         between the two, and at 4 nothing"
        depth;
      resource
        "fails a gather for a resource the record does not hold, and names the \
         resource it was asked for"
        no_such_resource;
      summary
        "answers with its model's summary, the resource's name, the \
         annotation's entity types, and the selected text with a bounded run \
         of the text on each side"
        annotation_summary;
      summary
        "fails for an annotation the record does not hold, and asks its model \
         nothing"
        summary_no_such_annotation;
      two
        "answers the gathers for one resource one after the other, and does \
         not hold a gather for another resource behind them"
        one_after_the_other;
    ]
