(* barriers.test.ts: what a Librarian does when the graph or the vector
   index is behind the record (LIBRARIAN.md § The knowledge graph, § The two
   waits, § Gathering around a resource, § Text search, § Reference search):
   a gather waits for the Weaver, and says the graph is behind when the
   Weaver does not come; a resource a vector hit names is described from its
   view while the graph does not hold it; a resource gather waits for the
   Smelter's decision, for no longer than its settle bound; and a Librarian
   that was stopped answers as it did before, for it owns no store.

   A store is behind here because its writer is stopped. Every case that
   stops one starts it again, whatever happens in between. *)

open Xynolog_conformance
open Support

(* A resource gather that asks for the graph and the similar passages and
   for nothing a model writes: no generation is made for it. *)
let plain =
  `Assoc
    [
      ("depth", `Int 1);
      ("maxResources", `Int 10);
      ("includeContent", `Bool false);
      ("includeSummary", `Bool false);
    ]

(* A settle bound short enough for a case to wait out, and one a Librarian
   starts on. *)
let short_settle_ms = 2_000
let settle_ms = float_of_int W.settle_timeout_ms

let gather_plain (w : W.t) resource_id =
  W.answered w "gather:resource-requested"
    (`Assoc [ ("resourceId", `String resource_id); ("options", plain) ])

(* Run [body] with the Weaver stopped: what is recorded in it is not in the
   graph. The Weaver is started again whatever [body] does. *)
let without_weaver (w : W.t) body =
  ignore (W.stop_weaver w : int option);
  Fun.protect ~finally:(fun () -> W.start_weaver w) body

(* Run [body] with the Smelter stopped: what is recorded in it has no
   vectors, and is not settled. The Smelter is started again whatever [body]
   does. *)
let without_smelter (w : W.t) body =
  ignore (W.stop_smelter w : int option);
  Fun.protect ~finally:(fun () -> W.start_smelter w) body

(* The similar passage a gather answered from one resource, if it answered
   one. *)
let passage_of gathered resource_id =
  List.find_opt
    (fun passage ->
      Protocol.at passage [ "resourceId" ] = Some (`String resource_id))
    (listed gathered [ "response"; "semanticContext"; "similar" ])

let no_semantic_context gathered =
  Check.that "no semanticContext"
    (Protocol.at gathered [ "response"; "semanticContext" ] = None)

let indexed (w : W.t) resource_id =
  Check.matching "its decision"
    [ ("outcome", `String "indexed") ]
    (W.settled w resource_id)

let degraded_lines lines =
  List.filter (fun line -> mentions "degraded" line) lines

(* ── The graph barrier ──────────────────────────────────────────────── *)

let made_this_moment (w : W.t) =
  let resource =
    W.created w "A wren nests in the hawthorn hedge.\n" ~name:"Wren"
  in
  (* Asked before the Weaver is waited for. Whether the graph held the
     resource already is not seen from here: the gather is answered either
     way. *)
  let gathered = gather_plain w resource in
  Check.equal "the resource" (`String resource)
    (Check.at gathered [ "resourceId" ]);
  has_node (Check.at gathered [ "response" ]) resource "resource"

let graph_behind (w : W.t) =
  let resource =
    without_weaver w (fun () ->
        let made =
          W.created w "A stoat hunts along the dry stone wall.\n" ~name:"Stoat"
        in
        ignore
          (Run.eventually ("the view of " ^ made) 10.0 (fun () ->
               Archivist_world.view w.kb made));
        let behind, ms =
          timed (fun () ->
              W.failed w "gather:resource-requested"
                (`Assoc [ ("resourceId", `String made); ("options", plain) ]))
        in
        (* The record holds the resource, so the failure is about the graph,
           and not that there is no such resource. *)
        Check.equal "the resource" (`String made)
          (Check.at behind [ "resourceId" ]);
        Check.that "the failure is about the graph"
          (mentions "graph" (message behind));
        Check.that "the failure is not that there is no such resource"
          (not (mentions "not found" (message behind)));
        (* It waited for the Weaver, for about a second, and did not leave
           the request to its timeout. *)
        Check.that
          (Printf.sprintf "it waited about a second, not %.0f ms" ms)
          (ms >= 700.0 && ms < 10_000.0);
        Check.that "the Weaver applied nothing of it"
          (W.applied_signals w made = []);
        (* A resource nobody made is no resource, whatever the graph holds. *)
        let unknown =
          W.failed w "gather:resource-requested"
            (`Assoc
               [ ("resourceId", `String "made-by-no-one"); ("options", plain) ])
        in
        Check.that "a resource nobody made is not found"
          (mentions "not found" (message unknown));
        made)
  in
  (* A Weaver that has started holds in the graph what the record held when
     it started. *)
  has_node
    (Check.at (gather_plain w resource) [ "response" ])
    resource "resource"

(* ── A resource the graph does not hold yet, described from its view ─ *)

let text_search_from_view (w : W.t) =
  (* No resource of this file has a name, a path or an entity type with
     these words in it, so nothing matches them as words. *)
  let text = "Kelp forests shelter urchins and otters" in
  without_weaver w (fun () ->
      let resource = W.created w text ~name:"Held by the view alone" in
      indexed w resource;
      let found =
        Check.at
          (W.answered w "match:resources-requested"
             (`Assoc [ ("search", `String text) ]))
          [ "response" ]
      in
      Check.that "the Weaver applied nothing of it"
        (W.applied_signals w resource = []);
      Check.equal "matchKind" (`String "semantic")
        (Check.at found [ "matchKind" ]);
      Check.that "at least one hit"
        (Yojson.Safe.Util.to_int (Check.at found [ "total" ]) >= 1);
      (* Its description is the view's, and with it the passage that
         matched. *)
      Check.matching "its hit"
        [
          ("name", `String "Held by the view alone"); ("content", `String text);
        ]
        (one_of "@id" resource (Check.list found [ "resources" ])))

let reference_search_from_view (w : W.t) =
  let source =
    W.created w "The heron waits at the weir for a silver eel.\n" ~name:"Heron"
  in
  let reference = W.referenced w source "silver eel" in
  W.ready w [ source ];
  (* The context is gathered while the graph is in step: one generation, of
     how the passage relates to what is around it. *)
  Ollama.script w.gatherer [ answer "The passage names a fish the heron eats." ];
  let gathered =
    W.answered w "gather:requested"
      (`Assoc
         [ ("annotationId", `String reference); ("resourceId", `String source) ])
  in
  without_weaver w (fun () ->
      (* Its whole text is the words the reference quotes, so its one vector
         is the vector of the search term. *)
      let target = W.created w "silver eel" ~name:"Caught by the index" in
      indexed w target;
      (* No model is asked: the search states that it wants none. *)
      let found =
        W.answered w "match:search-requested"
          (`Assoc
             [
               ("resourceId", `String source);
               ("referenceId", `String reference);
               ("context", Check.at gathered [ "response" ]);
               ("useSemanticScoring", `Bool false);
             ])
      in
      Check.that "the Weaver applied nothing of it"
        (W.applied_signals w target = []);
      Check.equal "the reference" (`String reference)
        (Check.at found [ "referenceId" ]);
      let candidate = one_of "@id" target (Check.list found [ "response" ]) in
      Check.matching "its candidate"
        [ ("name", `String "Caught by the index") ]
        candidate;
      Check.contains_text "its reason" "semantic similarity"
        (Yojson.Safe.Util.to_string (Check.at candidate [ "matchReason" ])))

(* ── The settle barrier ─────────────────────────────────────────────── *)

let waits_for_smelter (w : W.t) =
  (* Two resources of one text: each one's chunk has the vector of the
     other's. *)
  let text = "A lamplighter walks the quay at dusk.\n" in
  let neighbour = W.created w text ~name:"Quay, the first copy" in
  W.ready w [ neighbour ];
  (* The provider holds the request for the second copy's vector, so the
     Smelter has decided nothing of it. *)
  let provider = w.smelter_embedder in
  provider.choose <-
    Some
      (fun (request : Ollama_embed.recorded) ->
        if List.mem text request.texts then Some Ollama_embed.Hold else None);
  let resource = W.created w text ~name:"Quay, the second copy" in
  W.woven w [ resource ];
  Run.eventually "the Smelter to ask for the second copy's vector" 10.0
    (fun () -> if Ollama_embed.held provider = 1 then Some () else None);
  Eio.Switch.run (fun sw ->
      let asking =
        Eio.Fiber.fork_promise ~sw (fun () ->
            timed (fun () -> gather_plain w resource))
      in
      Run.sleep 1.0;
      Check.that
        "the gather is not answered while the Smelter has decided nothing"
        (Eio.Promise.peek asking = None);
      (* Answered well inside the 15 seconds a Smelter gives a request, the
         resource is indexed, and the gather goes on. *)
      Ollama_embed.release provider;
      let gathered, ms = Eio.Promise.await_exn asking in
      Check.that
        (Printf.sprintf "it waited for the Smelter, and not its bound: %.0f ms"
           ms)
        (ms >= 1_000.0 && ms < settle_ms);
      match passage_of gathered neighbour with
      | Some passage ->
          Check.matching "the neighbour's passage"
            [
              ("resourceName", `String "Quay, the first copy");
              ("text", `String text);
            ]
            passage;
          Check.that "its score is above 0.99"
            (Yojson.Safe.Util.to_number (Check.at passage [ "score" ]) > 0.99);
          (* A resource is no neighbour of itself. *)
          Check.that "no passage of its own"
            (passage_of gathered resource = None)
      | None -> failwith "the gather answered no passage of the first copy")

let skipped (content, format) (w : W.t) =
  let resource = W.created w content ~name:"Skipped" ~format in
  (* Asked before the Smelter is waited for: its decision, that it skips the
     resource, ends the wait. *)
  let gathered, ms = timed (fun () -> gather_plain w resource) in
  no_semantic_context gathered;
  has_node (Check.at gathered [ "response" ]) resource "resource";
  Check.that
    (Printf.sprintf "it did not wait out its settle bound: %.0f ms" ms)
    (ms < settle_ms /. 2.0);
  Check.matching "its decision"
    [ ("outcome", `String "skipped") ]
    (W.settled w resource)

let at_once_after_restart (w : W.t) =
  let lone =
    W.created w "A wheelwright trues the rim of a cart wheel.\n" ~name:"Wheel"
  in
  let image =
    W.created w image ~name:"A plate of a wheel" ~format:"image/png"
  in
  W.ready w [ lone; image ];
  indexed w lone;
  (* The Librarian that answers next has heard the Smelter say nothing of
     these two resources. *)
  W.restart_librarian w;
  List.iter
    (fun resource ->
      let gathered, ms = timed (fun () -> gather_plain w resource) in
      no_semantic_context gathered;
      has_node (Check.at gathered [ "response" ]) resource "resource";
      Check.that
        (Printf.sprintf "it answered at once, not in %.0f ms" ms)
        (ms < settle_ms /. 2.0))
    [ lone; image ];
  Check.equal "no line says a gather is degraded" (`List [])
    (`List (List.map (fun l -> `String l) (degraded_lines (W.output w))))

let settle_bound (w : W.t) =
  let text = "A cooper hoops the staves of an oak cask.\n" in
  let neighbour = W.created w text ~name:"Cask, the first copy" in
  W.ready w [ neighbour ];
  W.restart_librarian w
    ~launch:
      (W.changed w
         [ "gather"; "settleTimeoutMs" ]
         (Some (`Int short_settle_ms)));
  Fun.protect
    ~finally:(fun () -> W.restart_librarian w)
    (fun () ->
      let resource =
        without_smelter w (fun () ->
            let made = W.created w text ~name:"Cask, the second copy" in
            (* The graph is in step, so the gather's one wait is for the
               Smelter. *)
            W.woven w [ made ];
            let said = List.length (W.output w) in
            let gathered, ms = timed (fun () -> gather_plain w made) in
            (* Answered, with its graph and without the passages: after about
               the bound, and not before it. *)
            has_node (Check.at gathered [ "response" ]) made "resource";
            no_semantic_context gathered;
            let bound = float_of_int short_settle_ms in
            Check.that
              (Printf.sprintf "it waited out the bound, and no more: %.0f ms" ms)
              (ms >= bound -. 100.0 && ms < bound +. 10_000.0);
            Check.that "the Smelter decided nothing of it"
              (W.settled_signals w made = []);
            let degraded =
              List.filteri (fun at _ -> at >= said) (W.output w)
              |> degraded_lines
              |> List.filter (fun line ->
                  Re.execp (Re.compile (Re.str made)) line)
            in
            Check.that "a line says the gather is degraded" (degraded <> []);
            made)
      in
      (* A Smelter that has started holds in the index what the record held
         when it started: the same gather now finds the first copy. *)
      match passage_of (gather_plain w resource) neighbour with
      | Some passage ->
          Check.matching "the first copy's passage"
            [
              ("resourceName", `String "Cask, the first copy");
              ("text", `String text);
            ]
            passage
      | None -> failwith "the gather answered no passage of the first copy")

(* ── A Librarian that was stopped ───────────────────────────────────── *)

let as_before (w : W.t) =
  let text = "A clerk rules the columns of a ledger.\n" in
  let first = W.created w text ~name:"Ledger, the first copy" in
  let second = W.created w text ~name:"Ledger, the second copy" in
  ignore (W.highlighted w first "ledger" : string);
  W.ready w [ first; second ];
  let search () =
    W.answered w "match:resources-requested"
      (`Assoc [ ("search", `String "Ledger") ])
  in
  let gathered_before = gather_plain w first in
  let found_before = search () in
  (match passage_of gathered_before second with
  | Some passage ->
      Check.matching "the second copy's passage"
        [
          ("resourceName", `String "Ledger, the second copy");
          ("text", `String text);
        ]
        passage
  | None -> failwith "the gather answered no passage of the second copy");
  Check.equal "matchKind" (`String "lexical")
    (Check.at found_before [ "response"; "matchKind" ]);
  let sorted items = List.sort compare items in
  Check.equal "the hits"
    (`List (sorted [ `String first; `String second ]))
    (`List
       (sorted
          (ids "@id" (Check.list found_before [ "response"; "resources" ]))));
  (* The Librarian that answers next has heard neither writer say anything
     of these two resources. *)
  W.restart_librarian w;
  let gathered_after, ms = timed (fun () -> gather_plain w first) in
  Check.equal "the gather, as before" gathered_before gathered_after;
  Check.that
    (Printf.sprintf "it answered at once, not in %.0f ms" ms)
    (ms < settle_ms /. 2.0);
  Check.equal "the search, as before" found_before (search ());
  Check.equal "no line says a gather is degraded" (`List [])
    (`List (List.map (fun l -> `String l) (degraded_lines (W.output w))))

let tests implementations =
  let world = world_of "barriers" implementations in
  let category = [ "a Librarian whose stores are behind the record" ] in
  let case ?(max_duration = 120.0) name body =
    Testo.create ~category ~max_duration name (fun () -> W.end_case world body)
  in
  [
    case ~max_duration:60.0
      "answers a gather asked the moment a resource is made with the resource \
       in its graph"
      made_this_moment;
    case
      "fails a gather for a resource the graph is behind on, soon and saying \
       so; and answers it once the Weaver has caught up"
      graph_behind;
    case
      "answers a text search by meaning with a resource the graph does not \
       hold yet, described from its view"
      text_search_from_view;
    case
      "answers a reference search with a resource only the vector index and \
       the view hold"
      reference_search_from_view;
    case
      "waits for the Smelter to index a resource that was just made, and then \
       answers with the passages near its own"
      waits_for_smelter;
  ]
  @ List.map
      (fun (what, content, format) ->
        case ~max_duration:60.0
          (Printf.sprintf
             "answers a gather for %s without similar passages, and does not \
              wait out its settle bound"
             what)
          (skipped (content, format)))
      [
        ("a text of nothing but white space", " \n\t\n", "text/plain");
        ("an image, whose media type has no text", image, "image/png");
      ]
  @ [
      case
        "answers at once, after it is started again, a gather for an indexed \
         resource that nothing is like and one for an image: it has heard no \
         decision of either, and waits for none"
        at_once_after_restart;
      case ~max_duration:240.0
        "answers without similar passages when the Smelter has not decided by \
         the settle bound, and says in its log that the gather is degraded"
        settle_bound;
      case ~max_duration:180.0
        "answers a gather and a search as it did before it was stopped, and \
         waits on neither writer for what both stores hold"
        as_before;
    ]
