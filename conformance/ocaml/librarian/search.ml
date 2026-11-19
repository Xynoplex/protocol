(* search.test.ts: what a Librarian finds (LIBRARIAN.md § Reference
   search, § Text search, § What refers to a resource, § Names, § Limits),
   case for case. *)

open Xynolog_conformance
open Support

let member = Yojson.Safe.Util.member

(* Another knowledge base than the one the suite's Librarian serves. *)
let other_kb () =
  (Protocol.kb_identity "plain scalars, and a version, which names nothing").did

(* A text the stand-in provider puts at least [least] near [to_] and at most
   [most]. No text is near another but by chance, so texts are tried until
   one is. *)
let text_near to_ least most =
  let rec try_ n =
    if n > 500_000 then
      failwith
        (Printf.sprintf "no text was found between %g and %g near %S" least most
           to_)
    else
      let text =
        Printf.sprintf "Passage %d of the survey of the lower marsh" n
      in
      let score = Ollama_embed.nearness to_ text in
      if score >= least && score <= most then text else try_ (n + 1)
  in
  try_ 1

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
            member (Qdrant.payload_member "resources" "text") (Qdrant.payload p))
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
      (exact, "Quillwort", exact_text); (longer, "Quillwort survey", longer_text);
    ];
  (* A later page of it: the total is still of all that match. *)
  let second =
    search
      [ ("search", `String "quillwort"); ("offset", `Int 1); ("limit", `Int 1) ]
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

let texts_of_points (w : W.t) resource_id =
  `List
    (List.map
       (fun p ->
         member (Qdrant.payload_member "resources" "text") (Qdrant.payload p))
       (Qdrant.points_of w.index "resources" resource_id))

let reason candidate =
  match Protocol.at candidate [ "matchReason" ] with
  | Some (`String reason) -> reason
  | _ -> ""

let reason_has what candidate needle =
  Check.contains_text what needle (reason candidate)

let embedded_texts (w : W.t) mark =
  List.map
    (fun (r : Ollama_embed.recorded) -> r.texts)
    (Ollama_embed.since w.embedder mark)

(* The number a score prompt gives a candidate: the digits its line of the
   prompt begins with. None when the prompt lists no such candidate. *)
let numbered prompt resource_id =
  List.find_map
    (fun line ->
      if
        Re.execp
          (Re.compile (Re.seq [ Re.bos; Re.rep1 Re.digit; Re.char '.' ]))
          line
        && Re.execp (Re.compile (Re.str resource_id)) line
      then Some (String.sub line 0 (String.index line '.'))
      else None)
    (String.split_on_char '\n' prompt)

(* A provider's refusal of one request. *)
let refusal_body = {|{"error":"the stand-in refuses this request"}|}

(* The limits a Librarian reports of an engine of Ollama's with a window of
   [window] tokens: an Ollama has one window, for what a model reads and
   what it writes. *)
let limits_of model window =
  `Assoc
    [
      ( "response",
        `Assoc
          [
            ( "limits",
              `List
                [
                  `Assoc
                    [
                      ( "engine",
                        `Assoc
                          [
                            ("provider", `String "ollama");
                            ("model", `String model);
                          ] );
                      ( "limits",
                        `Assoc
                          [
                            ("contextTokens", `Int window);
                            ("maxOutputTokens", `Int window);
                            ("acceptsTemperature", `Bool true);
                          ] );
                    ];
                ] );
          ] );
    ]

(* The context of a reference, gathered as a client gathers it before it
   searches. The reference links nowhere, so the Gatherer's model is asked
   once: for how the passage relates to what is around it. *)
let gathered (w : W.t) resource_id reference_id =
  Ollama.script w.gatherer
    [ answer "The passage names something another resource is about." ];
  Check.at
    (W.answered w "gather:requested"
       (`Assoc
          [
            ("annotationId", `String reference_id);
            ("resourceId", `String resource_id);
          ]))
    [ "response" ]

let search_body ?(semantic = false) ?limit source reference context =
  `Assoc
    ([
       ("resourceId", `String source);
       ("referenceId", `String reference);
       ("context", context);
     ]
    @ (if semantic then [] else [ ("useSemanticScoring", `Bool false) ])
    @ match limit with Some n -> [ ("limit", `Int n) ] | None -> [])

let candidates found = Check.list found [ "response" ]

let page_of (w : W.t) body =
  Check.at
    (W.answered w "match:resources-requested" (`Assoc body))
    [ "response" ]

let empty_page ~offset =
  `Assoc
    [
      ("resources", `List []);
      ("total", `Int 0);
      ("offset", `Int offset);
      ("limit", `Int 50);
      ("matchKind", `String "lexical");
    ]

(* ── candidates for a reference ─────────────────────────────────────── *)

let four_sources (w : W.t) =
  let quoted = "bittern booms from the sedge" in
  let by_name =
    W.created w "A page about one bird of the reedbed.\n"
      ~name:"Bittern booms from the sedge"
  in
  let by_type =
    W.created w "A drawing of a brown bird among the reeds.\n"
      ~name:"Plate nine"
  in
  W.tagged w by_type "Marshbird";
  (* Its one chunk is the quoted text itself, and its name has no word of
     it: only the index finds it. *)
  let by_vector = W.created w quoted ~name:"Recording four" in
  let neighbour =
    W.created w "Places of the fen, listed from north to south.\n"
      ~name:"Gazetteer of the fen"
  in
  let source =
    W.created w
      (Printf.sprintf "At dusk a %s, and the tide turns.\n" quoted)
      ~name:"Estuary notes"
  in
  (* A second reference of the source's, bound to the neighbour: the graph
     then connects the two. *)
  W.bound w source (W.referenced w source "the tide") neighbour;
  let reference = W.referenced w source quoted ~entity_types:[ "Marshbird" ] in
  W.ready w [ by_name; by_type; by_vector; neighbour; source ];
  Check.equal "its one chunk is the quoted text"
    (`List [ `String quoted ])
    (texts_of_points w by_vector);
  let context = gathered w source reference in
  let asked = Ollama_embed.mark w.embedder in
  let found =
    W.answered w "match:search-requested" (search_body source reference context)
  in
  Check.equal "the reference" (`String reference)
    (Check.at found [ "referenceId" ]);
  let of_ id = one_of "@id" id (candidates found) in
  Check.matching "by name"
    [
      ("@id", `String by_name); ("name", `String "Bittern booms from the sedge");
    ]
    (of_ by_name);
  reason_has "by name" (of_ by_name) "name match";
  Check.matching "by type"
    [
      ("@id", `String by_type);
      ("name", `String "Plate nine");
      ("entityTypes", `List [ `String "Marshbird" ]);
    ]
    (of_ by_type);
  reason_has "by type" (of_ by_type) "entity types: Marshbird";
  Check.matching "by vector"
    [ ("@id", `String by_vector); ("name", `String "Recording four") ]
    (of_ by_vector);
  reason_has "by vector" (of_ by_vector) "semantic similarity";
  Check.matching "the neighbour"
    [ ("@id", `String neighbour); ("name", `String "Gazetteer of the fen") ]
    (of_ neighbour);
  reason_has "the neighbour" (of_ neighbour) "connected";
  List.iter
    (fun one ->
      Check.that "a score that is a number"
        (match Protocol.at one [ "score" ] with
        | Some (`Int _ | `Float _) -> true
        | _ -> false);
      Check.that "a reason that is a string"
        (match Protocol.at one [ "matchReason" ] with
        | Some (`String _) -> true
        | _ -> false))
    (candidates found);
  (* The best candidate is first. *)
  let scores =
    List.map
      (fun one -> Yojson.Safe.Util.to_number (Check.at one [ "score" ]))
      (candidates found)
  in
  Check.that "the best candidate first"
    (scores = List.sort (fun a b -> compare b a) scores);
  (* The index was searched by the quoted text, embedded once; and no model
     was asked. *)
  Check.that "the quoted text embedded once"
    (embedded_texts w asked = [ [ quoted ] ]);
  Check.that "the Matcher's model was asked nothing"
    (Dynarray.length w.matcher.generations = 0)

let not_its_own (w : W.t) =
  let quoted = "dunlin turn as one over the mud" in
  (* Two resources whose one chunk is the quoted text: the reference's own,
     and another. *)
  let source = W.created w quoted ~name:"Shore log" in
  let twin = W.created w quoted ~name:"Fair copy of a log" in
  let reference = W.referenced w source quoted in
  W.ready w [ source; twin ];
  List.iter
    (fun resource ->
      Check.equal "its one chunk is the quoted text"
        (`List [ `String quoted ])
        (texts_of_points w resource))
    [ source; twin ];
  let context = gathered w source reference in
  let found =
    W.answered w "match:search-requested" (search_body source reference context)
  in
  reason_has "the twin"
    (one_of "@id" twin (candidates found))
    "semantic similarity";
  Check.that "the resource the reference is on is no candidate"
    (not (List.mem (`String source) (ids "@id" (candidates found))))

let model_scores (w : W.t) =
  let target =
    W.created w "A white wader with a flat bill.\n" ~name:"Spoonbill"
  in
  let source =
    W.created w "A spoonbill sweeps the shallows at first light.\n"
      ~name:"Dawn log"
  in
  let reference = W.referenced w source "spoonbill" in
  W.ready w [ target; source ];
  let context = gathered w source reference in
  (* The model scores the one candidate this case made, by the number the
     prompt gives it. *)
  w.matcher.choose <-
    Some
      (fun body ->
        match
          numbered Yojson.Safe.Util.(member "prompt" body |> to_string) target
        with
        | Some number ->
            Some
              (answer
                 (Printf.sprintf {|[{"candidate":%s,"score":0.9}]|} number))
        | None -> None);
  let found =
    W.answered w "match:search-requested"
      (search_body ~semantic:true source reference context)
  in
  (match Ollama.prompts w.matcher with
  | [ prompt ] ->
      Check.contains_text "the prompt" "spoonbill" prompt;
      Check.contains_text "the prompt" "Spoonbill" prompt;
      Check.contains_text "the prompt" target prompt
  | sent ->
      failwith
        (Printf.sprintf "the Matcher's model was asked %d times, not once"
           (List.length sent)));
  let body = fst (Dynarray.get w.matcher.generations 0) in
  Check.equal "the model asked" (`String W.matcher_model) (member "model" body);
  (* The scores are asked for as elements its provider holds to a schema,
     and not as lines of text. *)
  Check.that "the generation states a format"
    (Protocol.at body [ "format" ] <> None);
  let scored = one_of "@id" target (candidates found) in
  reason_has "the scored candidate" scored "semantic match";
  (* The same search with no model asked: the candidate scores less, and its
     reason does not say the model matched it. *)
  let plain =
    one_of "@id" target
      (candidates
         (W.answered w "match:search-requested"
            (search_body source reference context)))
  in
  Check.that "the plain candidate's reason does not say the model matched it"
    (not (Re.execp (Re.compile (Re.str "semantic match")) (reason plain)));
  let score c = Yojson.Safe.Util.to_number (Check.at c [ "score" ]) in
  Check.that "the scored candidate scores more" (score scored > score plain);
  Check.that "the model was asked once"
    (List.length (Ollama.prompts w.matcher) = 1)

let model_fails (w : W.t) =
  let target = W.created w "A small falcon of open moor.\n" ~name:"Merlin" in
  let source =
    W.created w "A merlin hunts low over the heather.\n" ~name:"Moor log"
  in
  let reference = W.referenced w source "merlin" in
  W.ready w [ target; source ];
  let context = gathered w source reference in
  w.matcher.choose <- Some (fun _ -> Some (Ollama.Fails (500, refusal_body)));
  let found =
    W.answered w "match:search-requested"
      (search_body ~semantic:true source reference context)
  in
  Check.that "its model was asked" (Ollama.prompts w.matcher <> []);
  Check.equal "the reference" (`String reference)
    (Check.at found [ "referenceId" ]);
  reason_has "the target" (one_of "@id" target (candidates found)) "name match";
  Check.that "no reason says the model matched"
    (List.for_all
       (fun one ->
         not (Re.execp (Re.compile (Re.str "semantic match")) (reason one)))
       (candidates found))

let limited (w : W.t) =
  let exact =
    W.created w "A black and white wader with an upturned bill.\n"
      ~name:"Avocet"
  in
  let longer =
    W.created w "Where they nest on the lagoon.\n" ~name:"Avocet colony"
  in
  let source =
    W.created w "An avocet sifts the brackish pool.\n" ~name:"Lagoon log"
  in
  let reference = W.referenced w source "avocet" in
  W.ready w [ exact; longer; source ];
  let context = gathered w source reference in
  let all =
    candidates
      (W.answered w "match:search-requested"
         (search_body source reference context))
  in
  Check.that "more than one candidate" (List.length all > 1);
  let one =
    candidates
      (W.answered w "match:search-requested"
         (search_body ~limit:1 source reference context))
  in
  Check.equal "the best, alone"
    (`List [ Check.at (List.hd all) [ "@id" ] ])
    (`List (ids "@id" one))

let focus_not_a_reference (w : W.t) =
  let target = W.created w "A diving duck of the open sea.\n" ~name:"Scoter" in
  let source =
    W.created w "A scoter rides the swell beyond the bar.\n" ~name:"Sea watch"
  in
  let reference = W.referenced w source "scoter" in
  W.ready w [ target; source ];
  let context = gathered w source reference in
  let view =
    match Archivist_world.view w.kb source with
    | Some view -> view
    | None -> failwith ("the record holds no view of " ^ source)
  in
  let asked = Ollama_embed.mark w.embedder in
  let failure =
    W.failed w "match:search-requested"
      (`Assoc
         [
           ("resourceId", `String source);
           ("referenceId", `String reference);
           ( "context",
             `Assoc
               [
                 ( "focus",
                   `Assoc
                     [
                       ("kind", `String "resource");
                       ("resource", Check.at view [ "resource" ]);
                     ] );
                 ("graph", Check.at context [ "graph" ]);
                 ("metadata", Check.at context [ "metadata" ]);
               ] );
         ])
  in
  Check.equal "the reference" (`String reference)
    (Check.at failure [ "referenceId" ]);
  Check.that "the failure is about the focus"
    (mentions "focus" (message failure)
    || mentions "annotation" (message failure));
  Check.that "nothing was embedded" (Ollama_embed.since w.embedder asked = []);
  Check.that "the Matcher's model was asked nothing"
    (Dynarray.length w.matcher.generations = 0)

let embedding_fails_candidates (w : W.t) =
  let quoted = "whimbrel passes over at night" in
  let by_name =
    W.created w "A page about one bird of passage.\n"
      ~name:"Whimbrel passes over at night"
  in
  (* The index would answer this one, were the quoted text embedded. *)
  let by_vector = W.created w quoted ~name:"Recording nine" in
  let source =
    W.created w
      (Printf.sprintf "In May a %s, calling.\n" quoted)
      ~name:"Night log"
  in
  let reference = W.referenced w source quoted in
  W.ready w [ by_name; by_vector; source ];
  let context = gathered w source reference in
  let asked = Ollama_embed.mark w.embedder in
  w.embedder.choose <-
    Some (fun _ -> Some (Ollama_embed.Fails (500, refusal_body)));
  let found =
    W.answered w "match:search-requested" (search_body source reference context)
  in
  Check.that "the provider was asked" (Ollama_embed.since w.embedder asked <> []);
  reason_has "by name" (one_of "@id" by_name (candidates found)) "name match";
  Check.that "the index gave no candidate"
    (not (List.mem (`String by_vector) (ids "@id" (candidates found))));
  Check.that "no reason says semantic similarity"
    (List.for_all
       (fun one ->
         not (Re.execp (Re.compile (Re.str "semantic similarity")) (reason one)))
       (candidates found))

(* ── resources by text ──────────────────────────────────────────────── *)

let later_page_lexical (w : W.t) =
  let text = "Sea lavender greys the creek banks" in
  let sheet = recorded w text "Sheet fourteen" in
  let first = page_of w [ ("search", `String text) ] in
  Check.equal "matchKind" (`String "semantic") (Check.at first [ "matchKind" ]);
  Check.that "the sheet is on the first page"
    (List.mem (`String sheet) (ids "@id" (Check.list first [ "resources" ])));
  let asked = Ollama_embed.mark w.embedder in
  Check.equal "the later page" (empty_page ~offset:1)
    (page_of w [ ("search", `String text); ("offset", `Int 1) ]);
  Check.that "nothing was embedded" (Ollama_embed.since w.embedder asked = [])

let blank (w : W.t) =
  let asked = Ollama_embed.mark w.embedder in
  Check.equal "the page" (empty_page ~offset:0)
    (page_of w [ ("search", `String " \t ") ]);
  Check.that "nothing was embedded" (Ollama_embed.since w.embedder asked = [])

let sorted_ids page =
  List.sort compare (ids "@id" (Check.list page [ "resources" ]))

let filters_lexical (w : W.t) =
  let tagged =
    W.created w "It traps midges on the wet peat.\n" ~name:"Sundew of the bog"
  in
  let shelved =
    W.created w "Drawn in July, in flower.\n" ~name:"Sundew in flower"
  in
  let plain =
    W.created w "Raised under glass from seed.\n" ~name:"Sundew seedlings"
  in
  W.tagged w tagged "Carnivore";
  W.ready w [ tagged; shelved; plain ];
  W.archived w shelved;
  W.woven w [ shelved ];
  let found filter =
    let page = page_of w (("search", `String "sundew") :: filter) in
    Check.equal "matchKind" (`String "lexical") (Check.at page [ "matchKind" ]);
    Check.equal "the total"
      (`Int (List.length (Check.list page [ "resources" ])))
      (Check.at page [ "total" ]);
    `List (sorted_ids page)
  in
  let sorted ids =
    `List (List.sort compare (List.map (fun id -> `String id) ids))
  in
  Check.equal "every sundew" (sorted [ tagged; shelved; plain ]) (found []);
  Check.equal "of the entity type" (sorted [ tagged ])
    (found [ ("entityType", `String "Carnivore") ]);
  Check.equal "archived" (sorted [ shelved ])
    (found [ ("archived", `Bool true) ]);
  Check.equal "not archived"
    (sorted [ tagged; plain ])
    (found [ ("archived", `Bool false) ])

let filters_semantic (w : W.t) =
  (* No name here has a word of either text, so a search by one of them
     matches nothing by name. *)
  let of_type = "Butterwort holds its prey on a sticky leaf" in
  let of_none = "Bladderwort floats free in the peat cutting" in
  let tagged = recorded w of_type "Sheet forty-one" in
  let plain = recorded w of_none "Sheet forty-two" in
  W.tagged w tagged "Bogplant";
  (* The Smelter writes a resource's entity types on its points when they
     change. *)
  let entity_types = Qdrant.payload_member "resources" "entityTypes" in
  Run.eventually
    (Printf.sprintf "the points of %s to carry its entity type" tagged) 20.0
    (fun () ->
      if
        List.for_all
          (fun p ->
            member entity_types (Qdrant.payload p)
            = `List [ `String "Bogplant" ])
          (Qdrant.points_of w.index "resources" tagged)
      then Some ()
      else None);
  let found search filter =
    let page = page_of w (("search", `String search) :: filter) in
    Check.equal "the total"
      (`Int (List.length (Check.list page [ "resources" ])))
      (Check.at page [ "total" ]);
    (Check.at page [ "matchKind" ], ids "@id" (Check.list page [ "resources" ]))
  in
  (* By meaning, each text finds its own resource. *)
  Check.that "the plain sheet is found by its text"
    (List.mem (`String plain) (snd (found of_none [])));
  Check.that "of the entity type, the tagged sheet alone, by meaning"
    (found of_type [ ("entityType", `String "Bogplant") ]
    = (`String "semantic", [ `String tagged ]));
  (* Kept to an entity type, a search does not answer a resource that has
     none, though the index holds its very text. *)
  let kind, kept = found of_none [ ("entityType", `String "Bogplant") ] in
  Check.equal "matchKind" (`String "semantic") kind;
  Check.that "the plain sheet is not answered"
    (not (List.mem (`String plain) kept));
  (* The index holds no archived resource: a search for them is the graph's
     answer, and nothing is embedded. *)
  let asked = Ollama_embed.mark w.embedder in
  Check.that "archived: the graph's empty answer"
    (found of_none [ ("archived", `Bool true) ] = (`String "lexical", []));
  Check.that "nothing was embedded" (Ollama_embed.since w.embedder asked = []);
  Check.that "not archived: the plain sheet"
    (List.mem (`String plain)
       (snd (found of_none [ ("archived", `Bool false) ])))

let hits_of page =
  List.map
    (fun hit ->
      `Assoc
        [
          ("resourceId", Check.at hit [ "@id" ]);
          ("passage", Option.value ~default:`Null (text_of hit));
        ])
    (Check.list page [ "resources" ])

let expected_hits expected =
  List.filteri (fun at _ -> at < 50) expected
  |> List.map (fun (id, passage) ->
      `Assoc [ ("resourceId", `String id); ("passage", `String passage) ])

let hit id passage =
  `Assoc [ ("resourceId", `String id); ("passage", `String passage) ]

let semantic_floor (w : W.t) =
  let text = "Samphire is cut on the flats in June" in
  let below = text_near text 0.3 0.5 in
  let above = text_near text 0.7 0.95 in
  let exact = recorded w text "Sheet thirty-one" in
  let below_floor = recorded w below "Sheet thirty-two" in
  let above_floor = recorded w above "Sheet thirty-three" in
  (* The world's floor is 0.6: the resource at 0.7 or more is answered, with
     its own passage, and the one at 0.5 or less is not. *)
  let expected = within w text W.semantic_floor in
  let page = page_of w [ ("search", `String text) ] in
  Check.matching "the page"
    [
      ("total", `Int (List.length expected)); ("matchKind", `String "semantic");
    ]
    page;
  let hits = hits_of page in
  Check.equal "the hits" (`List (expected_hits expected)) (`List hits);
  Check.equal "the first hit" (hit exact text) (List.hd hits);
  Check.that "the sheet above the floor is answered"
    (List.mem (hit above_floor above) hits);
  Check.that "the sheet below the floor is not"
    (not
       (List.exists
          (fun h -> Protocol.at h [ "resourceId" ] = Some (`String below_floor))
          hits));
  (* The floor is the Librarian's setting: started with one of 0.2, it
     answers the resource it left out. *)
  W.restart_librarian w
    ~launch:(W.changed w [ "search"; "semanticFloor" ] (Some (`Float 0.2)));
  Fun.protect
    ~finally:(fun () -> W.restart_librarian w)
    (fun () ->
      let lower = within w text 0.2 in
      let wider = page_of w [ ("search", `String text) ] in
      Check.matching "the wider page"
        [
          ("total", `Int (List.length lower)); ("matchKind", `String "semantic");
        ]
        wider;
      let more = hits_of wider in
      Check.equal "the hits" (`List (expected_hits lower)) (`List more);
      Check.that "the sheet below the old floor is answered"
        (List.mem (hit below_floor below) more))

let embedding_fails_page (w : W.t) =
  let text = "Scurvy grass flowers under the sea wall" in
  (* The index would answer this one, were the text embedded. *)
  ignore (recorded w text "Sheet twenty-one" : string);
  let asked = Ollama_embed.mark w.embedder in
  w.embedder.choose <-
    Some (fun _ -> Some (Ollama_embed.Fails (500, refusal_body)));
  Check.equal "the page" (empty_page ~offset:0)
    (page_of w [ ("search", `String text) ]);
  Check.that "the provider was asked" (Ollama_embed.since w.embedder asked <> [])

let another_kb_token (w : W.t) =
  let text = "Sea purslane edges the saltings" in
  let sheet = recorded w text "Sheet forty" in
  let own = page_of w [ ("search", `String text) ] in
  Check.equal "matchKind" (`String "semantic") (Check.at own [ "matchKind" ]);
  Check.that "the sheet is found"
    (List.mem (`String sheet) (ids "@id" (Check.list own [ "resources" ])));
  (* A token signed by the Qdrant's key, which reads another knowledge
     base's collections alone: the index refuses it every search of this
     one's. *)
  W.restart_librarian w
    ~launch:
      (W.with_variable w Qdrant.token_variable
         (Some
            (Qdrant.minted w.stores.vectors.key
               (Qdrant.claims (other_kb ()) "librarian"))));
  Fun.protect
    ~finally:(fun () -> W.restart_librarian w)
    (fun () ->
      let asked = Ollama_embed.mark w.embedder in
      Check.equal "the page" (empty_page ~offset:0)
        (page_of w [ ("search", `String text) ]);
      (* The text was embedded: it is the index that answered nothing. *)
      Check.that "the text was embedded once"
        (embedded_texts w asked = [ [ text ] ]))

let pdf_preview (w : W.t) =
  let text = "It floats, and traps what swims past.\n" in
  let pdf =
    W.created w
      (Pdf.of_pages [ [ "A bladderwort in flower"; "drawn from the dyke" ] ])
      ~name:"Bladderwort plate" ~format:"application/pdf"
  in
  let notes = W.created w text ~name:"Bladderwort notes" in
  W.woven w [ pdf; notes ];
  let page = page_of w [ ("search", `String "bladderwort") ] in
  Check.matching "the page"
    [ ("total", `Int 2); ("matchKind", `String "lexical") ]
    page;
  let resources = Check.list page [ "resources" ] in
  Check.equal "the PDF's preview" (`String "")
    (Check.at (one_of "@id" pdf resources) [ "content" ]);
  Check.equal "the text's preview" (`String text)
    (Check.at (one_of "@id" notes resources) [ "content" ]);
  (* The Smelter has then decided both, and the index is still for the next
     case. *)
  ignore (W.settled w pdf : Yojson.Safe.t);
  ignore (W.settled w notes : Yojson.Safe.t)

(* ── the references bound to a resource ─────────────────────────────── *)

let referenced_by (w : W.t) ?motivation resource_id =
  W.answered w "gather:referenced-by-requested"
    (`Assoc
       (("resourceId", `String resource_id)
       ::
       (match motivation with
       | Some m -> [ ("motivation", `String m) ]
       | None -> [])))

let nothing_refers =
  `Assoc [ ("response", `Assoc [ ("referencedBy", `List []) ]) ]

let bound_reference (w : W.t) =
  let citing = W.created w fox ~name:"Ledger of the hunt" in
  let cited = W.created w tide ~name:"Table of the tides" in
  let reference = W.referenced w citing "fox" in
  W.bound w citing reference cited;
  W.woven w [ citing; cited ];
  Check.equal "what refers to the cited resource"
    (`Assoc
       [
         ( "response",
           `Assoc
             [
               ( "referencedBy",
                 `List
                   [
                     `Assoc
                       [
                         ("id", `String reference);
                         ("resourceName", `String "Ledger of the hunt");
                         ( "target",
                           `Assoc
                             [
                               ("source", `String citing);
                               ("selector", `Assoc [ ("exact", `String "fox") ]);
                             ] );
                       ];
                   ] );
             ] );
       ])
    (referenced_by w cited);
  (* The reference is in the citing resource, and nothing is bound to that
     one. *)
  Check.equal "what refers to the citing resource" nothing_refers
    (referenced_by w citing)

let one_motivation (w : W.t) =
  let citing = W.created w fox ~name:"Ledger of the chase" in
  let cited = W.created w tide ~name:"Table of the neaps" in
  let reference = W.referenced w citing "dog" in
  W.bound w citing reference cited;
  ignore (W.highlighted w citing "quick" : string);
  W.woven w [ citing; cited ];
  Check.equal "the linking references"
    (`List [ `String reference ])
    (`List
       (ids "id"
          (Check.list
             (referenced_by w ~motivation:"linking" cited)
             [ "response"; "referencedBy" ])));
  Check.equal "the highlighting references" nothing_refers
    (referenced_by w ~motivation:"highlighting" cited)

let nothing_refers_to (w : W.t) =
  let alone = W.created w tide ~name:"Table of the springs" in
  W.woven w [ alone ];
  Check.equal "what refers to it" nothing_refers (referenced_by w alone)

let nobody_made (w : W.t) =
  let failure =
    W.failed w "gather:referenced-by-requested"
      (`Assoc [ ("resourceId", `String "made-by-no-one") ])
  in
  Check.that "the failure says not found"
    (mentions "not found" (message failure))

(* ── the persons a page mentions ────────────────────────────────────── *)

let named_persons (w : W.t) =
  let text = "Thrift flowers pink along the shingle ridge" in
  let resource = W.created w text ~name:"Thrift on the shingle" ~by:"grace" in
  (* A person is given a name when they first write through the gateway. An
     upload goes to the Archivist, and gives none. *)
  W.tagged w resource "Coastal" ~by:"grace";
  let did = W.person_did w ~sub:"grace" () in
  let profile =
    Run.eventually (Printf.sprintf "the profiles projection to hold %s" did)
      10.0 (fun () ->
        Option.bind (Archivist_world.projection w.kb "profiles.json") (fun p ->
            Protocol.at p [ "profiles"; did ]))
  in
  W.ready w [ resource ];
  Check.equal "its one chunk is the text"
    (`List [ `String text ])
    (texts_of_points w resource);
  (* The record says who, and gives no name. *)
  Check.equal "who the record says made it"
    (`List [ `Assoc [ ("@type", `String "Person"); ("@id", `String did) ] ])
    (Check.at
       (member "payload" (List.hd (Archivist_world.stored w.kb resource)))
       [ "wasAttributedTo" ]);
  let named =
    `List
      [
        `Assoc
          [
            ("@type", `String "Person");
            ("@id", `String did);
            ("name", Check.at profile [ "name" ]);
          ];
      ]
  in
  let lexical = page_of w [ ("search", `String "thrift") ] in
  Check.equal "matchKind" (`String "lexical") (Check.at lexical [ "matchKind" ]);
  Check.equal "who made it, named" named
    (Check.at
       (one_of "@id" resource (Check.list lexical [ "resources" ]))
       [ "wasAttributedTo" ]);
  let semantic = page_of w [ ("search", `String text) ] in
  Check.equal "matchKind" (`String "semantic")
    (Check.at semantic [ "matchKind" ]);
  Check.equal "who made it, named" named
    (Check.at
       (one_of "@id" resource (Check.list semantic [ "resources" ]))
       [ "wasAttributedTo" ]);
  (* A candidate of a reference is named too. The reference quotes the text,
     so the index finds the resource. *)
  let source =
    W.created w (Printf.sprintf "Noted in June: %s.\n" text) ~name:"Shore notes"
  in
  let reference = W.referenced w source text in
  W.ready w [ source ];
  let context = gathered w source reference in
  let found =
    W.answered w "match:search-requested" (search_body source reference context)
  in
  Check.equal "who made the candidate, named" named
    (Check.at (one_of "@id" resource (candidates found)) [ "wasAttributedTo" ])

(* ── the limits of its engines ──────────────────────────────────────── *)

let showed (o : Ollama.t) model =
  Dynarray.exists
    (fun body -> Protocol.at body [ "model" ] = Some (`String model))
    o.shows

let own_limits (w : W.t) =
  (* A Librarian may keep what a provider showed it, so it is started again
     to be shown these. *)
  w.matcher.show <- Context 4096;
  w.gatherer.show <- Context 2048;
  W.restart_librarian w;
  Fun.protect
    ~finally:(fun () ->
      (* The next case's Librarian has been shown nothing. *)
      W.restart_librarian w)
    (fun () ->
      Check.equal "the Matcher's limits"
        (limits_of W.matcher_model 4096)
        (W.answered w "match:limits-requested" (`Assoc []));
      Check.equal "the Gatherer's limits"
        (limits_of W.gatherer_model 2048)
        (W.answered w "gather:limits-requested" (`Assoc []));
      (* Each provider was asked about its own actor's model, and no other. *)
      Check.that "the Matcher's provider was asked of its model"
        (showed w.matcher W.matcher_model);
      Check.that "and of no other" (not (showed w.matcher W.gatherer_model));
      Check.that "the Gatherer's provider was asked of its model"
        (showed w.gatherer W.gatherer_model);
      Check.that "and of no other" (not (showed w.gatherer W.matcher_model)))

let provider_silent (w : W.t) =
  w.matcher.show <- Drops;
  w.gatherer.show <- Refuses 500;
  W.restart_librarian w;
  let no_limits = `Assoc [ ("response", `Assoc [ ("limits", `List []) ]) ] in
  Check.equal "the Matcher's limits" no_limits
    (W.answered ~timeout:5.0 w "match:limits-requested" (`Assoc []));
  Check.equal "the Gatherer's limits" no_limits
    (W.answered ~timeout:5.0 w "gather:limits-requested" (`Assoc []));
  Check.that "the Matcher's provider was asked"
    (Dynarray.length w.matcher.shows > 0);
  Check.that "the Gatherer's provider was asked"
    (Dynarray.length w.gatherer.shows > 0);
  w.matcher.show <- Context 8192;
  w.gatherer.show <- Context 8192;
  Check.equal "the Matcher's limits"
    (limits_of W.matcher_model 8192)
    (W.answered ~timeout:5.0 w "match:limits-requested" (`Assoc []));
  Check.equal "the Gatherer's limits"
    (limits_of W.gatherer_model 8192)
    (W.answered ~timeout:5.0 w "gather:limits-requested" (`Assoc []))

let tests implementations =
  let world = world_of "search" implementations in
  let case group ?(max_duration = 120.0) name body =
    Testo.create ~category:[ "a Librarian, asked to find"; group ]
      ~max_duration name (fun () -> W.end_case world body)
  in
  let candidates = case "candidates for a reference"
  and by_text = case "resources by text"
  and bound = case "the references bound to a resource"
  and persons = case "the persons a page mentions"
  and limits = case "the limits of its engines" in
  [
    candidates
      "answers a candidate from each of its four sources, each a description \
       of its resource with a score and a reason; told not to score by \
       meaning, it asks no model"
      four_sources;
    candidates
      "does not answer the resource the reference is on, though the index \
       holds its very text"
      not_its_own;
    candidates
      "has its model score the candidates in one prompt, which states the \
       passage and lists each candidate; a candidate it scores above a half \
       says so in its reason"
      model_scores;
    candidates
      "answers all the same when its model fails: the candidates are scored \
       without it, and no reason says the model matched"
      model_fails;
    candidates
      "answers no more candidates than the limit, and those are the best"
      limited;
    candidates
      "fails a search whose context was gathered for a resource and not for a \
       reference: the failure names the reference and is about the focus, and \
       nothing is searched"
      focus_not_a_reference;
    candidates
      "answers from its other sources when the embedding provider fails: the \
       index gives no candidate, and the one found by name is answered"
      embedding_fails_candidates;
    by_text
      "answers a page of the resources the text matches by name, labelled \
       `lexical`, each with a preview of its text; and asks for no embedding"
      lexical_page;
    by_text
      "answers a search that no name matches from the index, labelled \
       `semantic`: each hit carries the passage that matched, the nearest \
       first, and the text is embedded once"
      semantic_page;
    by_text
      "answers a later page of such a search with nothing, labelled `lexical`, \
       and asks for no embedding: the index answers a first page only"
      later_page_lexical;
    by_text "answers a blank text with nothing, and asks for no embedding" blank;
    by_text
      "keeps to the resources of an entity type, and to those that are \
       archived or those that are not, where the search says so"
      filters_lexical;
    by_text
      "keeps a page from the index to the entity type the search says, and \
       answers a search for archived resources from the graph alone"
      filters_semantic;
    by_text ~max_duration:240.0
      "leaves out of a page from the index a resource below the semantic floor \
       it was started with, and answers it under a lower floor"
      semantic_floor;
    by_text
      "answers the empty lexical page, and not a failure, when the embedding \
       provider fails"
      embedding_fails_page;
    by_text ~max_duration:180.0
      "answers the empty lexical page, and not a failure, when its token reads \
       another knowledge base's index and none of its own"
      another_kb_token;
    by_text
      "previews a PDF it finds by name as the empty string, beside a text that \
       previews as itself"
      pdf_preview;
    bound
      "answers a reference that is bound to the resource, with the name of the \
       resource it is in and the text it quotes"
      bound_reference;
    bound
      "keeps to the references of one motivation, where the request states one"
      one_motivation;
    bound "answers an empty list for a resource nothing refers to"
      nothing_refers_to;
    bound
      "fails for a resource nobody made: that is no resource nothing refers to"
      nobody_made;
    persons
      "names who made a resource as the profiles projection holds them, on a \
       lexical page and on a semantic one, where the record carries no name"
      named_persons;
    limits ~max_duration:240.0
      "answers each limits request with the limits of its own actor's engine, \
       as that engine's provider shows them"
      own_limits;
    limits ~max_duration:180.0
      "leaves out a model whose provider does not answer for it, and still \
       answers with a result within a few seconds; the model is reported again \
       once its provider answers"
      provider_silent;
  ]
