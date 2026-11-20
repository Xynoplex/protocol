(* linking.test.ts: a mark job of motivation linking, end to end
   (WORKER-SERVICE.md § Detection, § The five kinds of mark job): one unit
   for each entity type, each an extraction and a count of the same text,
   committed and checkpointed as it finishes. *)

open Xynolog_conformance
open Support
open Case

(* The size, in tokens, a linking job cuts [text] at: what its units'
   cursors state. *)
let chunk_size = 2658
let model = (List.hd W.ollama_engines).model

(* The cursor of a unit that took [text] as one piece and emitted all it
   found. *)
let whole found =
  cursor ~next:(code_points text) ~size:chunk_size ~found ~emitted:found
    ~errors:0

(* What a linking job echoes of what it was asked with, on its reports: the
   entity types it names, as [types]. *)
let request_params types =
  ( "requestParams",
    `List [ `Assoc [ ("label", str "entity-types"); ("value", str types) ] ] )

(* An entity type among the finished, with what it found and recorded. *)
let completed_item ?under_reported kind ~found ~persisted =
  `Assoc
    ([
       ("value", str kind);
       ("foundCount", `Int found);
       ("persistedCount", `Int persisted);
     ]
    @
    match under_reported with
    | Some (pieces, found, counted) ->
        [
          ( "underReported",
            `Assoc
              [
                ("pieces", `Int pieces);
                ("found", `Int found);
                ("counted", `Int counted);
              ] );
        ]
    | None -> [])

(* A report of where [job] stands while it works on the entity type
   [kind], one of [total]. [expected] is left out until a count has said
   it. *)
let standing ~request_params ~total job percentage kind ~processed ~found
    ?expected ~emitted completed =
  report job percentage
    (code ~members:[ ("entityType", str kind) ] "detecting-entities")
    ~extra:
      ([
         ("current", `Assoc [ ("kind", str "entity-type"); ("value", str kind) ]);
         ("processed", `Int processed);
         ("total", `Int total);
         ("entitiesFound", `Int found);
       ]
      @ (match expected with
        | Some n -> [ ("entitiesExpected", `Int n) ]
        | None -> [])
      @ [
          ("entitiesEmitted", `Int emitted);
          ("completedItems", `List completed);
          request_params;
        ])

(* The report of a linking job that finished, having made [count]. *)
let finished_report ~request_params job ~count ~expected completed =
  report job 100
    (code
       ~members:[ ("count", `Int count); ("motivation", str "linking") ]
       "complete-created")
    ~extra:
      [
        ("entitiesExpected", `Int expected);
        ("completedItems", `List completed);
        request_params;
      ]

let sizes_of_batches (w : W.t) =
  `List
    (List.map
       (fun (c : W.commit) -> `Int (List.length c.annotations))
       (Dynarray.to_list w.own.commits))

let spans items = Yojson.Safe.to_string (`List items)

let mention ?prefix ?suffix exact kind =
  `Assoc
    ([ ("exact", str exact); ("entityType", str kind) ]
    @ (match prefix with Some p -> [ ("prefix", str p) ] | None -> [])
    @ match suffix with Some s -> [ ("suffix", str s) ] | None -> [])

let each_type_in_turn (w : W.t) =
  let job =
    mark_job w.own "linking"
      (`Assoc
         [
           ("motivation", str "linking");
           ("entityTypes", strs [ "Person"; "Place" ]);
         ])
  in
  let resource_id = job.resource_id in
  Ollama.script w.ollama
    [
      answer
        (spans
           [
             mention "Ada Lovelace" "Person" ~prefix:"" ~suffix:" published";
             mention "Charles Babbage" "Person";
           ]);
      counting 2;
      answer
        (spans
           [
             mention "London" "Place" ~prefix:"engine in " ~suffix:", but";
             mention "London" "Place" ~prefix:"standards, and "
               ~suffix:" ignored";
             mention "London" "Place" ~prefix:"ignored it. "
               ~suffix:" was wrong";
           ]);
      counting 3;
    ];
  let served = W.start w in
  let completion = settled served job in
  (* An Ollama model is asked one thing at a time: the types run in the
     order the job names them. *)
  expect_generations w.ollama
    [
      generation model "linking-person";
      generation model "linking-person-count";
      generation model "linking-place";
      generation model "linking-place-count";
    ];
  expect_sequence served
    [
      "emit job:claim";
      "emit job:start";
      "emit browse:resource-requested";
      "GET /resources/" ^ resource_id;
      "emit mark:commit";
      "emit job:checkpoint";
      "emit job:checkpoint";
      "emit mark:commit";
      "emit job:checkpoint";
      "emit job:checkpoint";
      "emit job:complete";
      "emit job:claim";
    ];
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
                   text_annotation generator resource_id "linking"
                     "_qdCaMUODSin_sHZFtof7" ~start:0 ~stop:12
                     ~suffix:
                       " published the first program in 1843 \u{2014} a method \
                        for computing Bernoulli"
                     ~body:(entity "Person") "Ada Lovelace";
                   text_annotation generator resource_id "linking"
                     "89Jk8b_cyDcqcecXwQwaP" ~start:119 ~stop:134
                     ~prefix:
                       "method for computing Bernoulli numbers on the \
                        Analytical Engine.\n\n"
                     ~suffix:
                       " designed the engine in London, but it was never \
                        built. Lovelace"
                     ~body:(entity "Person") "Charles Babbage";
                 ] );
           ];
         `Assoc
           [
             ("resourceId", str resource_id);
             ("jobId", str job.id);
             ( "annotations",
               `List
                 [
                   (* The same six letters three times: three references,
                      each where the model said it was. *)
                   text_annotation generator resource_id "linking"
                     "OjagYqpBXoXouWpVXL3Jq" ~start:158 ~stop:164
                     ~prefix:
                       "on the Analytical Engine.\n\n\
                        Charles Babbage designed the engine in "
                     ~suffix:
                       ", but it was never built. Lovelace argued that the \
                        engine could manipulate"
                     ~body:(entity "Place") "London";
                   text_annotation generator resource_id "linking"
                     "AjMlDd0jqNBcBCAYUepkL" ~start:354 ~stop:360
                     ~prefix:
                       "claim for a century.\n\n\
                        The program was na\u{00ef}ve by later standards, and "
                     ~suffix:" ignored it. London was wrong.\n"
                     ~body:(entity "Place") "London";
                   text_annotation generator resource_id "linking"
                     "5Ub7ESqaglP5YEBeuDNBO" ~start:373 ~stop:379
                     ~prefix:
                       "The program was na\u{00ef}ve by later standards, and \
                        London ignored it. "
                     ~suffix:" was wrong.\n" ~body:(entity "Place") "London";
                 ] );
           ];
       ])
    (committed served);
  (* A unit's cursor from its first batch, and the unit among the finished
     once its last batch is on the record. A finished unit keeps its cursor:
     every checkpoint after carries it. *)
  let person = ("Person", whole 2) and place = ("Place", whole 3) in
  Check.equal "its checkpoints"
    (`List
       [
         checkpoint job [ person ];
         checkpoint job ~completed:[ "Person" ] [ person ];
         checkpoint job ~completed:[ "Person" ] [ person; place ];
         checkpoint job ~completed:[ "Person"; "Place" ] [ person; place ];
       ])
    (checkpoints served);
  let request_params = request_params "Person, Place" in
  let person = completed_item "Person" ~found:2 ~persisted:2
  and place = completed_item "Place" ~found:3 ~persisted:3 in
  let standing = standing ~request_params ~total:2 job in
  expect_progress served job
    [
      report job 10 (code "loading") ~extra:[ request_params ];
      (* Person: begun, counted, committed, finished. *)
      standing 20 "Person" ~processed:0 ~found:0 ~emitted:0 [];
      standing 20 "Person" ~processed:0 ~found:0 ~expected:2 ~emitted:0 [];
      standing 20 "Person" ~processed:0 ~found:2 ~expected:2 ~emitted:2 [];
      standing 50 "Person" ~processed:1 ~found:2 ~expected:2 ~emitted:2
        [ person ];
      (* Place: the same four. *)
      standing 50 "Place" ~processed:1 ~found:2 ~expected:2 ~emitted:2
        [ person ];
      standing 50 "Place" ~processed:1 ~found:2 ~expected:5 ~emitted:2
        [ person ];
      standing 50 "Place" ~processed:1 ~found:5 ~expected:5 ~emitted:5
        [ person ];
      standing 80 "Place" ~processed:2 ~found:5 ~expected:5 ~emitted:5
        [ person; place ];
      finished_report ~request_params job ~count:5 ~expected:5 [ person; place ];
    ];
  Check.equal "its completion"
    (identified job
       [
         ("result", `Assoc [ ("found", `Int 5); ("persisted", `Int 5) ]);
         ("durability", str "acknowledged");
       ])
    completion

let other_type (w : W.t) =
  let job = mark_job w.own "linking-other-type" (params "linking-person") in
  let resource_id = job.resource_id in
  Ollama.script w.ollama
    [
      (* Asked for people: a person, a place, and a person spelled as no type
         that was asked for. *)
      answer
        (spans
           [
             mention "Ada Lovelace" "Person";
             mention "London" "Place" ~prefix:"engine in " ~suffix:", but";
             mention "Charles Babbage" "person";
           ]);
      counting 2;
    ];
  let served = W.start w in
  let completion = settled served job in
  Check.equal "what it committed"
    (`List
       [
         `List
           [
             text_annotation (W.generator w.own) resource_id "linking"
               "-QGhpbszCgntubnrm_6iV" ~start:0 ~stop:12
               ~suffix:
                 " published the first program in 1843 \u{2014} a method for \
                  computing Bernoulli"
               ~body:(entity "Person") "Ada Lovelace";
           ];
       ])
    (batches served);
  let person =
    ( "Person",
      cursor ~next:(code_points text) ~size:chunk_size ~found:3 ~emitted:1
        ~errors:2 )
  in
  Check.equal "its checkpoints"
    (`List
       [
         checkpoint job [ person ];
         checkpoint job ~completed:[ "Person" ] [ person ];
       ])
    (checkpoints served);
  (* Three proposed, one recorded, two that made nothing. *)
  Check.equal "its completion"
    (identified job
       [
         ( "result",
           `Assoc
             [ ("found", `Int 3); ("persisted", `Int 1); ("errors", `Int 2) ] );
         ("durability", str "acknowledged");
       ])
    completion

let unchecked (w : W.t) =
  let job = mark_job w.own "linking-unchecked" (params "linking-place") in
  Ollama.script w.ollama
    [
      answer
        (spans
           [ mention "London" "Place" ~prefix:"engine in " ~suffix:", but" ]);
      (* Words, cut off at the count's limit before any number: no count. *)
      Ollama.Answer
        {
          response = "There are several mentions of places in";
          stop = Cut_off;
          usage = None;
        };
    ];
  let served = W.start w in
  let completion = settled served job in
  (* A count that answers no count is not asked again, and what the
     extraction found is committed. *)
  expect_generations w.ollama
    [ generation model "linking-place"; generation model "linking-place-count" ];
  Check.equal "the sizes of its batches" (`List [ `Int 1 ]) (sizes_of_batches w);
  (* Nothing was counted: the reports state no expectation, and the type no
     under-report. *)
  let finished =
    match
      List.filter
        (fun p -> canonical (member "percentage" p) = canonical (`Int 100))
        (W.progress served job.id)
    with
    | [ one ] -> member "progress" one
    | reports ->
        failwith
          (Printf.sprintf "it reported %d times at 100 percent, not once"
             (List.length reports))
  in
  Check.that
    ("its finished report states no expectation: " ^ Check.shown finished)
    (match finished with
    | `Assoc members -> not (List.mem_assoc "entitiesExpected" members)
    | _ -> false);
  Check.equal "the types its finished report names"
    (`List [ completed_item "Place" ~found:1 ~persisted:1 ])
    (Check.at finished [ "completedItems" ]);
  Check.equal "its completion"
    (identified job
       [
         ( "result",
           `Assoc
             [
               ("found", `Int 1);
               ("persisted", `Int 1);
               ("uncheckedPieces", `Int 1);
             ] );
         ("durability", str "acknowledged");
       ])
    completion

let under_reported (w : W.t) =
  let job = mark_job w.own "linking-under-reported" (params "linking-place") in
  Ollama.script w.ollama
    [
      answer
        (spans
           [ mention "London" "Place" ~prefix:"engine in " ~suffix:", but" ]);
      (* More than twice what the extraction found. *)
      counting 9;
    ];
  let served = W.start w in
  let completion = settled served job in
  (* The text is one piece, so there is no half to ask for instead: asked
     once, and counted once. *)
  expect_generations w.ollama
    [ generation model "linking-place"; generation model "linking-place-count" ];
  Check.equal "the sizes of its batches" (`List [ `Int 1 ]) (sizes_of_batches w);
  let request_params = request_params "Place" in
  let place =
    completed_item "Place" ~found:1 ~persisted:1 ~under_reported:(1, 1, 9)
  in
  let standing percentage ~processed ~found ?expected completed =
    standing ~request_params ~total:1 job percentage "Place" ~processed ~found
      ?expected ~emitted:found completed
  in
  expect_progress served job
    [
      report job 10 (code "loading") ~extra:[ request_params ];
      standing 20 ~processed:0 ~found:0 [];
      standing 20 ~processed:0 ~found:0 ~expected:9 [];
      standing 20 ~processed:0 ~found:1 ~expected:9 [];
      standing 80 ~processed:1 ~found:1 ~expected:9 [ place ];
      finished_report ~request_params job ~count:1 ~expected:9 [ place ];
    ];
  Check.equal "its completion"
    (identified job
       [
         ( "result",
           `Assoc
             [
               ("found", `Int 1);
               ("persisted", `Int 1);
               ("underReportedPieces", `Int 1);
             ] );
         ("durability", str "acknowledged");
       ])
    completion

let at_the_boundary (w : W.t) =
  w.ollama.show <- Ollama.Context middling_context_length;
  let job =
    mark_job ~about:long_text w.own "linking-pieces" (params "linking-person")
  in
  let resource_id = job.resource_id in
  (* The text is two pieces, each an extraction and a count. Neither answer
     names a marker of the overlap. *)
  Ollama.script w.ollama
    [
      answer
        (spans [ mention "marker 103" "Person"; mention "marker 601" "Person" ]);
      counting 2;
      answer (spans [ mention "marker 702" "Person" ]);
      counting 1;
    ];
  let served = W.start w in
  let completion = settled served job in
  Check.equal "the generations its model was asked for" (`Int 4)
    (`Int (Dynarray.length w.ollama.generations));
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
      "emit job:checkpoint";
      "emit job:complete";
      "emit job:claim";
    ];
  Check.equal "the sizes of its batches"
    (`List [ `Int 2; `Int 1 ])
    (sizes_of_batches w);
  (* The first piece ends where the sixth paragraph does, and the second
     starts 256 code points before that. *)
  let person next found =
    ("Person", cursor ~next ~size:728 ~found ~emitted:found ~errors:0)
  in
  let length = code_points long_text in
  Check.equal "its checkpoints"
    (`List
       [
         checkpoint job [ person 2628 2 ];
         checkpoint job [ person length 3 ];
         checkpoint job ~completed:[ "Person" ] [ person length 3 ];
       ])
    (checkpoints served);
  let request_params = request_params "Person" in
  let person = completed_item "Person" ~found:3 ~persisted:3 in
  let standing percentage ~processed ~found ?expected completed =
    standing ~request_params ~total:1 job percentage "Person" ~processed ~found
      ?expected ~emitted:found completed
  in
  expect_progress served job
    [
      report job 10 (code "loading") ~extra:[ request_params ];
      standing 20 ~processed:0 ~found:0 [];
      (* The first piece: counted, and committed. *)
      standing 20 ~processed:0 ~found:0 ~expected:2 [];
      standing 20 ~processed:0 ~found:2 ~expected:2 [];
      (* The boundary. Text remains, and nothing has moved that a linking
         job's report states: the same report, a second time. *)
      standing 20 ~processed:0 ~found:2 ~expected:2 [];
      (* The second piece: counted, and committed. It is the text's last, so
         no boundary follows it. *)
      standing 20 ~processed:0 ~found:2 ~expected:3 [];
      standing 20 ~processed:0 ~found:3 ~expected:3 [];
      standing 80 ~processed:1 ~found:3 ~expected:3 [ person ];
      finished_report ~request_params job ~count:3 ~expected:3 [ person ];
    ];
  Check.equal "its completion"
    (identified job
       [
         ("result", `Assoc [ ("found", `Int 3); ("persisted", `Int 3) ]);
         ("durability", str "acknowledged");
       ])
    completion

let tests implementations =
  in_world implementations "linking" ~category:[ "a linking job" ]
    [
      case
        "extracts and counts each entity type in turn, commits each type's \
         references, and checkpoints each type as it finishes"
        each_type_in_turn;
      case
        "makes no annotation of a mention whose entity type is not the one \
         asked for, and counts it among what was found and what made nothing"
        other_type;
      case
        "keeps what a piece found when its count answers no count, and counts \
         the piece in its result as unchecked"
        unchecked;
      case
        "keeps what a piece too small to halve found, when the count says \
         mentions were missed, and says so in its result and its reports"
        under_reported;
      case
        "says where it stands once more at the boundary between two pieces of \
         the text"
        at_the_boundary;
    ]
