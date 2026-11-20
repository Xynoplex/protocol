(* resume.test.ts: a job resumed from the checkpoint an earlier attempt left
   (WORKER-SERVICE.md § Resuming): a unit partway is taken up at its cursor,
   a finished unit is not done again and is counted by its cursor, and
   nothing is committed twice. *)

open Xynolog_conformance
open Support
open Case

(* What every lifecycle message of [job] carries, said of attempt [n], with
   [members] after it. *)
let of_attempt n job members =
  `Assoc
    (List.map
       (fun (k, v) -> if k = "attempt" then (k, `Int n) else (k, v))
       (identity_members job)
    @ members)

(* The entity types of the linking jobs here. *)
let types = [ "Person"; "Place" ]
let on_place job = detecting ~types job "Place"

let linking_job (w : W.t) name metadata =
  mark_job w.own name ~metadata (linking types)

(* The extraction and the count of Place, and no more. *)
let place_asked () =
  [ generation model "linking-place"; generation model "linking-place-count" ]

let london_after_engine generator resource_id id =
  text_annotation generator resource_id "linking" id ~start:158 ~stop:164
    ~prefix:
      "on the Analytical Engine.\n\nCharles Babbage designed the engine in "
    ~suffix:
      ", but it was never built. Lovelace argued that the engine could \
       manipulate"
    ~body:(entity "Place") "London"

let first_london = mention "London" "Place" ~prefix:"engine in " ~suffix:", but"

let completion_of ?(errors = []) n job ~found ~persisted =
  of_attempt n job
    [
      ( "result",
        `Assoc
          ([ ("found", `Int found); ("persisted", `Int persisted) ] @ errors) );
      ("durability", str "acknowledged");
    ]

let expect_start served n job =
  Check.equal "its start"
    (`List [ of_attempt n job [] ])
    (`List (W.payloads served "job:start"))

let at_cursor (w : W.t) =
  w.ollama.show <- Ollama.Context small_context_length;
  (* The earlier attempt got through two pieces of 311 tokens: four
     proposed, three recorded. *)
  let job =
    mark_job ~about:long_text w.own "resume"
      ~metadata:
        [
          ("retryCount", `Int 1);
          ( "unitCursors",
            `Assoc
              [
                ( "highlighting",
                  cursor ~next:1666 ~size:small_highlighting_piece ~found:4
                    ~emitted:3 ~errors:0 );
              ] );
        ]
      (params "resume-1")
  in
  let resource_id = job.resource_id in
  Ollama.script w.ollama
    [ finding [ 502 ]; finding []; finding [ 701 ]; finding [] ];
  let served = W.start w in
  let completion = settled served job in
  (* It says which attempt this is. *)
  expect_start served 2 job;
  (* Nothing before the cursor is asked for again, and the pieces are smaller
     than the attempt that died cut them: seven tenths the size. *)
  expect_generations w.ollama
    (List.map
       (fun name -> generation ~context_length:small_context_length model name)
       [ "resume-1"; "resume-2"; "resume-3"; "resume-4" ]);
  Dynarray.iter
    (fun (asked, _) ->
      let prompt = text_of (member "prompt" asked) in
      Check.that
        (Printf.sprintf "a prompt it sent carries marker 205: %s" prompt)
        (not (Re.execp (Re.compile (Re.str "marker 205")) prompt)))
    w.ollama.generations;
  (* Nothing the earlier attempt committed is committed again. *)
  let generator = W.generator w.own in
  Check.equal "what it committed"
    (`List
       [
         `List
           [
             marker_highlight generator resource_id "kt2ObciadyGT8niATrisW" 502
               2055;
           ];
         `List
           [
             marker_highlight generator resource_id "FfDja00V2TxfmuDTppb2d" 701
               2937;
           ];
       ])
    (batches served);
  let at next ~found ~emitted =
    checkpoint job
      [ ("highlighting", cursor ~next ~size:217 ~found ~emitted ~errors:0) ]
  in
  Check.equal "its checkpoints"
    (`List
       [
         at 2147 ~found:5 ~emitted:4;
         at 2628 ~found:5 ~emitted:4;
         at 3109 ~found:6 ~emitted:5;
         at (code_points long_text) ~found:6 ~emitted:5;
       ])
    (checkpoints served);
  expect_progress served job
    [
      report job 10 (code "loading");
      report job 30 (code "analyzing");
      creating job 4;
      report job 47 (code "analyzing");
      creating job 4;
      report job 50 (code "analyzing");
      creating job 5;
      report job 54 (code "analyzing");
      creating job 5;
      complete_created job "highlighting" 5;
    ];
  (* The counts are the whole text's: the earlier attempt's four and three,
     and this one's two. *)
  Check.equal "its completion"
    (completion_of 2 job ~found:6 ~persisted:5)
    completion

let finished_not_again (w : W.t) =
  let job =
    linking_job w "resume-units"
      [ ("retryCount", `Int 2); ("completedUnits", strs [ "Person" ]) ]
  in
  let resource_id = job.resource_id in
  Ollama.script w.ollama [ proposed [ first_london ]; counting 1 ];
  let served = W.start w in
  let completion = settled served job in
  expect_start served 3 job;
  (* Only the unfinished type is asked for: its extraction, and its count. *)
  expect_generations w.ollama (place_asked ());
  let commits = Check.list (batches served) [] in
  Check.equal "how many batches it committed" (`Int 1)
    (`Int (List.length commits));
  let annotations = Check.list (List.hd commits) [] in
  Check.equal "the entity types of its batch" (strs [ "Place" ])
    (`List
       (List.map
          (fun a -> Check.at (List.hd (Check.list a [ "body" ])) [ "value" ])
          annotations));
  Check.equal "where its first annotation is"
    (`Assoc
       [
         ("type", str "TextPositionSelector");
         ("start", `Int 158);
         ("end", `Int 164);
       ])
    (List.hd (Check.list (List.hd annotations) [ "target"; "selector" ]));
  Check.equal "the text there" (str "London")
    (str (code_point_slice text 158 164));
  Check.equal "its resource" (str "res-ws-resume-units") (str resource_id);
  (* The type this attempt finishes is named, with its cursor. The other is
     on the record already, and the job was claimed with no cursor for
     it. *)
  let place =
    ( "Place",
      cursor ~next:(code_points text) ~size:linking_piece ~found:1 ~emitted:1
        ~errors:0 )
  in
  Check.equal "its checkpoints"
    (`List
       [
         checkpoint job [ place ];
         checkpoint job ~completed:[ "Place" ] [ place ];
       ])
    (checkpoints served);
  (* Both types are the job's, and one of them is finished from the first
     report. Nothing says what it found: it is not listed, and adds
     nothing. *)
  let place_item = finished_type "Place" ~found:1 ~persisted:1 in
  expect_progress served job
    [
      report job 10 (code "loading") ~extra:[ echoed_entity_types types ];
      on_place job 50 ~processed:1 ~found:0 ~emitted:0 [];
      on_place job 50 ~expected:1 ~processed:1 ~found:0 ~emitted:0 [];
      on_place job 50 ~expected:1 ~processed:1 ~found:1 ~emitted:1 [];
      on_place job 80 ~expected:1 ~processed:2 ~found:1 ~emitted:1
        [ place_item ];
      linking_complete ~types job ~count:1 ~expected:1 [ place_item ];
    ];
  Check.equal "its completion"
    (completion_of 3 job ~found:1 ~persisted:1)
    completion

let finished_by_cursor (w : W.t) =
  (* The earlier attempt finished Person: three proposed, two recorded, one
     that made nothing. Its cursor is where it ended. *)
  let person =
    ( "Person",
      cursor ~next:(code_points text) ~size:linking_piece ~found:3 ~emitted:2
        ~errors:1 )
  in
  let job =
    linking_job w "resume-counts"
      [
        ("retryCount", `Int 1);
        ("completedUnits", strs [ "Person" ]);
        ("unitCursors", `Assoc [ person ]);
      ]
  in
  let resource_id = job.resource_id in
  Ollama.script w.ollama
    [
      proposed
        [
          first_london;
          mention "London" "Place" ~prefix:"standards, and " ~suffix:" ignored";
          mention "London" "Place" ~prefix:"ignored it. " ~suffix:" was wrong";
        ];
      counting 3;
    ];
  let served = W.start w in
  let completion = settled served job in
  expect_start served 2 job;
  (* The finished type is not asked about again. *)
  expect_generations w.ollama (place_asked ());
  let generator = W.generator w.own in
  Check.equal "what it committed"
    (`List
       [
         `List
           [
             london_after_engine generator resource_id "T2TKs0Jg-PQWeqox5fGnh";
             text_annotation generator resource_id "linking"
               "Xkpj1ubZZRS5i6pmLeXW0" ~start:354 ~stop:360
               ~prefix:
                 "claim for a century.\n\n\
                  The program was na\u{00ef}ve by later standards, and "
               ~suffix:" ignored it. London was wrong.\n" ~body:(entity "Place")
               "London";
             text_annotation generator resource_id "linking"
               "MuYzJyXJorvNj5mhK1Ryw" ~start:373 ~stop:379
               ~prefix:
                 "The program was na\u{00ef}ve by later standards, and London \
                  ignored it. "
               ~suffix:" was wrong.\n" ~body:(entity "Place") "London";
           ];
       ])
    (batches served);
  (* Every checkpoint carries the finished type's cursor, as the job was
     claimed with it, beside the cursor of the type this attempt runs. *)
  let place =
    ( "Place",
      cursor ~next:(code_points text) ~size:linking_piece ~found:3 ~emitted:3
        ~errors:0 )
  in
  Check.equal "its checkpoints"
    (`List
       [
         checkpoint job [ person; place ];
         checkpoint job ~completed:[ "Place" ] [ person; place ];
       ])
    (checkpoints served);
  (* The finished type is among the finished, with what its cursor counted,
     from the first report. What was counted of mentions is this attempt's:
     a cursor carries none. *)
  let person_item = finished_type "Person" ~found:3 ~persisted:2 in
  let place_item = finished_type "Place" ~found:3 ~persisted:3 in
  expect_progress served job
    [
      report job 10 (code "loading") ~extra:[ echoed_entity_types types ];
      on_place job 50 ~processed:1 ~found:3 ~emitted:2 [ person_item ];
      on_place job 50 ~expected:3 ~processed:1 ~found:3 ~emitted:2
        [ person_item ];
      on_place job 50 ~expected:3 ~processed:1 ~found:6 ~emitted:5
        [ person_item ];
      on_place job 80 ~expected:3 ~processed:2 ~found:6 ~emitted:5
        [ person_item; place_item ];
      linking_complete ~types job ~count:5 ~expected:3
        [ person_item; place_item ];
    ];
  (* The counts are the whole job's: the finished type's three, two and one,
     and this attempt's three and three. *)
  Check.equal "its completion"
    (completion_of ~errors:[ ("errors", `Int 1) ] 2 job ~found:6 ~persisted:5)
    completion

let ended_named_finished (w : W.t) =
  (* The earlier attempt established Person's last batch, and died before it
     named the type finished. *)
  let person =
    ( "Person",
      cursor ~next:(code_points text) ~size:linking_piece ~found:2 ~emitted:2
        ~errors:0 )
  in
  let job =
    linking_job w "resume-ended"
      [ ("retryCount", `Int 1); ("unitCursors", `Assoc [ person ]) ]
  in
  let resource_id = job.resource_id in
  Ollama.script w.ollama [ proposed [ first_london ]; counting 1 ];
  let served = W.start w in
  let completion = settled served job in
  (* Nothing is asked about the type that has no text left. *)
  expect_generations w.ollama (place_asked ());
  Check.equal "what it committed"
    (`List
       [
         `List
           [
             london_after_engine (W.generator w.own) resource_id
               "NqihZbhiWq88D2HD70JAq";
           ];
       ])
    (batches served);
  (* The type is named finished before anything is committed, with the
     cursor the job was claimed with. *)
  let place =
    ( "Place",
      cursor ~next:(code_points text) ~size:linking_piece ~found:1 ~emitted:1
        ~errors:0 )
  in
  Check.equal "its checkpoints"
    (`List
       [
         checkpoint job ~completed:[ "Person" ] [ person ];
         checkpoint job ~completed:[ "Person" ] [ person; place ];
         checkpoint job ~completed:[ "Person"; "Place" ] [ person; place ];
       ])
    (checkpoints served);
  let person_item = finished_type "Person" ~found:2 ~persisted:2 in
  let place_item = finished_type "Place" ~found:1 ~persisted:1 in
  let on_person = detecting ~types job "Person" in
  expect_progress served job
    [
      report job 10 (code "loading") ~extra:[ echoed_entity_types types ];
      (* Person: begun, and finished. No piece was asked about, so none was
         counted and none committed. *)
      on_person 20 ~processed:0 ~found:2 ~emitted:2 [];
      on_person 50 ~processed:1 ~found:2 ~emitted:2 [ person_item ];
      on_place job 50 ~processed:1 ~found:2 ~emitted:2 [ person_item ];
      on_place job 50 ~expected:1 ~processed:1 ~found:2 ~emitted:2
        [ person_item ];
      on_place job 50 ~expected:1 ~processed:1 ~found:3 ~emitted:3
        [ person_item ];
      on_place job 80 ~expected:1 ~processed:2 ~found:3 ~emitted:3
        [ person_item; place_item ];
      linking_complete ~types job ~count:3 ~expected:1
        [ person_item; place_item ];
    ];
  Check.equal "its completion"
    (completion_of 2 job ~found:3 ~persisted:3)
    completion

let unit_at_end (w : W.t) =
  (* The earlier attempt asked about Claim to the end of the text, and never
     began Evidence: three proposed, two recorded, one that made nothing. *)
  let claim =
    ( "Claim",
      cursor ~next:(code_points text) ~size:claim_piece ~found:3 ~emitted:2
        ~errors:1 )
  in
  let job =
    mark_job w.own "resume-tagging"
      ~metadata:[ ("retryCount", `Int 1); ("unitCursors", `Assoc [ claim ]) ]
      (tagging (argument ()) [ "Claim"; "Evidence" ])
  in
  let resource_id = job.resource_id in
  Ollama.script w.ollama
    [ proposed [ exact "nobody tested that claim for a century" ] ];
  let served = W.start w in
  let completion = settled served job in
  (* One generation: the category that was never begun. *)
  expect_generations w.ollama [ generation model "tagging-evidence" ];
  Check.equal "what it committed"
    (`List
       [
         `List
           [
             text_annotation (W.generator w.own) resource_id "tagging"
               "mX0u-GJ2Asq1XSaDxTpYB" ~start:267 ~stop:305
               ~prefix:
                 "argued that the engine could manipulate symbols as well as \
                  numbers; "
               ~suffix:
                 ".\n\n\
                  The program was na\u{00ef}ve by later standards, and London \
                  ignored it"
               ~body:(tagged "Evidence")
               "nobody tested that claim for a century";
           ];
       ])
    (batches served);
  (* The checkpoint carries the cursor the job was claimed with beside the
     one this attempt made. *)
  Check.equal "its checkpoints"
    (`List
       [
         checkpoint job
           [
             claim;
             ( "Evidence",
               cursor ~next:(code_points text) ~size:evidence_piece ~found:1
                 ~emitted:1 ~errors:0 );
           ];
       ])
    (checkpoints served);
  expect_progress served job
    [
      report job 10 (code "loading");
      analyzing_tags job 30;
      analyzing_tags job 30 ~extra:(on_category "Claim" 0 []);
      analyzing_tags job 45
        ~extra:(on_category "Evidence" 1 [ found_in "Claim" 3 ]);
      creating_tags job 3;
      complete_created job "tagging" 3;
    ];
  (* The counts are the whole job's, by category as in all: the earlier
     attempt's two for Claim, and this one's for Evidence. *)
  Check.equal "its completion"
    (completion_of
       ~errors:
         [
           ("errors", `Int 1);
           ("byCategory", `Assoc [ ("Claim", `Int 2); ("Evidence", `Int 1) ]);
         ]
       2 job ~found:4 ~persisted:3)
    completion

let tests implementations =
  in_world implementations "resume"
    ~category:[ "a job resumed from a checkpoint" ]
    [
      case
        "takes a unit up at its cursor, with a smaller piece, and reports \
         counts that include what the earlier attempt made"
        at_cursor;
      case
        "does not do again an entity type an earlier attempt finished, and \
         counts nothing for one the record holds no cursor for"
        finished_not_again;
      case
        "counts an entity type an earlier attempt finished by the cursor it \
         ended at: the result and every report are of both types"
        finished_by_cursor;
      case
        "names finished, with its cursor and with nothing asked, an entity \
         type whose cursor stands at the end of the text"
        ended_named_finished;
      case
        "counts a unit whose cursor stands at the end of the text by that \
         cursor, and asks nothing about it"
        unit_at_end;
    ]
