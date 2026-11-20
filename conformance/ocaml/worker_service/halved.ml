(* halved.test.ts: a piece whose answer is cut off (WORKER-SERVICE.md
   § Failures): what it carried is not used, the piece is asked again in
   halves, and the pieces after it are cut smaller. *)

open Xynolog_conformance
open Support
open Case

let model = (List.hd W.ollama_engines).model
let marker n = `Assoc [ ("exact", str (Printf.sprintf "marker %d" n)) ]

(* What the model says when it finds each marker of [markers]. *)
let said markers = Yojson.Safe.to_string (`List (List.map marker markers))

(* An answer that finds each marker of [markers]. *)
let found markers = answer (said markers)

(* A window at which [long_text] is cut at 400 tokens, and its first piece,
   ended at a paragraph break, is three paragraphs. *)
let window = 1467

let halves (w : W.t) =
  w.ollama.show <- Ollama.Context window;
  let job = mark_job ~about:long_text w.own "halved" (params "halved-first") in
  let resource_id = job.resource_id in
  Ollama.script w.ollama
    ([
       (* The first piece, of 1441 code points, 361 tokens, though it was cut
          at 400: the model runs out of room partway through its answer. *)
       Ollama.Answer { response = said [ 103 ]; stop = Cut_off; usage = None };
       (* The same text again in four pieces of at most 180 tokens: half the
          piece's own 361, and not half the 400 it was cut at. *)
       found [ 103 ];
       found [ 205 ];
       found [];
       found [ 304 ];
       (* The rest of the text, in pieces of 180 tokens, the size the halves
          came back whole at. Marker 304 and marker 404 are each seen
          twice. *)
       found [ 304; 404 ];
       found [ 404 ];
     ]
    @ List.init 6 (fun _ -> found []));
  let served = W.start w in
  let completion = settled served job in
  expect_generations w.ollama
    (List.map
       (fun name -> generation ~context_length:window model name)
       ([
          "halved-first";
          "halved-half-1";
          "halved-half-2";
          "halved-half-3";
          "halved-half-4";
        ]
       @ List.init 8 (fun i -> Printf.sprintf "halved-rest-%d" (i + 1))));
  (* The halves are one piece to the job: one batch, and one cursor, which
     states the size the piece was first cut at. *)
  let generator = W.generator w.own in
  Check.equal "what it committed"
    (`List
       [
         `List
           [
             marker_highlight generator resource_id "W30xEEdxJok2bPhyoZdHL" 103
               211;
             marker_highlight generator resource_id "fWhWTeophhJTzKBl4xDgP" 205
               852;
             marker_highlight generator resource_id "S4STm5zK0eVKRpM67nFmp" 304
               1253;
           ];
         `List
           [
             marker_highlight generator resource_id "7cbv9AChTFxaDsfio0sPh" 404
               1734;
           ];
       ])
    (batches served);
  let at next ~size ~found ~emitted =
    checkpoint job
      [ ("highlighting", cursor ~next ~size ~found ~emitted ~errors:0) ]
  in
  Check.equal "its checkpoints"
    (`List
       ([
          at 1185 ~size:400 ~found:3 ~emitted:3;
          at 1586 ~size:180 ~found:5 ~emitted:4;
        ]
       @ List.map
           (fun next -> at next ~size:180 ~found:6 ~emitted:4)
           [ 1987; 2147; 2548; 2949; 3109; 3510; code_points long_text ]))
    (checkpoints served);
  let creating n =
    report job 60 (code ~members:[ ("count", `Int n) ] "creating-annotations")
  in
  expect_progress served job
    ([
       report job 10 (code "loading");
       report job 30 (code "analyzing");
       creating 3;
       report job 39 (code "analyzing");
     ]
    @ List.concat_map
        (fun percentage ->
          [ creating 4; report job percentage (code "analyzing") ])
        [ 42; 45; 47; 50; 53; 54; 57 ]
    @ [
        creating 4;
        report job 100
          (code
             ~members:[ ("count", `Int 4); ("motivation", str "highlighting") ]
             "complete-created");
      ]);
  (* What the cut-off answer carried is counted nowhere. *)
  Check.equal "its completion"
    (identified job
       [
         ("result", `Assoc [ ("found", `Int 6); ("persisted", `Int 4) ]);
         ("durability", str "acknowledged");
       ])
    completion

let tests implementations =
  in_world implementations "halved"
    ~category:[ "a job whose model is cut off" ]
    [
      case
        "asks the piece again in halves of its own size, takes what the halves \
         find together, and cuts the rest of the text at the size the halves \
         were cut at"
        halves;
    ]
