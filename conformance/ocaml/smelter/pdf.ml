(* What a Smelter does with a PDF (SMELTER.md § A PDF's text,
   § The anchored-text store, § Rebuild): what it stores of the text it
   derives and of where each word is on a page, what the Archivist then
   serves from that store, and what it does when it is asked to derive
   anchored text again. With it, what it signals of a PDF it has decided on,
   one it indexed and one it declined (§ What it decides).

   What each kind of PDF becomes is specs/src/smelter/extraction-cases.json,
   which every implementation runs through its own reader. The documents
   here are a few lines of ASCII (harness/pdf.ml), and a case looks for
   whole words of them: never for a page's text as a whole, nor for where a
   space falls between two words, in which readers differ. *)

open Xynolog_conformance
open Support

let sha256 = Archivist_world.sha256
let to_list = Yojson.Safe.Util.to_list
let to_string = Yojson.Safe.Util.to_string
let to_int = Yojson.Safe.Util.to_int

(* How many PDFs the cases have recorded so far. A rebuild that names no
   resource works through every one of them, whatever was decided of it. *)
let recorded = ref 0

(* Record bytes as a PDF, and wait for what the Smelter decides of them: its
   id, the checksum of its bytes, and the decision. *)
let recorded_pdf (w : W.t) bytes =
  let id = W.created w bytes pdf in
  incr recorded;
  (id, sha256 bytes, W.settled w id)

(* What the store holds for bytes of [checksum], which is to be their
   text. *)
let stored_text (w : W.t) checksum =
  match W.anchored_entry w checksum with
  | Some entry when Protocol.at entry [ "declined" ] = None -> entry
  | entry ->
      failwith
        (Printf.sprintf
           "the anchored-text store holds %s for the bytes %s, and not their \
            text"
           (match entry with Some e -> Check.shown e | None -> "nothing")
           checksum)

(* Of [text], the run from [start] to [end_]. Its offsets count code points,
   and these documents are ASCII, where a string's own positions are the
   same. *)
let run text start end_ = String.sub text start (end_ - start)

(* The pages a stored text has a word on. A word is on a page when a run of
   the text that is placed on that page has the word in it, which is what
   specs/src/smelter/extraction-cases.json says of onPage. *)
let pages_stored entry word =
  let text = to_string (Check.at entry [ "text" ]) in
  `List
    (List.filter_map
       (fun line ->
         if
           List.exists
             (fun placed ->
               match to_list placed with
               | _ :: _ :: start :: end_ :: _ ->
                   mentions word (run text (to_int start) (to_int end_))
               | _ -> failwith ("a word placed as " ^ Check.shown placed))
             (to_list (Check.at line [ "words" ]))
         then Some (Check.at line [ "p" ])
         else None)
       (to_list (Check.at entry [ "lines" ])))

(* The pages the Archivist's answer has a word on, by its items as by the
   runs of an entry. *)
let pages_served served word =
  let text = to_string (Check.at served [ "text" ]) in
  `List
    (List.filter_map
       (fun item ->
         let at name = to_int (Check.at item [ name ]) in
         if mentions word (run text (at "start") (at "end")) then
           Some (Check.at item [ "page" ])
         else None)
       (to_list (Check.at served [ "items" ])))

(* The text of a resource's points, one chunk after another. *)
let indexed_text (w : W.t) resource_id =
  String.concat "\n"
    (List.map payload_text (Qdrant.points_of w.index "resources" resource_id))

let contains what text word =
  Check.that (Printf.sprintf "%s holds %s" what word) (mentions word text)

(* Hold a rebuild's answer to be [ok], on [channel]. *)
let answered ok channel (answer : Archivist_world.answer) =
  Check.equal "the rebuild's answer"
    (`Assoc [ ("ok", `Bool ok); ("channel", str channel) ])
    (`Assoc [ ("ok", `Bool answer.ok); ("channel", str answer.channel) ])

let rebuilt = answered true "smelt:rebuild-anchors-ok"

let stamp (w : W.t) =
  match W.anchored_stamp w with
  | Some stamp -> str stamp
  | None -> failwith "the anchored-text store states no stamp"

let entry_of (w : W.t) checksum =
  match W.anchored_entry w checksum with Some e -> e | None -> `Null

let read_from_its_text (w : W.t) =
  let id, checksum, decided =
    recorded_pdf w
      (Pdf.of_pages
         [ [ "A heron waits in the reedbed"; "while a kingfisher dives" ] ])
  in
  Check.equal "the decision" (decision id checksum "indexed") decided;
  (* indexed is signalled once the points are written. *)
  let text = indexed_text w id in
  List.iter
    (contains "its points' text" text)
    [ "heron"; "reedbed"; "kingfisher" ];
  let machine_read = Qdrant.payload_member_of_some "resources" "machineRead" in
  List.iter
    (fun point ->
      Check.that
        ("a point is marked as read from pixels: " ^ Check.shown point)
        (Protocol.at (payload point) [ machine_read ] = None))
    (Qdrant.points_of w.index "resources" id)

let stores_its_text (w : W.t) =
  let _, checksum, _ =
    recorded_pdf w
      (Pdf.of_pages [ [ "An osprey circles"; "over the tideline" ] ])
  in
  let entry = stored_text w checksum in
  List.iter
    (fun word ->
      contains "the stored text" (to_string (Check.at entry [ "text" ])) word;
      Check.equal ("the pages of " ^ word)
        (`List [ `Int 1 ])
        (pages_stored entry word))
    [ "osprey"; "tideline" ];
  (* The Smelter states its stamp, which the Archivist keeps in the file
     STAMP, and its entries carry that one. *)
  Check.equal "the entry's stamp" (stamp w) (Check.at entry [ "stamp" ])

let archivist_reads (w : W.t) =
  let id, checksum, _ =
    recorded_pdf w (Pdf.of_pages [ [ "A bittern booms"; "from the sedge" ] ])
  in
  (* What a Smelter wrote, the Archivist reads: the one contract of the
     store that outlives any one Smelter. *)
  let served = Archivist_world.anchored_text w.kb id in
  Check.like "what the Archivist serves"
    (`Assoc
       [
         ("kind", str "extracted");
         ("text", Check.at (stored_text w checksum) [ "text" ]);
       ])
    served;
  List.iter
    (fun word ->
      contains "the served text" (to_string (Check.at served [ "text" ])) word;
      Check.equal ("the pages of " ^ word)
        (`List [ `Int 1 ])
        (pages_served served word))
    [ "bittern"; "sedge" ]

let two_pages (w : W.t) =
  let id, checksum, _ =
    recorded_pdf w
      (Pdf.of_pages
         [ [ "A marten crosses the scree" ]; [ "and a ptarmigan keeps still" ] ])
  in
  let entry = stored_text w checksum in
  let served = Archivist_world.anchored_text w.kb id in
  Check.like "what the Archivist serves"
    (`Assoc [ ("kind", str "extracted") ])
    served;
  List.iter
    (fun (word, page) ->
      Check.equal
        ("the stored pages of " ^ word)
        (`List [ `Int page ])
        (pages_stored entry word);
      Check.equal
        ("the served pages of " ^ word)
        (`List [ `Int page ])
        (pages_served served word))
    [ ("marten", 1); ("scree", 1); ("ptarmigan", 2) ]

(* Bytes recorded as a PDF that the Smelter declines for [reason]: it is
   skipped, has no points, and the decline is what is stored and served. *)
let declined_as reason bytes (w : W.t) =
  let id, checksum, decided = recorded_pdf w bytes in
  Check.equal "the decision" (decision ~reason id checksum "skipped") decided;
  Check.equal "its points" (`List [])
    (`List (Qdrant.points_of w.index "resources" id));
  Check.like "the store's entry"
    (`Assoc [ ("stamp", stamp w); ("declined", str reason) ])
    (entry_of w checksum);
  Check.equal "what the Archivist serves"
    (`Assoc [ ("kind", str "declined"); ("declined", str reason) ])
    (Archivist_world.anchored_text w.kb id)

let rebuild_named (w : W.t) =
  let id, checksum, _ =
    recorded_pdf w (Pdf.of_pages [ [ "A dipper bobs on a wet stone" ] ])
  in
  let entry = stored_text w checksum in
  let points = Qdrant.points_of ~vectors:true w.index "resources" id in
  Check.that "it has points" (points <> []);
  W.lose_anchored_entry w checksum;
  let before = Ollama_embed.mark w.provider in
  rebuilt (W.rebuild_anchors ~resource_id:id w);
  Check.equal "the store's entry" entry (entry_of w checksum);
  Check.equal "how many requests the provider was given" (`Int 0)
    (`Int (List.length (Ollama_embed.since w.provider before)));
  (* The index is not touched: the same points, by their ids, their payloads
     and their vectors. *)
  Check.equal "its points" (`List points)
    (`List (Qdrant.points_of ~vectors:true w.index "resources" id))

let deletes_nothing_first (w : W.t) =
  let id, checksum, _ =
    recorded_pdf w (Pdf.of_pages [ [ "A snipe drums over the fen" ] ])
  in
  let entry = stored_text w checksum in
  rebuilt (W.rebuild_anchors ~resource_id:id w);
  Check.equal "the store's entry" entry (entry_of w checksum)

let rebuild_all (w : W.t) =
  let _, first, _ =
    recorded_pdf w (Pdf.of_pages [ [ "A curlew calls across the saltmarsh" ] ])
  in
  let _, second, _ =
    recorded_pdf w (Pdf.of_pages [ [ "A lapwing tumbles over the furrows" ] ])
  in
  let decoded =
    "A text that is decoded from its bytes has no places on a page.\n"
  in
  let text = W.created w decoded plain in
  ignore (W.settled w text : Yojson.Safe.t);
  let entries = `List [ stored_text w first; stored_text w second ] in
  let points = Qdrant.points_of ~vectors:true w.index "resources" text in
  Check.that "the text has points" (points <> []);
  W.lose_anchored_entry w first;
  W.lose_anchored_entry w second;
  let before = Ollama_embed.mark w.provider in
  (* It works through every PDF the knowledge base holds, the earlier cases'
     too. Deriving a declined one again comes to its decline: an outcome,
     and no failure. *)
  rebuilt (W.rebuild_anchors w);
  Check.equal "the store's entries" entries
    (`List [ entry_of w first; entry_of w second ]);
  Check.equal "how many requests the provider was given" (`Int 0)
    (`Int (List.length (Ollama_embed.since w.provider before)));
  (* The text resource is as it was: no entry under the checksum of its
     bytes, and the points it had. *)
  Check.equal "the store's entry for the text" `Null
    (entry_of w (sha256 decoded));
  Check.equal "the text's points" (`List points)
    (`List (Qdrant.points_of ~vectors:true w.index "resources" text))

let entry_unwritable (w : W.t) =
  let bytes = Pdf.of_pages [ [ "A wren sings from the bracken" ] ] in
  let checksum = sha256 bytes in
  (* A file where the directory of this entry would be leaves it nowhere to
     be written, and every other entry where it was. *)
  let in_the_way =
    Filename.dirname (Archivist_world.anchored_text_entry_path w.kb checksum)
  in
  Archivist_world.write_file in_the_way "";
  Fun.protect
    ~finally:(fun () ->
      (* Left in the way, it would fail every later rebuild of this
         resource's anchored text. *)
      Sys.remove in_the_way)
    (fun () ->
      let id, _, decided = recorded_pdf w bytes in
      Check.equal "the decision" (decision id checksum "indexed") decided;
      contains "its points' text" (indexed_text w id) "wren";
      (* The entry was not written some other way: the case was of an entry
         that could not be. *)
      Check.equal "the store's entry" `Null (entry_of w checksum))

(* The last case of the file. It leaves the knowledge base holding a PDF
   whose bytes cannot be read, and every rebuild that names no resource
   would fail on it from here on. *)
let rebuild_incomplete (w : W.t) =
  let done_id, done_checksum, _ =
    recorded_pdf w (Pdf.of_pages [ [ "A dunlin probes the mudflat" ] ])
  in
  let undone, undone_checksum, _ =
    recorded_pdf w (Pdf.of_pages [ [ "A redshank pipes on the strand" ] ])
  in
  ignore done_id;
  let restored = stored_text w done_checksum in
  let kept = stored_text w undone_checksum in
  W.lose_anchored_entry w done_checksum;
  Sys.remove (W.content_file w undone);
  let answer = W.rebuild_anchors w in
  answered false "smelt:rebuild-anchors-failed" answer;
  (* One resource, of every PDF the knowledge base holds: the message says
     both numbers, as smelter/messages.json's rebuild-incomplete says it. *)
  Check.equal "the rebuild's message"
    (str
       (refusal "rebuild-incomplete"
          [ ("failed", "1"); ("total", string_of_int !recorded) ]))
    (Check.at answer.payload [ "message" ]);
  Check.equal "the store's entry of the one done" restored
    (entry_of w done_checksum);
  (* Nothing was deleted first, so the entry of the one that could not be
     derived again is not lost. *)
  Check.equal "the store's entry of the one not done" kept
    (entry_of w undone_checksum)

let tests implementations =
  in_world implementations "pdf"
    ~category:[ "a Smelter, given a PDF" ]
    [
      Case.case
        "reads a PDF with text of its own from that text: it is settled as \
         indexed under the checksum of its bytes, its points have the words \
         drawn, and none is marked as read from pixels"
        read_from_its_text;
      Case.case
        "stores what deriving a PDF's text came to under the checksum of its \
         bytes: the text, and each word's place on its page, under the stamp \
         the store states"
        stores_its_text;
      Case.case
        "writes an entry the Archivist reads: the text it stored, with each \
         word on its page, is what the Archivist then serves"
        archivist_reads;
      Case.case
        "places the words of a document of two pages each on its own page"
        two_pages;
      Case.case
        "declines a PDF whose pages draw nothing as having no text layer: it \
         is skipped, has no points, and the decline is what is stored and \
         served"
        (declined_as "no-text-layer" (Pdf.of_pages [ []; [] ]));
      Case.case
        "declines bytes that are no PDF, recorded as one, as corrupt: it is \
         skipped, has no points, and the decline is what is stored and served"
        (declined_as "corrupt" not_a_pdf);
      Case.case
        "derives a resource's anchored text again when a rebuild names it: the \
         lost entry is back, nothing is embedded, and its points are the ones \
         it had"
        rebuild_named;
      Case.case
        "deletes nothing before it derives again: an entry that is there when \
         a rebuild is asked for is there after it"
        deletes_nothing_first;
      Case.case
        "derives the anchored text of every PDF again when a rebuild names \
         none, and gives a resource whose text has no places on a page no \
         entry"
        rebuild_all;
      Case.case "indexes a PDF although its entry cannot be written"
        entry_unwritable;
      Case.case
        "answers a rebuild that could not do every resource with how many of \
         how many it could not, having done the rest and lost no entry"
        rebuild_incomplete;
    ]
