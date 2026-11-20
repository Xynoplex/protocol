(* The anchored-text store, as the Smelter keeps it (ARCHIVIST.md § The
   anchored-text store, § HTTP surface): the stamp it states, an entry it
   stores for a resource's bytes, the entry it reads back by checksum, and
   the keys it lists. Each over the Archivist's surface, as a service
   account. *)

open Xynolog_conformance
open Support
open Case

let stamp = "conformance-reader-1+conformance-engine-1"
let entry_of ?(stamp = stamp) text = A.stamped stamp (anchored_entry text)

let status what expected (reply : Call.reply) =
  Check.equal (what ^ ": its status") (`Int expected) (`Int reply.status)

let stored what reply = status what 204 reply

let extracted w id text =
  Check.like "the anchored text"
    (`Assoc [ ("kind", str "extracted"); ("text", str text) ])
    (A.anchored_text w id)

let same_bytes w =
  let content = "%PDF-1.4 stored through the surface\n" in
  let first = scan w "First" "file://scans/first.pdf" content in
  let second = scan w "Second" "file://scans/second.pdf" content in
  A.state_stamp w stamp;
  stored "the write"
    (A.store_anchored_text w first
       (A.entry_write (A.sha256 content) (entry_of "alpha beta")));
  List.iter
    (fun id ->
      Check.like "the anchored text"
        (`Assoc
           [
             ("kind", str "extracted");
             ("text", str "alpha beta");
             ("method", str "ocr");
           ])
        (A.anchored_text w id))
    [ first; second ];
  (* A second write for the same bytes replaces the first. *)
  stored "the second write"
    (A.store_anchored_text w second
       (A.entry_write (A.sha256 content) (entry_of "gamma delta")));
  extracted w first "gamma delta"

let read_back w =
  let content = "%PDF-1.4 read back by checksum\n" in
  let id = scan w "Read back" "file://scans/read-back.pdf" content in
  A.state_stamp w stamp;
  let entry = entry_of "read back" in
  stored "the write"
    (A.store_anchored_text w id (A.entry_write (A.sha256 content) entry));
  let held = A.stored_anchored_text w id (Some (A.sha256 content)) in
  status "the entry read back" 200 held;
  Check.equal "the entry read back" entry (Option.get held.json);
  (* Bytes with no entry, of a resource nothing has settled: answered at
     once. *)
  let unread = "%PDF-1.4 never derived\n" in
  let never = scan w "Never derived" "file://scans/never.pdf" unread in
  let asked = Run.now () in
  status "the entry of bytes never derived"
    (http_status "anchored-text-no-entry")
    (A.stored_anchored_text w never (Some (A.sha256 unread)));
  Check.that "the entry of bytes never derived is answered within 5 s"
    (Run.now () -. asked < 5.0);
  (* Read with a checksum the resource's content does not have: refused,
     naming the one it has. *)
  let stale = A.stored_anchored_text w id (Some (A.sha256 unread)) in
  status "a read with another checksum" 409 stale;
  Check.contains_text "a read with another checksum's error" (A.sha256 content)
    (A.string_member "error" (Option.get stale.json));
  (* Read with no checksum, or one that is none. *)
  status "a read with no checksum" 400 (A.stored_anchored_text w id None);
  status "a read with a checksum that is none" 400
    (A.stored_anchored_text w id (Some "ABC"))

let listed w =
  let bytes =
    List.map
      (fun n -> Printf.sprintf "%%PDF-1.4 listed %s\n" n)
      [ "one"; "two"; "three" ]
  in
  let ids =
    List.mapi
      (fun i content ->
        scan w
          (Printf.sprintf "Listed %d" i)
          (Printf.sprintf "file://scans/listed-%d.pdf" i)
          content)
      bytes
  in
  let a, b, c =
    match List.map A.sha256 bytes with
    | [ a; b; c ] -> (a, b, c)
    | _ -> assert false
  in
  let id n = List.nth ids n in
  A.state_stamp w "stamp-before";
  List.iter
    (fun i ->
      let content = List.nth bytes i in
      stored "a write under the stamp before"
        (A.store_anchored_text w (id i)
           (A.entry_write (A.sha256 content)
              (entry_of ~stamp:"stamp-before" (Printf.sprintf "before %d" i)))))
    [ 0; 1 ];
  let keys = A.anchored_text_keys w in
  Check.that
    (Printf.sprintf "the keys %s hold %s and %s" (String.concat ", " keys) a b)
    (List.mem a keys && List.mem b keys);
  Check.that
    (Printf.sprintf "the keys %s hold no %s" (String.concat ", " keys) c)
    (not (List.mem c (A.anchored_text_keys w)));
  (* A new stamp: what was stored under the old one is listed and answered
     to no one; nothing is deleted. *)
  A.state_stamp w "stamp-after";
  Check.equal "the keys under a new stamp" (`List [])
    (strs (A.anchored_text_keys w));
  status "an entry stored under the old stamp"
    (http_status "anchored-text-no-entry")
    (A.stored_anchored_text w (id 0) (Some a));
  stored "a write under the new stamp"
    (A.store_anchored_text w (id 2)
       (A.entry_write c (entry_of ~stamp:"stamp-after" "after")));
  Check.equal "the keys under the new stamp" (strs [ c ])
    (strs (A.anchored_text_keys w));
  (* The old stamp stated again: its entries are there. *)
  A.state_stamp w "stamp-before";
  Check.equal "the keys under the old stamp again"
    (strs (sorted [ a; b ]))
    (strs (A.anchored_text_keys w));
  status "an entry stored under the old stamp, stated again" 200
    (A.stored_anchored_text w (id 0) (Some a))

let kept w =
  let content = "%PDF-1.4 kept across a restart\n" in
  let id = scan w "Kept" "file://scans/kept.pdf" content in
  A.state_stamp w "stamp-kept";
  stored "the write"
    (A.store_anchored_text w id
       (A.entry_write (A.sha256 content) (entry_of ~stamp:"stamp-kept" "kept")));
  A.restart w;
  status "the entry after a restart" 200
    (A.stored_anchored_text w id (Some (A.sha256 content)));
  extracted w id "kept"

let refused_writes w =
  let content = "%PDF-1.4 the bytes the resource has\n" in
  let id = scan w "Changed" "file://scans/changed.pdf" content in
  A.state_stamp w stamp;
  let other = A.sha256 "%PDF-1.4 bytes it does not have\n" in
  let stale =
    A.store_anchored_text w id (A.entry_write other (entry_of "stale"))
  in
  status "a write for other bytes" 409 stale;
  Check.contains_text "a write for other bytes' error" (A.sha256 content)
    (A.string_member "error" (Option.get stale.json));
  status "a write for a resource the record does not hold"
    (http_status "anchored-text-write-not-found")
    (A.store_anchored_text w (String.make 32 'c')
       (A.entry_write (A.sha256 content) (entry_of "nowhere")));
  let with_member name value = function
    | `Assoc members ->
        `Assoc (List.remove_assoc name members @ [ (name, value) ])
    | other -> other
  in
  List.iter
    (fun body ->
      let shown = Check.shown body in
      status
        (String.sub shown 0 (min 80 (String.length shown)))
        (http_status "anchored-text-not-a-write")
        (A.store_anchored_text w id body))
    [
      `Assoc [ ("contentChecksum", str (A.sha256 content)) ];
      A.entry_write "not-a-checksum" (entry_of "bad key");
      A.entry_write (A.sha256 content)
        (with_member "v" (`Int (A.entry_format () - 1)) (entry_of "old form"));
      with_member "extra" (`Bool true)
        (A.entry_write (A.sha256 content) (entry_of "extra"));
    ];
  status "the entry after the refused writes"
    (http_status "anchored-text-no-entry")
    (A.stored_anchored_text w id (Some (A.sha256 content)));
  let route = "/anchored-text/stamp" in
  status "a stamp of two words" 400
    (A.http w ~route
       ~json:(`Assoc [ ("stamp", str "two words") ])
       "PUT" "/anchored-text/stamp");
  (* A body larger than the operation's maxBodyBytes, as the Archivist's API
     states it. *)
  let limit = Protocol.limit Archivist "PUT" route "maxBodyBytes" in
  status "an oversized stamp" 413
    (A.http w ~route
       ~json:(`Assoc [ ("stamp", str (String.make limit 's')) ])
       "PUT" "/anchored-text/stamp")

let service_only w =
  let content = "%PDF-1.4 admitted only as a service\n" in
  let id = scan w "Admitted" "file://scans/admitted.pdf" content in
  let roleless = A.service_token ~roles:[] w in
  let unauthorized = (messages ()).unauthorized.status in
  List.iter
    (fun (what, call) -> status what unauthorized (call ()))
    [
      ( "the stamp",
        fun () ->
          A.http w ~route:"/anchored-text/stamp" ~token:roleless
            ~json:(`Assoc [ ("stamp", str stamp) ])
            "PUT" "/anchored-text/stamp" );
      ( "a write",
        fun () ->
          A.store_anchored_text ~token:roleless w id
            (A.entry_write (A.sha256 content) (entry_of "refused")) );
      ( "a read",
        fun () ->
          A.stored_anchored_text ~token:roleless w id (Some (A.sha256 content))
      );
      ( "the keys",
        fun () ->
          A.http w ~route:"/anchored-text/entries" ~token:roleless "GET"
            "/anchored-text/entries" );
    ];
  status "the keys, anonymously" unauthorized
    (A.http w ~route:"/anchored-text/entries" ~anonymous:true "GET"
       "/anchored-text/entries")

let tests implementations =
  with_archivist implementations "the anchored-text store"
    [
      case
        "stores an entry for a resource's bytes, which a reader then gets for \
         that resource and for another with the same bytes"
        same_bytes;
      case
        "answers the entry stored for a resource's content at once, as it was \
         stored, 404 for content it has none of without waiting for the \
         Smelter, and 409 for content the resource does not have"
        read_back;
      case
        "lists the keys stored under the stamp last stated, and answers \
         nothing stored under another"
        listed;
      case "keeps the stamp it was told across a restart" kept;
      case
        "refuses a write for bytes the resource does not have, for a resource \
         the record does not hold, and a body that is no write"
        refused_writes;
      case "admits each of its four operations only with xynolog-service"
        service_only;
    ]
