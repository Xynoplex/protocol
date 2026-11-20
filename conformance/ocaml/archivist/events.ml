(* The record read by position (ARCHIVIST.md § The event log, § The HTTP
   surface): a stream's events from a sequence number on, a page at a time,
   each answer with the sequence number of the stream's last event;
   __system__ read as a resource's stream is; and every stream listed with
   where it stands. *)

open Xynolog_conformance
open Support
open Case

(* The most events one answer holds, as the Archivist's API states it. *)
let page_size () =
  Protocol.limit Archivist "GET" "/events/{resourceId}" "maxEvents"

let from first last = List.init (last - first + 1) (fun at -> first + at)

let events_path stream from_sequence =
  Printf.sprintf "/events/%s?fromSequence=%d"
    (Protocol.uri_component stream)
    from_sequence

(* One answer of a stream's events from [from_sequence] on: its events, and
   the sequence number of the stream's last. *)
let page w stream from_sequence =
  let reply =
    A.http w ~route:"/events/{resourceId}" "GET"
      (events_path stream from_sequence)
  in
  Check.equal (Call.shown reply.text) (`Int 200) (`Int reply.status);
  let answer = Option.get reply.json in
  ( Check.list answer [ "events" ],
    match Check.at answer [ "lastSequence" ] with
    | `Int n -> n
    | other -> failwith (Check.shown other ^ " is no lastSequence") )

let paged events last_sequence =
  `Assoc [ ("events", `List events); ("lastSequence", `Int last_sequence) ]

(* Every event of a stream from [from_sequence] on: each page read from the
   sequence after the last one, until the stream's last. *)
let read_on w stream from_sequence =
  let rec read next =
    let events, last_sequence = page w stream next in
    Check.that "a page holds at most the API's maxEvents"
      (List.length events <= page_size ());
    match List.rev events with
    | last :: _ when sequence_of last < last_sequence ->
        events @ read (sequence_of last + 1)
    | _ -> events
  in
  read from_sequence

(* The sequence number of a stream's last event, as its files hold it: 0
   for one with none. *)
let last_on_disk w stream =
  match List.rev (A.stored w stream) with
  | last :: _ -> sequence_of last
  | [] -> 0

let from_a_sequence (w : A.t) =
  let id =
    A.created w
      (World.person_did w.world "ada")
      [ ("name", "Replayed"); ("storageUri", "file://docs/replayed.md") ]
      "replayed\n"
  in
  let editor = A.person w "editor" in
  ignore (A.ask editor "mark:archive" (on id) : Yojson.Safe.t);
  ignore (A.ask editor "mark:unarchive" (on id) : Yojson.Safe.t);
  Check.equal "the stream"
    (numbered_as
       [ ("yield:created", 1); ("mark:archived", 2); ("mark:unarchived", 3) ])
    (numbered_events (A.stored w id));

  let answered stream from_sequence =
    let events, last_sequence = page w stream from_sequence in
    paged events last_sequence
  in
  Check.equal "from the first" (paged (A.stored w id) 3) (answered id 1);
  Check.equal "from the third"
    (paged (List.filteri (fun at _ -> at >= 2) (A.stored w id)) 3)
    (answered id 3);
  (* Past the end, and a stream that is not there: no events, and where the
     stream stands. *)
  Check.equal "past the end" (paged [] 3) (answered id 4);
  Check.equal "a stream that is not there" (paged [] 0)
    (answered "0123456789abcdef0123456789abcdef" 1);

  List.iter
    (fun query ->
      let refused =
        A.http w ~route:"/events/{resourceId}" "GET"
          ("/events/" ^ Protocol.uri_component id ^ query)
      in
      holds_http_error query "events-bad-request" [] refused)
    [
      "";
      "?fromSequence=0";
      "?fromSequence=-1";
      "?fromSequence=abc";
      "?fromSequence=1.5";
    ]

let long_stream (w : A.t) =
  (* A stream longer than a file, written as the Archivist writes one: its
     first file full, the rest in the next. *)
  let id = String.concat "" (String.split_on_char '-' (Fresh.uuid ())) in
  let file = (A.layout ()).event_file in
  let per_file = file.max_events in
  let total = per_file + 5 in
  let user = World.person_did w.world "ada" in
  let line sequence_number =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("type", str "mark:archived");
           ("resourceId", str id);
           ("userId", str user);
           ("version", `Int 1);
           ("payload", `Assoc []);
           ("id", str (Fresh.uuid ()));
           ("timestamp", str "2026-01-01T00:00:00.000Z");
           ("metadata", `Assoc [ ("sequenceNumber", `Int sequence_number) ]);
         ])
  in
  let write number sequences =
    A.write_file
      (A.stream_dir w id // A.event_file_name number)
      (String.concat "" (List.map (fun n -> line n ^ "\n") sequences))
  in
  write file.first (from 1 per_file);
  write (file.first + 1) (from (per_file + 1) total);

  let page_size = page_size () in
  let events, last_sequence = page w id 1 in
  Check.equal "the first page"
    (ints (from 1 page_size))
    (ints (List.map sequence_of events));
  Check.equal "the first page's lastSequence" (`Int total) (`Int last_sequence);
  (* From the end of the first file into the second. *)
  let events, _ = page w id (per_file - 1) in
  Check.equal "across the files"
    (ints (from (per_file - 1) total))
    (ints (List.map sequence_of events));
  (* Every event, once, in order, as stored. *)
  Check.equal "every event" (`List (A.stored w id)) (`List (read_on w id 1));
  let events, last_sequence = page w id (total + 1) in
  Check.equal "past the end" (paged [] total) (paged events last_sequence)

let system_stream (w : A.t) =
  let curator = A.person w "curator" in
  Check.equal "the add's response" (`Assoc [])
    (A.ask curator "frame:add-entity-type"
       (`Assoc [ ("tag", str "Positioned") ]));

  (* The knowledge base's own stream goes on beside the case (a person is
     profiled as they first act): what the read gave is the stream on disk
     as far as the read went. *)
  let read = read_on w A.system 1 in
  Check.equal "the stream read" (`List read)
    (`List
       (List.filteri (fun at _ -> at < List.length read) (A.stored w A.system)));
  let positioned =
    match
      List.find_opt
        (fun e ->
          of_type "frame:entity-type-added" e
          && Check.at e [ "payload"; "entityType" ] = str "Positioned")
        read
    with
    | Some e -> e
    | None -> failwith "the stream read holds no Positioned"
  in
  let events, last_sequence = page w A.system (sequence_of positioned) in
  Check.equal "the first from it" positioned (List.hd events);
  Check.that "its lastSequence is at least its own"
    (last_sequence >= sequence_of positioned)

(* Every stream the log holds on disk: each directory under [dir] that
   holds its files. *)
let rec streams_under dir =
  let entries =
    List.map (fun e -> dir // e) (Array.to_list (Sys.readdir dir))
  in
  if entries <> [] && List.for_all (fun e -> not (Sys.is_directory e)) entries
  then [ Filename.basename dir ]
  else List.concat_map streams_under (List.filter Sys.is_directory entries)

let every_stream (w : A.t) =
  let ada = World.person_did w.world "ada" in
  let once =
    A.created w ada
      [ ("name", "Once"); ("storageUri", "file://docs/once.md") ]
      "once\n"
  in
  let twice =
    A.created w ada
      [ ("name", "Twice"); ("storageUri", "file://docs/twice.md") ]
      "twice\n"
  in
  ignore (A.ask (A.person w "editor") "mark:archive" (on twice) : Yojson.Safe.t);

  let listed = A.streams w in
  let ids = List.map fst listed in
  (* Each stream once. *)
  Check.equal "the streams listed once"
    (`Int (List.length listed))
    (`Int (List.length (List.sort_uniq compare ids)));
  let listed_at id =
    match List.assoc_opt id listed with
    | Some last -> last
    | None -> failwith (id ^ " is not listed")
  in
  Check.equal "once's" (`Int 1) (`Int (listed_at once));
  Check.equal "twice's" (`Int 2) (`Int (listed_at twice));
  Check.that "__system__'s is more than 0" (listed_at A.system > 0);

  (* Every stream the log holds on disk, and no other, each where its files
     say it stands. *)
  let on_disk = streams_under (Filename.dirname (A.stream_dir w A.system)) in
  Check.equal "the streams on disk" (strs (sorted on_disk)) (strs (sorted ids));
  (* The knowledge base's own stream goes on beside the case: it stands at
     least where it was listed. *)
  List.iter
    (fun (stream, last_sequence) ->
      if stream = A.system then
        Check.that "__system__ stands at least where it was listed"
          (last_sequence <= last_on_disk w stream)
      else
        Check.equal stream (`Int (last_on_disk w stream)) (`Int last_sequence))
    listed

let tests implementations =
  with_archivist implementations "the record read by position"
    [
      case
        "serves a stream's stored events from a sequence on, with the sequence \
         number of its last event"
        from_a_sequence;
      case
        "serves a long stream a page at a time, across the files it is kept in"
        long_stream;
      case "reads `__system__` as it reads a resource's stream" system_stream;
      case
        "lists every stream, `__system__` among them, with the sequence number \
         of its last event"
        every_stream;
    ]
