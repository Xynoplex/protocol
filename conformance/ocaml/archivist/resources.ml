(* Resources (ARCHIVIST.md § Resources, § The record on disk, § Facts, § The
   HTTP surface): an upload stored and recorded, what the record then holds
   and publishes, and the commands that change a resource. *)

open Xynolog_conformance
open Support
open Case

let uuid =
  "^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$"

let timestamp = "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d{3}Z$"
let minted = "^[0-9a-f]{32}$"

let json_of what (reply : Call.reply) =
  match reply.json with
  | Some json -> json
  | None -> failwith (what ^ " is not JSON: " ^ Call.shown reply.text)

let text_at json path =
  match Check.at json path with
  | `String text -> text
  | other -> failwith (Check.shown other ^ " is not a string")

let sequence_of event =
  match Check.at event [ "metadata"; "sequenceNumber" ] with
  | `Int n -> n
  | other -> failwith (Check.shown other ^ " is no sequence number")

let header (reply : Call.reply) name = Http.Header.get reply.headers name

(* The id the gateway gives the fact [sequence] of the stream of [id], on
   that resource's scope. *)
let persisted_id channel id sequence =
  match
    Bus_stream.expected_id
      {
        Bus_stream.channel;
        correlation_id = None;
        scope = Some id;
        payload =
          `Assoc [ ("metadata", `Assoc [ ("sequenceNumber", `Int sequence) ]) ];
      }
  with
  | Bus_stream.Persisted persisted -> persisted
  | Bus_stream.Reply _ | Bus_stream.Ephemeral ->
      failwith "a scoped fact's id is no persisted id"

let view_of w id =
  match A.view w id with
  | Some view -> view
  | None -> failwith ("the Archivist keeps no view of " ^ id)

let types names = `List (List.map str names)

let uploaded w =
  let ada = World.person_did w.A.world "ada" in
  let watcher = A.sidecar w "watcher" in
  let content = "# Overview\n\nThe record of everything.\n" in
  let uri = "file://docs/overview.md" in
  let typed = types (entity_types 1) in
  let id =
    A.created w ada
      [
        ("name", "Overview");
        ("storageUri", uri);
        ("language", "en");
        ("entityTypes", Yojson.Safe.to_string typed);
      ]
      content
  in
  (* The id is the Archivist's to mint. *)
  Check.that ("the id's form: " ^ id) (Check.matches_re minted id);
  (* The bytes are in the working tree, where the storage URI says. *)
  Check.equal "the bytes in the working tree" (str content)
    (str (A.read_file (A.content_path w uri)));
  (* One line, in the stream filed by the shard of the id. *)
  Check.equal "the stream's files"
    (`List [ str (A.event_file_name (A.layout ()).event_file.first) ])
    (`List (List.map str (A.stream_files w id)));
  let line =
    match A.stream_lines w id with
    | [ line ] -> line
    | lines ->
        failwith
          (Printf.sprintf "the stream holds %d lines" (List.length lines))
  in
  let event = Yojson.Safe.from_string line in
  Check.equal "the event's keys, in order"
    (types
       [
         "type";
         "resourceId";
         "userId";
         "version";
         "payload";
         "id";
         "timestamp";
         "metadata";
       ])
    (match event with
    | `Assoc members -> types (List.map fst members)
    | other -> failwith (Check.shown other ^ " is not an object"));
  Check.equal "the line, as compact JSON"
    (str (Yojson.Safe.to_string event))
    (str line);
  like "the event"
    (`Assoc
       [
         ("type", str "yield:created");
         ("resourceId", str id);
         ("userId", str ada);
         ("version", `Int 1);
         ("metadata", `Assoc [ ("sequenceNumber", `Int 1) ]);
       ])
    event;
  let event_id = text_at event [ "id" ] in
  Check.that ("the event's id: " ^ event_id) (Check.matches_re uuid event_id);
  let at = text_at event [ "timestamp" ] in
  Check.that ("the event's timestamp: " ^ at) (Check.matches_re timestamp at);
  let payload = Check.at event [ "payload" ] in
  let ada_as_person = `Assoc [ ("@type", str "Person"); ("@id", str ada) ] in
  like "the event's payload"
    (`Assoc
       [
         ("name", str "Overview");
         ("format", str "text/markdown");
         ("contentChecksum", str (sha256 content));
         ("contentByteSize", `Int (String.length content));
         ("storageUri", str uri);
         ("entityTypes", typed);
         ("language", str "en");
         ("isDraft", `Bool false);
         ("creator", ada_as_person);
         ("wasAttributedTo", `List [ ada_as_person ]);
       ])
    payload;
  Check.that "the payload carries no generator"
    (Protocol.at payload [ "generator" ] = None);
  Check.that "the event carries no correlationId"
    (Protocol.at event [ "correlationId" ] = None);
  (* The view, filed by the same shard, is what the event adds up to. *)
  let view = view_of w id in
  Check.equal "the view's last sequence" (`Int 1)
    (Check.at view [ "lastSequence" ]);
  Check.equal "the view's annotations"
    (`Assoc
       [
         ("resourceId", str id);
         ("annotations", `List []);
         ("version", `Int 1);
         ("updatedAt", str at);
       ])
    (Check.at view [ "annotations" ]);
  like "the view's resource"
    (`Assoc
       [
         ("@id", str id);
         ("name", str "Overview");
         ("archived", `Bool false);
         ("entityTypes", typed);
         ("dateCreated", str at);
         ("isDraft", `Bool false);
         ( "representations",
           `List
             [
               `Assoc
                 [
                   ("mediaType", str "text/markdown");
                   ("checksum", str (sha256 content));
                   ("byteSize", `Int (String.length content));
                   ("rel", str "original");
                   ("language", str "en");
                   ("storageUri", str uri);
                 ];
             ] );
       ])
    (Check.at view [ "resource" ]);
  (* The index answers which resource is at the URI. *)
  Check.equal "the storage-uri entry"
    (`Assoc [ ("uri", str uri); ("resourceId", str id) ])
    (Option.get (A.storage_uri_entry w uri));
  Check.equal "the storage-uri entry's keys, in order"
    (types [ "uri"; "resourceId" ])
    (match Yojson.Safe.from_string (A.read_file (A.storage_uri_path w uri)) with
    | `Assoc members -> types (List.map fst members)
    | other -> failwith (Check.shown other ^ " is not an object"));
  (* The fact is the stored event, published once with no scope. *)
  let of_it e = Protocol.at e [ "resourceId" ] = Some (str id) in
  Check.equal "the fact" event (A.fact watcher ~matches:of_it "yield:created");
  Check.equal "the facts of its creation" (`Int 1)
    (`Int
       (List.length
          (List.filter of_it (A.facts ~type_:"yield:created" watcher))))

let scoped w =
  let ada = World.person_did w.A.world "ada" in
  let id =
    A.created w ada
      [ ("name", "Scoped"); ("storageUri", "file://docs/scoped.md") ]
      "scoped\n"
  in
  let follower = A.person w "follower" ~scopes:[ id ] in
  Check.equal "the answer" (`Assoc [])
    (A.ask follower "mark:archive" (`Assoc [ ("resourceId", str id) ]));
  let message =
    Bus_stream.next follower.stream "the scoped fact" (fun m ->
        match m.frame with
        | Some f -> f.channel = "mark:archived" && f.scope = Some id
        | None -> false)
  in
  let frame = Option.get message.frame in
  Check.equal "the scoped fact's id"
    (str (persisted_id "mark:archived" id 2))
    (match message.id with Some i -> str i | None -> `Null);
  like "the scoped fact"
    (`Assoc
       [
         ("type", str "mark:archived");
         ("resourceId", str id);
         ("metadata", `Assoc [ ("sequenceNumber", `Int 2) ]);
       ])
    frame.payload;
  Check.that "the scoped fact carries no correlationId"
    (frame.correlation_id = None);
  Check.equal "the unscoped fact"
    (Protocol.unstamped frame.payload)
    (A.fact follower
       ~matches:(fun e -> Protocol.at e [ "resourceId" ] = Some (str id))
       "mark:archived")

let served w =
  let content = "\x89PNG\r\n\x1a\n\x00\xff\x10" in
  let id =
    A.created w
      (World.person_did w.A.world "ada")
      [
        ("name", "Pixels");
        ("storageUri", "file://images/pixels.png");
        ("format", "image/png");
      ]
      content
  in
  let reply =
    A.http w ~route:"/resources/{id}/content" "GET"
      (A.resource_path id "/content")
  in
  Check.equal "its status" (`Int 200) (`Int reply.status);
  Check.equal "its content type" (str "image/png")
    (match header reply "content-type" with Some t -> str t | None -> `Null);
  Check.that "its bytes are those stored" (reply.text = content)

let not_held w =
  let id = "0123456789abcdef0123456789abcdef" in
  let answered name fill route suffix =
    let status, _, _ = http_error name fill in
    let reply = A.http w ~route "GET" (A.resource_path id suffix) in
    Check.equal (route ^ ": its status") (`Int status) (`Int reply.status);
    Check.equal route (error_body name fill) (json_of route reply)
  in
  answered "content-no-resource"
    [ ("id", id) ]
    "/resources/{id}/content" "/content";
  answered "jsonld-not-found" [] "/resources/{id}/jsonld" "/jsonld"

let described w =
  let reader = A.person w "reader" in
  let id =
    A.created w reader.did
      [ ("name", "Described"); ("storageUri", "file://docs/described.md") ]
      "described\n"
  in
  let reply =
    A.http w ~route:"/resources/{id}/jsonld" "GET"
      (A.resource_path id "/jsonld")
  in
  Check.equal "its status" (`Int 200) (`Int reply.status);
  Check.equal "its content type"
    (str "application/ld+json; charset=utf-8")
    (match header reply "content-type" with Some t -> str t | None -> `Null);
  let browsed =
    A.ask reader "browse:resource-requested" (`Assoc [ ("resourceId", str id) ])
  in
  like "the descriptor browse answers"
    (`Assoc
       [
         ("resource", `Assoc [ ("@id", str id); ("name", str "Described") ]);
         ("lastSequence", `Int 1);
       ])
    browsed;
  Check.equal "the JSON-LD"
    (`Assoc
       [
         ("resource", Check.at browsed [ "resource" ]); ("annotations", `List []);
       ])
    (json_of "the JSON-LD" reply)

let guarded w =
  let id =
    A.created w
      (World.person_did w.A.world "ada")
      [ ("name", "Guarded"); ("storageUri", "file://docs/guarded.md") ]
      "guarded\n"
  in
  let person = World.person w.world "ada" in
  let unauthorized = (messages ()).unauthorized in
  let refused what (reply : Call.reply) challenge =
    Check.equal (what ^ ": its status") (`Int unauthorized.status)
      (`Int reply.status);
    Check.equal what
      (`Assoc [ ("error", str unauthorized.error) ])
      (json_of what reply);
    Check.equal (what ^ ": its challenge") (str challenge)
      (match header reply "www-authenticate" with
      | Some c -> str c
      | None -> `Null)
  in
  List.iter
    (fun (meth, path, route) ->
      refused
        (Printf.sprintf "%s %s with no token" meth path)
        (A.http w ~anonymous:true ~route meth path)
        unauthorized.absent;
      List.iter
        (fun (who, token) ->
          refused
            (Printf.sprintf "%s %s as %s" meth path who)
            (A.http w ~token ~route meth path)
            unauthorized.presented)
        [
          ("a person", person);
          ("a token that is not one", "not-a-token");
          ("a service account without the role", A.service_token ~roles:[] w);
        ])
    [
      ("GET", "/events", "/events");
      ("GET", "/events/" ^ id ^ "?fromSequence=1", "/events/{resourceId}");
      ("GET", A.resource_path id "/content", "/resources/{id}/content");
      ("GET", A.resource_path id "/jsonld", "/resources/{id}/jsonld");
      ("GET", A.resource_path id "/annotations", "/resources/{id}/annotations");
      ( "GET",
        A.resource_path id "/anchored-text",
        "/resources/{id}/anchored-text" );
      ("POST", "/resources", "/resources");
    ]

let unrecordable w =
  let ada = World.person_did w.A.world "ada" in
  let form ?(file = true) fields =
    Call.multipart
      (List.map (fun (name, value) -> Call.Field (name, value)) fields
      @
      if file then
        [
          Call.File
            {
              name = "file";
              filename = "upload";
              media = "application/octet-stream";
              bytes = "bytes";
            };
        ]
      else [])
  in
  let uri = "file://docs/refused.md" in
  let good =
    [ ("name", "Refused"); ("storageUri", uri); ("format", "text/markdown") ]
  in
  let keyed () =
    [ ("Xynolog-Principal", ada); ("Idempotency-Key", Fresh.uuid ()) ]
  in
  let post ?(headers = keyed ()) (kind, body) =
    A.http w ~route:"/resources" ~body
      ~headers:(("content-type", kind) :: headers)
      "POST" "/resources"
  in
  let answered what name fill (reply : Call.reply) =
    let status, _, _ = http_error name fill in
    Check.equal (what ^ ": its status") (`Int status) (`Int reply.status);
    Check.equal what (error_body name fill) (json_of what reply)
  in
  let naming what field (reply : Call.reply) =
    Check.equal (what ^ ": its status") (`Int 400) (`Int reply.status);
    Check.contains_text what field (text_at (json_of what reply) [ "error" ])
  in
  answered "unattributed" "upload-unattributed" []
    (post ~headers:[ ("Idempotency-Key", Fresh.uuid ()) ] (form good));
  naming "unkeyed" "Idempotency-Key"
    (post ~headers:[ ("Xynolog-Principal", ada) ] (form good));
  answered "not multipart" "upload-not-multipart" []
    (A.http w ~route:"/resources"
       ~json:(`Assoc (List.map (fun (name, value) -> (name, str value)) good))
       ~headers:(keyed ()) "POST" "/resources");
  List.iter
    (fun missing ->
      naming ("without " ^ missing) missing
        (post (form (List.remove_assoc missing good))))
    [ "name"; "format"; "storageUri" ];
  naming "without a file" "file" (post (form ~file:false good));
  let unknown = "application/x-not-a-thing" in
  answered "an unknown media type" "upload-unsupported-media"
    [ ("type", unknown) ]
    (post (form (("format", unknown) :: List.remove_assoc "format" good)));
  List.iter
    (fun (field, value) ->
      naming
        (field ^ "=" ^ value)
        field
        (post (form (good @ [ (field, value) ]))))
    [
      ("entityTypes", List.hd (entity_types 1));
      ("entityTypes", {|{"a":1}|});
      ("generator", "not json");
      ("generator", {|{"name":"x"}|});
    ];
  (* Nothing was recorded for any of them. *)
  Check.that "nothing is indexed at the URI" (A.storage_uri_entry w uri = None)

let media_type w =
  let format = "text/plain; charset=utf-8" in
  let id =
    A.created w
      (World.person_did w.A.world "ada")
      [
        ("name", "Charset");
        ("storageUri", "file://docs/charset.txt");
        ("format", format);
      ]
      "plain\n"
  in
  Check.equal "the format recorded" (str format)
    (Check.at (List.hd (A.stored w id)) [ "payload"; "format" ])

let updated w =
  let editor = A.person w "editor" in
  let uri = "file://docs/updated.md" in
  let id =
    A.created w editor.did
      [ ("name", "Updated"); ("storageUri", uri) ]
      "first\n"
  in
  let next = "second, and longer\n" in
  A.write w uri next;
  Check.equal "the answer"
    (`Assoc [ ("resourceId", str id) ])
    (A.ask editor "yield:update"
       (`Assoc
          [
            ("resourceId", str id);
            ("storageUri", str uri);
            ("contentChecksum", str (sha256 next));
            ("byteSize", `Int (String.length next));
          ]));
  let update =
    match A.stored w id with
    | [ _; update ] -> update
    | events ->
        failwith
          (Printf.sprintf "the stream holds %d events" (List.length events))
  in
  like "the update"
    (`Assoc
       [
         ("type", str "yield:updated");
         ("userId", str editor.did);
         ( "payload",
           `Assoc
             [
               ("contentChecksum", str (sha256 next));
               ("contentByteSize", `Int (String.length next));
             ] );
         ("metadata", `Assoc [ ("sequenceNumber", `Int 2) ]);
       ])
    update;
  let view = view_of w id in
  like "the representations"
    (`List
       [
         `Assoc
           [
             ("checksum", str (sha256 next));
             ("byteSize", `Int (String.length next));
             ("storageUri", str uri);
           ];
       ])
    (Check.at view [ "resource"; "representations" ]);
  Check.equal "the view's dateModified"
    (Check.at update [ "timestamp" ])
    (Check.at view [ "resource"; "dateModified" ]);
  Check.equal "the view's last sequence" (`Int 2)
    (Check.at view [ "lastSequence" ])

let mismatched w =
  let editor = A.person w "editor" in
  let uri = "file://docs/mismatch.md" in
  let id =
    A.created w editor.did
      [ ("name", "Mismatch"); ("storageUri", uri) ]
      "as stored\n"
  in
  let on_disk = "changed on disk\n" in
  A.write w uri on_disk;
  let first8 text = String.sub (sha256 text) 0 8 in
  Check.equal "the refusal"
    (str
       (refusal "checksum-mismatch"
          [
            ("uri", uri);
            ("expected", first8 "something else");
            ("actual", first8 on_disk);
          ]))
    (str
       (A.refused editor "yield:update"
          (`Assoc
             [
               ("resourceId", str id);
               ("storageUri", str uri);
               ("contentChecksum", str (sha256 "something else"));
               ("byteSize", `Int 1);
             ])));
  Check.equal "the stream's events" (`Int 1)
    (`Int (List.length (A.stored w id)));
  let not_a_file = "docs/mismatch.md" in
  Check.equal "the refusal of a URI that is no file URI"
    (str (refusal "storage-uri-scheme" [ ("uri", not_a_file) ]))
    (str
       (A.refused editor "yield:update"
          (`Assoc
             [
               ("resourceId", str id);
               ("storageUri", str not_a_file);
               ("contentChecksum", str (sha256 "x"));
               ("byteSize", `Int 1);
             ])))

let already_there w =
  let author = A.person w "author" in
  let uri = "file://notes/already-here.md" in
  let content = "written by hand\n" in
  A.write w uri content;
  let id =
    A.string_member "resourceId"
      (A.ask author "yield:create"
         (`Assoc
            [
              ("name", str "Already here");
              ("storageUri", str uri);
              ("contentChecksum", str (sha256 content));
              ("byteSize", `Int (String.length content));
              ("format", str "text/markdown");
            ]))
  in
  Check.that ("the id's form: " ^ id) (Check.matches_re minted id);
  let created = List.hd (A.stored w id) in
  like "the creation"
    (`Assoc
       [
         ("type", str "yield:created");
         ("userId", str author.did);
         ( "payload",
           `Assoc
             [
               ("name", str "Already here");
               ("storageUri", str uri);
               ("entityTypes", `List []);
               ("isDraft", `Bool false);
             ] );
       ])
    created;
  Check.that "the creation names no language"
    (Protocol.at created [ "payload"; "language" ] = None);
  Check.equal "the storage-uri entry"
    (`Assoc [ ("uri", str uri); ("resourceId", str id) ])
    (Option.get (A.storage_uri_entry w uri))

let archived w =
  let owner = A.person w "owner" in
  let uri = "file://docs/archived.md" in
  let id =
    A.created w owner.did
      [ ("name", "Archived"); ("storageUri", uri) ]
      "to be archived\n"
  in
  let command extra =
    `Assoc ([ ("resourceId", str id); ("storageUri", str uri) ] @ extra)
  in
  let is_archived () = Check.at (view_of w id) [ "resource"; "archived" ] in
  Check.equal "the archive's answer" (`Assoc [])
    (A.ask owner "mark:archive" (command [ ("keepFile", `Bool true) ]));
  Check.that "the file is kept" (Sys.file_exists (A.content_path w uri));
  Check.equal "archived" (`Bool true) (is_archived ());
  like "the archive"
    (`Assoc [ ("type", str "mark:archived"); ("payload", `Assoc []) ])
    (List.nth (A.stored w id) 1);
  (* An archived resource is still found by its URI. *)
  Check.equal "the storage-uri entry"
    (`Assoc [ ("uri", str uri); ("resourceId", str id) ])
    (Option.get (A.storage_uri_entry w uri));
  Check.equal "the unarchive's answer" (`Assoc [])
    (A.ask owner "mark:unarchive" (command []));
  Check.equal "archived" (`Bool false) (is_archived ());
  Check.equal "the archive's answer" (`Assoc [])
    (A.ask owner "mark:archive" (command []));
  Check.that "the file is removed"
    (not (Sys.file_exists (A.content_path w uri)));
  Check.equal "the unarchive's refusal"
    (str (refusal "unarchive-without-file" [ ("uri", uri) ]))
    (str (A.refused owner "mark:unarchive" (command [])));
  Check.equal "the stream's events"
    (types
       [ "yield:created"; "mark:archived"; "mark:unarchived"; "mark:archived" ])
    (`List (List.map (fun e -> Check.at e [ "type" ]) (A.stored w id)))

let retyped w =
  let curator = A.person w "curator" in
  let vocabulary = entity_types 4 in
  let kind = List.nth vocabulary in
  let id =
    A.created w curator.did
      [
        ("name", "Typed");
        ("storageUri", "file://docs/typed.md");
        ("entityTypes", Yojson.Safe.to_string (types [ kind 0; kind 1 ]));
      ]
      "typed\n"
  in
  Check.equal "the answer" (`Assoc [])
    (A.ask curator "mark:update-entity-types"
       (`Assoc
          [
            ("resourceId", str id);
            ("currentEntityTypes", types [ kind 0; kind 1 ]);
            ("updatedEntityTypes", types [ kind 1; kind 2; kind 3 ]);
          ]));
  Check.equal "the events after the creation"
    (`List
       [
         `List [ str "mark:entity-tag-added"; str (kind 2) ];
         `List [ str "mark:entity-tag-added"; str (kind 3) ];
         `List [ str "mark:entity-tag-removed"; str (kind 0) ];
       ])
    (`List
       (List.map
          (fun e ->
            `List
              [ Check.at e [ "type" ]; Check.at e [ "payload"; "entityType" ] ])
          (List.tl (A.stored w id))));
  Check.equal "the view's entity types"
    (types [ kind 1; kind 2; kind 3 ])
    (Check.at (view_of w id) [ "resource"; "entityTypes" ]);
  let unheard = [ "Unheard"; "Of" ] in
  let seeded =
    (Protocol.tables ()).archivist_default_entity_types.entity_types
  in
  Check.that "the vocabulary has neither Unheard nor Of"
    (not (List.exists (fun t -> List.mem t seeded) unheard));
  Check.equal "the refusal"
    (str
       (refusal "entity-type-not-registered"
          [ ("types", String.concat ", " unheard) ]))
    (str
       (A.refused curator "mark:update-entity-types"
          (`Assoc
             [
               ("resourceId", str id);
               ("currentEntityTypes", types [ kind 1 ]);
               ("updatedEntityTypes", types (kind 1 :: unheard));
             ])));
  Check.equal "the stream's events" (`Int 4)
    (`Int (List.length (A.stored w id)))

let filed w =
  let uri = "file://docs/résumé 📚.md" in
  let id =
    A.created w
      (World.person_did w.A.world "ada")
      [ ("name", "Filed"); ("storageUri", uri) ]
      "filed\n"
  in
  Check.that "the stream is where the layout files it"
    (A.stream_files w id <> []);
  Check.that "the view is where the layout files it"
    (Sys.file_exists (A.view_path w id));
  Run.eventually "the index entry" 5.0 (fun () -> A.storage_uri_entry w uri)
  |> Check.equal "the storage-uri entry"
       (`Assoc [ ("uri", str uri); ("resourceId", str id) ])

let ordered w =
  let owner = A.person w "owner" in
  let id =
    A.created w owner.did
      [ ("name", "Ordered"); ("storageUri", "file://docs/ordered.md") ]
      "ordered\n"
  in
  let follower = A.person w "follower" ~scopes:[ id ] in
  (* Twenty events of one stream, each sent as the last is answered. *)
  for _ = 1 to 10 do
    List.iter
      (fun operation ->
        ignore
          (A.ask owner operation (`Assoc [ ("resourceId", str id) ])
            : Yojson.Safe.t))
      [ "mark:archive"; "mark:unarchive" ]
  done;
  let last = persisted_id "mark:unarchived" id 21 in
  ignore
    (Bus_stream.next follower.stream "the last scoped fact" (fun m ->
         match m.frame with
         | Some f -> f.scope = Some id && m.id = Some last
         | None -> false)
      : Bus_stream.message);
  let of_it e = Protocol.at e [ "resourceId" ] = Some (str id) in
  ignore
    (Run.eventually "the last unscoped fact" 10.0 (fun () ->
         List.find_opt
           (fun e -> of_it e && sequence_of e = 21)
           (A.facts follower))
      : Yojson.Safe.t);
  (* The upload is answered with the creation's fact queued, not published,
     so it may reach the follower, who subscribed after the answer, first. *)
  let without_creation = function
    | 1 :: rest -> rest
    | sequences -> sequences
  in
  let expected = `List (List.init 20 (fun i -> `Int (i + 2))) in
  let ints sequences = `List (List.map (fun n -> `Int n) sequences) in
  Check.equal "the scoped facts' sequence numbers" expected
    (ints
       (without_creation
          (List.filter_map
             (fun (f : Bus_stream.frame) ->
               if f.scope = Some id then Some (sequence_of f.payload) else None)
             (Bus_stream.frames follower.stream))));
  Check.equal "the unscoped facts' sequence numbers" expected
    (ints
       (without_creation
          (List.map sequence_of (List.filter of_it (A.facts follower)))));
  Check.equal "the stream's sequence numbers"
    (ints (1 :: List.init 20 (fun i -> i + 2)))
    (ints (List.map sequence_of (A.stored w id)))

let tests implementations =
  with_archivist implementations "a resource"
    [
      case "is stored, recorded and published by an upload" uploaded;
      case
        "publishes each event to its resource's scope too, with the id a \
         stream resumes from"
        scoped;
      case "serves the content it stored, as the media type recorded" served;
      case
        "answers 404 for content and for a description of a resource it does \
         not hold"
        not_held;
      case
        "describes a resource as JSON-LD, its descriptor as browse answers it, \
         and browse with the sequence number of its last event"
        described;
      case "admits only a service to its HTTP surface" guarded;
      case "refuses an upload it cannot record, saying why" unrecordable;
      case "records the media type as given, parameters included" media_type;
      case "records new content for a resource: its checksum and size, and when"
        updated;
      case
        "refuses content that is not the checksum the command states, and \
         records nothing"
        mismatched;
      case "records content already in the working tree, on yield:create"
        already_there;
      case "archives and unarchives, removing the file only when told to"
        archived;
      case "changes a resource's entity types, to types the vocabulary has"
        retyped;
      case "files every stream, view and index entry where the shard table says"
        filed;
    ]
  @ with_archivist implementations "the facts"
      [
        case
          "are published in the order their events were appended, to each \
           scope and to none"
          ordered;
      ]
