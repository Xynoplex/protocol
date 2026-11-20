(* Browse (ARCHIVIST.md § Browse, § The HTTP surface): the reads the
   Archivist answers from its views, its log, the working tree and the
   knowledge base's engines, on the bus; and from the anchored-text store,
   over HTTP. *)

open Xynolog_conformance
open Support
open Case

(* Well short of how long a read of anchored text is held for the Smelter:
   a read answered within it was not held. *)
let unheld () =
  float_of_int
    (Protocol.limit Archivist "GET" "/resources/{id}/anchored-text"
       "settleWaitSeconds")
  *. 2.0 /. 3.0

let bus_message_bytes () =
  Protocol.limit Gateway "POST" "/bus/emit" "maxBodyBytes"

let engine provider model =
  `Assoc [ ("provider", str provider); ("model", str model) ]

(* Written out of the order the engines' schema states them in: the answer
   is in the schema's. *)
let engines =
  `Assoc
    [
      ( "jobs",
        `Assoc
          [
            ("yield", engine "anthropic" "claude-sonnet-4-5");
            ( "mark",
              `Assoc
                [
                  ("tagging", engine "ollama" "gemma2:27b");
                  ("linking", engine "anthropic" "claude-haiku-4-5");
                  ("highlighting", engine "anthropic" "claude-haiku-4-5");
                ] );
          ] );
      ( "actors",
        `Assoc
          [
            ("gatherer", engine "ollama" "llama3");
            ("matcher", engine "anthropic" "claude-haiku-4-5");
          ] );
    ]

let empty = `Assoc []

let members = function
  | `Assoc members -> members
  | other -> failwith (Check.shown other ^ " is not an object")

let strings json path =
  List.map
    (function
      | `String s -> s | other -> failwith (Check.shown other ^ " is no string"))
    (Check.list json path)

let unscoped what event =
  Check.that
    (what ^ " has no resourceId: " ^ Check.shown event)
    (Protocol.at event [ "resourceId" ] = None)

let describes (w : A.t) =
  let reader = A.person w "reader" in
  let kb = A.ask reader "browse:kb-requested" empty in
  Check.matching "the knowledge base"
    [ ("name", str w.world.kb.name); ("domain", str w.world.kb.domain) ]
    kb;
  (* A checkout with no commit is on no branch yet. *)
  Check.equal "its other members" (`List [])
    (strs
       (List.filter
          (fun k -> not (List.mem k [ "name"; "domain"; "gitBranch" ]))
          (List.map fst (members kb))))

let vocabulary (w : A.t) =
  let curator = A.person w "curator" in
  let defaults =
    (Protocol.tables ()).archivist_default_entity_types.entity_types
  in
  let entity_types () =
    strings
      (A.ask curator "browse:entity-types-requested" empty)
      [ "entityTypes" ]
  in
  Check.equal "the entity types"
    (strs (sorted defaults))
    (strs (sorted (entity_types ())));
  let added () =
    List.filter (of_type "frame:entity-type-added") (A.stored w A.system)
  in
  let seeded = added () in
  Check.equal "the entity types seeded" (strs defaults)
    (`List (List.map (fun e -> Check.at e [ "payload"; "entityType" ]) seeded));
  Check.equal "their senders" (strs [ w.world.kb.did ])
    (`List
       (List.sort_uniq compare
          (List.map (fun e -> Check.at e [ "userId" ]) seeded)));
  List.iter (unscoped "a seeded entity type") seeded;
  Check.equal "their sequence numbers"
    (`List (List.mapi (fun at _ -> `Int (at + 1)) defaults))
    (`List
       (List.map (fun e -> Check.at e [ "metadata"; "sequenceNumber" ]) seeded));

  Check.equal "the add's response" empty
    (A.ask curator "frame:add-entity-type"
       (`Assoc [ ("tag", str "Zeitgeist") ]));
  let latest = last "the entity types added" (added ()) in
  Check.equal "the last added's sender" (str curator.did)
    (Check.at latest [ "userId" ]);
  Check.equal "the last added" (str "Zeitgeist")
    (Check.at latest [ "payload"; "entityType" ]);
  Check.that "the entity types hold Zeitgeist"
    (List.mem "Zeitgeist" (entity_types ()));
  Check.equal "entitytypes.json"
    (strs (sorted (defaults @ [ "Zeitgeist" ])))
    (strs
       (sorted
          (strings
             (present "entitytypes.json" (A.projection w "entitytypes.json"))
             [ "entityTypes" ])));

  (* A system fact is published once, with no scope. *)
  unscoped "the fact"
    (A.fact curator
       ~matches:(fun e ->
         Check.at e [ "payload"; "entityType" ] = str "Zeitgeist")
       "frame:entity-type-added")

let tag_schemas (w : A.t) =
  let curator = A.person w "curator" in
  let schema id name =
    `Assoc
      [
        ("id", str id);
        ("name", str name);
        ("description", str (name ^ " analysis"));
        ("domain", str "general");
        ( "tags",
          `List
            [
              `Assoc
                [
                  ("name", str "Claim");
                  ("description", str "What is asserted");
                  ("examples", strs [ "The sky is blue" ]);
                ];
            ] );
      ]
  in
  let answered () =
    Check.list
      (A.ask curator "browse:tag-schemas-requested" empty)
      [ "tagSchemas" ]
  in
  Check.equal "the tag schemas, before any" (`List []) (`List (answered ()));
  let add id name =
    A.ask curator "frame:add-tag-schema" (`Assoc [ ("schema", schema id name) ])
  in
  Check.equal "the add's response" empty (add "toulmin" "Toulmin");
  ignore (add "irac" "IRAC" : Yojson.Safe.t);
  ignore (add "toulmin" "Toulmin, revised" : Yojson.Safe.t);

  let answered = answered () in
  Check.equal "the tag schemas"
    (`List
       [
         `List [ str "irac"; str "IRAC" ];
         `List [ str "toulmin"; str "Toulmin, revised" ];
       ])
    (`List
       (List.map
          (fun s -> `List [ Check.at s [ "id" ]; Check.at s [ "name" ] ])
          answered));
  Check.equal "tagschemas.json"
    (`Assoc [ ("tagSchemas", `List answered) ])
    (present "tagschemas.json" (A.projection w "tagschemas.json"));
  Check.equal "the tag schemas added" (`Int 3)
    (`Int
       (List.length
          (List.filter (of_type "frame:tag-schema-added") (A.stored w A.system))))

let lists_resources (w : A.t) =
  let ada = World.person_did w.world "ada" in
  let reader = A.person w "reader" in
  let ids =
    List.map
      (fun (n, types) ->
        let id =
          A.created w ada
            [
              ("name", "Listed " ^ n);
              ("storageUri", "file://listed/" ^ n ^ ".md");
              ("entityTypes", Yojson.Safe.to_string (strs types));
            ]
            (n ^ "\n")
        in
        Run.sleep 0.005;
        id)
      [
        ("one", [ "Concept" ]);
        ("two", [ "Event" ]);
        ("three", [ "Concept"; "Event" ]);
      ]
  in
  let id at = List.nth ids at in
  ignore (A.ask reader "mark:archive" (on (id 1)) : Yojson.Safe.t);

  let mine response =
    strs
      (List.filter
         (fun id -> List.mem id ids)
         (List.map (A.string_member "@id")
            (Check.list response [ "resources" ])))
  in
  let browse filters =
    A.ask reader "browse:resources-requested" (`Assoc filters)
  in
  let all = browse [] in
  Check.equal "every resource" (strs [ id 2; id 1; id 0 ]) (mine all);
  Check.matching "the first page" [ ("offset", `Int 0); ("limit", `Int 50) ] all;
  Check.equal "the whole count"
    (`Int (List.length (Check.list all [ "resources" ])))
    (Check.at all [ "total" ]);

  Check.equal "those not archived"
    (strs [ id 2; id 0 ])
    (mine (browse [ ("archived", `Bool false) ]));
  Check.equal "those archived"
    (strs [ id 1 ])
    (mine (browse [ ("archived", `Bool true) ]));
  Check.equal "the events"
    (strs [ id 2; id 1 ])
    (mine (browse [ ("entityType", str "Event") ]));

  let page =
    browse
      [ ("entityType", str "Concept"); ("offset", `Int 1); ("limit", `Int 1) ]
  in
  Check.matching "the page"
    [ ("total", `Int 2); ("offset", `Int 1); ("limit", `Int 1) ]
    page;
  Check.equal "the page's resources" (strs [ id 0 ]) (mine page)

let resource_events (w : A.t) =
  let reader = A.person w "reader" in
  let editor = A.person w "editor" in
  let id =
    A.created w reader.did
      [ ("name", "Eventful"); ("storageUri", "file://listed/eventful.md") ]
      "eventful\n"
  in
  ignore (A.ask editor "mark:archive" (on id) : Yojson.Safe.t);
  ignore (A.ask reader "mark:unarchive" (on id) : Yojson.Safe.t);

  let events filters =
    A.ask reader "browse:events-requested"
      (`Assoc (("resourceId", str id) :: filters))
  in
  let all = events [] in
  let listed = Check.list all [ "events" ] in
  Check.equal "the events"
    (numbered_as
       [ ("yield:created", 1); ("mark:archived", 2); ("mark:unarchived", 3) ])
    (numbered_events listed);
  Check.equal "their agents"
    (strs [ reader.did; editor.did; reader.did ])
    (`List (List.map (fun e -> Check.at e [ "agent"; "@id" ]) listed));
  Check.matching "the answer" [ ("total", `Int 3); ("resourceId", str id) ] all;

  Check.equal "those of a type" (`Int 1)
    (`Int
       (List.length
          (Check.list (events [ ("type", str "mark:archived") ]) [ "events" ])));
  Check.equal "those of a sender"
    (strs [ "yield:created"; "mark:unarchived" ])
    (types_of (Check.list (events [ ("userId", str reader.did) ]) [ "events" ]));
  Check.equal "the first two"
    (strs [ "yield:created"; "mark:archived" ])
    (types_of (Check.list (events [ ("limit", `Int 2) ]) [ "events" ]));
  Check.equal "those of a resource with none" (`List [])
    (Check.at
       (A.ask reader "browse:events-requested" (on (String.make 32 'a')))
       [ "events" ])

let not_found (w : A.t) =
  let reader = A.person w "reader" in
  let channel = "browse:resource-requested" in
  let answer = A.request reader channel (on (String.make 32 'b')) in
  Check.that "it is refused" (not answer.ok);
  Check.equal "the refusal"
    (refusal_body ~channel "resource-not-found" [])
    answer.payload

let attributed described = Check.at described [ "resource"; "wasAttributedTo" ]

let first_agent (reader : A.client) id =
  Check.at
    (List.hd
       (Check.list
          (A.ask reader "browse:events-requested" (on id))
          [ "events" ]))
    [ "agent" ]

let profiled_by (reader : A.client) (principal : A.client) =
  A.fact reader
    ~matches:(fun e -> Check.at e [ "userId" ] = str principal.did)
    "principal:profiled"

let profile_of (profiled : Yojson.Safe.t) =
  `Assoc
    [
      ("name", Check.at profiled [ "payload"; "name" ]);
      ("since", Check.at profiled [ "timestamp" ]);
    ]

let profiles (w : A.t) =
  Check.at
    (present "profiles.json" (A.projection w "profiles.json"))
    [ "profiles" ]

(* The gateway profiles a person, from their token, when they first write. *)
let first_write (person : A.client) id =
  ignore
    (A.ask person "mark:update-entity-types"
       (`Assoc
          [
            ("resourceId", str id);
            ("currentEntityTypes", `List []);
            ("updatedEntityTypes", strs [ "Person" ]);
          ])
      : Yojson.Safe.t)

let names_person (w : A.t) =
  let lovelace = A.person w "lovelace" in
  let reader = A.person w "reader" in
  let id =
    A.created w lovelace.did
      [ ("name", "Named"); ("storageUri", "file://listed/named.md") ]
      "named\n"
  in
  first_write lovelace id;
  let profiled = profiled_by reader lovelace in
  unscoped "the profile" profiled;
  let name = Check.at profiled [ "payload"; "name" ] in
  Check.equal "profiles.json's profile" (profile_of profiled)
    (Check.at (profiles w) [ lovelace.did ]);

  let named =
    `Assoc
      [ ("@type", str "Person"); ("@id", str lovelace.did); ("name", name) ]
  in
  Check.equal "the resource's attribution" (`List [ named ])
    (attributed (A.ask reader "browse:resource-requested" (on id)));
  Check.equal "the first event's agent" named (first_agent reader id);
  (* The log holds the event as recorded: no name in it. *)
  Check.equal "the attribution recorded"
    (`List [ `Assoc [ ("@type", str "Person"); ("@id", str lovelace.did) ] ])
    (Check.at (List.hd (A.stored w id)) [ "payload"; "wasAttributedTo" ])

let names_agent (w : A.t) =
  let agent = A.sidecar w "names-itself" in
  let reader = A.person w "reader" in
  (* An agent's token carries no name: it says what it is called itself. *)
  A.emit agent "principal:profile" (`Assoc [ ("name", str "The Indexer") ]);
  ignore
    (A.fact reader
       ~matches:(fun e ->
         Check.at e [ "userId" ] = str agent.did
         && Check.at e [ "payload"; "name" ] = str "The Indexer")
       "principal:profiled"
      : Yojson.Safe.t);
  let id =
    A.created w agent.did
      [
        ("name", "Made by an agent");
        ("storageUri", "file://listed/by-an-agent.md");
      ]
      "by an agent\n"
  in

  let named =
    `Assoc
      [
        ("@type", str "Software");
        ("@id", str agent.did);
        ("name", str "The Indexer");
      ]
  in
  Check.equal "the resource's attribution" (`List [ named ])
    (attributed (A.ask reader "browse:resource-requested" (on id)));
  Check.equal "the first event's agent" named (first_agent reader id);
  (* The log holds the event as recorded: no name in it. *)
  Check.equal "the attribution recorded"
    (`List [ `Assoc [ ("@type", str "Software"); ("@id", str agent.did) ] ])
    (Check.at (List.hd (A.stored w id)) [ "payload"; "wasAttributedTo" ])

let profiles_asked (w : A.t) =
  let lovelace = A.person w "lovelace-asked" in
  let reader = A.person w "reader" in
  let id =
    A.created w lovelace.did
      [ ("name", "Asked after"); ("storageUri", "file://listed/asked.md") ]
      "asked\n"
  in
  first_write lovelace id;
  let profiled = profiled_by reader lovelace in

  let nobody = World.person_did w.world "nobody-profiled" in
  Check.equal "the profiles"
    (`Assoc [ ("profiles", `Assoc [ (lovelace.did, profile_of profiled) ]) ])
    (A.ask reader "browse:profiles-requested"
       (`Assoc [ ("dids", strs [ lovelace.did; nobody ]) ]))

let records_profile (w : A.t) =
  let namer = A.sidecar w "namer" in
  let mine () =
    List.filter_map
      (fun e ->
        if Check.at e [ "userId" ] = str namer.did then
          Some (Check.at e [ "payload"; "name" ])
        else None)
      (List.filter (of_type "principal:profiled") (A.stored w A.system))
  in
  let profile name =
    A.emit namer "principal:profile" (`Assoc [ ("name", str name) ])
  in
  let profiled name =
    A.fact namer
      ~matches:(fun e ->
        Check.at e [ "userId" ] = str namer.did
        && Check.at e [ "payload"; "name" ] = str name)
      "principal:profiled"
  in

  profile "First";
  ignore (profiled "First" : Yojson.Safe.t);
  profile "First";
  profile "Second";
  let second = profiled "Second" in

  Check.equal "the names recorded"
    (strs [ "First"; "Second" ])
    (`List (mine ()));
  Check.equal "profiles.json's profile" (profile_of second)
    (Check.at (profiles w) [ namer.did ])

let lists_directory (w : A.t) =
  let reader = A.person w "reader" in
  let tree = w.dirs.root // "tree" in
  A.make_dirs (tree // "inner");
  A.write_file (tree // "b.md") "bb";
  A.write_file (tree // "a.md") "a";
  A.write_file (tree // ".hidden") "h";

  let listing =
    A.ask reader "browse:directory-requested" (`Assoc [ ("path", str "tree") ])
  in
  Check.equal "the path" (str "tree") (Check.at listing [ "path" ]);
  let entries = Check.list listing [ "entries" ] in
  Check.equal "the entries"
    (`List
       [
         strs [ "file"; "a.md"; "tree/a.md" ];
         strs [ "file"; "b.md"; "tree/b.md" ];
         strs [ "dir"; "inner"; "tree/inner" ];
       ])
    (`List
       (List.map
          (fun e ->
            `List
              [
                Check.at e [ "type" ];
                Check.at e [ "name" ];
                Check.at e [ "path" ];
              ])
          entries));
  Check.equal "b.md's size" (`Int 2)
    (Check.at
       (List.find (fun e -> Check.at e [ "name" ] = str "b.md") entries)
       [ "size" ]);

  let root =
    List.map
      (fun e -> Check.at e [ "name" ])
      (Check.list
         (A.ask reader "browse:directory-requested"
            (`Assoc [ ("path", str ".") ]))
         [ "entries" ])
  in
  Check.that "the root lists no .xynolog" (not (List.mem (str ".xynolog") root));
  Check.that "the root lists no .git" (not (List.mem (str ".git") root));

  List.iter
    (fun (path, refused) ->
      let answer =
        A.request reader "browse:directory-requested"
          (`Assoc [ ("path", str path) ])
      in
      Check.that (path ^ ": it is refused") (not answer.ok);
      Check.equal path
        (`Assoc [ ("path", str path); ("message", str (refusal refused [])) ])
        answer.payload)
    [
      ("../outside", "path-escapes");
      ("/etc", "path-escapes");
      ("tree/missing", "path-not-found");
    ]

let answer_too_large (w : A.t) =
  let reader = A.person w "reader" in
  (* Each entry names its file twice, by name and by path: 24,000 names of
     200 characters are more than the 8 MiB a bus message may be. *)
  let wide = w.dirs.root // "wide" in
  Sys.mkdir wide 0o755;
  for n = 0 to 23_999 do
    A.write_file (wide // Printf.sprintf "%05d%s.md" n (String.make 195 'w')) ""
  done;

  (* Well inside the 30 s a requester waits: the failure is the answer, not
     the wait running out. *)
  let answer =
    A.request ~timeout:10.0 reader "browse:directory-requested"
      (`Assoc [ ("path", str "wide") ])
  in
  Check.that "it is refused" (not answer.ok);
  Check.equal "the path" (str "wide") (Check.at answer.payload [ "path" ]);
  let limit = bus_message_bytes () in
  let message = A.string_member "message" answer.payload in
  let said =
    Re.exec_opt
      (Re.compile
         (Re.whole_string
            (Boot_refusals.pattern_of
               ~held:[ ("bytes", Re.group (Re.rep1 Re.digit)) ]
               (unsent "too-large")
               [ ("maxBodyBytes", string_of_int limit) ])))
      message
  in
  match said with
  | Some said ->
      Check.that
        (message ^ ": the answer's size is more than a bus message may be")
        (int_of_string (Re.Group.get said 1) > limit)
  | None ->
      failwith
        (Printf.sprintf "%S is not archivist/messages.json's too-large" message)

let not_yet = `Assoc [ ("kind", str "not-yet") ]

let from_store (w : A.t) =
  let content = "%PDF-1.4 the suite never parses this\n" in
  let id = scan w "Scanned" "file://scans/scanned.pdf" content in
  (* An emoji and a mathematical letter, each outside the Basic Multilingual
     Plane: every word after the first is at another place in a count of
     code points than in one of UTF-16 code units or of bytes. *)
  let text = "😀 alpha 𝒜eta gamma" in
  A.smelt w ~entry:(anchored_entry text) id (A.sha256 content);

  (* Each word's offsets are the entry's own, in code points, as stored. *)
  let answer = A.anchored_text w id in
  Check.matching "the answer"
    [ ("kind", str "extracted"); ("text", str text); ("method", str "ocr") ]
    answer;
  let item start end_ y width =
    `Assoc
      [
        ("start", `Int start);
        ("end", `Int end_);
        ("page", `Int 1);
        ("x", `Int 72);
        ("y", `Int y);
        ("width", `Int width);
        ("height", `Int 12);
      ]
  in
  Check.equal "its items"
    (`List
       [ item 0 1 700 6; item 2 7 686 30; item 8 12 672 24; item 13 18 658 30 ])
    (Check.at answer [ "items" ]);

  (* The entry stands; the writer moves on to another stamp; the entry is no
     longer taken. *)
  A.state_stamp w "a-newer-smelter";
  let other = scan w "Scanned again" "file://scans/again.pdf" content in
  A.smelt w ~outcome:Indexed other (A.sha256 content);
  Check.equal "another resource of the same bytes" not_yet
    (A.anchored_text w other)

let other_version (w : A.t) =
  let content = "%PDF-1.4 an entry of another version\n" in
  let checksum = A.sha256 content in
  let id = scan w "Read before" "file://scans/before.pdf" content in
  A.smelt w ~entry:(anchored_entry "alpha beta") id checksum;

  (* The same entry, under the stamp its writer states, as another version. *)
  let entry = A.anchored_text_entry_path w checksum in
  let v = A.entry_format () in
  let rest =
    match Yojson.Safe.from_file entry with
    | `Assoc members ->
        Check.equal "the entry's version" (`Int v)
          (present "its v" (List.assoc_opt "v" members));
        List.remove_assoc "v" members
    | other -> failwith (Check.shown other ^ " is no entry")
  in
  A.write_file entry
    (Yojson.Safe.to_string (`Assoc (("v", `Int (v - 1)) :: rest)));

  Check.equal "the answer" not_yet (A.anchored_text w id)

let declined (w : A.t) =
  let locked = "%PDF-1.4 locked\n" in
  let id = scan w "Locked" "file://scans/locked.pdf" locked in
  A.smelt w
    ~entry:(`Assoc [ ("declined", str "encrypted") ])
    id (A.sha256 locked);
  Check.equal "the decline"
    (`Assoc [ ("kind", str "declined"); ("declined", str "encrypted") ])
    (A.anchored_text w id);

  let unknown = `Assoc [ ("kind", str "unknown") ] in
  Check.equal "no resource" unknown (A.anchored_text w (String.make 32 'c'));
  (* An id the record could file nothing under names no resource either. *)
  Check.equal "an id of nothing" unknown (A.anchored_text w "not.an.id")

let no_map = `Assoc [ ("kind", str "no-map") ]

let no_map_at_once (w : A.t) =
  let reader = World.person_did w.world "reader" in
  let pixels = "\x89PNG\r\n\x1a\n\x00\xff\x10" in
  let image =
    A.created w reader
      [
        ("name", "Pictured");
        ("storageUri", "file://scans/pictured.png");
        ("format", "image/png");
      ]
      pixels
  in
  let markdown =
    A.created w reader
      [
        ("name", "Written");
        ("storageUri", "file://scans/written.md");
        ("format", "text/markdown");
      ]
      "# Written\n"
  in

  (* Asked with nothing stored for either and no signal sent for either. *)
  let asked = Run.now () in
  Check.equal "the image" no_map (A.anchored_text w image);
  Check.equal "the markdown" no_map (A.anchored_text w markdown);
  (* Neither of these was held. *)
  Check.that "neither was held" (Run.now () -. asked < unheld ())

let no_map_beside_entry (w : A.t) =
  let reader = World.person_did w.world "reader" in
  let content = "%PDF-1.4 the same bytes, recorded as two media types\n" in
  let pdf = scan w "As a PDF" "file://scans/as-a-pdf.pdf" content in
  let image =
    A.created w reader
      [
        ("name", "As an image");
        ("storageUri", "file://scans/as-an-image.png");
        ("format", "image/png");
      ]
      content
  in
  A.smelt w ~entry:(anchored_entry "alpha beta") pdf (A.sha256 content);

  Check.matching "the PDF"
    [ ("kind", str "extracted"); ("text", str "alpha beta") ]
    (A.anchored_text w pdf);
  Check.equal "the image" no_map (A.anchored_text w image)

let skipped (w : A.t) =
  let content = "%PDF-1.4 skipped\n" in
  let id = scan w "Skipped" "file://scans/skipped.pdf" content in

  (* The Smelter stores what deriving a PDF's text came to, a decline too.
     One it settled with no entry has lost its entry, and is to be asked for
     again. *)
  A.smelt w ~outcome:Skipped id (A.sha256 content);
  Check.equal "the answer" not_yet (A.anchored_text w id)

let held (w : A.t) =
  let content = "%PDF-1.4 asked for before it is read\n" in
  let id = scan w "Awaited" "file://scans/awaited.pdf" content in

  (* Asked first, with nothing stored and nothing settled; the entry and the
     signal come while the read waits. *)
  let answer, () =
    Eio.Fiber.pair
      (fun () -> A.anchored_text w id)
      (fun () ->
        Run.sleep 0.25;
        A.smelt w ~entry:(anchored_entry "alpha beta") id (A.sha256 content))
  in
  Check.matching "the answer"
    [
      ("kind", str "extracted");
      ("text", str "alpha beta");
      ("method", str "ocr");
    ]
    answer

let stopped (w : A.t) =
  let content = "%PDF-1.4 asked for as the Archivist stops\n" in
  let id = scan w "Interrupted" "file://scans/interrupted.pdf" content in

  (* Asked with nothing stored and nothing settled, so the read waits; told
     to stop while it does. *)
  let asked = Run.now () in
  Fun.protect ~finally:(fun () -> A.restart w) @@ fun () ->
  let answer, code =
    Eio.Fiber.pair
      (fun () -> A.anchored_text w id)
      (fun () ->
        Run.sleep 0.25;
        Archivist.stop w.archivist)
  in
  Check.equal "the answer" not_yet answer;
  Check.equal "its exit code" (`Int 0)
    (match code with Some c -> `Int c | None -> `Null);
  (* The stop ended this one: it did not wait the read out. *)
  Check.that "the read was not waited out" (Run.now () -. asked < unheld ())

let whole (w : A.t) =
  let content = "%PDF-1.4 a long document\n" in
  let id = scan w "Long" "file://scans/long.pdf" content in
  let limit = bus_message_bytes () in
  (* Each word's item is more than sixty bytes of the answer. *)
  let words = List.init ((limit + 59) / 60) (Printf.sprintf "word%d") in
  let text = String.concat " " words in
  A.smelt w ~entry:(anchored_entry text) id (A.sha256 content);

  let reply =
    A.http w ~route:"/resources/{id}/anchored-text" "GET"
      (A.resource_path id "/anchored-text")
  in
  Check.equal (Call.shown reply.text) (`Int 200) (`Int reply.status);
  (* The gateway takes no emit larger than this, so no reply on the bus could
     carry this answer. *)
  Check.that "the answer is larger than a bus message"
    (String.length reply.text > limit);
  let answer = present "the answer's JSON" reply.json in
  Check.equal "its kind" (str "extracted") (Check.at answer [ "kind" ]);
  Check.equal "its text" (str text) (Check.at answer [ "text" ]);
  (* The words are ASCII: a count of code points is one of bytes. *)
  Check.equal "its items' words" (strs words)
    (`List
       (List.map
          (fun item ->
            match (Check.at item [ "start" ], Check.at item [ "end" ]) with
            | `Int start, `Int end_ ->
                str (String.sub text start (end_ - start))
            | _ -> failwith (Check.shown item ^ " has no offsets"))
          (Check.list answer [ "items" ])))

let lists_engines (w : A.t) =
  let reader = A.person w "reader" in
  let job ?motivation type_ =
    `Assoc
      (("jobType", str type_)
      ::
      (match motivation with
      | Some m -> [ ("params", `Assoc [ ("motivation", str m) ]) ]
      | None -> []))
  in
  Check.equal "the engines"
    (`Assoc
       [
         ( "engines",
           `List
             [
               `Assoc
                 [
                   ("engine", engine "anthropic" "claude-haiku-4-5");
                   ( "jobs",
                     `List
                       [
                         job ~motivation:"highlighting" "mark";
                         job ~motivation:"linking" "mark";
                       ] );
                 ];
               `Assoc
                 [
                   ("engine", engine "ollama" "gemma2:27b");
                   ("jobs", `List [ job ~motivation:"tagging" "mark" ]);
                 ];
               `Assoc
                 [
                   ("engine", engine "anthropic" "claude-sonnet-4-5");
                   ("jobs", `List [ job "yield" ]);
                 ];
               `Assoc [ ("engine", engine "ollama" "llama3") ];
             ] );
       ])
    (A.ask reader "browse:engines-requested" empty)

let no_engines (w : A.t) =
  let reader = A.person w "reader" in
  Check.equal "the engines"
    (`Assoc [ ("engines", `List []) ])
    (A.ask reader "browse:engines-requested" empty);
  Check.equal "the knowledge base"
    (`Assoc [ ("name", str w.world.kb.name); ("domain", str w.world.kb.domain) ])
    (A.ask reader "browse:kb-requested" empty)

let tests implementations =
  with_archivist implementations "browsing"
    [
      case "describes the knowledge base: its name, its domain and its branch"
        describes;
      case
        "answers the vocabulary: the default entity types, seeded as the \
         knowledge base itself, and what is added"
        vocabulary;
      case "answers the tag schemas, by id, the latest under each" tag_schemas;
      case
        "lists resources newest first, filtered and paged, with the whole count"
        lists_resources;
      case
        "answers a resource's events, each with its sender as an agent, \
         filtered and limited"
        resource_events;
      case "answers not-found, by code, for a resource it has no events of"
        not_found;
      case "names each person a reply mentions by their profile" names_person;
      case
        "names each agent a reply mentions by what it said it is called, as a \
         person is named"
        names_agent;
      case
        "answers the profiles of the principals asked after, and leaves out \
         one who has none"
        profiles_asked;
      case
        "records a profile when the name is new, and not when it is the name \
         last recorded"
        records_profile;
      case "lists a directory of the working tree, and nothing outside it"
        lists_directory;
      case
        "answers a request whose answer is larger than the largest bus message \
         with a failure, at once"
        answer_too_large;
      case
        "answers anchored text from the store, under the stamp its writer \
         states"
        from_store;
      case
        "takes no anchored-text entry of another version than the store's, \
         whatever its stamp"
        other_version;
      case "answers a decline, and unknown for no resource" declined;
      case
        "answers no-map at once for a media type whose text is not derived, \
         with nothing settled"
        no_map_at_once;
      case
        "answers no-map for a media type whose text is not derived, though the \
         store holds an entry for the same bytes"
        no_map_beside_entry;
      case
        "answers not-yet for a PDF the Smelter skipped and stored no entry of"
        skipped;
      case
        "holds a read of anchored text until the Smelter has settled the \
         content"
        held;
      case
        "answers a read of anchored text it is holding when it is told to \
         stop, and stops"
        stopped;
      case "answers an anchored text larger than the largest bus message, whole"
        whole;
    ]
  @ with_archivist implementations ~engines "the engines"
      [
        case
          "lists each engine once, in the order it is first named, with the \
           jobs it is used for as a claim would name them, and no identity of \
           its own"
          lists_engines;
      ]
  @ with_archivist implementations ~git_sync:false
      "a knowledge base with no engines, that does not sync git"
      [ case "lists no engines, and names no branch" no_engines ]
