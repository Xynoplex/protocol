(* Bytes in and out: POST /resources, the pipe at GET /resources/{id} and its
   media-token alias, the JSON-LD description, and a resource's anchored
   text. The gateway stores nothing and records nothing: it forwards an
   upload to the Archivist, which stores the bytes and records the resource,
   and reads bytes, descriptions and anchored text back from it (the
   stand-in Archivist in harness/archivist_standin.ml). *)

open Xynolog_conformance
open Support

(* Every byte value, several times: nothing may be decoded, re-encoded or
   trimmed. *)
let every_byte = String.init 1024 (fun i -> Char.chr (i mod 256))

(* The requests an upload and a description once cost the gateway on the
   bus. *)
let requests =
  [ "yield:create"; "yield:clone-create"; "browse:resource-requested" ]

(* A multipart form of [fields], and of [file]'s bytes and media type. *)
let form ?file fields =
  List.map (fun (name, value) -> Call.Field (name, value)) fields
  @
  match file with
  | Some (bytes, media) ->
      [ Call.File { name = "file"; filename = "upload.bin"; media; bytes } ]
  | None -> []

type shared = {
  world : World.t;
  requests : Bus_stream.t;
      (** Hears every request channel an upload or a description could publish.
      *)
}

let make implementations plane =
  let world = plain implementations plane in
  let listener, _ = World.agent world "listener" in
  let requests =
    World.subscribe ~standing:true world listener
      (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ()) ~global:requests ())
  in
  { world; requests }

(* The claims the gateway's last call of [path] to the Archivist carried. *)
let last_call (w : World.t) matches =
  match
    List.find_opt matches (List.rev (Dynarray.to_list w.archivist.calls))
  with
  | Some call -> call
  | None -> failwith "the gateway made no such call of the Archivist"

let azp (call : Archivist_standin.call) =
  Option.bind call.claims (fun jwt -> Jose.Jwt.get_string_claim jwt "azp")

let last_upload (w : World.t) =
  Dynarray.get w.archivist.uploads (Dynarray.length w.archivist.uploads - 1)

let profile_watcher (w : World.t) =
  World.subscribe w (World.person w "watcher")
    (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ())
       ~global:[ "principal:profile" ] ())

let stamped_by (w : World.t) sub (f : Bus_stream.frame) =
  Protocol.at f.payload [ (Protocol.tables ()).bus_registry.stamps.emitter ]
  = Some (`String (World.person_did w sub))

let read_bytes ?(headers = []) (w : World.t) token id =
  Call.call ~token ~headers (World.origin w) "GET"
    (Protocol.path_of Gateway "GET" "/resources/{id}" [ ("id", id) ])

let upload_whole { world = w; _ } =
  let token =
    World.person w ~claims:[ ("name", `String "Una Uploader") ] "uploader"
  in
  let watcher = profile_watcher w in
  let storage_uri = Printf.sprintf "file://uploads/%s.png" (Fresh.uuid ()) in
  let fields =
    [
      ("name", "Every byte");
      ("format", "image/png");
      ("storageUri", storage_uri);
      ("language", "en");
      ("entityTypes", {|["Person","Place"]|});
      ("sourceResourceId", "res-source");
      ("sourceAnnotationId", "ann-source");
      ("generationPrompt", "a prompt");
      ( "generator",
        {|{"@type":"Software","name":"a model","provider":"ollama","model":"a-model"}|}
      );
      ("jobId", "job-1");
      ("isDraft", "true");
    ]
  in
  let reply =
    World.upload w token (form ~file:(every_byte, "image/png") fields)
  in
  status "the upload" 202 reply;
  conforms "POST" "/resources" reply;
  let recorded = last_upload w in
  Check.equal "the upload's answer"
    (`Assoc [ ("resourceId", `String recorded.resource_id) ])
    (Option.get reply.json);
  Check.that "the Archivist was sent the fields as given"
    (List.sort compare recorded.fields = List.sort compare fields);
  Check.that "the Archivist was sent every byte" (recorded.file = every_byte);
  Check.that "the upload is the uploader's"
    (recorded.principal = World.person_did w "uploader");
  Check.that "a person's upload carries no roles" (recorded.roles = None);
  Check.that "the gateway called the Archivist as itself"
    (azp (last_call w (fun c -> c.meth = "POST" && c.path = "/resources"))
    = Some World.gateway_client);
  let profile =
    Bus_stream.frame watcher "principal:profile"
      ~matches:(stamped_by w "uploader")
  in
  Check.equal "the profile's name" (`String "Una Uploader")
    (Check.at profile.payload [ "name" ]);
  let back = read_bytes w token recorded.resource_id in
  Check.that "the pipe serves every byte back" (back.text = every_byte)

let pipe_verbatim { world = w; _ } =
  let id = "res-" ^ Fresh.uuid () in
  Hashtbl.replace w.archivist.resources id
    {
      storage_uri = "file://" ^ id;
      media_type = "text/plain; charset=iso-8859-1";
    };
  Hashtbl.replace w.archivist.content ("file://" ^ id) every_byte;
  let token = World.person w "reader" in
  let reply =
    read_bytes ~headers:[ ("accept", "application/ld+json") ] w token id
  in
  status "the pipe" 200 reply;
  conforms "GET" "/resources/{id}" reply;
  Check.that "the pipe serves the bytes verbatim" (reply.text = every_byte);
  Check.equal "its content type" (`String "text/plain; charset=iso-8859-1")
    (`String (Option.get (Http.Header.get reply.headers "content-type")));
  Check.equal "its link"
    (`String
       (Printf.sprintf
          {|</resources/%s/jsonld>; rel="describedby"; type="application/ld+json"|}
          id))
    (`String (Option.get (Http.Header.get reply.headers "link")));
  Check.that "the gateway read the bytes as itself"
    (azp
       (last_call w (fun c ->
            c.path = Printf.sprintf "/resources/%s/content" id))
    = Some World.gateway_client)

let header name (reply : Call.reply) =
  match Http.Header.get reply.headers name with
  | Some value -> value
  | None ->
      failwith
        (Printf.sprintf "the reply carries no %s: %d %s" name reply.status
           (Call.shown reply.text))

(* A GET of [template] with its id filled by [id], encoded. *)
let get ?token (w : World.t) template id =
  Call.call ?token (World.origin w) "GET"
    (Protocol.path_of Gateway "GET" template [ ("id", id) ])

let store (w : World.t) id media_type =
  Hashtbl.replace w.archivist.resources id
    { storage_uri = "file://" ^ id; media_type };
  Hashtbl.replace w.archivist.content ("file://" ^ id) every_byte

let calls_since (w : World.t) asked =
  List.filteri (fun at _ -> at >= asked) (Dynarray.to_list w.archivist.calls)

let error_of (reply : Call.reply) = text_member "error" reply

(* Run [body] with the stand-in's mode changed by [set], and set back by
   [reset] after, however it ends. *)
let with_mode ~set ~reset body =
  set ();
  Fun.protect ~finally:reset body

let worker_roles () = [ Protocol.service_role (); Protocol.worker_role () ]

let small_upload ?(extra = []) () =
  form ~file:("x", "text/plain")
    ([
       ("name", "n");
       ("format", "text/plain");
       ("storageUri", "file://uploads/" ^ Fresh.uuid ());
     ]
    @ extra)

let worker_upload { world = w; _ } =
  let watcher = profile_watcher w in
  let worker, did = World.agent w ~roles:(worker_roles ()) "uploading-model" in
  let reply =
    World.upload w worker
      (form
         ~file:("# generated", "text/markdown")
         [
           ("name", "Generated");
           ("format", "text/markdown");
           ("storageUri", Printf.sprintf "file://uploads/%s.md" (Fresh.uuid ()));
           ("jobId", "job-2");
         ])
  in
  status "the worker's upload" 202 reply;
  let recorded = last_upload w in
  Check.that "the upload is the worker's agent's" (recorded.principal = did);
  (* The roles its account holds, as its token carries them. *)
  Check.equal "the roles the record is told"
    (`String (String.concat "," (worker_roles ())))
    (match recorded.roles with Some r -> `String r | None -> `Null);
  Bus_stream.quiet watcher "principal:profile" 0.5

let malformed_upload { world = w; _ } =
  let token = World.person w "uploader" in
  let bytes = ("x", "text/plain") in
  List.iter
    (fun (why, parts, names) ->
      let reply = World.upload w token parts in
      status why 400 reply;
      conforms "POST" "/resources" reply;
      Check.that
        (Printf.sprintf "%s: the error names /%s/: %s" why names reply.text)
        (matches_re names (error_of reply)))
    [
      ( "no name",
        form ~file:bytes
          [ ("format", "text/plain"); ("storageUri", "file://a") ],
        "name" );
      ( "no file",
        form
          [
            ("name", "n"); ("format", "text/plain"); ("storageUri", "file://a");
          ],
        "file" );
      ( "an unsupported media type",
        form ~file:bytes
          [
            ("name", "n");
            ("format", "application/x-conformance");
            ("storageUri", "file://a");
          ],
        {|application\/x-conformance|} );
    ]

let refused_upload { world = w; _ } =
  let watcher = profile_watcher w in
  let token =
    World.person w
      ~claims:[ ("name", `String "Rhea Refused") ]
      "refused-uploader"
  in
  with_mode
    ~set:(fun () ->
      w.archivist.mode.refuse_uploads <-
        Some "a worker-role emitter must cite the job it fulfils")
    ~reset:(fun () -> w.archivist.mode.refuse_uploads <- None)
    (fun () ->
      let refused = World.upload w token (small_upload ()) in
      status "an upload the record refuses" 500 refused;
      conforms "POST" "/resources" refused;
      Check.that "the error carries the record's reason"
        (matches_re "must cite the job" (error_of refused)));
  with_mode
    ~set:(fun () -> w.archivist.mode.refuses_gateway <- true)
    ~reset:(fun () -> w.archivist.mode.refuses_gateway <- false)
    (fun () ->
      let unavailable = World.upload w token (small_upload ()) in
      status "an upload the Archivist cannot take" 503 unavailable;
      conforms "POST" "/resources" unavailable);
  Bus_stream.quiet watcher "principal:profile" 0.5
    ~matches:(stamped_by w "refused-uploader")

let pipe_for_a_worker { world = w; _ } =
  let id = "res-" ^ Fresh.uuid () in
  store w id "text/markdown";
  let worker, _ = World.agent w ~roles:(worker_roles ()) "reading-model" in
  let reply = read_bytes w worker id in
  status "the pipe, for a worker" 200 reply;
  conforms "GET" "/resources/{id}" reply;
  Check.that "the pipe serves the bytes verbatim" (reply.text = every_byte);
  Check.equal "its content type" (`String "text/markdown")
    (`String (header "content-type" reply));
  (* The Archivist is asked by the gateway, as itself: the worker shows the
     Archivist nothing, and needs no address for it. *)
  Check.that "the gateway read the bytes as itself"
    (azp
       (last_call w (fun c ->
            c.path = Printf.sprintf "/resources/%s/content" id))
    = Some World.gateway_client)

let media_alias { world = w; _ } =
  let id = "res-" ^ Fresh.uuid () in
  store w id "application/pdf";
  let token = World.person w "reader" in
  let media =
    text_member "token"
      (Call.call ~token
         ~json:(`Assoc [ ("resourceId", `String id) ])
         (World.origin w) "POST"
         (Protocol.path Gateway "POST" "/api/tokens/media"))
  in
  let by_bearer = get ~token w "/api/resources/{id}" id in
  let by_media =
    Call.call (World.origin w) "GET"
      (Protocol.path_of Gateway "GET" "/api/resources/{id}" [ ("id", id) ]
      ^ "?token=" ^ media)
  in
  List.iter
    (fun (reply : Call.reply) ->
      status "the media alias" 200 reply;
      conforms "GET" "/api/resources/{id}" reply;
      Check.that "the alias serves the bytes verbatim" (reply.text = every_byte);
      Check.equal "its content type" (`String "application/pdf")
        (`String (header "content-type" reply)))
    [ by_bearer; by_media ]

let pipe_not_found { world = w; _ } =
  let token = World.person w "reader" in
  let without_bytes = "res-" ^ Fresh.uuid () in
  Hashtbl.replace w.archivist.resources without_bytes
    { storage_uri = "file://" ^ without_bytes; media_type = "text/plain" };
  List.iter
    (fun (id, message) ->
      List.iter
        (fun path ->
          let reply = get ~token w path id in
          status path 404 reply;
          conforms "GET" path reply;
          Check.equal path
            (`String (Protocol.message_text message))
            (`String (error_of reply)))
        [ "/resources/{id}"; "/api/resources/{id}" ])
    [
      ("res-" ^ Fresh.uuid (), "resource-not-found");
      (without_bytes, "representation-not-found");
    ]

let every_read =
  [
    "/resources/{id}";
    "/resources/{id}/jsonld";
    "/resources/{id}/annotations";
    "/resources/{id}/anchored-text";
  ]

let paths_called calls =
  `List (List.map (fun (c : Archivist_standin.call) -> `String c.path) calls)

let not_an_id { world = w; _ } =
  let token = World.person w "reader" in
  let kind = Protocol.identifiers "ResourceId" in
  (* Left out: the empty string, . and .., which a URL parser resolves before
     the request is sent, so they reach no route. *)
  let named =
    List.filter_map
      (fun (r : Xynolog_protocol.Tables.Identifiers_kinds.accepts_item) ->
        if List.mem r.id [ ""; "."; ".." ] then None
        else Some (Protocol.uri_component r.id, Some r.id, r.why))
      kind.refuses
    @ [ ("%FF", None, "bytes that are no text at all") ]
  in
  List.iter
    (fun (segment, id, why) ->
      (* Stored under that very name, so an answer other than 404 would be its
         bytes. *)
      Option.iter
        (fun id ->
          Hashtbl.replace w.archivist.resources id
            { storage_uri = "file://" ^ segment; media_type = "text/plain" };
          Hashtbl.replace w.archivist.content ("file://" ^ segment) "bytes";
          Hashtbl.replace w.archivist.descriptions id
            (`Assoc
               [
                 ( "resource",
                   `Assoc
                     [
                       ("@context", `String "https://schema.org/");
                       ("@id", `String "res-stored");
                       ("name", `String "Stored");
                       ("representations", `List []);
                     ] );
                 ("annotations", `List []);
               ]);
          Hashtbl.replace w.archivist.annotations id (fun () ->
              Some (`Assoc [ ("annotations", `List []); ("total", `Int 0) ]));
          Hashtbl.replace w.archivist.anchored_texts id (fun () ->
              `Assoc [ ("kind", `String "no-map") ]))
        id;
      let asked = Dynarray.length w.archivist.calls in
      List.iter
        (fun path ->
          let what = Printf.sprintf "%s with %s: %s" path segment why in
          let reply =
            Call.call ~token (World.origin w) "GET"
              (Protocol.fill_template
                 (Protocol.path Gateway "GET" path)
                 [ ("id", segment) ])
          in
          status what 404 reply;
          match Call.non_conformance Gateway "GET" path reply with
          | [] -> ()
          | problems -> failwith (what ^ ": " ^ String.concat "; " problems))
        (every_read @ [ "/api/resources/{id}" ]);
      Check.equal
        (Printf.sprintf "%s: %s: the Archivist's calls" segment why)
        (`List [])
        (paths_called (calls_since w asked)))
    named

let cannot_serve { world = w; _ } =
  let token = World.person w "reader" in
  with_mode
    ~set:(fun () -> w.archivist.mode.refuses_gateway <- true)
    ~reset:(fun () -> w.archivist.mode.refuses_gateway <- false)
    (fun () ->
      List.iter
        (fun path ->
          let reply = get ~token w path ("res-" ^ Fresh.uuid ()) in
          status path 503 reply;
          conforms "GET" path reply)
        every_read)

let methods_and_paths calls =
  `List
    (List.map
       (fun (c : Archivist_standin.call) -> `String (c.meth ^ " " ^ c.path))
       calls)

(* The gateway's reads of a resource's bytes while the Archivist closes the
   next [hang_ups] connections unanswered. *)
let read_met_by_hang_ups (w : World.t) hang_ups =
  let id = "res-" ^ Fresh.uuid () in
  store w id "text/plain";
  let token = World.person w "reader" in
  let asked = Dynarray.length w.archivist.calls in
  with_mode
    ~set:(fun () -> w.archivist.mode.hangs_up <- hang_ups)
    ~reset:(fun () -> w.archivist.mode.hangs_up <- 0)
    (fun () ->
      let reply = read_bytes w token id in
      conforms "GET" "/resources/{id}" reply;
      (reply, methods_and_paths (calls_since w asked), id))

let two_reads id =
  let read = `String (Printf.sprintf "GET /resources/%s/content" id) in
  `List [ read; read ]

let sent_once_more { world = w; _ } =
  let reply, reads, id = read_met_by_hang_ups w 1 in
  status "a read met by one close" 200 reply;
  Check.that "the bytes are served whole" (reply.text = every_byte);
  Check.equal "the reads" (two_reads id) reads

let not_again { world = w; _ } =
  let reply, reads, id = read_met_by_hang_ups w 2 in
  status "a read met by two closes" 503 reply;
  Check.equal "its error"
    (`String (Protocol.message_text "content-store-unavailable"))
    (`String (error_of reply));
  Check.equal "the reads" (two_reads id) reads

let upload_not_resent { world = w; _ } =
  let token = World.person w "uploader" in
  let asked = Dynarray.length w.archivist.calls in
  let recorded = Dynarray.length w.archivist.uploads in
  with_mode
    ~set:(fun () -> w.archivist.mode.hangs_up <- 1)
    ~reset:(fun () -> w.archivist.mode.hangs_up <- 0)
    (fun () ->
      let reply = World.upload w token (small_upload ()) in
      status "an upload met by a close" 503 reply;
      conforms "POST" "/resources" reply);
  Check.equal "the Archivist's calls"
    (`List [ `String "POST /resources" ])
    (methods_and_paths (calls_since w asked));
  Check.that "nothing more was recorded"
    (Dynarray.length w.archivist.uploads = recorded)

let description_whole { world = w; _ } =
  let id = "res-" ^ Fresh.uuid () in
  let description =
    `Assoc
      [
        ( "resource",
          `Assoc
            [
              ("@context", `String "https://schema.org/");
              ("@id", `String id);
              ("name", `String "Described");
              ( "representations",
                `List [ `Assoc [ ("mediaType", `String "text/plain") ] ] );
            ] );
        ("annotations", `List []);
      ]
  in
  Hashtbl.replace w.archivist.descriptions id description;
  let token = World.person w "reader" in
  let reply = get ~token w "/resources/{id}/jsonld" id in
  status "the description" 200 reply;
  conforms "GET" "/resources/{id}/jsonld" reply;
  Check.that "its content type is JSON-LD"
    (matches_re {|^application\/ld\+json|} (header "content-type" reply));
  Check.equal "the description" description (Option.get reply.json)

let annotations_passed_through { world = w; _ } =
  let id = "res-" ^ Fresh.uuid () in
  let held = `Assoc [ ("annotations", `List []); ("total", `Int 0) ] in
  Hashtbl.replace w.archivist.annotations id (fun () -> Some held);
  let token = World.person w "reader" in
  let reply = get ~token w "/resources/{id}/annotations" id in
  status "the annotations" 200 reply;
  conforms "GET" "/resources/{id}/annotations" reply;
  Check.equal "its content type" (`String "application/json")
    (`String (header "content-type" reply));
  Check.equal "its cache-control" (`String "no-cache")
    (`String (header "cache-control" reply));
  Check.equal "the annotations" held (Option.get reply.json);
  let unknown =
    get ~token w "/resources/{id}/annotations" ("res-" ^ Fresh.uuid ())
  in
  status "an unknown resource's annotations" 404 unknown;
  conforms "GET" "/resources/{id}/annotations" unknown

let description_unknown { world = w; _ } =
  let token = World.person w "reader" in
  let reply = get ~token w "/resources/{id}/jsonld" ("res-" ^ Fresh.uuid ()) in
  status "an unknown resource's description" 404 reply;
  conforms "GET" "/resources/{id}/jsonld" reply

let read_anchored ?token (w : World.t) id =
  get ?token w "/resources/{id}/anchored-text" id

let text_item ~start ~end_ ~page ~y ~width =
  `Assoc
    [
      ("start", `Int start);
      ("end", `Int end_);
      ("page", `Int page);
      ("x", `Int 72);
      ("y", `Int y);
      ("width", `Int width);
      ("height", `Int 12);
    ]

let anchored_verbatim { world = w; _ } =
  let id = "res-" ^ Fresh.uuid () in
  (* Its members in an order of their own, and an emoji: what reading the
     answer and writing it again could change. *)
  let answer =
    `Assoc
      [
        ("kind", `String "extracted");
        ("text", `String "\xf0\x9f\x98\x80 alpha");
        ( "items",
          `List
            [
              text_item ~start:0 ~end_:1 ~page:1 ~y:700 ~width:6;
              text_item ~start:2 ~end_:7 ~page:1 ~y:686 ~width:30;
            ] );
        ("method", `String "ocr");
      ]
  in
  Hashtbl.replace w.archivist.anchored_texts id (fun () -> answer);
  let token = World.person w "reader" in
  let reply = read_anchored ~token w id in
  status "the anchored text" 200 reply;
  conforms "GET" "/resources/{id}/anchored-text" reply;
  Check.that "the answer is the Archivist's, byte for byte"
    (reply.text = Yojson.Safe.to_string answer);
  Check.equal "its content type" (`String "application/json")
    (`String (header "content-type" reply));
  Check.equal "its cache-control" (`String "no-cache")
    (`String (header "cache-control" reply));
  Check.that "the gateway read it as itself"
    (azp
       (last_call w (fun c ->
            c.path = Printf.sprintf "/resources/%s/anchored-text" id))
    = Some World.gateway_client)

let anchored_each_time { world = w; _ } =
  let id = "res-" ^ Fresh.uuid () in
  let text =
    `Assoc
      [
        ("kind", `String "extracted");
        ("text", `String "alpha");
        ("items", `List [ text_item ~start:0 ~end_:5 ~page:1 ~y:700 ~width:30 ]);
        ("method", `String "pdf-text-layer");
      ]
  in
  let not_yet = `Assoc [ ("kind", `String "not-yet") ] in
  let reads = ref 0 in
  Hashtbl.replace w.archivist.anchored_texts id (fun () ->
      incr reads;
      if !reads = 1 then not_yet else text);
  let token = World.person w "reader" in
  List.iter
    (fun answer ->
      let reply = read_anchored ~token w id in
      status "the anchored text" 200 reply;
      conforms "GET" "/resources/{id}/anchored-text" reply;
      Check.equal "the answer" answer (Option.get reply.json))
    [ not_yet; text ]

let absence_and_decline { world = w; _ } =
  let token = World.person w "reader" in
  let answered id =
    let reply = read_anchored ~token w id in
    status "the anchored text" 200 reply;
    conforms "GET" "/resources/{id}/anchored-text" reply;
    Option.get reply.json
  in
  Check.equal "an unknown resource's anchored text"
    (`Assoc [ ("kind", `String "unknown") ])
    (answered ("res-" ^ Fresh.uuid ()));
  List.iter
    (fun answer ->
      let id = "res-" ^ Fresh.uuid () in
      Hashtbl.replace w.archivist.anchored_texts id (fun () -> answer);
      Check.equal "the Archivist's answer" answer (answered id))
    [
      `Assoc [ ("kind", `String "no-map") ];
      `Assoc [ ("kind", `String "declined"); ("declined", `String "encrypted") ];
    ]

let anchored_fails { world = w; _ } =
  let token = World.person w "reader" in
  with_mode
    ~set:(fun () -> w.archivist.mode.anchored_text_fails <- true)
    ~reset:(fun () -> w.archivist.mode.anchored_text_fails <- false)
    (fun () ->
      let reply = read_anchored ~token w ("res-" ^ Fresh.uuid ()) in
      status "an anchored text whose read fails" 503 reply;
      conforms "GET" "/resources/{id}/anchored-text" reply)

let anchored_needs_a_token { world = w; _ } =
  let id = "res-" ^ Fresh.uuid () in
  Hashtbl.replace w.archivist.anchored_texts id (fun () ->
      `Assoc [ ("kind", `String "no-map") ]);
  let asked = Dynarray.length w.archivist.calls in
  let reply = read_anchored w id in
  status "an anchored text without a token" 401 reply;
  conforms "GET" "/resources/{id}/anchored-text" reply;
  Check.equal "the Archivist's calls" (`List [])
    (paths_called (calls_since w asked))

let anchored_larger_than_the_bus { world = w; _ } =
  (* No emit may be larger than this, so no reply on the bus could carry the
     answer below. *)
  let bus_message_bytes =
    Protocol.limit Gateway "POST" "/bus/emit" "maxBodyBytes"
  in
  (* Plain ASCII, forty lines to a page and one word to a line: each item's
     offsets index its word, and each word's item is more than sixty bytes of
     the answer. *)
  let words =
    List.init
      ((bus_message_bytes + 59) / 60)
      (fun index -> Printf.sprintf "word%d" index)
  in
  let offset = ref 0 in
  let items =
    List.mapi
      (fun index word ->
        let length = String.length word in
        let item =
          text_item ~start:!offset ~end_:(!offset + length)
            ~page:((index / 40) + 1)
            ~y:(700 - (index mod 40 * 14))
            ~width:(length * 6)
        in
        offset := !offset + length + 1;
        item)
      words
  in
  let answer =
    `Assoc
      [
        ("kind", `String "extracted");
        ("text", `String (String.concat " " words));
        ("items", `List items);
        ("method", `String "pdf-text-layer");
      ]
  in
  let stored = Yojson.Safe.to_string answer in
  Check.that "the answer is larger than the largest bus message"
    (String.length stored > bus_message_bytes);
  let id = "res-" ^ Fresh.uuid () in
  Hashtbl.replace w.archivist.anchored_texts id (fun () -> answer);
  let reply = read_anchored ~token:(World.person w "reader") w id in
  status "a large anchored text" 200 reply;
  conforms "GET" "/resources/{id}/anchored-text" reply;
  Check.that
    (Printf.sprintf "it came whole: %d of %d bytes" (String.length reply.text)
       (String.length stored))
    (String.length reply.text = String.length stored);
  Check.that "it came byte for byte" (reply.text = stored)

let no_bus_request { world = w; requests } =
  let token = World.person w "uploader" in
  ignore (World.upload w token (small_upload ()) : Call.reply);
  ignore
    (World.upload w token (small_upload ~extra:[ ("cloneToken", "t") ] ())
      : Call.reply);
  ignore
    (get ~token w "/resources/{id}/jsonld" ("res-" ^ Fresh.uuid ())
      : Call.reply);
  Run.sleep 0.3;
  Check.equal "the requests the gateway published" (`List [])
    (`List
       (List.map
          (fun (f : Bus_stream.frame) -> `String f.channel)
          (Bus_stream.frames requests)))

let tests implementations =
  each_plane ~make:(make implementations)
    ~world:(fun { world; _ } -> world)
    ~close:(fun { world; _ } -> World.close world)
    "content"
    [
      case
        "an upload goes to the Archivist whole, named for the uploader, and \
         answers the id it recorded"
        upload_whole;
      case
        "a worker's upload carries its roles to the record, and its token, \
         carrying no name, publishes no profile"
        worker_upload;
      case
        "an upload the Archivist finds malformed is a 400 carrying its reason"
        malformed_upload;
      case
        "an upload the record refuses is a 500 carrying its reason, and one \
         the Archivist cannot take is a 503 — and neither names its uploader"
        refused_upload;
      case
        "the pipe serves the stored bytes verbatim, with the stored media type \
         and the declared headers"
        pipe_verbatim;
      case
        "the pipe serves a worker's agent the stored bytes, as it serves a \
         person"
        pipe_for_a_worker;
      case
        "the media alias serves the same bytes to a bearer or to the \
         resource's media token"
        media_alias;
      case
        "the pipe answers 404 for an unknown resource, and for a resource \
         without bytes"
        pipe_not_found;
      case
        "a path that names what is not a resource's id names no resource: a \
         404, and the Archivist is not asked (identifiers/kinds.json)"
        not_an_id;
      case
        "the pipe, the description, the annotations and the anchored text \
         answer 503 when the Archivist cannot serve them"
        cannot_serve;
      case
        "a read the Archivist closed its connection on, unanswered, is sent \
         once more, and its answer served"
        sent_once_more;
      case
        "a read is sent once more, not again: a second close unanswered is a \
         503"
        not_again;
      case
        "an upload the Archivist closed its connection on, unanswered, is not \
         sent again: a 503"
        upload_not_resent;
      case "the JSON-LD description is the record's answer, never cached"
        description_whole;
      case
        "a resource's annotations are the record's answer, passed through \
         unread and never cached, and 404 for an unknown resource"
        annotations_passed_through;
      case "the JSON-LD description of an unknown resource is a 404"
        description_unknown;
      case
        "the anchored text is the Archivist's answer, byte for byte, never \
         cached, and asked for as the gateway itself"
        anchored_verbatim;
      case
        "the anchored text is read from the Archivist each time it is asked \
         for: not-yet, and then the text"
        anchored_each_time;
      case
        "an absence and a decline are answers, not errors: unknown for a \
         resource the Archivist does not hold, and what it answers for one it \
         does"
        absence_and_decline;
      case "the anchored text answers 503 when the Archivist's read of it fails"
        anchored_fails;
      case
        "the anchored text is refused without a token: a 401, and the \
         Archivist is not asked"
        anchored_needs_a_token;
      case
        "an anchored text larger than the largest bus message comes through \
         whole"
        anchored_larger_than_the_bus;
      case
        "the gateway makes no bus request of its own: an upload and a \
         description ask the Archivist over HTTP"
        no_bus_request;
    ]
