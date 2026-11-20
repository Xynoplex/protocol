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
        "the pipe serves the stored bytes verbatim, with the stored media type \
         and the declared headers"
        pipe_verbatim;
    ]
