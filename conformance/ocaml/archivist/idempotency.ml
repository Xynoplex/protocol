(* An upload sent again (ARCHIVIST.md § The HTTP surface; the Archivist's
   API, POST /resources): an upload carrying an Idempotency-Key its principal
   has used makes nothing new. With the fields and bytes the key was first
   sent with it is answered with the resource the key made, however it was
   sent again; with others it is refused. *)

open Xynolog_conformance
open Support
open Case

(* The id of every stream the log holds, as the Archivist lists them,
   sorted. *)
let streams w =
  let listed = A.http w ~route:"/events" "GET" "/events" in
  if listed.status <> 200 then
    failwith
      (Printf.sprintf "the streams were not listed: %d %s" listed.status
         listed.text);
  List.sort compare
    (List.map
       (fun stream -> A.string_member "resourceId" stream)
       (Check.list (Option.get listed.json) [ "streams" ]))

let shown ids = `List (List.map str ids)

(* The id of the resource an upload was answered with; the Archivist must
   have recorded it. *)
let id_of (reply : Call.reply) =
  if reply.status <> 200 then
    failwith
      (Printf.sprintf "the upload was not recorded: %d %s" reply.status
         reply.text);
  A.string_member "resourceId" (Option.get reply.json)

let types w id =
  `List (List.map (fun event -> Check.at event [ "type" ]) (A.stored w id))

let answered_with what id (reply : Call.reply) =
  Check.equal (what ^ ": its status") (`Int 200) (`Int reply.status);
  match reply.json with
  | Some json -> Check.equal what (`Assoc [ ("resourceId", str id) ]) json
  | None -> failwith (what ^ " is not JSON: " ^ Call.shown reply.text)

let fields name storage_uri = [ ("name", name); ("storageUri", storage_uri) ]

let recorded_once w =
  let ada = World.person_did w.A.world "ada" in
  let key = Fresh.uuid () in
  let content = "sent twice, recorded once\n" in
  let uri = "file://keyed/sent-twice.md" in
  let sent = fields "Sent twice" uri in
  let id = id_of (A.upload w ~key ada sent content) in
  let before = streams w in
  let again = A.upload w ~key ada sent content in
  answered_with "the upload sent again" id again;
  Check.equal "the streams" (shown before) (shown (streams w));
  Check.equal "the resource's events"
    (`List [ str "yield:created" ])
    (types w id);
  Check.equal "the bytes in the working tree" (str content)
    (str (A.read_file (A.content_path w uri)));
  Check.equal "the storage-uri entry"
    (`Assoc [ ("uri", str uri); ("resourceId", str id) ])
    (Option.get (A.storage_uri_entry w uri))

let at_once w =
  let ada = World.person_did w.A.world "ada" in
  let key = Fresh.uuid () in
  let sent = fields "Sent at once" "file://keyed/sent-at-once.md" in
  let content = "two at once, one resource\n" in
  let before = streams w in
  let one, two =
    Eio.Fiber.pair
      (fun () -> A.upload w ~key ada sent content)
      (fun () -> A.upload w ~key ada sent content)
  in
  let id = id_of one in
  answered_with "the second upload" id two;
  Check.equal "the streams"
    (shown (List.sort compare (id :: before)))
    (shown (streams w));
  Check.equal "the resource's events"
    (`List [ str "yield:created" ])
    (types w id)

let other_fields_refused w =
  let ada = World.person_did w.A.world "ada" in
  let key = Fresh.uuid () in
  let content = "the bytes the key was first sent with\n" in
  let uri = "file://keyed/first-of-its-key.md" in
  let first = fields "First of its key" uri in
  let elsewhere = "file://keyed/elsewhere.md" in
  let id = id_of (A.upload w ~key ada first content) in
  let before = streams w in
  let status, _, _ = http_error "upload-key-reused" [] in
  List.iter
    (fun (why, sent, content) ->
      Check.equal why (`Int status)
        (`Int (A.upload w ~key ada sent content).status))
    [
      ("other bytes", first, "other bytes under the same key\n");
      ("another name", fields "Another name" uri, content);
      ("another storage URI", fields "First of its key" elsewhere, content);
    ];
  Check.equal "the streams" (shown before) (shown (streams w));
  Check.equal "the resource's events"
    (`List [ str "yield:created" ])
    (types w id);
  Check.equal "the bytes in the working tree" (str content)
    (str (A.read_file (A.content_path w uri)));
  Check.that "nothing is stored elsewhere"
    (not (Sys.file_exists (A.content_path w elsewhere)));
  Check.that "nothing is indexed elsewhere"
    (A.storage_uri_entry w elsewhere = None)

let another_principal w =
  let ada = World.person_did w.A.world "ada" in
  let bea = World.person_did w.A.world "bea" in
  let key = Fresh.uuid () in
  let content = "one key text, two principals\n" in
  let adas =
    id_of (A.upload w ~key ada (fields "Ada's" "file://keyed/adas.md") content)
  in
  let beas =
    id_of (A.upload w ~key bea (fields "Bea's" "file://keyed/beas.md") content)
  in
  Check.that "the two principals' resources are two" (beas <> adas);
  like "Ada's resource's events"
    (`List [ `Assoc [ ("type", str "yield:created"); ("userId", str ada) ] ])
    (`List (A.stored w adas));
  like "Bea's resource's events"
    (`List [ `Assoc [ ("type", str "yield:created"); ("userId", str bea) ] ])
    (`List (A.stored w beas))

let with_a_clone_token w =
  let owner = A.person w "owner" in
  let source =
    A.created w owner.did
      [ ("name", "Cloned once"); ("storageUri", "file://keyed/cloned-once.md") ]
      "cloned once\n"
  in
  let token =
    A.string_member "token"
      (A.ask owner "yield:clone-token-requested"
         (`Assoc [ ("resourceId", str source) ]))
  in
  let key = Fresh.uuid () in
  let sent =
    fields "The one clone" "file://keyed/the-one-clone.md"
    @ [ ("cloneToken", token) ]
  in
  let content = "the clone\n" in
  let clone = id_of (A.upload w ~key owner.did sent content) in
  let before = streams w in
  let again = A.upload w ~key owner.did sent content in
  answered_with "the upload sent again" clone again;
  Check.equal "the streams" (shown before) (shown (streams w));
  like "the clone's events"
    (`List
       [
         `Assoc
           [
             ("type", str "yield:cloned");
             ("userId", str owner.did);
             ("payload", `Assoc [ ("parentResourceId", str source) ]);
           ];
       ])
    (`List (A.stored w clone))

let across_a_restart w =
  let ada = World.person_did w.A.world "ada" in
  let key = Fresh.uuid () in
  let sent = fields "Across a restart" "file://keyed/across-a-restart.md" in
  let content = "sent before a restart and after it\n" in
  let id = id_of (A.upload w ~key ada sent content) in
  A.restart w;
  let before = streams w in
  let again = A.upload w ~key ada sent content in
  answered_with "the upload sent again" id again;
  Check.equal "the streams" (shown before) (shown (streams w));
  Check.equal "the resource's events"
    (`List [ str "yield:created" ])
    (types w id)

let tests implementations =
  with_archivist implementations "an upload sent again with its key"
    [
      case
        "is answered with the resource the first made, and nothing more is \
         stored or recorded"
        recorded_once;
      case "twice at once makes one resource, and both are answered with it"
        at_once;
      case
        "is refused 422 with other bytes or other fields, and nothing is \
         stored or recorded"
        other_fields_refused;
      case "from another principal is another key, and makes another resource"
        another_principal;
      case
        "carrying a clone token is answered with the clone the first made, \
         though the token is spent"
        with_a_clone_token;
      case
        "is answered with the resource the first made after the Archivist \
         restarts on the same tree"
        across_a_restart;
    ]
