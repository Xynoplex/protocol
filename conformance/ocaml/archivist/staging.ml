(* Staging (ARCHIVIST.md § Staging): what reaches the git index, within the
   bound the document sets, and that a knowledge base that does not sync git
   runs none. *)

open Xynolog_conformance
open Support
open Case

(* A path of the working tree, as git names it: from the root. *)
let in_tree (w : A.t) path =
  let root = w.dirs.root ^ "/" in
  if not (String.starts_with ~prefix:root path) then
    failwith (path ^ " is not in the working tree");
  String.sub path (String.length root) (String.length path - String.length root)

let content_of w uri = in_tree w (A.content_path w uri)

(* The first file of a stream's log. *)
let log_of w id =
  in_tree w
    (Filename.concat (A.stream_dir w id)
       (A.event_file_name (A.layout ()).event_file.first))

let resource name uri = [ ("name", name); ("storageUri", uri) ]
let on id = `Assoc [ ("resourceId", str id) ]

let event_types w id =
  `List (List.map (fun e -> Check.at e [ "type" ]) (A.stored w id))

let new_resource (w : A.t) =
  let started = Run.now () in
  let uri = "file://staged/new.md" in
  let id =
    A.created w (World.person_did w.world "ada") (resource "Staged" uri) "new\n"
  in
  let content = content_of w uri and log = log_of w id in
  A.until_indexed w content;
  A.until_indexed w log;
  Check.that "staged within the bound"
    (Run.now () -. started < A.max_wait w +. 5.0);
  List.iter
    (fun path -> Check.that (path ^ " is staged") (List.mem path (A.staged w)))
    [ content; log ]

let system_stream w = A.until_indexed w (log_of w A.system)

let later_events (w : A.t) =
  let editor = A.person w "editor" in
  let id =
    A.created w editor.did
      (resource "Appended" "file://staged/appended.md")
      "appended\n"
  in
  ignore (A.ask editor "mark:archive" (on id) : Yojson.Safe.t);
  ignore (A.ask editor "mark:unarchive" (on id) : Yojson.Safe.t);
  let log = log_of w id in
  let on_disk () = A.read_file (Filename.concat w.dirs.root log) in
  Run.eventually "the index to hold all three events"
    (A.max_wait w +. 5.0)
    (fun () ->
      if
        List.mem log (A.indexed w) && A.git w [ "show"; ":" ^ log ] = on_disk ()
      then Some ()
      else None);
  Check.equal "the events on disk" (`Int 3)
    (`Int
       (List.length
          (List.filter
             (fun l -> l <> "")
             (String.split_on_char '\n' (on_disk ())))))

let updated w =
  let editor = A.person w "editor" in
  let uri = "file://staged/edited.md" in
  let id = A.created w editor.did (resource "Edited" uri) "first\n" in
  let content = content_of w uri in
  A.until_indexed w content;
  let second = "second\n" in
  A.write w uri second;
  ignore
    (A.ask editor "yield:update"
       (`Assoc
          [
            ("resourceId", str id);
            ("storageUri", str uri);
            ("contentChecksum", str (sha256 second));
            ("byteSize", `Int (String.length second));
          ])
      : Yojson.Safe.t);
  Run.eventually "the index to hold the new content"
    (A.max_wait w +. 5.0)
    (fun () ->
      if A.git w [ "show"; ":" ^ content ] = second then Some () else None)

let archived w =
  let owner = A.person w "owner" in
  let removed_uri = "file://staged/removed.md"
  and kept_uri = "file://staged/kept.md" in
  let removed =
    A.created w owner.did (resource "Removed" removed_uri) "removed\n"
  in
  let kept = A.created w owner.did (resource "Kept" kept_uri) "kept\n" in
  A.until_indexed w (content_of w removed_uri);
  A.until_indexed w (content_of w kept_uri);
  ignore
    (A.ask owner "mark:archive"
       (`Assoc [ ("resourceId", str removed); ("storageUri", str removed_uri) ])
      : Yojson.Safe.t);
  ignore
    (A.ask owner "mark:archive"
       (`Assoc
          [
            ("resourceId", str kept);
            ("storageUri", str kept_uri);
            ("keepFile", `Bool true);
          ])
      : Yojson.Safe.t);
  (* A remove tells git before it answers. *)
  let indexed = A.indexed w in
  List.iter
    (fun uri ->
      Check.that
        (content_of w uri ^ " is unstaged")
        (not (List.mem (content_of w uri) indexed)))
    [ removed_uri; kept_uri ];
  Check.that "the removed file is gone"
    (not (Sys.file_exists (A.content_path w removed_uri)));
  Check.that "the kept file is kept"
    (Sys.file_exists (A.content_path w kept_uri))

let branch w =
  let reader = A.person w "reader" in
  let git args = ignore (A.git_by_hand w args : string) in
  let named () =
    Check.at (A.ask reader "browse:kb-requested" (`Assoc [])) [ "gitBranch" ]
  in
  git [ "commit"; "--quiet"; "--allow-empty"; "-m"; "a first commit" ];
  git [ "checkout"; "--quiet"; "-b"; "a-branch-of-its-own" ];
  Check.equal "the branch named" (str "a-branch-of-its-own") (named ());
  git [ "checkout"; "--quiet"; "-b"; "another" ];
  Check.equal "the branch named next" (str "another") (named ())

let degraded (w : A.t) =
  let editor = A.person w "editor" in
  let uri = "file://staged/degraded.md" in
  let release = A.hold_git_index w in
  Fun.protect ~finally:release (fun () ->
      let id = A.created w editor.did (resource "Degraded" uri) "degraded\n" in
      ignore (A.ask editor "mark:archive" (on id) : Yojson.Safe.t);
      Check.equal "the events recorded"
        (`List [ str "yield:created"; str "mark:archived" ])
        (event_types w id);
      Check.equal "its health's status" (`Int 200)
        (`Int (A.http w ~anonymous:true ~route:"/health" "GET" "/health").status));
  (* Held back by the lock, the changes are staged once it is gone. *)
  A.until_indexed w (content_of w uri)

let unsynced (w : A.t) =
  let owner = A.person w "owner" in
  let uri = "file://unstaged/plain.md" in
  let id = A.created w owner.did (resource "Plain" uri) "plain\n" in
  let edited = "plain, edited\n" in
  A.write w uri edited;
  ignore
    (A.ask owner "yield:update"
       (`Assoc
          [
            ("resourceId", str id);
            ("storageUri", str uri);
            ("contentChecksum", str (sha256 edited));
            ("byteSize", `Int (String.length edited));
          ])
      : Yojson.Safe.t);
  Check.that "the knowledge base names no branch"
    (Protocol.at (A.ask owner "browse:kb-requested" (`Assoc [])) [ "gitBranch" ]
    = None);
  ignore
    (A.ask owner "mark:archive"
       (`Assoc [ ("resourceId", str id); ("storageUri", str uri) ])
      : Yojson.Safe.t);
  Check.that "the archived file is gone"
    (not (Sys.file_exists (A.content_path w uri)));
  Check.equal "the events recorded"
    (`List [ str "yield:created"; str "yield:updated"; str "mark:archived" ])
    (event_types w id);
  Run.sleep (A.max_wait w +. 0.5);
  Check.equal "the runs of git" (`List []) (`List (List.map str (A.git_runs w)));
  Check.that "the tree is no checkout"
    (not (Sys.file_exists (Filename.concat w.dirs.root ".git")))

let tests implementations =
  with_archivist implementations
    ~staging:[ ("flushMs", 100); ("maxWaitMs", 1_000) ]
    "staging, in a knowledge base that syncs git"
    [
      case "stages the content and the log of a new resource, within the bound"
        new_resource;
      case "stages the system stream, which the vocabulary seeded at boot wrote"
        system_stream;
      case
        "stages each later event of a stream: the index holds the log as it is \
         on disk"
        later_events;
      case "stages new content recorded by yield:update" updated;
      case "unstages what an archive removes, keeping the file when told to"
        archived;
      case "names the branch the tree is on, read when asked" branch;
      case "stays up, and goes on recording, when a batch cannot be staged"
        degraded;
    ]
  @ with_archivist implementations ~git_sync:false ~git_tripwire:true
      "staging, in a knowledge base that does not sync git"
      [ case "records, moves nothing to an index, and never runs git" unsynced ]
