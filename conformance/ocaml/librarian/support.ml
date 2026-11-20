(* What the librarian files share: texts to record, scripted replies, a
   file's world, and how long a request took. *)

open Xynolog_conformance
module W = Librarian_world

(* A text every word of which occurs once, so a case can annotate any of
   them by quoting it. *)
let fox = "The quick brown fox jumps over the lazy dog.\n"

(* A second text, which shares no word of four letters or more with the first. *)
let tide = "Twice each day high water floods a salt marsh.\n"

(* The eight bytes a PNG begins with, and no more: a media type with no text. *)
let image = "\x89PNG\r\n\x1a\n"

(* What a provider that fails answers. *)
let fails =
  Ollama.Fails (500, {|{"error":"the stand-in fails this generation"}|})

let answer response = Ollama.Answer { response; stop = Done; usage = None }

(* A file's world: one Librarian world, made for the file's first case. *)
let world_of file implementations =
  let command =
    W.python "Librarian" implementations.Implementations.librarians
  in
  Suite.world ("librarian/" ^ file)
    ~make:(fun () -> W.create implementations command)
    ~close:W.close

(* What [asking] came to, and how many milliseconds it took. *)
let timed asking =
  let began = Run.now () in
  let value = asking () in
  (value, (Run.now () -. began) *. 1000.0)

(* The ids of a reply's list of items, each read at [key]. *)
let ids key items = List.map (fun item -> Check.at item [ key ]) items

(* Whether [text] holds [word], in any case: what a case's /word/i holds. *)
let mentions word text = Re.execp (Re.compile (Re.no_case (Re.str word))) text

let message failure =
  Yojson.Safe.Util.to_string (Check.at failure [ "message" ])

(* The item of [items] whose [key] is [id], which must be there. *)
let one_of key id items =
  match
    List.find_opt
      (fun item -> Protocol.at item [ key ] = Some (`String id))
      items
  with
  | Some item -> item
  | None ->
      failwith
        (Printf.sprintf "%s answered no %s: %s" id key
           (Check.shown (`List (ids key items))))

(* The list at [path] of [json]; empty when nothing is there. *)
let listed json path =
  match Protocol.at json path with Some (`List items) -> items | _ -> []

(* Whether a context's graph holds [id] as a node of [kind]. *)
let has_node context id kind =
  Check.containing "the graph's nodes"
    [ ("id", `String id); ("type", `String kind) ]
    (Check.list context [ "graph"; "nodes" ])
