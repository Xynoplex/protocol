(* A stand-in embedding provider: POST /api/embed with a model and a list of
   texts, answered with a vector for each. A vector is worked out from its
   text alone, so the same text has the same vector however often it is
   asked for. It answers every request with its vectors, unless a case
   chose otherwise: a failing status, or a request held until the case
   releases it. Every request is recorded as it was sent. A request of any
   other path, and a body that is not a model and a list of
   texts, lands in [violations]. *)

let width = 16

(* The vector the stand-in gives a text: of length 1, so an index that
   compares by cosine keeps it as it was given. *)
let embedding_of text =
  let digest = Digestif.SHA256.(to_raw_string (digest_string text)) in
  let raw =
    List.init width (fun at -> float_of_int (Char.code digest.[at]) -. 127.5)
  in
  let length = sqrt (List.fold_left (fun sum n -> sum +. (n *. n)) 0.0 raw) in
  List.map (fun n -> n /. length) raw

(* How near two texts are: the cosine of their vectors. *)
let nearness a b =
  List.fold_left2
    (fun sum x y -> sum +. (x *. y))
    0.0 (embedding_of a) (embedding_of b)

type recorded = { model : string; texts : string list }

(* What the stand-in does with one request, in place of answering it with
   its vectors. *)
type instead =
  | Fails of int * string  (** The provider refused: this status, this body. *)
  | Hold
      (** Accepted, and not answered until [release], or until the client gives
          up on it. *)

type t = {
  server : Serve.t;
  origin : string;
  requests : recorded Dynarray.t;
  violations : string Dynarray.t;
  mutable choose : (recorded -> instead option) option;
      (** What a case does with each request, by what was asked. A request it
          returns nothing for is answered with its vectors. *)
  held :
    (Serve.request * recorded * Serve.reply option Eio.Promise.u) Dynarray.t;
}

let embedded recorded =
  Serve.json_reply 200
    (`Assoc
       [
         ("model", `String recorded.model);
         ( "embeddings",
           `List
             (List.map
                (fun text ->
                  `List (List.map (fun n -> `Float n) (embedding_of text)))
                recorded.texts) );
       ])

let asked body =
  match Call.parsed body with
  | Some (`Assoc members) -> (
      match
        (List.assoc_opt "model" members, List.assoc_opt "input" members)
      with
      | Some (`String model), Some (`List input) ->
          let texts =
            List.filter_map (function `String s -> Some s | _ -> None) input
          in
          if List.length texts = List.length input then Some (model, texts)
          else None
      | _ -> None)
  | _ -> None

let start () =
  let requests = Dynarray.create ()
  and violations = Dynarray.create ()
  and held = Dynarray.create () in
  let rec handler (request : Serve.request) =
    let json status body = Serve.Reply (Serve.json_reply status body) in
    if request.meth <> "POST" || request.path <> "/api/embed" then (
      Dynarray.add_last violations
        (Printf.sprintf
           "%s %s, which the stand-in embedding provider does not serve"
           request.meth request.path);
      json 404 (`Assoc [ ("error", `String "not found") ]))
    else
      match asked request.body with
      | None ->
          Dynarray.add_last violations
            ("an embedding request that is not a model and a list of texts: "
           ^ Call.shown request.body);
          json 400
            (`Assoc [ ("error", `String "not a model and a list of texts") ])
      | Some (model, texts) -> (
          let recorded = { model; texts } in
          Dynarray.add_last requests recorded;
          match
            match (Lazy.force t).choose with
            | Some choose -> choose recorded
            | None -> None
          with
          | None -> Serve.Reply (embedded recorded)
          | Some (Fails (status, body)) ->
              Serve.Reply
                {
                  status;
                  headers = [ ("content-type", "application/json") ];
                  body;
                }
          | Some Hold ->
              let answered, answer = Eio.Promise.create () in
              Dynarray.add_last held (request, recorded, answer);
              Serve.Held answered)
  and server = lazy (Serve.start handler)
  and t =
    lazy
      {
        server = Lazy.force server;
        origin = (Lazy.force server).origin;
        requests;
        violations;
        choose = None;
        held;
      }
  in
  Lazy.force t

(* How many requests are held now: accepted, unanswered, and not given up
   on. *)
let held t =
  Dynarray.fold_left
    (fun n ((request : Serve.request), _, _) ->
      if request.abandoned then n else n + 1)
    0 t.held

(* Answer every held request with its vectors. *)
let release t =
  Dynarray.iter
    (fun (_, recorded, answer) ->
      ignore (Eio.Promise.try_resolve answer (Some (embedded recorded))))
    t.held;
  Dynarray.clear t.held

(* A point a case marks: how many requests have arrived by now. *)
let mark t = Dynarray.length t.requests

(* The requests that arrived after a mark, in order. *)
let since t mark =
  List.filteri (fun at _ -> at >= mark) (Dynarray.to_list t.requests)

(* Forget what was chosen and recorded, and end every held request
   unanswered: the next case starts clean. *)
let reset t =
  Dynarray.iter
    (fun (_, _, answer) -> ignore (Eio.Promise.try_resolve answer None))
    t.held;
  Dynarray.clear t.held;
  Dynarray.clear t.requests;
  Dynarray.clear t.violations;
  t.choose <- None

let close t = t.server.stop ()
