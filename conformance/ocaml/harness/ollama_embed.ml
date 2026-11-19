(* A stand-in embedding provider: POST /api/embed with a model and a list of
   texts, answered with a vector for each. A vector is worked out from its
   text alone, so the same text has the same vector however often it is
   asked for. It answers every request. Every request is recorded as it was
   sent. A
   request of any other path, and a body that is not a model and a list of
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

type t = {
  server : Serve.t;
  origin : string;
  requests : recorded Dynarray.t;
  violations : string Dynarray.t;
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
  let requests = Dynarray.create () and violations = Dynarray.create () in
  let handler (request : Serve.request) =
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
      | Some (model, texts) ->
          let recorded = { model; texts } in
          Dynarray.add_last requests recorded;
          Serve.Reply (embedded recorded)
  in
  let server = Serve.start handler in
  { server; origin = server.origin; requests; violations }

(* A point a case marks: how many requests have arrived by now. *)
let mark t = Dynarray.length t.requests

(* The requests that arrived after a mark, in order. *)
let since t mark =
  List.filteri (fun at _ -> at >= mark) (Dynarray.to_list t.requests)

let reset t =
  Dynarray.clear t.requests;
  Dynarray.clear t.violations

let close t = t.server.stop ()
