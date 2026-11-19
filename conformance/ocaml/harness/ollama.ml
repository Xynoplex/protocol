(* A stand-in Ollama for a model: the two calls a driver makes of one,
   answered from what a case scripted. POST /api/show answers its context
   length; POST /api/generate answers the next scripted reply. Every
   generation is recorded as it was sent. A generation no case scripted, and
   a request of any other path, lands in [violations]. *)

type scripted =
  | Answer of { response : string; usage : (int * int) option }
      (** The model answered, and stopped; [usage] is the prompt's and the
          output's token counts, as a real Ollama reports them. *)
  | Fails of int * string  (** The provider refused: this status, this body. *)

type t = {
  server : Serve.t;
  origin : string;
  context_length : int;
  generations : (Yojson.Safe.t * Serve.request) Dynarray.t;
  violations : string Dynarray.t;
  scripted : scripted Queue.t;
}

let generated response usage =
  Serve.json_reply 200
    (`Assoc
       ([
          ("response", `String response);
          ("done", `Bool true);
          ("done_reason", `String "stop");
        ]
       @
       match usage with
       | Some (prompt, output) ->
           [ ("prompt_eval_count", `Int prompt); ("eval_count", `Int output) ]
       | None -> []))

let start ?(context_length = 8192) () =
  let generations = Dynarray.create () and violations = Dynarray.create () in
  let scripted = Queue.create () in
  let handler (request : Serve.request) =
    let json status body = Serve.Reply (Serve.json_reply status body) in
    match (request.meth, request.path) with
    | "POST", "/api/show" ->
        json 200
          (`Assoc
             [
               ( "model_info",
                 `Assoc
                   [
                     ("general.architecture", `String "standin");
                     ("standin.context_length", `Int context_length);
                   ] );
             ])
    | "POST", "/api/generate" -> (
        match Call.parsed request.body with
        | Some (`Assoc _ as body) -> (
            Dynarray.add_last generations (body, request);
            match Queue.take_opt scripted with
            | None ->
                Dynarray.add_last violations
                  (Printf.sprintf "generation %d, which no case scripted: %s"
                     (Dynarray.length generations)
                     (Call.shown
                        (Yojson.Safe.to_string
                           (Yojson.Safe.Util.member "prompt" body))));
                json 500
                  (`Assoc
                     [ ("error", `String "the stand-in has no reply scripted") ])
            | Some (Answer { response; usage }) ->
                Serve.Reply (generated response usage)
            | Some (Fails (status, body)) ->
                Serve.Reply
                  {
                    status;
                    headers = [ ("content-type", "application/json") ];
                    body;
                  })
        | _ ->
            Dynarray.add_last violations
              ("a generation whose body is not a JSON object: "
             ^ Call.shown request.body);
            json 400 (`Assoc [ ("error", `String "not a JSON object") ]))
    | meth, path ->
        Dynarray.add_last violations
          (Printf.sprintf "%s %s, which the stand-in Ollama does not serve" meth
             path);
        json 404 (`Assoc [ ("error", `String "not found") ])
  in
  let server = Serve.start handler in
  {
    server;
    origin = server.origin;
    context_length;
    generations;
    violations;
    scripted;
  }

let script t replies =
  List.iter (fun reply -> Queue.add reply t.scripted) replies

(* The prompts it was sent since the case began, in order. *)
let prompts t =
  List.map
    (fun (body, _) -> Yojson.Safe.Util.(member "prompt" body |> to_string))
    (Dynarray.to_list t.generations)

(* Forget what was scripted and recorded: the next case starts clean. *)
let reset t =
  Queue.clear t.scripted;
  Dynarray.clear t.generations;
  Dynarray.clear t.violations

let close t = t.server.stop ()
