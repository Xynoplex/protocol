(* A stand-in Ollama for a model: the two calls a driver makes of one,
   answered from what a case scripted. POST /api/show answers the context
   length the case set, or refuses, or ends the connection unanswered, as the
   case said; POST /api/generate answers the next scripted reply. Every
   request is recorded as it was sent. A generation no case scripted, and a
   request of any other path, lands in [violations]. *)

type scripted =
  | Answer of { response : string; usage : (int * int) option }
      (** The model answered, and stopped; [usage] is the prompt's and the
          output's token counts, as a real Ollama reports them. *)
  | Fails of int * string  (** The provider refused: this status, this body. *)
  | Hold
      (** Accepted, and not answered until [release], or until the client gives
          up on it. *)
  | Drop  (** The connection is ended with no answer. *)

(* What POST /api/show does. *)
type show = Context of int | Refuses of int | Drops

type t = {
  server : Serve.t;
  origin : string;
  context_length : int;
  mutable show : show;
  shows : Yojson.Safe.t Dynarray.t;
  generations : (Yojson.Safe.t * Serve.request) Dynarray.t;
  violations : string Dynarray.t;
  scripted : scripted Queue.t;
  mutable choose : (Yojson.Safe.t -> scripted option) option;
      (** Answer by what was asked, for a case whose requests arrive in no fixed
          order: given each generation's body. A generation it returns nothing
          for takes the next scripted reply. *)
  held : Serve.reply option Eio.Promise.u Dynarray.t;
  arrived : Eio.Condition.t;
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
  let shows = Dynarray.create ()
  and generations = Dynarray.create ()
  and violations = Dynarray.create ()
  and held = Dynarray.create () in
  let scripted = Queue.create () and arrived = Eio.Condition.create () in
  let rec handler (request : Serve.request) =
    let json status body = Serve.Reply (Serve.json_reply status body) in
    match (request.meth, request.path) with
    | "POST", "/api/show" -> (
        Dynarray.add_last shows
          (Option.value ~default:`Null (Call.parsed request.body));
        match (Lazy.force t).show with
        | Drops -> Serve.Dropped
        | Refuses status ->
            json status
              (`Assoc [ ("error", `String "the stand-in refuses /api/show") ])
        | Context length ->
            json 200
              (`Assoc
                 [
                   ( "model_info",
                     `Assoc
                       [
                         ("general.architecture", `String "standin");
                         ("standin.context_length", `Int length);
                       ] );
                 ]))
    | "POST", "/api/generate" -> (
        match Call.parsed request.body with
        | Some (`Assoc _ as body) -> (
            Dynarray.add_last generations (body, request);
            Eio.Condition.broadcast arrived;
            let chosen =
              match (Lazy.force t).choose with
              | Some choose -> choose body
              | None -> None
            in
            match
              match chosen with
              | Some reply -> Some reply
              | None -> Queue.take_opt scripted
            with
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
                  }
            | Some Hold ->
                let answered, answer = Eio.Promise.create () in
                Dynarray.add_last held answer;
                Serve.Held answered
            | Some Drop -> Serve.Dropped)
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
  and server = lazy (Serve.start handler)
  and t =
    lazy
      {
        server = Lazy.force server;
        origin = (Lazy.force server).origin;
        context_length;
        show = Context context_length;
        shows;
        generations;
        violations;
        scripted;
        choose = None;
        held;
        arrived;
      }
  in
  Lazy.force t

let script t replies =
  List.iter (fun reply -> Queue.add reply t.scripted) replies

(* The prompts it was sent since the case began, in order. *)
let prompts t =
  List.map
    (fun (body, _) -> Yojson.Safe.Util.(member "prompt" body |> to_string))
    (Dynarray.to_list t.generations)

(* Wait until [count] generations have arrived. *)
let asked ?(timeout = 15.0) t count =
  let deadline = Run.now () +. timeout in
  let rec wait () =
    if Dynarray.length t.generations < count then (
      let remaining = deadline -. Run.now () in
      if remaining <= 0.0 then
        raise
          (Run.Timed_out
             (Printf.sprintf
                "timed out after %g s waiting for generation %d to be asked \
                 for: %d arrived"
                timeout count
                (Dynarray.length t.generations)));
      ignore
        (Eio.Time.with_timeout (Run.clock ()) remaining (fun () ->
             Ok (Eio.Condition.await_no_mutex t.arrived)));
      wait ())
  in
  wait ()

let end_held t answer =
  Dynarray.iter
    (fun held -> ignore (Eio.Promise.try_resolve held answer))
    t.held;
  Dynarray.clear t.held

(* Answer every held generation with [response]. *)
let release t response = end_held t (Some (generated response None))

(* Forget what was scripted and recorded, and end every held generation
   unanswered: the next case starts clean. *)
let reset t =
  end_held t None;
  Queue.clear t.scripted;
  Dynarray.clear t.shows;
  Dynarray.clear t.generations;
  Dynarray.clear t.violations;
  t.show <- Context t.context_length;
  t.choose <- None

let close t = t.server.stop ()
