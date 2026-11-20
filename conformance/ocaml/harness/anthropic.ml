(* A stand-in Anthropic: the two calls a worker's Anthropic agent makes,
   answered from what a case scripted. GET /v1/models/{model} answers what
   the case said of the model. POST /v1/messages is a generation, and takes
   the next scripted reply: as one JSON message, or as a stream of events when
   the request asks for one ([stream: true]). Every request is recorded as it
   was sent, with its headers, so a case can hold a worker to the exact
   request it made and to how many times it made it.

   It plays the provider under version 2026-10-21 of its API. A request is
   taken only with the key as [Authorization: Bearer <key>], and is otherwise
   answered 401. A generation that carries [temperature], [top_p] or [top_k]
   is refused on every model, as the provider refuses one: with 400, saying
   so.

   A request no case scripted, a request of any other path, a request about a
   model the case has not described, a request without the key, and a
   generation that carries [temperature], [top_p] or [top_k] lands in
   [violations], which fail the case whatever it was about. *)

(* What the provider says of a model. *)
type facts = {
  max_input_tokens : int;  (** [max_input_tokens]: what it reads. *)
  max_output_tokens : int;
      (** [max_tokens]: the most it writes in one answer. *)
  structured_outputs : bool;
      (** What it reports as [capabilities.structured_outputs.supported]. *)
}

(* The token counts every answer of the provider states. *)
type usage = { input : int; output : int }

(* What the stand-in does with one generation. *)
type scripted =
  | Answer of { text : string; usage : usage; stop_reason : string option }
      (** The model answered [text], and stopped for [stop_reason]: [end_turn]
          when it is [None]. *)
  | Fails of { status : int; body : string; headers : (string * string) list }
      (** The provider refused: this status and this body, with these headers
          beside its own. *)
  | Hold
      (** Accepted, and not answered until [release], or until the worker gives
          up on it. *)
  | Drop  (** The connection is ended with no answer. *)

(* What the stand-in refuses a request with: this status, with these headers
   beside its own. *)
type refusal = { status : int; headers : (string * string) list }

(* One request, as it arrived. *)
type request = {
  meth : string;
  path : string;  (** With its query. *)
  headers : (string * string) list;
      (** Their names in lower case, in the order they came. *)
  body : Yojson.Safe.t option;
      (** Parsed; [None] when it had none, or what it had is not JSON. A
          generation's is always [Some] JSON object. *)
  at : float;  (** When it arrived, in milliseconds since the epoch. *)
  served : Serve.request;
}

type held = {
  generation : request;
  members : (string * Yojson.Safe.t) list;
  answer : Serve.reply option Eio.Promise.u;
}

type t = {
  server : Serve.t;
  origin : string;  (** What a worker's [baseUrl] names. *)
  api_key : string;
      (** The key a request must carry, as [Authorization: Bearer <key>]. *)
  named : string list;
  facts : facts;
  models : (string, facts) Hashtbl.t;
      (** What the provider says of each model it has, by the model's name. A
          model it lacks is answered 404. *)
  mutable models_refusal : refusal option;
      (** What the Models API refuses every request with, when a case says it
          does. *)
  requests : request Dynarray.t;
  described : request Dynarray.t;  (** Every [GET /v1/models/<model>]. *)
  generations : request Dynarray.t;
  violations : string Dynarray.t;
  scripted : scripted Queue.t;
  mutable choose : (Yojson.Safe.t -> scripted option) option;
      (** Answer by what was asked, for a case whose requests arrive in no fixed
          order: given each generation's body. A generation it returns nothing
          for takes the next scripted reply. *)
  held : held Dynarray.t;
  mutable answered : int;
  arrived : Eio.Condition.t;
}

(* Whether the worker ended the connection before it was answered. *)
let abandoned request = request.served.abandoned

(* How many code points of an answer one event of a stream carries. *)
let streamed_piece = 24

(* What the provider refuses in a generation under version 2026-10-21 of its
   API, on every model and at any value. *)
let sampling = [ "temperature"; "top_p"; "top_k" ]

let refusal kind message =
  `Assoc
    [
      ("type", `String "error");
      ("error", `Assoc [ ("type", `String kind); ("message", `String message) ]);
    ]

(* [ours] with [theirs] spread over them: a name both have keeps its place in
   [ours] and takes the value in [theirs]. *)
let beside ours theirs =
  List.fold_left
    (fun headers (name, value) ->
      if List.mem_assoc name headers then
        List.map (fun (n, v) -> if n = name then (n, value) else (n, v)) headers
      else headers @ [ (name, value) ])
    ours theirs

(* A member of a parsed object, as JSON.parse keeps it: the last of its name. *)
let field name members =
  List.fold_left
    (fun found (n, value) -> if n = name then Some value else found)
    None members

(* What JavaScript's [String] makes of a member: [None] is its absence. *)
let rec js_string = function
  | None -> "undefined"
  | Some (`String text) -> text
  | Some `Null -> "null"
  | Some (`Assoc _) -> "[object Object]"
  | Some (`List items) ->
      String.concat ","
        (List.map
           (function `Null -> "" | item -> js_string (Some item))
           items)
  | Some json -> Yojson.Safe.to_string json

(* Each code point of [text]: where it starts, its length in bytes and its
   length in UTF-16 units. An ill-formed byte sequence counts as the one
   replacement character a decoder makes of it. *)
let code_points text =
  let rec from at points =
    if at >= String.length text then List.rev points
    else
      let decoded = String.get_utf_8_uchar text at in
      let bytes = Uchar.utf_decode_length decoded in
      let units =
        if Uchar.to_int (Uchar.utf_decode_uchar decoded) > 0xFFFF then 2 else 1
      in
      from (at + bytes) ((at, bytes, units) :: points)
  in
  from 0 []

(* The first [count] UTF-16 units of [text], as JavaScript's [slice] cuts it,
   short of a code point it would cut in two. *)
let first_units count text =
  let rec upto taken = function
    | (_, bytes, units) :: rest when taken + units <= count ->
        bytes + upto (taken + units) rest
    | _ -> 0
  in
  String.sub text 0 (upto 0 (code_points text))

(* [text] in pieces of [streamed_piece] code points. *)
let pieces text =
  let rec split = function
    | [] -> []
    | (start, _, _) :: _ as points ->
        let rec piece taken bytes = function
          | (_, length, _) :: rest when taken < streamed_piece ->
              piece (taken + 1) (bytes + length) rest
          | rest -> (bytes, rest)
        in
        let bytes, rest = piece 0 0 points in
        String.sub text start bytes :: split rest
  in
  split (code_points text)

(* What a generation the stand-in answers comes to on the wire. *)
type outcome =
  | Holds
  | Drops
  | Replies of Serve.reply
  | Streams of { headers : (string * string) list; events : string list }

(* What [reply] makes of a generation that asked with [members]. *)
let outcome t members reply =
  t.answered <- t.answered + 1;
  let id = Printf.sprintf "standin_%d" t.answered in
  let request_id = ("request-id", "req_" ^ id) in
  match reply with
  | Hold -> Holds
  | Drop -> Drops
  | Fails { status; body; headers } ->
      Replies
        {
          status;
          headers =
            beside [ ("content-type", "application/json"); request_id ] headers;
          body;
        }
  | Answer { text; usage; stop_reason } -> (
      let stop_reason = Option.value ~default:"end_turn" stop_reason in
      let message =
        [
          ("id", `String ("msg_" ^ id));
          ("type", `String "message");
          ("role", `String "assistant");
        ]
        @ (match field "model" members with
          | Some model -> [ ("model", model) ]
          | None -> [])
        @ [ ("stop_sequence", `Null) ]
      in
      match field "stream" members with
      | Some (`Bool true) ->
          (* A stream, as the provider sends one: the message opens empty, its
             text follows in pieces, and its stop reason and what it wrote
             come last. *)
          let event kind data =
            Printf.sprintf "event: %s\ndata: %s\n\n" kind
              (Yojson.Safe.to_string (`Assoc (("type", `String kind) :: data)))
          in
          Streams
            {
              headers =
                [
                  ("content-type", "text/event-stream");
                  ("cache-control", "no-cache");
                  request_id;
                ];
              events =
                [
                  event "message_start"
                    [
                      ( "message",
                        `Assoc
                          (message
                          @ [
                              ("content", `List []);
                              ("stop_reason", `Null);
                              ( "usage",
                                `Assoc
                                  [
                                    ("input_tokens", `Int usage.input);
                                    ("output_tokens", `Int 1);
                                  ] );
                            ]) );
                    ];
                  event "content_block_start"
                    [
                      ("index", `Int 0);
                      ( "content_block",
                        `Assoc
                          [ ("type", `String "text"); ("text", `String "") ] );
                    ];
                ]
                @ List.map
                    (fun piece ->
                      event "content_block_delta"
                        [
                          ("index", `Int 0);
                          ( "delta",
                            `Assoc
                              [
                                ("type", `String "text_delta");
                                ("text", `String piece);
                              ] );
                        ])
                    (pieces text)
                @ [
                    event "content_block_stop" [ ("index", `Int 0) ];
                    event "message_delta"
                      [
                        ( "delta",
                          `Assoc
                            [
                              ("stop_reason", `String stop_reason);
                              ("stop_sequence", `Null);
                            ] );
                        ( "usage",
                          `Assoc [ ("output_tokens", `Int usage.output) ] );
                      ];
                    event "message_stop" [];
                  ];
            }
      | _ ->
          let reply =
            Serve.json_reply 200
              (`Assoc
                 (message
                 @ [
                     ( "content",
                       `List
                         [
                           `Assoc
                             [
                               ("type", `String "text"); ("text", `String text);
                             ];
                         ] );
                     ("stop_reason", `String stop_reason);
                     ( "usage",
                       `Assoc
                         [
                           ("input_tokens", `Int usage.input);
                           ("output_tokens", `Int usage.output);
                         ] );
                   ]))
          in
          Replies { reply with headers = reply.headers @ [ request_id ] })

let describe t =
  Hashtbl.reset t.models;
  List.iter (fun model -> Hashtbl.replace t.models model t.facts) t.named

(* A stand-in that has [models], each as [facts] says, and takes [api_key]. *)
let start ~api_key ~models ~facts () =
  let requests = Dynarray.create ()
  and described = Dynarray.create ()
  and generations = Dynarray.create ()
  and violations = Dynarray.create ()
  and held = Dynarray.create () in
  let scripted = Queue.create () and arrived = Eio.Condition.create () in
  let rec handler (served : Serve.request) =
    let t = Lazy.force t in
    let body = Call.parsed served.body in
    let recorded =
      {
        meth = served.meth;
        path =
          (match served.query with
          | [] -> served.path
          | query -> served.path ^ "?" ^ Uri.encoded_of_query query);
        headers =
          List.map
            (fun (name, value) -> (String.lowercase_ascii name, value))
            (Http.Header.to_list served.headers);
        body;
        at = Run.now () *. 1000.;
        served;
      }
    in
    Dynarray.add_last requests recorded;
    let json status payload = Serve.Reply (Serve.json_reply status payload) in
    let what = recorded.meth ^ " " ^ recorded.path in
    let model =
      let prefix = "/v1/models/" in
      if String.starts_with ~prefix recorded.path then
        let rest =
          String.sub recorded.path (String.length prefix)
            (String.length recorded.path - String.length prefix)
        in
        if rest <> "" && not (String.exists (fun c -> c = '/' || c = '?') rest)
        then Some rest
        else None
      else None
    in
    if
      List.assoc_opt "authorization" recorded.headers
      <> Some ("Bearer " ^ api_key)
    then (
      Dynarray.add_last violations
        (what
       ^ " without the key its agent's apiKeyEnv names, as `Authorization: \
          Bearer`");
      json 401 (refusal "authentication_error" "invalid bearer token"))
    else
      match (recorded.meth, model) with
      | "GET", Some model -> (
          Dynarray.add_last described recorded;
          match t.models_refusal with
          | Some how ->
              let reply =
                Serve.json_reply how.status
                  (refusal "api_error" "the stand-in refuses the Models API")
              in
              Serve.Reply
                { reply with headers = beside reply.headers how.headers }
          | None -> (
              let id = Uri.pct_decode model in
              match Hashtbl.find_opt t.models id with
              | None ->
                  Dynarray.add_last violations
                    (what ^ ", a model the case has not described");
                  json 404 (refusal "not_found_error" ("model: " ^ id))
              | Some said ->
                  json 200
                    (`Assoc
                       [
                         ("type", `String "model");
                         ("id", `String id);
                         ("display_name", `String id);
                         ("created_at", `String "2026-01-01T00:00:00Z");
                         ("max_input_tokens", `Int said.max_input_tokens);
                         ("max_tokens", `Int said.max_output_tokens);
                         ( "capabilities",
                           `Assoc
                             [
                               ( "structured_outputs",
                                 `Assoc
                                   [
                                     ("supported", `Bool said.structured_outputs);
                                   ] );
                             ] );
                       ])))
      | "POST", _ when recorded.path = "/v1/messages" -> (
          match body with
          | Some (`Assoc members as body) -> (
              Dynarray.add_last generations recorded;
              Eio.Condition.broadcast arrived;
              let asked = Dynarray.length generations in
              let model = js_string (field "model" members) in
              let sampled =
                List.filter_map
                  (fun name ->
                    if List.mem_assoc name members then Some ("`" ^ name ^ "`")
                    else None)
                  sampling
              in
              if not (Hashtbl.mem t.models model) then (
                Dynarray.add_last violations
                  (Printf.sprintf "%s of %s, a model the case has not described"
                     what model);
                json 404 (refusal "not_found_error" ("model: " ^ model)))
              else if sampled <> [] then (
                let sampled = String.concat ", " sampled in
                Dynarray.add_last violations
                  (Printf.sprintf "generation %d, which carries %s" asked
                     sampled);
                json 400
                  (refusal "invalid_request_error"
                     (sampled
                    ^ ": this version of the API takes no sampling parameter, \
                       on any model")))
              else
                let chosen =
                  match t.choose with
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
                      (Printf.sprintf
                         "generation %d, which no case scripted: %s" asked
                         (first_units 160
                            (match field "messages" members with
                            | Some messages -> Yojson.Safe.to_string messages
                            | None -> "undefined")));
                    json 400
                      (refusal "invalid_request_error"
                         "the stand-in has no reply scripted")
                | Some reply -> (
                    match outcome t members reply with
                    | Holds ->
                        let answered, answer = Eio.Promise.create () in
                        Dynarray.add_last held
                          { generation = recorded; members; answer };
                        Serve.Held answered
                    | Drops -> Serve.Dropped
                    | Replies reply -> Serve.Reply reply
                    | Streams { headers; events } ->
                        Serve.Stream
                          {
                            status = 200;
                            headers;
                            events = (fun write -> List.iter write events);
                          }))
          | _ ->
              Dynarray.add_last violations
                ("a message whose body is not a JSON object: "
                ^ first_units 200 served.body);
              json 400 (refusal "invalid_request_error" "not a JSON object"))
      | _ ->
          Dynarray.add_last violations
            (what ^ ", which the stand-in Anthropic does not serve");
          json 404 (refusal "not_found_error" "not found")
  and server = lazy (Serve.start handler)
  and t =
    lazy
      (let t =
         {
           server = Lazy.force server;
           origin = (Lazy.force server).origin;
           api_key;
           named = models;
           facts;
           models = Hashtbl.create 4;
           models_refusal = None;
           requests;
           described;
           generations;
           violations;
           scripted;
           choose = None;
           held;
           answered = 0;
           arrived;
         }
       in
       describe t;
       t)
  in
  Lazy.force t

(* The replies the next generations get, in order, after those already
   scripted. *)
let script t replies =
  List.iter (fun reply -> Queue.add reply t.scripted) replies

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

(* Answer every held generation with what [reply] makes of its body. One
   whose worker has gone is answered nothing. A stream is sent whole, its
   length stated, since a held answer is a single reply. *)
let release t reply =
  let waiting = Dynarray.to_list t.held in
  Dynarray.clear t.held;
  List.iter
    (fun held ->
      if not (abandoned held.generation) then
        let resolve answer =
          ignore (Eio.Promise.try_resolve held.answer answer)
        in
        match outcome t held.members (reply (`Assoc held.members)) with
        | Holds -> Dynarray.add_last t.held held
        | Drops -> resolve None
        | Replies reply -> resolve (Some reply)
        | Streams { headers; events } ->
            resolve
              (Some { status = 200; headers; body = String.concat "" events }))
    waiting

(* Forget what was scripted and recorded, end every held generation
   unanswered, and say of each model what was said when the stand-in
   started. *)
let reset t =
  Dynarray.iter
    (fun held -> ignore (Eio.Promise.try_resolve held.answer None))
    t.held;
  Dynarray.clear t.held;
  Queue.clear t.scripted;
  Dynarray.clear t.requests;
  Dynarray.clear t.described;
  Dynarray.clear t.generations;
  Dynarray.clear t.violations;
  t.models_refusal <- None;
  t.choose <- None;
  describe t

let close t = t.server.stop ()
