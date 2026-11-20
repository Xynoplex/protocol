(* A recording HTTP proxy in front of a real gateway, for the suites whose
   subject is the client: a Worker service is pointed at it instead of the
   gateway. Every request passes on to the gateway, and every answer back,
   streams included, as they arrive; the proxy records each request with its
   JSON body, its answer, and every Server-Sent Event a stream carried — the
   transcript a case holds the client to. [cut] ends the client's
   connections from outside: a dropped stream, as the client meets one. *)

type carried = {
  event : string;
  id : string option;
  data : string;
  at : int;
      (** When the proxy read it, on the clock [arrived] and [answered] share.
      *)
}

type request = {
  meth : string;
  path : string;  (** With its query, when it has one. *)
  headers : (string * string) list;  (** Each name lower-cased. *)
  json : Yojson.Safe.t option;  (** The body, when it is JSON. *)
  mutable status : int option;
      (** Unset until it has been answered, and for good if the exchange was cut
          first. *)
  arrived : int;
  arrived_at : float;  (** When it arrived, in seconds, as [Run.now] reads. *)
  mutable answered : int option;
      (** [arrived] and [answered] are on one clock every request and event
          shares: [a.answered < b.arrived] reads "a was answered before b
          arrived". *)
  mutable answer : Yojson.Safe.t option;
      (** The answer's body, when it is JSON and not a stream. *)
  events : carried Dynarray.t;
      (** What its answer carried, when the answer is a stream. *)
  mutable closed : bool;
      (** Whether the exchange is over: answered in full, or its connection
          ended. *)
}

type t = {
  server : Serve.t;
  origin : string;  (** Where a client is pointed instead of the gateway. *)
  requests : request Dynarray.t;  (** In the order they arrived. *)
  mutable clock : int;
  mutable cutting : unit Eio.Promise.t * unit Eio.Promise.u;
      (** Resolved by the next [cut], and replaced: each exchange ends when the
          one current as it began is. *)
  changed : Eio.Condition.t;
}

let tick t =
  t.clock <- t.clock + 1;
  t.clock

let changed t = Eio.Condition.broadcast t.changed

(* The headers that frame a message on its own connection: each side writes
   its own. *)
let framing =
  [ "host"; "connection"; "keep-alive"; "content-length"; "transfer-encoding" ]

let unframed headers =
  List.filter
    (fun (name, _) -> not (List.mem (String.lowercase_ascii name) framing))
    (Http.Header.to_list headers)

let media_type headers =
  match Http.Header.get headers "content-type" with
  | Some value ->
      String.lowercase_ascii
        (String.trim (List.hd (String.split_on_char ';' value)))
  | None -> ""

let target (asked : Serve.request) =
  match asked.query with
  | [] -> asked.path
  | query -> asked.path ^ "?" ^ Uri.encoded_of_query query

(* What reads Server-Sent Events out of a body, whatever the chunks it
   arrives in: given each chunk, it hands [on_event] every event the chunk
   completes. *)
let event_reader on_event =
  let pending = Buffer.create 4096 in
  let event = ref ""
  and id = ref None
  and data = ref []
  and saw_data = ref false in
  let line = function
    | "" ->
        if !event <> "" || !saw_data then
          on_event
            (if !event = "" then "message" else !event)
            !id
            (String.concat "\n" (List.rev !data));
        event := "";
        id := None;
        data := [];
        saw_data := false
    | line -> (
        let field, value =
          match String.index_opt line ':' with
          | None -> (line, "")
          | Some colon ->
              let value =
                String.sub line (colon + 1) (String.length line - colon - 1)
              in
              ( String.sub line 0 colon,
                if String.starts_with ~prefix:" " value then
                  String.sub value 1 (String.length value - 1)
                else value )
        in
        match field with
        | "event" -> event := value
        | "id" -> id := Some value
        | "data" ->
            data := value :: !data;
            saw_data := true
        | _ -> ())
  in
  fun chunk ->
    Buffer.add_string pending chunk;
    let text = Buffer.contents pending in
    let rec split from =
      match String.index_from_opt text from '\n' with
      | Some stop ->
          let found = String.sub text from (stop - from) in
          line
            (if String.ends_with ~suffix:"\r" found then
               String.sub found 0 (String.length found - 1)
             else found);
          split (stop + 1)
      | None ->
          Buffer.clear pending;
          Buffer.add_string pending
            (String.sub text from (String.length text - from))
    in
    split 0

(* Pass [asked] on to the gateway, from a fiber of its own that holds the
   gateway's connection until [over] is resolved: the answer's head and a
   reader of its body, or why there is none. *)
let upstream gateway (asked : Serve.request) over =
  let opened, open_ = Eio.Promise.create () in
  Eio.Fiber.fork ~sw:(Run.sw ()) (fun () ->
      Eio.Switch.run @@ fun sw ->
      Eio.Fiber.first
        (fun () -> Eio.Promise.await over)
        (fun () ->
          Eio.Promise.resolve open_
            (match
               Call.exchange ~sw ~headers:(unframed asked.headers)
                 ?body:(if asked.body = "" then None else Some asked.body)
                 asked.meth
                 (gateway ^ target asked)
             with
            | exchanged -> Ok exchanged
            | exception ((End_of_file | Eio.Io _ | Failure _) as failure) ->
                Error failure);
          Eio.Fiber.await_cancel ()));
  opened

(* Pass a stream on as it arrives, each chunk read for its events before it
   is written, so nothing the client does on reading an event is recorded
   ahead of it. *)
let stream t recorded next cut finish write =
  Fun.protect ~finally:finish @@ fun () ->
  let read =
    event_reader (fun event id data ->
        Dynarray.add_last recorded.events { event; id; data; at = tick t };
        changed t)
  in
  let rec pass () =
    match next () with
    | Cohttp.Transfer.Chunk text ->
        read text;
        write text;
        pass ()
    | Final_chunk text ->
        read text;
        write text
    | Done -> ()
  in
  match
    Eio.Fiber.first
      (fun () ->
        (* An empty chunk writes nothing but the head, which must reach the
           client before the stream's first event does. *)
        write "";
        pass ();
        `Ended)
      (fun () ->
        Eio.Promise.await cut;
        `Cut)
  with
  | `Ended -> ()
  | `Cut | (exception (End_of_file | Eio.Io _)) -> raise Serve.Drop

let forward t gateway (asked : Serve.request) =
  let recorded =
    {
      meth = asked.meth;
      path = target asked;
      headers =
        List.map
          (fun (name, value) -> (String.lowercase_ascii name, value))
          (Http.Header.to_list asked.headers);
      json = Call.parsed asked.body;
      status = None;
      arrived = tick t;
      arrived_at = Run.now ();
      answered = None;
      answer = None;
      events = Dynarray.create ();
      closed = false;
    }
  in
  Dynarray.add_last t.requests recorded;
  changed t;
  let cut = fst t.cutting in
  let over, ending = Eio.Promise.create () in
  let finish () =
    ignore (Eio.Promise.try_resolve ending () : bool);
    recorded.closed <- true;
    changed t
  in
  let until_cut read =
    Eio.Fiber.first
      (fun () -> Some (read ()))
      (fun () ->
        Eio.Promise.await cut;
        None)
  in
  try
    match
      until_cut (fun () -> Eio.Promise.await (upstream gateway asked over))
    with
    | None | Some (Error _) ->
        finish ();
        Serve.Dropped
    | Some (Ok (response, next)) -> (
        let status = Http.Status.to_int (Http.Response.status response) in
        let headers = unframed (Http.Response.headers response) in
        let media = media_type (Http.Response.headers response) in
        recorded.status <- Some status;
        recorded.answered <- Some (tick t);
        changed t;
        if media = "text/event-stream" then
          Serve.Stream
            { status; headers; events = stream t recorded next cut finish }
        else
          match until_cut (fun () -> Call.body_of next) with
          | None | (exception (End_of_file | Eio.Io _)) ->
              finish ();
              Serve.Dropped
          | Some body ->
              if String.ends_with ~suffix:"json" media then
                recorded.answer <- Call.parsed body;
              finish ();
              Serve.Reply { status; headers; body })
  with failure ->
    finish ();
    raise failure

let start gateway =
  let rec server =
    lazy (Serve.start (fun asked -> forward (Lazy.force t) gateway asked))
  and t =
    lazy
      {
        server = Lazy.force server;
        origin = (Lazy.force server).origin;
        requests = Dynarray.create ();
        clock = 0;
        cutting = Eio.Promise.create ();
        changed = Eio.Condition.create ();
      }
  in
  Lazy.force t

(* The clock's reading: everything recorded from here on is later than it. *)
let now t = t.clock

(* What [probe] answers once it answers something, asked again whenever
   anything the proxy records changes; fails after [timeout] s naming
   [what]. *)
let until ?(timeout = 10.0) t what probe =
  let deadline = Run.now () +. timeout in
  let rec wait () =
    match probe () with
    | Some found -> found
    | None ->
        let remaining = deadline -. Run.now () in
        if remaining <= 0.0 then
          raise
            (Run.Timed_out
               (Printf.sprintf "timed out after %g s waiting for %s" timeout
                  what));
        ignore
          (Eio.Time.with_timeout (Run.clock ()) remaining (fun () ->
               Ok (Eio.Condition.await_no_mutex t.changed)));
        wait ()
  in
  wait ()

(* End every connection the proxy holds — open streams and requests in
   flight. New ones are still accepted. *)
let cut t =
  let _, cutting = t.cutting in
  t.cutting <- Eio.Promise.create ();
  Eio.Promise.resolve cutting ()

let close t = t.server.stop ()
