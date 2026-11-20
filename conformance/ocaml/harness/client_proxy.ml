(* An HTTP proxy between a client and a gateway, for the suites whose subject
   is the client. It does what a case cannot do from inside a client it does
   not own:

   - it records every request the client makes, in order, with its JSON body:
     the authoritative account of what the client put on the wire;
   - it records every event each stream carried to the client, as the
     gateway wrote it, so what a client says it received can be held to what
     it was sent;
   - it ends the client's connections from outside ([cut]), and refuses new
     ones ([down]) until told otherwise ([up]): a dropped stream and an
     outage, as the client meets them;
   - it keeps a request waiting ([hold], [release]), answers one itself with
     a refusal a case scripted ([answer]), reads one whole and ends its
     connection with no answer ([leave_unanswered]), passes one on and ends
     its connection with none of the gateway's answer written
     ([lose_answer]), and passes a stream on a few bytes at a time
     ([rechunk]);
   - it writes onto the client's stream a frame the gateway never sends
     ([carry]), as a client meets one from a gateway newer or older than it.

   Everything else passes through, streams included. The gateway behind it
   is the real one. [answer] and [carry] are the two places the proxy speaks
   for it: the caller holds what [answer] says to the spec before using it,
   and what [carry] writes is recorded among the events its stream carried.

   Serve's server cannot end a connection from outside or pass a gateway's
   stream on as it arrives, so the accept loop is the proxy's own; every
   message on the wire is cohttp's. *)

open Wire

(* One Server-Sent Event a stream carried, as the gateway wrote it, or as
   [carry] wrote it in the gateway's place. [at] is when the proxy read it,
   on the clock [arrived] and [answered] share. *)
type event = { event : string; id : string option; data : string; at : int }

type request = {
  meth : string;
  path : string;  (** As the client sent it, with its query. *)
  headers : Http.Header.t;
  json : Yojson.Safe.t option;  (** The body, when it is JSON. *)
  mutable status : int option;
      (** None until it has been answered, and for good if the exchange was cut
          first. *)
  arrived : int;
  mutable answered : int option;
      (** When it arrived and when it was answered, on one clock every request
          and event shares: [a.answered < b.arrived] reads "a was answered
          before b arrived". *)
  arrived_at : float;
  mutable answered_at : float option;
      (** The same two moments in seconds, and the seconds its answer's
          Retry-After stated: for holding a client to a wait it was told. *)
  mutable retry_after : float option;
  mutable answer : Yojson.Safe.t option;
      (** The answer's body, when it is JSON and not a stream. *)
  events : event Dynarray.t;  (** What its answer carried, if a stream. *)
  mutable closed : bool;
      (** Whether the exchange is over: answered in full, or its connection
          ended, by either side. *)
  mutable answer_lost : bool;
      (** Whether the gateway's answer, which [status] and [answer] record, was
          kept from the client ([lose_answer]). *)
}

type rule = { matches : request -> bool; mutable left : int }

type t = {
  origin : string;  (** Where a client is pointed instead of the gateway. *)
  gateway : string;
  requests : request Dynarray.t;  (** In the order they arrived. *)
  changed : Eio.Condition.t;
  mutable clock : int;
  mutable refusing : bool;
  mutable held : (request -> bool) option;
  mutable releases : (unit -> unit) list;
  scripted : (rule * Serve.reply) Dynarray.t;
  unanswering : rule Dynarray.t;
  losing : rule Dynarray.t;
  mutable pieces : int option;
  mutable streams : (request * (string -> unit)) list;
      (** Each stream still open, in the order they opened, with what writes an
          event onto it. *)
  connections : (int, unit -> unit) Hashtbl.t;
      (** What ends each connection open, by a number of its own. *)
  mutable connected : int;
  sw : Eio.Switch.t;  (** What the proxy runs on, until [close]. *)
  close : unit -> unit;
}

exception Cut

let wake t = Eio.Condition.broadcast t.changed

let tick t =
  t.clock <- t.clock + 1;
  t.clock

let now t = t.clock

(* What [probe] answers, once it does; asked again whenever anything the
   proxy records changes. *)
let until ?timeout t what probe = Run.until t.changed ?timeout what probe

(* The first request, at or after [from], that [matches] accepts. *)
let next ?timeout ?(from = 0) t what matches =
  until ?timeout t what (fun () ->
      let rec find at =
        if at >= Dynarray.length t.requests then None
        else
          let r = Dynarray.get t.requests at in
          if matches r then Some r else find (at + 1)
      in
      find from)

let header (r : request) name = Http.Header.get r.headers name

let media_type headers =
  match Http.Header.get headers "content-type" with
  | None -> ""
  | Some value ->
      String.lowercase_ascii
        (String.trim (List.hd (String.split_on_char ';' value)))

let answered_now t r retry_after =
  r.answered <- Some (tick t);
  r.answered_at <- Some (Run.now ());
  r.retry_after <-
    Option.bind retry_after (fun text ->
        Option.bind
          (float_of_string_opt (String.trim text))
          (fun seconds ->
            if Float.is_finite seconds then Some seconds else None))

(* The first of [rules] with a rule left that accepts [r], spent once. *)
let spend rule_of rules r =
  Dynarray.find_opt
    (fun entry ->
      let rule = rule_of entry in
      rule.left > 0 && rule.matches r)
    rules
  |> Option.map (fun entry ->
      let rule = rule_of entry in
      rule.left <- rule.left - 1;
      entry)

(* What reads Server-Sent Events out of a byte stream, whatever the chunks
   it arrives in: [read] takes a chunk, and [between] says whether
   everything read so far ends between two events, so that an event written
   now is one of its own. *)
let event_reader on_event =
  let pending = Buffer.create 256 in
  let event = ref "" and id = ref None and data = ref [] and saw = ref false in
  let between () =
    Buffer.length pending = 0 && !event = "" && !id = None && not !saw
  in
  let line text =
    if text = "" then (
      if !event <> "" || !saw then
        on_event
          (if !event = "" then "message" else !event)
          !id
          (String.concat "\n" (List.rev !data));
      event := "";
      id := None;
      data := [];
      saw := false)
    else
      let field, value =
        match String.index_opt text ':' with
        | None -> (text, "")
        | Some colon ->
            let value =
              String.sub text (colon + 1) (String.length text - colon - 1)
            in
            ( String.sub text 0 colon,
              if String.starts_with ~prefix:" " value then
                String.sub value 1 (String.length value - 1)
              else value )
      in
      match field with
      | "event" -> event := value
      | "id" -> id := Some value
      | "data" ->
          data := value :: !data;
          saw := true
      | _ -> ()
  in
  let read chunk =
    Buffer.add_string pending chunk;
    let text = Buffer.contents pending in
    Buffer.clear pending;
    let rec lines from =
      match String.index_from_opt text from '\n' with
      | None ->
          Buffer.add_string pending
            (String.sub text from (String.length text - from))
      | Some newline ->
          let l = String.sub text from (newline - from) in
          line
            (if String.ends_with ~suffix:"\r" l then
               String.sub l 0 (String.length l - 1)
             else l);
          lines (newline + 1)
    in
    lines 0
  in
  (read, between)

(* The headers of a message passed on, without those that frame it on its
   own connection, which the proxy writes for the connection it writes on. *)
let framing =
  [
    "host";
    "connection";
    "keep-alive";
    "content-length";
    "transfer-encoding";
    "expect";
  ]

let passed headers =
  List.filter
    (fun (name, _) -> not (List.mem (String.lowercase_ascii name) framing))
    (Http.Header.to_list headers)

(* The request as the client sent it, put to the gateway: its head and the
   gateway's body reader. A failure to reach the gateway, or of the gateway
   midway, ends the client's connection, as it would unproxied. *)
let ask t ~sw r body =
  let body =
    if
      Http.Header.mem r.headers "content-length"
      || Http.Header.mem r.headers "transfer-encoding"
    then Some body
    else None
  in
  match
    Call.exchange ~sw ~headers:(passed r.headers) ?body r.meth
      (t.gateway ^ r.path)
  with
  | exchanged -> exchanged
  | exception (Eio.Io _ | End_of_file | Failure _) -> raise Serve.Drop

let whole next =
  match Call.body_of next with
  | text -> text
  | exception (Eio.Io _ | End_of_file | Failure _) -> raise Serve.Drop

(* Wait until the client ends the connection: while a request is in
   flight it sends nothing more but a request it pipelines, which is not its
   leaving. *)
let gone ic =
  match Serve.until_gone ic with
  | `Gone -> ()
  | `Spoke -> Eio.Fiber.await_cancel ()

(* A stream passed on as it arrives: each chunk read for its events before
   it is written, each event [carry] wrote held until the gateway is between
   two of its own, and every chunk written in [t.pieces] where it is set. *)
let stream t oc r status headers next =
  let queue = Eio.Stream.create max_int in
  let read, between =
    event_reader (fun event id data ->
        Dynarray.add_last r.events { event; id; data; at = tick t };
        wake t)
  in
  let waiting = ref [] in
  let written () =
    List.iter
      (fun event ->
        Eio.Stream.add queue (`Chunk event);
        read event)
      (List.rev !waiting);
    waiting := []
  in
  let carry event =
    waiting := event :: !waiting;
    if between () then written ()
  in
  t.streams <- t.streams @ [ (r, carry) ];
  Fun.protect ~finally:(fun () ->
      t.streams <- List.filter (fun (s, _) -> s != r) t.streams)
  @@ fun () ->
  let passed_on chunk =
    read chunk;
    Eio.Stream.add queue (`Chunk chunk);
    if between () then written ()
  in
  let rec copy () =
    match next () with
    | Cohttp.Transfer.Chunk chunk ->
        passed_on chunk;
        copy ()
    | Final_chunk chunk ->
        passed_on chunk;
        Eio.Stream.add queue `End
    | Done -> Eio.Stream.add queue `End
  in
  let write writer chunk =
    Response_io.write_body writer chunk;
    Eio.Buf_write.flush oc
  in
  let rec pieces writer chunk size at =
    if at < String.length chunk then (
      write writer (String.sub chunk at (min size (String.length chunk - at)));
      Eio.Fiber.yield ();
      pieces writer chunk size (at + size))
  in
  let headers =
    headers @ [ ("transfer-encoding", "chunked"); ("connection", "close") ]
  in
  match
    Eio.Fiber.both copy (fun () ->
        Response_io.write ~flush:true
          (fun writer ->
            (* A stream's head reaches the client before its first frame. *)
            Eio.Buf_write.flush oc;
            let rec drain () =
              match Eio.Stream.take queue with
              | `End -> ()
              | `Chunk chunk ->
                  (match t.pieces with
                  | None -> write writer chunk
                  | Some size -> pieces writer chunk size 0);
                  drain ()
            in
            drain ())
          (Serve.head status headers)
          oc)
  with
  | () -> ()
  | exception (Eio.Io _ | End_of_file | Failure _) -> raise Serve.Drop

(* [r] passed on to the gateway, and its answer recorded and written to
   [client], or to no one where its client is gone: whether the connection
   may carry another request. *)
let pass t client ~keep_alive r body =
  Eio.Switch.run @@ fun sw ->
  let response, next = ask t ~sw r body in
  let status = Http.Status.to_int (Http.Response.status response) in
  let headers = Http.Response.headers response in
  r.status <- Some status;
  answered_now t r (Http.Header.get headers "retry-after");
  wake t;
  let kind = media_type headers in
  match client with
  | Some oc when kind = "text/event-stream" ->
      stream t oc r status (passed headers) next;
      false
  | None when kind = "text/event-stream" -> false
  | _ ->
      let text = whole next in
      if String.ends_with ~suffix:"json" kind then (
        r.answer <- Call.parsed text;
        wake t);
      Option.iter
        (fun oc ->
          Serve.write_reply oc ~keep_alive
            { status; headers = passed headers; body = text })
        client;
      true

(* [r] passed on, until its client leaves, which ends the gateway's half
   too, as it would unproxied. *)
let forward t ic oc ~keep_alive r body =
  match
    Eio.Fiber.first
      (fun () -> `Answered (pass t (Some oc) ~keep_alive r body))
      (fun () ->
        gone ic;
        `Gone)
  with
  | `Answered kept -> kept
  | `Gone -> raise Serve.Drop

(* [r] kept waiting until [release]. A client that leaves first, or whose
   connection is cut, has left a request the gateway is still to get: it is
   passed on when released, with no one to write its answer to. *)
let hold t ic oc ~keep_alive r body =
  let released, release = Eio.Promise.create () in
  t.releases <- t.releases @ [ (fun () -> Eio.Promise.resolve release ()) ];
  let orphaned () =
    Eio.Fiber.fork ~sw:t.sw (fun () ->
        Eio.Promise.await released;
        match pass t None ~keep_alive:false r body with
        | (_ : bool) -> ()
        | exception Serve.Drop -> ())
  in
  match
    Eio.Fiber.first
      (fun () ->
        Eio.Promise.await released;
        `Released)
      (fun () ->
        gone ic;
        `Gone)
  with
  | `Released -> forward t ic oc ~keep_alive r body
  | `Gone ->
      orphaned ();
      raise Serve.Drop
  | exception (Eio.Cancel.Cancelled _ as cancelled) ->
      orphaned ();
      raise cancelled

(* [r] passed on, and the gateway's whole answer recorded, none of it
   written: the connection is ended once it has arrived. *)
let lose t ic r body =
  (match
     Eio.Fiber.first
       (fun () ->
         Eio.Switch.run @@ fun sw ->
         let response, next = ask t ~sw r body in
         `Lost (response, whole next))
       (fun () ->
         gone ic;
         `Gone)
   with
  | `Lost (response, text) ->
      r.status <- Some (Http.Status.to_int (Http.Response.status response));
      r.answer_lost <- true;
      answered_now t r
        (Http.Header.get (Http.Response.headers response) "retry-after");
      r.answer <- Call.parsed text;
      wake t
  | `Gone -> ());
  raise Serve.Drop

(* One exchange of [r]: whether the connection may carry another. *)
let exchange t ic oc ~keep_alive r body =
  match spend fst t.scripted r with
  | Some (_, answer) ->
      r.status <- Some answer.status;
      answered_now t r
        (List.assoc_opt "retry-after"
           (List.map
              (fun (name, value) -> (String.lowercase_ascii name, value))
              answer.headers));
      r.answer <- Call.parsed answer.body;
      wake t;
      Serve.write_reply oc ~keep_alive answer;
      true
  | None -> (
      if Option.is_some (spend Fun.id t.unanswering r) then raise Serve.Drop;
      if Option.is_some (spend Fun.id t.losing r) then lose t ic r body;
      match t.held with
      | Some held when held r -> hold t ic oc ~keep_alive r body
      | _ -> forward t ic oc ~keep_alive r body)

let record t (request : Http.Request.t) body =
  let r =
    {
      meth = Http.Method.to_string request.meth;
      path = Http.Request.resource request;
      headers = request.headers;
      json = Call.parsed body;
      status = None;
      arrived = tick t;
      answered = None;
      arrived_at = Run.now ();
      answered_at = None;
      retry_after = None;
      answer = None;
      events = Dynarray.create ();
      closed = false;
      answer_lost = false;
    }
  in
  Dynarray.add_last t.requests r;
  wake t;
  r

(* Each piece [rechunk] writes leaves in a segment of its own. *)
let no_delay flow =
  Option.iter
    (fun fd ->
      Eio_unix.Fd.use_exn "no_delay" fd (fun fd ->
          Unix.setsockopt fd Unix.TCP_NODELAY true))
    (Eio_unix.Resource.fd_opt flow)

let connection t flow =
  no_delay flow;
  Eio.Switch.run @@ fun sw ->
  let number = t.connected in
  t.connected <- number + 1;
  Hashtbl.replace t.connections number (fun () -> Eio.Switch.fail sw Cut);
  Fun.protect ~finally:(fun () -> Hashtbl.remove t.connections number)
  @@ fun () ->
  let ic = Eio.Buf_read.of_flow ~max_size:max_int flow in
  Eio.Buf_write.with_flow flow @@ fun oc ->
  let rec next () =
    match Request_io.read ic with
    | `Eof | `Invalid _ -> ()
    | `Ok request ->
        if Http.Header.get request.headers "expect" = Some "100-continue" then (
          Eio.Buf_write.string oc "HTTP/1.1 100 Continue\r\n\r\n";
          Eio.Buf_write.flush oc);
        let body = Serve.read_body request ic in
        let r = record t request body in
        let keep_alive = Http.Request.is_keep_alive request in
        let kept =
          Fun.protect
            ~finally:(fun () ->
              r.closed <- true;
              wake t)
            (fun () -> exchange t ic oc ~keep_alive r body)
        in
        if kept && keep_alive then next ()
  in
  next ()

(* End every connection the proxy holds: open streams and requests in
   flight. New ones are still accepted. *)
let cut t =
  List.iter
    (fun ending -> ending ())
    (List.of_seq (Hashtbl.to_seq_values t.connections))

(* Cut, and refuse every new connection until [up]. *)
let down t =
  t.refusing <- true;
  cut t

let up t = t.refusing <- false

(* Keep every request [matches] accepts waiting, recorded and not passed
   on, until [release]. *)
let hold_requests t matches = t.held <- Some matches

let release t =
  t.held <- None;
  let waiting = t.releases in
  t.releases <- [];
  List.iter (fun go -> go ()) waiting

(* Answer the next [times] requests [matches] accepts with [reply], without
   passing them on. *)
let answer t matches reply times =
  Dynarray.add_last t.scripted ({ matches; left = times }, reply)

(* Read the next [times] requests [matches] accepts whole, pass none on, and
   end each one's connection with nothing written: a request the gateway
   dropped unanswered, as the client meets it. *)
let leave_unanswered t matches times =
  Dynarray.add_last t.unanswering { matches; left = times }

(* Pass the next [times] requests [matches] accepts on, record the gateway's
   whole answer to each, and end each one's connection with none of it
   written: a request the gateway carried out whose answer the client never
   got. Not for a stream, whose answer has no end to wait for. *)
let lose_answer t matches times =
  Dynarray.add_last t.losing { matches; left = times }

(* Pass every stream on [bytes] at a time, each piece written on its own. *)
let rechunk t bytes = t.pieces <- Some bytes

(* Write a bus-event whose data is [data] onto the stream opened last of
   those still open, between two of the events the gateway writes, and
   record it among the events that stream carried. It carries no id. *)
let carry t data =
  match List.rev t.streams with
  | (_, write) :: _ -> write ("event: bus-event\ndata: " ^ data ^ "\n\n")
  | [] -> failwith "no stream is open to carry the frame"

(* A proxy in front of the gateway at [gateway], on a port of 127.0.0.1 the
   system picks. *)
let start gateway =
  let socket =
    Eio.Net.listen ~sw:(Run.sw ()) ~backlog:128 ~reuse_addr:true (Run.net ())
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | `Unix _ -> failwith "the client proxy listens on TCP"
  in
  let stopping, stop = Eio.Promise.create () in
  let stopped, finished = Eio.Promise.create () in
  let running, ran = Eio.Promise.create () in
  Eio.Fiber.fork ~sw:(Run.sw ()) (fun () ->
      Eio.Fiber.first
        (fun () -> Eio.Promise.await stopping)
        (fun () ->
          Eio.Switch.run @@ fun sw ->
          let t =
            {
              origin = Printf.sprintf "http://127.0.0.1:%d" port;
              gateway;
              requests = Dynarray.create ();
              changed = Eio.Condition.create ();
              clock = 0;
              refusing = false;
              held = None;
              releases = [];
              scripted = Dynarray.create ();
              unanswering = Dynarray.create ();
              losing = Dynarray.create ();
              pieces = None;
              streams = [];
              connections = Hashtbl.create 16;
              connected = 0;
              sw;
              close =
                (fun () ->
                  if not (Eio.Promise.is_resolved stopping) then
                    Eio.Promise.resolve stop ();
                  Eio.Promise.await stopped);
            }
          in
          Eio.Promise.resolve ran t;
          Eio.Net.run_server socket
            ~on_error:(fun _ -> ())
            (fun flow _ -> if not t.refusing then connection t flow));
      Eio.Resource.close socket;
      Eio.Promise.resolve finished ());
  Eio.Promise.await running

let close t = t.close ()
