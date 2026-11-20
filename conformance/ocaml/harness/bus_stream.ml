(* A bus-stream client. It opens POST /bus/subscribe, parses the
   Server-Sent Events as they arrive, and checks every message against the
   spec as it goes: the message against BusStreamMessage, with its data
   parsed, and its id against the frame it names — a persisted id exactly
   when the frame is scoped and its payload carries a sequence number, a
   reply id exactly when it carries a correlationId — and that the stream
   came as text/event-stream. A case reads [violations] at its end; any entry
   fails it, whatever it was about. *)

type frame = {
  channel : string;
  correlation_id : string option;
  payload : Yojson.Safe.t;
  scope : string option;
}

type message = {
  event : string;
  id : string option;
  frame : frame option;
  at : float;  (** When it arrived. *)
}

type t = {
  messages : message Dynarray.t;
  violations : string Dynarray.t;
  mutable listeners : (message -> unit) list;
  changed : Eio.Condition.t;
  mutable ended : bool;
  closing : unit Eio.Promise.u;
}

let string_member name json =
  match Protocol.at json [ name ] with Some (`String s) -> Some s | _ -> None

let frame_of json =
  match (Protocol.at json [ "channel" ], Protocol.at json [ "payload" ]) with
  | Some (`String channel), Some payload ->
      Some
        {
          channel;
          correlation_id = string_member "correlationId" json;
          payload;
          scope = string_member "scope" json;
        }
  | _ -> None

let violation t text = Dynarray.add_last t.violations text

(* The id the gateway gives [frame] (contract/TRANSPORT-HTTP.md § Event id
   and resumption): its position when it is scoped and its payload carries
   a sequence number, else its reply's when it carries a correlationId, else
   an ephemeral one, whose form EphemeralEventId states. *)
type expected_id = Persisted of string | Reply of string | Ephemeral

let expected_id frame =
  match
    ( frame.scope,
      Protocol.at frame.payload [ "metadata"; "sequenceNumber" ],
      frame.correlation_id )
  with
  | Some scope, Some (`Int sequence), _ ->
      Persisted (Printf.sprintf "p-%s-%d" scope sequence)
  | _, _, Some correlation ->
      Reply (Printf.sprintf "e-%s:%s" frame.channel correlation)
  | _ -> Ephemeral

(* bus/event-id-cases.json's every case, given the id the rule gives it. *)
let ids_checked =
  lazy
    (List.iter
       (fun (c : Xynolog_protocol.Tables.Bus_event_id_cases.cases_item) ->
         let given =
           expected_id
             {
               channel = c.frame.channel;
               correlation_id = c.frame.correlation_id;
               payload = c.frame.payload;
               scope = c.frame.scope;
             }
         in
         let agrees =
           match (given, c.id) with
           | Persisted id, Persisted { persisted } -> id = persisted
           | Reply id, Reply { reply } -> id = reply
           | Ephemeral, Ephemeral { ephemeral } -> ephemeral
           | _ -> false
         in
         if not agrees then
           failwith
             (Printf.sprintf
                "bus/event-id-cases.json: %S: the harness gives the frame \
                 another id"
                c.why))
       (Protocol.tables ()).bus_event_id_cases.cases)

let accept t event id data =
  let frame, parsed =
    if event = "bus-event" then (
      match Yojson.Safe.from_string data with
      | json -> (frame_of json, json)
      | exception Yojson.Json_error why ->
          violation t
            (Printf.sprintf "a bus-event whose data is not JSON (%s): %s" why
               (Call.shown data));
          (None, `String data))
    else (None, `String data)
  in
  let message =
    `Assoc
      ([ ("event", `String event) ]
      @ (match id with Some id -> [ ("id", `String id) ] | None -> [])
      @ [ ("data", parsed) ])
  in
  (match Protocol.against "BusStreamMessage" message with
  | [] -> ()
  | refusals ->
      violation t
        (Printf.sprintf "a message that is not a BusStreamMessage (%s): %s"
           (String.concat "; " refusals)
           (Call.shown (Yojson.Safe.to_string message))));
  (match (frame, id) with
  | Some frame, Some id -> (
      match expected_id frame with
      | Persisted expected ->
          if id <> expected then
            violation t
              (Printf.sprintf "a persisted frame on %s carried id %s, not %s"
                 frame.channel id expected)
      | Reply expected ->
          if id <> expected then
            violation t
              (Printf.sprintf "a reply on %s carried id %s, not %s"
                 frame.channel id expected)
      | Ephemeral ->
          if Protocol.against "EphemeralEventId" (`String id) <> [] then
            violation t
              (Printf.sprintf
                 "a frame on %s with no correlationId carried id %s"
                 frame.channel id))
  | _ -> ());
  let received = { event; id; frame; at = Run.now () } in
  Dynarray.add_last t.messages received;
  List.iter (fun listener -> listener received) t.listeners;
  Eio.Condition.broadcast t.changed

(* The lines of a body, as its chunks arrive: each without its line ending. *)
let lines_of next on_line =
  let pending = Buffer.create 4096 in
  let flush_lines () =
    let text = Buffer.contents pending in
    let rec split from =
      match String.index_from_opt text from '\n' with
      | Some stop ->
          let line = String.sub text from (stop - from) in
          on_line
            (if String.ends_with ~suffix:"\r" line then
               String.sub line 0 (String.length line - 1)
             else line);
          split (stop + 1)
      | None ->
          Buffer.clear pending;
          Buffer.add_string pending
            (String.sub text from (String.length text - from))
    in
    split 0
  in
  let rec chunks () =
    match next () with
    | Cohttp.Transfer.Chunk text ->
        Buffer.add_string pending text;
        flush_lines ();
        chunks ()
    | Final_chunk text ->
        Buffer.add_string pending text;
        flush_lines ()
    | Done -> ()
    | exception (End_of_file | Eio.Io _) -> ()
  in
  chunks ()

(* Read Server-Sent Events from a body's chunks until it ends. *)
let read t next =
  let event = ref ""
  and id = ref None
  and data = ref []
  and saw_data = ref false in
  lines_of next (function
    | "" ->
        (if !event <> "" || !saw_data then
           let event = if !event = "" then "message" else !event in
           try accept t event !id (String.concat "\n" (List.rev !data))
           with failure ->
             violation t
               (Printf.sprintf "the harness failed on a %s message: %s" event
                  (Printexc.to_string failure)));
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
        | _ -> ()))

let on t listener = t.listeners <- t.listeners @ [ listener ]
let close t = Eio.Promise.try_resolve t.closing () |> ignore

let frames ?channel t =
  List.filter_map
    (fun m ->
      match m.frame with
      | Some f when channel = None || channel = Some f.channel -> Some f
      | _ -> None)
    (Dynarray.to_list t.messages)

(* The first message, at or after index [from], that [matches] accepts. *)
let next ?(timeout = 10.0) ?(from = 0) t what matches =
  let deadline = Run.now () +. timeout in
  let rec look () =
    let found = ref None in
    Dynarray.iteri
      (fun at m ->
        if !found = None && at >= from && matches m then found := Some m)
      t.messages;
    match !found with
    | Some m -> m
    | None ->
        if t.ended then failwith ("the stream ended before " ^ what);
        let remaining = deadline -. Run.now () in
        if remaining <= 0.0 then
          raise
            (Run.Timed_out
               (Printf.sprintf "timed out after %g s waiting for %s" timeout
                  what));
        ignore
          (Eio.Time.with_timeout (Run.clock ()) remaining (fun () ->
               Ok (Eio.Condition.await_no_mutex t.changed)));
        look ()
  in
  look ()

(* The first frame on [channel] that [matches] accepts. *)
let frame ?timeout ?(matches = fun _ -> true) t channel =
  match
    (next ?timeout t ("a frame on " ^ channel) (fun m ->
         match m.frame with
         | Some f -> f.channel = channel && matches f
         | None -> false))
      .frame
  with
  | Some f -> f
  | None -> assert false

(* Return once the connection has ended, or fail after [timeout] s. *)
let ends ?(timeout = 10.0) t =
  let deadline = Run.now () +. timeout in
  let rec wait () =
    if not t.ended then (
      let remaining = deadline -. Run.now () in
      if remaining <= 0.0 then
        raise
          (Run.Timed_out
             (Printf.sprintf "the stream was still open after %g s" timeout));
      ignore
        (Eio.Time.with_timeout (Run.clock ()) remaining (fun () ->
             Ok (Eio.Condition.await_no_mutex t.changed)));
      wait ())
  in
  wait ()

(* Nothing [matches] ever arrives on [channel]: after waiting [wait] s for
   anything still in flight, no frame the stream has carried since it
   opened matches. *)
let quiet ?(matches = fun _ -> true) t channel wait =
  Run.sleep wait;
  match List.filter matches (frames ~channel t) with
  | [] -> ()
  | arrived ->
      failwith
        (Printf.sprintf "expected nothing on %s, got %s" channel
           (Yojson.Safe.to_string
              (`List (List.map (fun (f : frame) -> f.payload) arrived))))

type subscribed = Stream of t | Refused of Call.reply

(* Open a stream. A connection the gateway closes before it answers raises
   here, in the caller: End_of_file or Eio.Io. *)
let subscribe origin token body =
  let opened, open_ = Eio.Promise.create () in
  let closing, closing_u = Eio.Promise.create () in
  Eio.Fiber.fork ~sw:(Run.sw ()) (fun () ->
      Eio.Switch.run @@ fun sw ->
      match
        Call.exchange ~sw
          ~headers:
            ([
               ("content-type", "application/json");
               ("accept", "text/event-stream");
             ]
            @
            match token with
            | Some t -> [ ("authorization", "Bearer " ^ t) ]
            | None -> [])
          ~body:(Yojson.Safe.to_string body)
          "POST"
          (origin ^ Protocol.path Gateway "POST" "/bus/subscribe")
      with
      | exception ((End_of_file | Eio.Io _) as failure) ->
          Eio.Promise.resolve open_ (Error failure)
      | response, next ->
          let status = Http.Status.to_int (Http.Response.status response) in
          let headers = Http.Response.headers response in
          if status <> 200 then
            let text = Call.body_of next in
            Eio.Promise.resolve open_
              (Ok
                 (Refused
                    { Call.status; headers; text; json = Call.parsed text }))
          else
            let stream =
              {
                messages = Dynarray.create ();
                violations = Dynarray.create ();
                listeners = [];
                changed = Eio.Condition.create ();
                ended = false;
                closing = closing_u;
              }
            in
            (match Http.Header.get headers "content-type" with
            | Some kind
              when String.trim (List.hd (String.split_on_char ';' kind))
                   = "text/event-stream" ->
                ()
            | kind ->
                violation stream
                  (Printf.sprintf
                     "a stream that came as %S, not text/event-stream"
                     (Option.value ~default:"" kind)));
            Eio.Promise.resolve open_ (Ok (Stream stream));
            Eio.Fiber.first
              (fun () -> Eio.Promise.await closing)
              (fun () -> read stream next);
            stream.ended <- true;
            Eio.Condition.broadcast stream.changed);
  match Eio.Promise.await opened with
  | Ok subscribed -> subscribed
  | Error failure -> raise failure

(* A subscriber that opens the stream and stops reading once its head has
   come. [resume] starts reading again; [drain] does, and returns once the
   stream has ended — a socket that is not being read cannot see its peer
   close it; [close] ends it from this side. *)
type stalled = {
  resume : unit -> unit;
  drain : unit -> unit;
  close : unit -> unit;
}

let stalled_subscriber origin token body =
  let reading, read = Eio.Promise.create () in
  let closing, close = Eio.Promise.create () in
  let ended, ends = Eio.Promise.create () in
  Eio.Fiber.fork ~sw:(Run.sw ()) (fun () ->
      (try
         Eio.Fiber.first
           (fun () -> Eio.Promise.await closing)
           (fun () ->
             Eio.Switch.run @@ fun sw ->
             let _response, next =
               Call.exchange ~sw
                 ~headers:
                   [
                     ("authorization", "Bearer " ^ token);
                     ("content-type", "application/json");
                   ]
                 ~body:(Yojson.Safe.to_string body)
                 "POST"
                 (origin ^ Protocol.path Gateway "POST" "/bus/subscribe")
             in
             Eio.Promise.await reading;
             let rec chunks () =
               match next () with
               | Cohttp.Transfer.Chunk _ -> chunks ()
               | Final_chunk _ | Done -> ()
             in
             chunks ())
       with End_of_file | Eio.Io _ -> ());
      Eio.Promise.resolve ends ());
  let once promise resolver =
    if not (Eio.Promise.is_resolved promise) then
      Eio.Promise.resolve resolver ()
  in
  {
    resume = (fun () -> once reading read);
    drain =
      (fun () ->
        once reading read;
        Eio.Promise.await ended);
    close = (fun () -> once closing close);
  }

(* A scoped entry of a subscription: [scope]'s [channels], resumed after
   [last_event_id] when it is given. *)
let scope ?last_event_id scope channels =
  `Assoc
    ([
       ("scope", `String scope);
       ("channels", `List (List.map (fun c -> `String c) channels));
     ]
    @
    match last_event_id with
    | Some id -> [ ("lastEventId", `String id) ]
    | None -> [])

let subscribe_body ~client_id ?(global = []) ?(scoped = [])
    ?(pending_replies = []) () =
  `Assoc
    ([ ("clientId", `String client_id) ]
    @ (if global = [] then []
       else [ ("global", `List (List.map (fun c -> `String c) global)) ])
    @ (if scoped = [] then [] else [ ("scoped", `List scoped) ])
    @
    if pending_replies = [] then []
    else
      [
        ("pendingReplies", `List (List.map (fun c -> `String c) pending_replies));
      ])
