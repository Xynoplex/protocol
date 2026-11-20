(* How long the gateway waits for the Archivist, as the Archivist document's
   x-xynolog-limits say (specs/src/archivist/openapi.json, whose description
   states the rule): a read's answer begins within connectSeconds and
   readSeconds together of the read's first send, each next part of an
   answer within readSeconds, and an upload's answer within readSeconds of
   its last byte; no bound is put on a whole answer or on sending a request.
   A connection not taken within connectSeconds is not staged here: an
   address that takes no connection cannot be staged on the loopback. The
   gateway's own tests hold that a read whose connection is not taken in
   time is not sent again, and that a read's wait counts from its first send
   with its retry inside it, over a stand-in for its connector
   (consumer-owned Gateway implementation).

   Plane-independent, and slow: the one case waits four waits out side by
   side, the longest connectSeconds and readSeconds together. The fourth is
   an upload the Archivist recorded and never answered: its resend, under
   its key, is answered with the resource it made
   (specs/src/paths/upload-resource.json, A key). *)

open Xynolog_conformance
open Support
open Case

let read () = float_of_int (Protocol.document_limit Archivist "readSeconds")

(* How long a read's answer may take to begin from the read's first send,
   its retry included. *)
let answer_begins () =
  float_of_int (Protocol.document_limit Archivist "connectSeconds") +. read ()

(* How long the slow upload takes to send: a wait counted from its first
   byte would end this much before one counted from its last. *)
let sending = 10.0
let bytes = String.init 1024 (fun i -> Char.chr (i mod 256))

(* That [elapsed] waited [wait] out: no sooner than it, less a quarter
   second for the trip, and not past 1.3 times it. *)
let waited_out what elapsed wait =
  Check.that
    (Printf.sprintf "%s: %g s, against a wait of %g s" what elapsed wait)
    (elapsed >= wait -. 0.25 && elapsed < wait *. 1.3)

(* An upload whose file goes a piece at a time over [sending]: its status,
   when its last byte was written, and when its answer came. *)
let slow_upload origin token storage_uri =
  let boundary = "conformance-" ^ Fresh.uuid () in
  let field name value =
    Printf.sprintf
      "--%s\r\nContent-Disposition: form-data; name=\"%s\"\r\n\r\n%s\r\n"
      boundary name value
  in
  let head =
    field "name" "Slow"
    ^ field "format" "text/plain"
    ^ field "storageUri" storage_uri
    ^ Printf.sprintf
        "--%s\r\n\
         Content-Disposition: form-data; name=\"file\"; filename=\"slow.txt\"\r\n\
         Content-Type: text/plain\r\n\
         \r\n"
        boundary
  in
  let tail = Printf.sprintf "\r\n--%s--\r\n" boundary in
  let pieces = 10 in
  let piece = String.make 100 'x' in
  let uri = Uri.of_string origin in
  let host = Option.get (Uri.host uri) and port = Option.get (Uri.port uri) in
  Eio.Switch.run @@ fun sw ->
  let socket =
    Eio.Net.connect ~sw (Run.net ())
      (`Tcp (Eio_unix.Net.Ipaddr.of_unix (Unix.inet_addr_of_string host), port))
  in
  Eio.Flow.copy_string
    (Printf.sprintf
       "POST %s HTTP/1.1\r\n\
        Host: %s:%d\r\n\
        Authorization: Bearer %s\r\n\
        Idempotency-Key: %s\r\n\
        Content-Type: multipart/form-data; boundary=%s\r\n\
        Content-Length: %d\r\n\
        Connection: close\r\n\
        \r\n"
       (Protocol.path Gateway "POST" "/resources")
       host port token (Fresh.uuid ()) boundary
       (String.length head + (pieces * String.length piece) + String.length tail)
    ^ head)
    socket;
  for _ = 1 to pieces do
    Run.sleep (sending /. float_of_int pieces);
    Eio.Flow.copy_string piece socket
  done;
  Eio.Flow.copy_string tail socket;
  let last_byte = Run.now () in
  let input = Eio.Buf_read.of_flow ~max_size:max_int socket in
  match Wire.Response_io.read input with
  | `Ok response ->
      let answered = Run.now () in
      let reader = Wire.Response_io.make_body_reader response input in
      ignore
        (Call.body_of (fun () -> Wire.Response_io.read_body_chunk reader)
          : string);
      (Http.Status.to_int (Http.Response.status response), last_byte, answered)
  | `Eof -> failwith "the slow upload was closed unanswered"
  | `Invalid why ->
      failwith ("the slow upload was answered what is not HTTP: " ^ why)

(* Whether [rest], a chunked body as it came, ends with the chunk that ends
   one. *)
let rec chunks_end rest =
  match String.index_opt rest '\n' with
  | None -> false
  | Some eol -> (
      let line = String.trim (String.sub rest 0 eol) in
      let size =
        match String.index_opt line ';' with
        | Some semi -> String.sub line 0 semi
        | None -> line
      in
      match int_of_string_opt ("0x" ^ size) with
      | None -> false
      | Some 0 -> true
      | Some n ->
          let next = eol + 1 + n + 2 in
          String.length rest >= next
          && chunks_end (String.sub rest next (String.length rest - next)))

(* A GET of [path] as fetch makes one, and how its body came: its status,
   when its head came, and whether its body came "whole" or "cut short" —
   framed by its Content-Length or its chunks, as fetch reads one. cohttp's
   reader ends a body cut short as it ends a whole one, so the body is
   read here as it came, to the end of the connection. *)
let framed_read origin token path =
  let uri = Uri.of_string origin in
  let host = Option.get (Uri.host uri) and port = Option.get (Uri.port uri) in
  Eio.Switch.run @@ fun sw ->
  let socket =
    Eio.Net.connect ~sw (Run.net ())
      (`Tcp (Eio_unix.Net.Ipaddr.of_unix (Unix.inet_addr_of_string host), port))
  in
  Eio.Flow.copy_string
    (Printf.sprintf
       "GET %s HTTP/1.1\r\n\
        Host: %s:%d\r\n\
        Authorization: Bearer %s\r\n\
        Connection: close\r\n\
        \r\n"
       path host port token)
    socket;
  let input = Eio.Buf_read.of_flow ~max_size:max_int socket in
  match Wire.Response_io.read input with
  | `Ok response ->
      let headed = Run.now () in
      let rest =
        match Eio.Buf_read.take_all input with
        | rest -> Some rest
        | exception Eio.Io _ -> None
      in
      let headers = Http.Response.headers response in
      let whole =
        match
          ( rest,
            Http.Header.get headers "content-length",
            Http.Header.get headers "transfer-encoding" )
        with
        | None, _, _ -> false
        | Some rest, Some length, _ ->
            String.length rest >= int_of_string length
        | Some rest, None, Some coding
          when String.lowercase_ascii (String.trim coding) = "chunked" ->
            chunks_end rest
        | Some _, None, _ -> true
      in
      ( Http.Status.to_int (Http.Response.status response),
        headed,
        if whole then "whole" else "cut short" )
  | `Eof -> failwith ("GET " ^ path ^ " was closed unanswered")
  | `Invalid why ->
      failwith ("GET " ^ path ^ " was answered what is not HTTP: " ^ why)

let stops_answering (w : World.t) =
  let token = World.person w "waiting" in
  let stored id =
    Hashtbl.replace w.archivist.resources id
      { storage_uri = "file://" ^ id; media_type = "text/plain" };
    Hashtbl.replace w.archivist.content ("file://" ^ id) bytes
  in
  let bytes_path id =
    Protocol.path_of Gateway "GET" "/resources/{id}" [ ("id", id) ]
  in
  let never_answered () =
    let id = "res-" ^ Fresh.uuid () in
    stored id;
    Hashtbl.replace w.archivist.mode.unanswered id ();
    let started = Run.now () in
    let reply = Call.call ~token (World.origin w) "GET" (bytes_path id) in
    waited_out "a read never answered"
      (Run.now () -. started)
      (answer_begins ());
    status "a read never answered" 503 reply;
    conforms "GET" "/resources/{id}" reply;
    (* A read whose wait ran out is not sent again. *)
    let reads =
      List.filter
        (fun (c : Archivist_standin.call) ->
          c.path = Printf.sprintf "/resources/%s/content" id)
        (Dynarray.to_list w.archivist.calls)
    in
    Check.that
      (Printf.sprintf "the read was sent %d times, not once" (List.length reads))
      (List.length reads = 1)
  in
  let answered_in_part () =
    let id = "res-" ^ Fresh.uuid () in
    stored id;
    Hashtbl.replace w.archivist.mode.answered_in_part id ();
    let status, headed, body =
      framed_read (World.origin w) token (bytes_path id)
    in
    Check.equal "the answer's status" (`Int 200) (`Int status);
    waited_out "an answer that stopped part way"
      (Run.now () -. headed)
      (read ());
    Check.equal "the body" (`String "cut short") (`String body)
  in
  let upload_never_answered () =
    let storage_uri = "file://uploads/" ^ Fresh.uuid () in
    Hashtbl.replace w.archivist.mode.unanswered storage_uri ();
    let code, last_byte, answered =
      slow_upload (World.origin w) token storage_uri
    in
    Check.equal "the slow upload's status" (`Int 503) (`Int code);
    waited_out "an upload never answered, from its last byte"
      (answered -. last_byte) (read ());
    Check.that "the upload was not recorded"
      (not
         (List.exists
            (fun (u : Archivist_standin.upload) ->
              List.assoc_opt "storageUri" u.fields = Some storage_uri)
            (Dynarray.to_list w.archivist.uploads)))
  in
  let recorded_never_answered () =
    let key = Fresh.uuid () in
    let storage_uri = "file://uploads/" ^ Fresh.uuid () in
    let form () =
      [
        Call.Field ("name", "Recorded");
        Call.Field ("format", "text/plain");
        Call.Field ("storageUri", storage_uri);
        Call.File
          {
            name = "file";
            filename = "recorded.txt";
            media = "text/plain";
            bytes;
          };
      ]
    in
    let made () =
      List.filter
        (fun (u : Archivist_standin.upload) ->
          List.assoc_opt "storageUri" u.fields = Some storage_uri)
        (Dynarray.to_list w.archivist.uploads)
    in
    Hashtbl.replace w.archivist.mode.records_unanswered storage_uri ();
    let started = Run.now () in
    let lost = World.upload w token (form ()) ~key in
    waited_out "an upload recorded and never answered"
      (Run.now () -. started)
      (read ());
    status "an upload recorded and never answered" 503 lost;
    conforms "POST" "/resources" lost;
    Check.that "it was recorded once" (List.length (made ()) = 1);
    let resent = World.upload w token (form ()) ~key in
    status "its resend" 202 resent;
    conforms "POST" "/resources" resent;
    Check.equal "the resend's answer"
      (`Assoc [ ("resourceId", `String (List.hd (made ())).resource_id) ])
      (Option.get resent.json);
    Check.that "it is still recorded once" (List.length (made ()) = 1)
  in
  Eio.Fiber.all
    [
      never_answered;
      answered_in_part;
      upload_never_answered;
      recorded_never_answered;
    ]

let tests implementations =
  each_plane ~planes:[ In_process ] ~make:(plain implementations) ~world:Fun.id
    ~close:World.close "an Archivist that stops answering"
    [
      case
        ~timeout:((answer_begins () *. 2.0) +. sending +. 30.0)
        "is waited for connectSeconds and readSeconds for a read's answer to \
         begin, and readSeconds for each next part and for an upload's answer \
         from its last byte; an upload it recorded and never answered is \
         answered on its resend under its key"
        stops_answering;
    ]
