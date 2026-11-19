(* The HTTP/1.1 server every stand-in answers from: cohttp's request and
   response codecs, over cohttp-eio's IO, on a socket this module accepts.

   cohttp-eio's own server writes a response's head before its handler may
   decide anything, and keeps the socket from it, so it cannot hold a request
   unanswered while watching for its client to leave, or end a connection
   halfway through a body. Those are what stand-ins are for, so the accept
   loop is the harness's own; everything on the wire is cohttp's. *)

open Wire

type request = {
  meth : string;
  path : string;
  query : (string * string list) list;
  headers : Http.Header.t;
  body : string;
  mutable abandoned : bool;
      (** Whether the client ended the connection before it was answered. *)
}

type reply = { status : int; headers : (string * string) list; body : string }

type answer =
  | Reply of reply
  | Stream of {
      status : int;
      headers : (string * string) list;
      events : (string -> unit) -> unit;
    }
      (** A chunked body: each string [events] writes reaches the client before
          [events] goes on. *)
  | Held of reply option Eio.Promise.t
      (** Not answered until the promise is resolved: with a reply, or with
          [None], which ends the connection unanswered. *)
  | Dropped  (** The connection is ended with no byte of an answer. *)
  | Partly of reply * int
      (** The head, stating the whole body's length, and that many of its bytes;
          then nothing, until the client ends the connection. *)

type t = { port : int; origin : string; stop : unit -> unit }

exception Drop

let json_reply status json =
  {
    status;
    headers = [ ("content-type", "application/json") ];
    body = Yojson.Safe.to_string json;
  }

let read_body (request : Http.Request.t) ic =
  match Http.Request.has_body request with
  | `No -> ""
  | `Unknown | `Yes ->
      let reader = Request_io.make_body_reader request ic in
      let buffer = Buffer.create 1024 in
      let rec chunks () =
        match Request_io.read_body_chunk reader with
        | Cohttp.Transfer.Chunk text ->
            Buffer.add_string buffer text;
            chunks ()
        | Final_chunk text -> Buffer.add_string buffer text
        | Done -> ()
      in
      chunks ();
      Buffer.contents buffer

let head status headers =
  Http.Response.make
    ~status:(Http.Status.of_int status)
    ~headers:(Http.Header.of_list headers)
    ()

let write_reply oc ~keep_alive (reply : reply) =
  let headers =
    reply.headers
    @ [
        ("content-length", string_of_int (String.length reply.body));
        ("connection", if keep_alive then "keep-alive" else "close");
      ]
  in
  Response_io.write ~flush:true
    (fun writer -> Response_io.write_body writer reply.body)
    (head reply.status headers)
    oc

(* Wait until the client ends the connection: it sends nothing more while it
   waits for an answer. *)
let until_gone ic =
  match Eio.Buf_read.ensure ic 1 with
  | () -> `Spoke
  | exception End_of_file -> `Gone
  | exception Eio.Io _ -> `Gone

let connection handler flow _peer =
  let ic = Eio.Buf_read.of_flow ~max_size:max_int flow in
  Eio.Buf_write.with_flow flow @@ fun oc ->
  let rec next () =
    match Request_io.read ic with
    | `Eof | `Invalid _ -> ()
    | `Ok request -> (
        let body = read_body request ic in
        let uri = Uri.of_string (Http.Request.resource request) in
        let asked =
          {
            meth = Http.Method.to_string request.meth;
            path = Uri.path uri;
            query = Uri.query uri;
            headers = request.headers;
            body;
            abandoned = false;
          }
        in
        let keep_alive = Http.Request.is_keep_alive request in
        let again () = if keep_alive then next () in
        match handler asked with
        | Reply reply ->
            write_reply oc ~keep_alive reply;
            again ()
        | Stream { status; headers; events } ->
            let headers =
              headers
              @ [ ("transfer-encoding", "chunked"); ("connection", "close") ]
            in
            Response_io.write ~flush:true
              (fun writer ->
                events (fun text ->
                    Response_io.write_body writer text;
                    Eio.Buf_write.flush oc))
              (head status headers) oc
        | Held answered -> (
            match
              Eio.Fiber.first
                (fun () -> `Answered (Eio.Promise.await answered))
                (fun () -> until_gone ic)
            with
            | `Answered (Some reply) ->
                write_reply oc ~keep_alive reply;
                again ()
            | `Answered None -> raise Drop
            | `Gone | `Spoke ->
                asked.abandoned <- true;
                raise Drop)
        | Dropped -> raise Drop
        | Partly (reply, sent) ->
            let headers =
              reply.headers
              @ [ ("content-length", string_of_int (String.length reply.body)) ]
            in
            Response_io.write_header (head reply.status headers) oc;
            Eio.Buf_write.string oc (String.sub reply.body 0 sent);
            Eio.Buf_write.flush oc;
            ignore (until_gone ic : [ `Gone | `Spoke ]);
            raise Drop)
  in
  next ()

(* Serve [handler] on a port of 127.0.0.1 the system picks, until [stop],
   which ends every connection, kept-alive ones too. *)
let start handler =
  let sw = Run.sw () in
  let socket =
    Eio.Net.listen ~sw ~backlog:128 ~reuse_addr:true (Run.net ())
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | `Unix _ -> failwith "a stand-in listens on TCP"
  in
  let stopping, stop = Eio.Promise.create () in
  let stopped, closed = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Fiber.first
        (fun () -> Eio.Promise.await stopping)
        (fun () ->
          Eio.Net.run_server socket
            ~on_error:(function Drop -> () | _ -> ())
            (connection handler));
      Eio.Resource.close socket;
      Eio.Promise.resolve closed ());
  {
    port;
    origin = Printf.sprintf "http://127.0.0.1:%d" port;
    stop =
      (fun () ->
        if not (Eio.Promise.is_resolved stopping) then
          Eio.Promise.resolve stop ();
        Eio.Promise.await stopped);
  }
