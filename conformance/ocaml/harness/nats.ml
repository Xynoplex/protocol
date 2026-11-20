(* A real NATS broker for the NATS plane, and a client of it: a mocked broker
   would prove nothing about subjects, queue groups or the key-value tables.
   nats-server is spawned from PATH on a free port with JetStream. Like the
   broker the launcher runs, it takes a message twice as large as the largest
   emit, so the envelope a frame is published in always fits.

   The client is nats-client's protocol, on an Eio socket: enough of it for a
   suite to ask JetStream's API, under $JS.API, and read its JSON answers. *)

(* A user and a password the broker admits, and admits no one else. *)
type credentials = { user : string; password : string }

type broker = {
  url : string;
  port : int;
  config : string;
  store : string;
  jetstream : bool;
  mutable credentials : credentials option;
      (** What it admits alone, when anything. *)
  mutable process : Process.t option;
}

(* Start it on its port and store, admitting what it admits. *)
let up broker =
  let process =
    Process.spawn "broker"
      ([ "nats-server"; "-c"; broker.config; "-a"; "127.0.0.1"; "-p" ]
      @ [ string_of_int broker.port ]
      @ (if broker.jetstream then [ "-js"; "-sd"; broker.store ] else [])
      @
      match broker.credentials with
      | Some { user; password } -> [ "--user"; user; "--pass"; password ]
      | None -> [])
      (Process.run_environment ())
  in
  broker.process <- Some process;
  Net.wait_for_tcp "127.0.0.1" broker.port 10.0

(* A broker on a free port, with JetStream unless [jetstream] is false, and
   admitting only [credentials] when they are given. *)
let start_broker ?(jetstream = true) ?credentials () =
  let port = Net.free_port () in
  let store = Filename.temp_dir "conformance-nats-" "" in
  let config = Filename.concat store "xynolog.conf" in
  Out_channel.with_open_bin config (fun out ->
      Printf.fprintf out "max_payload: %d\n"
        (2 * Protocol.limit Gateway "POST" "/bus/emit" "maxBodyBytes"));
  let broker =
    {
      url = Printf.sprintf "nats://127.0.0.1:%d" port;
      port;
      config;
      store;
      jetstream;
      credentials;
      process = None;
    }
  in
  up broker;
  broker

(* Stop it, leaving its port closed until [up]. *)
let down broker =
  Option.iter (fun p -> ignore (Process.stop p : int option)) broker.process;
  broker.process <- None

(* Stop it and start it again on the same port and store: admitting
   [credentials] alone from now on, when they are given. *)
let restart ?credentials broker =
  down broker;
  Option.iter (fun c -> broker.credentials <- Some c) credentials;
  up broker

let stop_broker broker =
  down broker;
  Gateway.remove broker.store

module P = Nats_client.Protocol

type client = {
  output : Eio.Buf_write.t;
  replies : (string, string Eio.Promise.u) Hashtbl.t;
  inbox : string;
  close : unit -> unit;
}

let send client text =
  Eio.Buf_write.string client.output text;
  Eio.Buf_write.flush client.output

(* Connect to [port] and read what the server sends: every message on the
   client's inbox answers the request named by its subject's last token. *)
let connect port =
  let opened, open_ = Eio.Promise.create () in
  let closing, close = Eio.Promise.create () in
  Eio.Fiber.fork ~sw:(Run.sw ()) (fun () ->
      Eio.Switch.run @@ fun sw ->
      let flow =
        Eio.Net.connect ~sw (Run.net ())
          (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
      in
      let input = Eio.Buf_read.of_flow ~max_size:max_int flow in
      Eio.Buf_write.with_flow flow @@ fun output ->
      (match P.parse_server_line (Eio.Buf_read.line input) with
      | Info _ -> ()
      | _ -> failwith "the broker did not begin with INFO");
      let client =
        {
          output;
          replies = Hashtbl.create 8;
          inbox = "_INBOX." ^ Fresh.hex 8;
          close = (fun () -> Eio.Promise.resolve close ());
        }
      in
      send client
        (P.encode_connect
           ~connect:{ P.default_connect with verbose = false }
           ());
      send client
        (P.encode_sub ~subject:(client.inbox ^ ".*")
           ~sid:(Nats_client.Sid.create 1) ());
      send client (P.encode_ping ());
      let rec read () =
        match P.parse_server_line (Eio.Buf_read.line input) with
        | Ping ->
            send client (P.encode_pong ());
            read ()
        | Pong ->
            if not (Eio.Promise.is_resolved opened) then
              Eio.Promise.resolve open_ client;
            read ()
        | Msg_meta { subject; payload_size; _ } ->
            let payload = Eio.Buf_read.take payload_size input in
            ignore (Eio.Buf_read.line input);
            let token = List.nth (String.split_on_char '.' subject) 2 in
            (match Hashtbl.find_opt client.replies token with
            | Some waiting ->
                Hashtbl.remove client.replies token;
                Eio.Promise.resolve waiting payload
            | None -> ());
            read ()
        | Err why -> failwith ("the broker refused: " ^ why)
        | Ok | Info _ | Hmsg_meta _ -> read ()
      in
      Eio.Fiber.first
        (fun () -> Eio.Promise.await closing)
        (fun () -> match read () with () -> () | exception End_of_file -> ()));
  Eio.Promise.await opened

(* Publish [payload] to [subject] and read the one answer, as JSON. *)
let request ?(timeout = 10.0) client subject payload =
  let token = Fresh.hex 8 in
  let answer, answered = Eio.Promise.create () in
  Hashtbl.replace client.replies token answered;
  send client
    (P.encode_pub ~subject
       ~reply_to:(client.inbox ^ "." ^ token)
       (Yojson.Safe.to_string payload));
  match
    Eio.Time.with_timeout (Run.clock ()) timeout (fun () ->
        Ok (Eio.Promise.await answer))
  with
  | Ok text -> Yojson.Safe.from_string text
  | Error `Timeout -> failwith ("the broker did not answer " ^ subject)

(* JetStream's answer, failing when it carries an error. *)
let jetstream client subject payload =
  let answer = request client ("$JS.API." ^ subject) payload in
  match Yojson.Safe.Util.member "error" answer with
  | `Null -> answer
  | error ->
      failwith
        (Printf.sprintf "JetStream refused %s: %s" subject
           (Yojson.Safe.to_string error))
