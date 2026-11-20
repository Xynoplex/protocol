(* A TCP proxy in front of a broker that can hold what a client sends:
   after the NATS handshake (the client's CONNECT and its first PING), the
   client's bytes wait in the proxy while it holds them. The broker's bytes
   always pass. A client held there has connected, and nothing it registers
   or asks of the broker has been seen by it. *)

type t = {
  url : string;
  mutable holding : bool;
  released : Eio.Condition.t;
  stop : unit -> unit;
}

let handshake_end = "PING\r\n"

(* Hold every client's bytes after its handshake. *)
let hold t = t.holding <- true

(* Let every held and later byte through. *)
let release t =
  t.holding <- false;
  Eio.Condition.broadcast t.released

let close t = t.stop ()

let forward t client broker =
  let buffer = Cstruct.create 65536 in
  let seen = Buffer.create 256 in
  let handshaken = ref false in
  let rec pass () =
    let n = Eio.Flow.single_read client buffer in
    let chunk = Cstruct.to_string ~len:n buffer in
    let after =
      if !handshaken then chunk
      else
        let before = Buffer.length seen in
        Buffer.add_string seen chunk;
        match
          Re.exec_opt (Re.compile (Re.str handshake_end)) (Buffer.contents seen)
        with
        | None ->
            Eio.Flow.copy_string chunk broker;
            ""
        | Some found ->
            handshaken := true;
            let through = Re.Group.stop found 0 - before in
            Eio.Flow.copy_string (String.sub chunk 0 through) broker;
            String.sub chunk through (n - through)
    in
    if after <> "" then (
      while t.holding do
        Eio.Condition.await_no_mutex t.released
      done;
      Eio.Flow.copy_string after broker);
    pass ()
  in
  try pass () with End_of_file | Eio.Io _ -> ()

let start broker_port =
  let sw = Run.sw () in
  let socket =
    Eio.Net.listen ~sw ~backlog:32 ~reuse_addr:true (Run.net ())
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | `Unix _ -> failwith "the proxy listens on TCP"
  in
  let stopping, stop = Eio.Promise.create () in
  let stopped, closed = Eio.Promise.create () in
  let t =
    {
      url = Printf.sprintf "nats://127.0.0.1:%d" port;
      holding = false;
      released = Eio.Condition.create ();
      stop =
        (fun () ->
          if not (Eio.Promise.is_resolved stopping) then
            Eio.Promise.resolve stop ();
          Eio.Promise.await stopped);
    }
  in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Fiber.first
        (fun () -> Eio.Promise.await stopping)
        (fun () ->
          Eio.Net.run_server socket
            ~on_error:(fun _ -> ())
            (fun client _peer ->
              Eio.Switch.run @@ fun sw ->
              let broker =
                Eio.Net.connect ~sw (Run.net ())
                  (`Tcp (Eio.Net.Ipaddr.V4.loopback, broker_port))
              in
              Eio.Fiber.first
                (fun () ->
                  try Eio.Flow.copy broker client
                  with End_of_file | Eio.Io _ -> ())
                (fun () -> forward t client broker)));
      Eio.Resource.close socket;
      Eio.Promise.resolve closed ());
  t
