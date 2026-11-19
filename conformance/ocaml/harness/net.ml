(* The ports this harness hands to the processes it starts: 20000 to 29999,
   in a block of 500 for each harness program that runs at once.

   They are below every port a system gives a server that asks for any:
   Linux starts at 32768. A port taken from the system and closed is free
   only until something asks the system again, and the process it is meant
   for binds it later, after it has read its configuration and signed in; a
   stand-in of another program could be given it in between.

   A program claims the first block whose lock file, in the system's
   temporary directory, no other program holds, and holds it until it
   exits: testo's workers are programs, so two running at once never share
   a block. *)

let first_port = 20_000
let per_block = 500
let blocks = 20

let block =
  lazy
    (let rec claim slot =
       if slot = blocks then
         failwith
           (Printf.sprintf
              "every one of the %d blocks of ports is held by another program"
              blocks)
       else
         let path =
           Filename.concat
             (Filename.get_temp_dir_name ())
             (Printf.sprintf "xynolog-conformance-ports-%d.lock" slot)
         in
         let fd = Unix.openfile path [ Unix.O_RDWR; Unix.O_CREAT ] 0o644 in
         match Unix.lockf fd Unix.F_TLOCK 0 with
         | () -> first_port + (slot * per_block)
         | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EACCES), _, _) ->
             Unix.close fd;
             claim (slot + 1)
     in
     claim 0)

let nothing_listens_on port =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () -> Unix.close socket) @@ fun () ->
  Unix.setsockopt socket Unix.SO_REUSEADDR true;
  match Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, port)) with
  | () -> true
  | exception Unix.Unix_error (Unix.EADDRINUSE, _, _) -> false

let handed_out = ref 0

(* A port for a process the harness is about to start: the next of this
   program's block that nothing is listening on. *)
let free_port () =
  let first = Lazy.force block in
  let rec next tried =
    if tried = per_block then
      failwith
        (Printf.sprintf "something is listening on every port from %d" first)
    else
      let port = first + (!handed_out mod per_block) in
      incr handed_out;
      if nothing_listens_on port then port else next (tried + 1)
  in
  next 0

let wait_for_tcp host port timeout =
  let address =
    `Tcp (Eio_unix.Net.Ipaddr.of_unix (Unix.inet_addr_of_string host), port)
  in
  Run.eventually (Printf.sprintf "something to listen on %s:%d" host port)
    timeout (fun () ->
      match
        Eio.Switch.run (fun sw ->
            ignore (Eio.Net.connect ~sw (Run.net ()) address))
      with
      | () -> Some ()
      | exception Eio.Io _ -> None)
