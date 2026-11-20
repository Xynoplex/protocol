(* The ports the harness hands to the processes it starts.

   A port taken from the system (listen on 0) and closed is free only until
   something asks the system for a port again, and the process it is meant
   for binds it some time later. So a port the harness hands out is none
   the system would give, and none another harness program running at the
   same time is handed. *)

open Xynolog_conformance
open Expect

(* The lowest port Linux gives a server that asks for any
   (ip_local_port_range); macOS and the IANA range start higher. *)
let lowest_the_system_gives = 32_768

let listening port =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
  Unix.listen socket 1;
  socket

let below_the_system () =
  let socket = listening 0 in
  let given =
    match Unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port
    | Unix.ADDR_UNIX _ -> failwith "a TCP socket has no port"
  in
  Unix.close socket;
  check
    (Printf.sprintf "the system gives %d, below %d" given
       lowest_the_system_gives)
    (given >= lowest_the_system_gives);
  for _ = 1 to 20 do
    let port = Net.free_port () in
    check
      (Printf.sprintf "it handed out %d, which the system gives" port)
      (port < lowest_the_system_gives)
  done

(* The first port of the block a program forked from this one claims. *)
let claimed_by_another () =
  let reading, writing = Unix.pipe () in
  match Unix.fork () with
  | 0 ->
      Unix.close reading;
      let said =
        match Net.claim_block () with
        | first -> string_of_int first
        | exception Failure why -> why
      in
      ignore (Unix.write_substring writing said 0 (String.length said));
      Unix._exit 0
  | child ->
      Unix.close writing;
      let said = In_channel.input_all (Unix.in_channel_of_descr reading) in
      (try ignore (Unix.waitpid [] child)
       with Unix.Unix_error (Unix.ECHILD, _, _) -> ());
      int_of_string_opt said
      |> Option.to_result ~none:("the other program claimed none: " ^ said)

let in_its_block () =
  let first = Lazy.force Net.block in
  let past_the_last = Net.first_port + (Net.blocks * Net.per_block) in
  check "every block is below every port the system gives"
    (past_the_last <= lowest_the_system_gives);
  check
    (Printf.sprintf "%d begins a block" first)
    (first >= Net.first_port && first < past_the_last
    && (first - Net.first_port) mod Net.per_block = 0);
  for _ = 1 to 20 do
    let port = Net.free_port () in
    check
      (Printf.sprintf "%d is in the block from %d" port first)
      (port >= first && port < first + Net.per_block)
  done;
  match claimed_by_another () with
  | Ok theirs ->
      check
        (Printf.sprintf "another program claims another block, not %d" theirs)
        (theirs <> first)
  | Error why -> failwith why

let never_a_listened_port () =
  let held = Net.free_port () in
  let holder = listening held in
  Fun.protect ~finally:(fun () -> Unix.close holder) @@ fun () ->
  (* More than a whole block, so the count comes round to the held one. *)
  for _ = 1 to Net.per_block + 10 do
    check
      (Printf.sprintf "it handed out %d, which something listens on" held)
      (Net.free_port () <> held)
  done

let tests =
  List.map
    (fun (name, case) ->
      Testo.create
        ~category:[ "a port for a process the harness starts" ]
        name case)
    [
      ( "is none the system gives a server that asks for any port",
        below_the_system );
      ( "is in the block of the program that asks, and no two programs share a \
         block",
        in_its_block );
      ( "is never one something is listening on, however many are asked for",
        never_a_listened_port );
    ]
