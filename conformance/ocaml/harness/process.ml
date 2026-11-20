(* The processes a suite starts: a command, its arguments and an
   environment, with every line it writes kept, stdout and stderr
   interleaved, and stopped as every launcher stops one: SIGTERM, then
   SIGKILL if it has not exited 15 seconds later. *)

type t = {
  name : string;
  output : string Dynarray.t;
  stdout : string Dynarray.t;
  stderr : string Dynarray.t;
  drained : unit Eio.Promise.t;
      (** Resolved once both its pipes are read to their end. *)
  said : Eio.Condition.t;  (** Broadcast on each line, and when it exits. *)
  input : Eio_unix.sink_ty Eio.Resource.t option Eio.Promise.t;
      (** Its stdin, where it was started with one to read. *)
  process : Eio_unix.Process.ty Eio.Resource.t Eio.Promise.t;
  exited : int option Eio.Promise.t;
      (** Its exit code, once it has exited; None when a signal ended it. *)
}

(* What a process is given of the harness's own environment, beside what its
   service's table lists: the PATH it is found on, and, when a run measures
   coverage, what an instrumented binary or a JVM driver is measured with. *)
let run_environment () =
  ("PATH", Sys.getenv "PATH")
  :: List.filter_map
       (fun name ->
         Option.map (fun value -> (name, value)) (Sys.getenv_opt name))
       [ "LLVM_PROFILE_FILE"; "JAVA_OPTS" ]

let lines_of source into said =
  let input = Eio.Buf_read.of_flow ~max_size:max_int source in
  let rec next () =
    match Eio.Buf_read.line input with
    | line ->
        if line <> "" then (
          List.iter (fun list -> Dynarray.add_last list line) into;
          Eio.Condition.broadcast said);
        next ()
    | exception End_of_file -> ()
  in
  next ()

(* testo runs cases in worker processes, and SIGKILLs a worker whose case
   outlives its bound, and each worker once its queue is empty: a worker
   killed never closes the world it made, and what that world started
   outlives it. So every process a suite starts leads a process group of its
   own, recorded in the run's register beside the program that started it. A
   world reaps the groups of every program that has exited before it starts
   ([reap_orphans]), and the program testo was started as reaps every group
   when the run ends ([reap]). *)
let register =
  lazy
    (let master =
       if Array.mem "--worker" Sys.argv then Unix.getppid () else Unix.getpid ()
     in
     Filename.concat
       (Filename.get_temp_dir_name ())
       (Printf.sprintf "xynolog-conformance-groups-%d" master))

let record group =
  Out_channel.with_open_gen [ Open_append; Open_creat; Open_wronly ]
    0o600 (Lazy.force register) (fun out ->
      Printf.fprintf out "%d %d\n" group (Unix.getpid ()))

let signal_group group number =
  match Unix.kill (-group) number with
  | () -> true
  | exception Unix.Unix_error (ESRCH, _, _) -> false

let running program =
  match Unix.kill program 0 with
  | () -> true
  | exception Unix.Unix_error (ESRCH, _, _) -> false

(* Each group the run recorded, with the program that started it. *)
let recorded () =
  let file = Lazy.force register in
  if not (Sys.file_exists file) then []
  else
    List.filter_map
      (fun line ->
        match String.split_on_char ' ' line with
        | [ "" ] -> None
        | [ group; program ] -> Some (int_of_string group, int_of_string program)
        | _ -> failwith ("the register holds a line of no group: " ^ line))
      (String.split_on_char '\n'
         (In_channel.with_open_bin file In_channel.input_all))

(* SIGTERM each group, and SIGKILL what is left of them 15 seconds later. *)
let stop groups =
  let alive =
    List.filter (fun group -> signal_group group Sys.sigterm) groups
  in
  let deadline = Run.now () +. 15.0 in
  let rec wait alive =
    match List.filter (fun group -> signal_group group 0) alive with
    | [] -> ()
    | left when Run.now () > deadline ->
        List.iter
          (fun group -> ignore (signal_group group Sys.sigkill : bool))
          left
    | left ->
        Run.sleep 0.1;
        wait left
  in
  wait alive

(* Stop the groups of every program that has exited: a worker testo
   killed. *)
let reap_orphans () =
  stop
    (List.filter_map
       (fun (group, program) -> if running program then None else Some group)
       (recorded ()))

(* Stop every group the run recorded. *)
let reap () =
  stop (List.map fst (recorded ()));
  let file = Lazy.force register in
  if Sys.file_exists file then Sys.remove file

(* Start [command] with [env]. Its stdin is a pipe closed at once, unless
   [input]: then the suite writes to it ([write]) and closes it
   ([close_input]). *)
let spawn ?(input = false) name command env =
  let output = Dynarray.create ()
  and stdout = Dynarray.create ()
  and stderr = Dynarray.create () in
  let said = Eio.Condition.create () in
  let drained, drain = Eio.Promise.create () in
  let started, start = Eio.Promise.create () in
  let given, give = Eio.Promise.create () in
  let exited, exit = Eio.Promise.create () in
  let mgr = Run.process_mgr () in
  Eio.Fiber.fork ~sw:(Run.sw ()) (fun () ->
      Eio.Switch.run @@ fun sw ->
      let fd flow = Option.get (Eio_unix.Resource.fd_opt flow) in
      (* A child never reads the worker's own stdin, which is testo's. *)
      let in_r, in_w = Eio_unix.pipe sw in
      let out_r, out_w = Eio_unix.pipe sw in
      let err_r, err_w = Eio_unix.pipe sw in
      let child =
        Eio_unix.Process.spawn_unix ~sw mgr ~pgid:0
          ~fds:
            [
              (0, fd in_r, `Blocking);
              (1, fd out_w, `Blocking);
              (2, fd err_w, `Blocking);
            ]
          ~env:(Array.of_list (List.map (fun (k, v) -> k ^ "=" ^ v) env))
          command
      in
      record (Eio.Process.pid child);
      Eio.Resource.close in_r;
      if input then
        Eio.Promise.resolve give
          (Some (in_w :> Eio_unix.sink_ty Eio.Resource.t))
      else (
        Eio.Resource.close in_w;
        Eio.Promise.resolve give None);
      Eio.Resource.close out_w;
      Eio.Resource.close err_w;
      Eio.Promise.resolve start (child :> Eio_unix.Process.ty Eio.Resource.t);
      Eio.Fiber.all
        [
          (fun () ->
            Eio.Fiber.both
              (fun () -> lines_of out_r [ output; stdout ] said)
              (fun () -> lines_of err_r [ output; stderr ] said);
            Eio.Promise.resolve drain ());
          (fun () ->
            Eio.Promise.resolve exit
              (match Eio.Process.await child with
              | `Exited code -> Some code
              | `Signaled _ -> None);
            Eio.Condition.broadcast said);
        ]);
  {
    name;
    output;
    stdout;
    stderr;
    drained;
    said;
    input = given;
    process = started;
    exited;
  }

let stdin t =
  match Eio.Promise.await t.input with
  | Some sink -> sink
  | None -> failwith (t.name ^ " was started with no input to write to")

(* Write [text] to its stdin. *)
let write t text = Eio.Flow.copy_string text (stdin t)

(* Close its stdin: it reads to the end of its input. *)
let close_input t = Eio.Resource.close (stdin t)
let running t = not (Eio.Promise.is_resolved t.exited)

let signal t number =
  if running t then Eio.Process.signal (Eio.Promise.await t.process) number

(* SIGTERM, and wait: its exit code. *)
let stop t =
  signal t Sys.sigterm;
  match
    Eio.Time.with_timeout (Run.clock ()) 15.0 (fun () ->
        Ok (Eio.Promise.await t.exited))
  with
  | Ok code -> code
  | Error `Timeout ->
      signal t Sys.sigkill;
      Eio.Promise.await t.exited

(* SIGKILL: nothing it would settle on the way out is settled. *)
let crash t =
  signal t Sys.sigkill;
  ignore (Eio.Promise.await t.exited : int option)

let output t = Dynarray.to_list t.output

let tail n t =
  let all = output t in
  List.filteri (fun at _ -> at >= List.length all - n) all

let exited_text t =
  match Eio.Promise.peek t.exited with
  | Some (Some code) -> Printf.sprintf "code %d" code
  | Some None -> "a signal"
  | None -> "nothing yet"

(* Wait until [ready] answers, every 50 ms: fail with its output when it exits
   first, or after [timeout] seconds, killing it. *)
let until_ready t ~timeout what ready =
  let deadline = Run.now () +. timeout in
  let rec poll () =
    if not (running t) then
      failwith
        (Printf.sprintf "the %s exited before %s (%s):\n%s" t.name what
           (exited_text t)
           (String.concat "\n" (output t)))
    else if ready () then ()
    else if Run.now () > deadline then (
      crash t;
      failwith
        (Printf.sprintf "the %s did not %s in time:\n%s" t.name what
           (String.concat "\n" (output t))))
    else (
      Run.sleep 0.05;
      poll ())
  in
  poll ()

(* Wait for [t], which must refuse to start, to exit without serving:
   [serving] says whether it answers where it would. One that serves, or has
   neither served nor exited after [timeout] seconds, is killed and is a
   failure. Its exit code (None when a signal ended it). *)
let refused t ~timeout serving =
  let deadline = Run.now () +. timeout in
  let rec watch () =
    if not (running t) then Eio.Promise.await t.exited
    else if serving () then (
      crash t;
      failwith
        (Printf.sprintf
           "the %s served although it should have refused to start:\n%s" t.name
           (String.concat "\n" (output t))))
    else if Run.now () > deadline then (
      crash t;
      failwith
        (Printf.sprintf
           "the %s had neither refused to start nor served after %g s:\n%s"
           t.name timeout
           (String.concat "\n" (output t))))
    else (
      Run.sleep 0.05;
      watch ())
  in
  watch ()
