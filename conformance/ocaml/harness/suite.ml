(* A suite's program: testo's command line, run inside [Run.main], so every
   case runs in the program's Eio fiber. testo runs cases in workers it
   starts, one case at a time each, and stops a case that outlives its
   bound by killing its worker.

   A file's world is made by the first of its cases a worker runs and closed
   when that worker runs a case of another file, or exits. A worker exits by
   calling [exit] from inside its loop, still in the fiber, so what it made
   is closed there too. *)

let closing : (string * (unit -> unit)) option ref = ref None

(* Closing runs even where the case that asked for it was cancelled: what a
   world started is stopped either way. *)
let close_current () =
  match !closing with
  | None -> ()
  | Some (_, close) ->
      closing := None;
      Eio.Cancel.protect close

(* The world of the file [name]: made once, the first time a case of the file
   asks for it, after the world of any other file is closed. *)
let world name ~make ~close =
  let made = ref None in
  fun () ->
    match (!made, !closing) with
    | Some world, Some (current, _) when current = name -> world
    | _ ->
        close_current ();
        Process.reap_orphans ();
        let world = make () in
        made := Some world;
        closing :=
          Some
            ( name,
              fun () ->
                made := None;
                close world );
        world

(* [workers] is how many workers a run starts unless its command line says
   otherwise. *)
let main ~project ~workers tests =
  let code = ref 0 in
  Run.main (fun () ->
      at_exit close_current;
      Testo.interpret_argv ~project_name:project ~default_workers:(Some workers)
        ~handle_subcommand_result:(fun exit_code _ ->
          code := exit_code;
          (* Only the program testo was started as gets here: a worker is
             killed, or exits, first. *)
          close_current ();
          Process.reap ())
        tests;
      close_current ());
  exit !code
