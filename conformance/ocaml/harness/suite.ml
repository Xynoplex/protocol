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

(* testo's command line [argv], run inside [Run.main]. [workers] is how many
   workers a run starts unless [argv] says otherwise. *)
let main ~project ~workers ~argv tests =
  let code = ref 0 in
  Run.main (fun () ->
      at_exit close_current;
      Testo.interpret_argv ~argv ~project_name:project
        ~default_workers:(Some workers)
        ~handle_subcommand_result:(fun exit_code _ ->
          code := exit_code;
          (* Only the program testo was started as gets here: a worker is
             killed, or exits, first. *)
          close_current ();
          Process.reap ())
        tests;
      close_current ());
  exit !code

(* A suite of the conformance program: its name on the command line, how
   many workers it runs, and its cases, made for the implementations a run
   is held to. *)
type t = {
  name : string;
  workers : int;
  tests : Implementations.t -> Testo.t list;
}

let project = "xynolog-conformance"

(* xynolog-conformance <suite> --implementations <manifest> [testo's own
   arguments]. testo starts each worker as the program it was started as,
   with the arguments it was handed and --worker, so the suite and the
   manifest are handed to it as its -e options, which reach every worker
   and the cases. *)
let run suites =
  let names = String.concat ", " (List.map (fun suite -> suite.name) suites) in
  let named name =
    match List.find_opt (fun suite -> suite.name = name) suites with
    | Some suite -> suite
    | None ->
        prerr_endline
          (Printf.sprintf "xynolog-conformance has no suite %s: it has %s" name
             names);
        exit 2
  in
  let argv =
    match Array.to_list Sys.argv with
    | program :: name :: "--implementations" :: manifest :: rest
      when not (List.mem "--worker" rest) ->
        ignore (named name : t);
        Array.of_list
          ((program :: rest)
          @ [ "-e"; "suite=" ^ name; "-e"; "implementations=" ^ manifest ])
    | _ :: rest when List.mem "--worker" rest -> Sys.argv
    | _ ->
        prerr_endline
          (Printf.sprintf
             "usage: xynolog-conformance <suite> --implementations <manifest> \
              [testo's arguments], the suite one of %s"
             names);
        exit 2
  in
  let rec handed = function
    | "-e" :: setting :: _ when String.starts_with ~prefix:"suite=" setting ->
        String.sub setting 6 (String.length setting - 6)
    | _ :: rest -> handed rest
    | [] -> failwith "testo was handed no suite"
  in
  let suite = named (handed (Array.to_list argv)) in
  main ~project ~workers:suite.workers ~argv (fun env ->
      match List.assoc_opt "implementations" env with
      | Some manifest -> suite.tests (Implementations.read manifest)
      | None -> failwith "testo was handed no implementations manifest")
