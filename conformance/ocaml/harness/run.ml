(* The one Eio environment a harness program runs in, and the switch that
   holds what lives as long as the program: a world, its servers and its
   processes. [main] opens both; everything else reads them here. *)

let state : (Eio_unix.Stdenv.base * Eio.Switch.t) option ref = ref None

let current () =
  match !state with
  | Some current -> current
  | None -> failwith "the harness is asked for its environment outside Run.main"

let env () = fst (current ())
let sw () = snd (current ())
let net () = Eio.Stdenv.net (env ())
let clock () = Eio.Stdenv.clock (env ())
let process_mgr () = Eio.Stdenv.process_mgr (env ())
let sleep seconds = Eio.Time.sleep (clock ()) seconds
let now () = Eio.Time.now (clock ())

let main program =
  Eio_main.run (fun env ->
      Eio.Switch.run (fun sw ->
          state := Some ((env :> Eio_unix.Stdenv.base), sw);
          program ()))

exception Timed_out of string

(* Poll [probe] until it answers, every 25 ms, or fail after [timeout]
   seconds naming [what]. *)
let eventually what timeout probe =
  let deadline = now () +. timeout in
  let rec poll () =
    match probe () with
    | Some value -> value
    | None ->
        if now () > deadline then
          raise
            (Timed_out
               (Printf.sprintf "timed out after %g s waiting for %s" timeout
                  what))
        else (
          sleep 0.025;
          poll ())
  in
  poll ()

let () =
  Printexc.register_printer (function Timed_out what -> Some what | _ -> None)
