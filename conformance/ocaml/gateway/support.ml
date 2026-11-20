(* What the gateway files share: a file's cases run against a world on each
   signal plane, and what a case reads of a reply. *)

open Xynolog_conformance

type case = { name : string; timeout : float; run : World.t -> unit }

(* A case: [timeout] is its own bound, in seconds, as vitest's; its world's
   start is given a bound of its own on top. *)
let case ?(timeout = 60.0) name run = { name; timeout; run }

(* The world a file's cases share on [plane], unless a case changes it. *)
let plain implementations plane =
  World.create ~command:implementations.Implementations.gateway plane

(* [cases] against a world on each of [planes], made by [make], under
   [title] and the plane, as eachPlane names them. After every case, the
   streams it opened must have carried nothing the spec does not allow, and
   the gateway must have asked the stand-in Archivist only what its spec
   declares. *)
let each_plane ?(planes = Protocol.Service_rows.gateway_config_type_all)
    ?(sub = []) ~make title cases =
  List.concat_map
    (fun plane ->
      let word = Protocol.Service_rows.gateway_config_type_value plane in
      let category = Printf.sprintf "%s (%s plane)" title word :: sub in
      let world =
        Suite.world
          (String.concat "/" ("gateway" :: category))
          ~make:(fun () -> make plane)
          ~close:World.close
      in
      List.map
        (fun c ->
          Testo.create ~category ~max_duration:(c.timeout +. 60.0) c.name
            (fun () ->
              let w = world () in
              World.end_case
                ~account:(fun () ->
                  "gateway output:\n"
                  ^ String.concat "\n" (Process.output w.gateway.process))
                ~settle:(fun () -> World.drain w)
                (fun () -> c.run w)))
        cases)
    planes

(* How [reply] is not what the gateway's spec declares for [meth] [path]. *)
let conforms meth path (reply : Call.reply) =
  match Call.non_conformance Gateway meth path reply with
  | [] -> ()
  | problems ->
      failwith
        (Printf.sprintf "%s %s answered %d outside its declaration: %s" meth
           path reply.status
           (String.concat "; " problems))

let status what expected (reply : Call.reply) =
  if reply.status <> expected then
    failwith
      (Printf.sprintf "%s: expected %d, received %d: %s" what expected
         reply.status (Call.shown reply.text))

(* A reply's body member [name], which must be a string. *)
let text_member name (reply : Call.reply) =
  match Option.bind reply.json (fun json -> Protocol.at json [ name ]) with
  | Some (`String s) -> s
  | _ ->
      failwith
        (Printf.sprintf "the reply holds no %s: %s" name (Call.shown reply.text))
