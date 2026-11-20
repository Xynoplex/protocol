(* What the gateway files share: a file's cases run against a world on each
   signal plane, and what a case reads of a reply. *)

open Xynolog_conformance

type 'w case = {
  name : string;
  timeout : float;
  solo : string option;
  run : 'w -> unit;
}

(* A case: [timeout] is its own bound, in seconds, as vitest's; its world's
   start is given a bound of its own on top. [solo], the reason, runs it in
   order with the run's other solo cases, in one process, before any other:
   for a case judged on what the cases before it did. *)
let case ?(timeout = 60.0) ?solo name run = { name; timeout; solo; run }

(* The world a file's cases share on [plane], unless a case changes it. *)
let plain implementations plane =
  World.create ~command:implementations.Implementations.gateway plane

(* [cases] against a world on each of [planes], under [title] and the plane,
   as eachPlane names them. [make] makes what a file's cases share, [world]
   is the gateway's world within it and [close] ends it. After every case,
   the streams it opened must have carried nothing the spec does not allow,
   and the gateway must have asked the stand-in Archivist only what its spec
   declares. *)
let each_plane ?(planes = Protocol.Service_rows.gateway_config_type_all) ~make
    ~world ~close title cases =
  List.concat_map
    (fun plane ->
      let word = Protocol.Service_rows.gateway_config_type_value plane in
      let category = [ Printf.sprintf "%s (%s plane)" title word ] in
      let made =
        Suite.world
          (String.concat "/" ("gateway" :: category))
          ~make:(fun () -> make plane)
          ~close
      in
      List.map
        (fun c ->
          Testo.create ~category ~max_duration:(c.timeout +. 60.0) ?solo:c.solo
            c.name (fun () ->
              let shared = made () in
              let w : World.t = world shared in
              World.end_case
                ~account:(fun () ->
                  "gateway output:\n"
                  ^ String.concat "\n" (Process.output w.gateway.process))
                ~settle:(fun () -> World.drain w)
                (fun () -> c.run shared)))
        cases)
    planes

(* A reply to browse:resource-requested: resource "r", named [name]. *)
let described ?(name = "r") () =
  `Assoc
    [
      ( "response",
        `Assoc
          [
            ( "resource",
              `Assoc
                [
                  ("@context", `String "https://schema.org/");
                  ("@id", `String "r");
                  ("name", `String name);
                  ("representations", `List []);
                ] );
            ("lastSequence", `Int 1);
          ] );
    ]

(* How long the gateway waits before it fetches the issuer's keys again. *)
let key_refetch_cooldown () =
  float_of_int (Protocol.scheme_limit "bearerAuth" "keyRefetchCooldownSeconds")

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

(* A reply's header [name], when it carries one. *)
let header name (reply : Call.reply) = Http.Header.get reply.headers name

(* A reply's body member [name], which must be a string. *)
let text_member name (reply : Call.reply) =
  match Option.bind reply.json (fun json -> Protocol.at json [ name ]) with
  | Some (`String s) -> s
  | _ ->
      failwith
        (Printf.sprintf "the reply holds no %s: %s" name (Call.shown reply.text))
