(* What the gateway files share: the world a file's cases share, and what a
   case reads of a reply. *)

open Xynolog_conformance

(* The world a file's cases share on [plane], unless a case changes it. *)
let plain implementations plane =
  World.create ~command:implementations.Implementations.gateway plane

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
