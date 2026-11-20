(* What the archivist files share: a file's cases run against an Archivist
   world of their own, and the parts of what a case sends and reads. *)

open Xynolog_conformance
module A = Archivist_world

(* The end of a case on [w]: the streams it opened and the responses it read
   held to carrying nothing the spec does not allow. *)
let ended w body =
  World.end_case
    ~account:(fun () -> "Archivist output:\n" ^ String.concat "\n" (A.output w))
    ~settle:(fun () -> A.drain w)
    body

(* [cases] against one Archivist world, under [title]. The world's options
   are [A.create]'s. *)
let with_archivist (implementations : Implementations.t) ?git_sync ?engines
    ?staging ?before ?env ?git_tripwire title cases =
  let made =
    Suite.world ("archivist/" ^ title)
      ~make:(fun () ->
        A.create ?git_sync ?engines ?staging ?before ?env ?git_tripwire
          ~gateway:implementations.gateway ~command:implementations.archivist ())
      ~close:A.close
  in
  List.map
    (fun (c : A.t Case.t) ->
      Case.test ~category:[ title ] c (fun () ->
          let w = made () in
          ended w (fun () -> c.run w)))
    cases

(* An Archivist world of a case's own, made with [A.create]'s options and
   closed when [run] ends. *)
let own_world (implementations : Implementations.t) ?git_sync ?engines ?staging
    ?before ?env ?git_tripwire run =
  let w =
    A.create ?git_sync ?engines ?staging ?before ?env ?git_tripwire
      ~gateway:implementations.gateway ~command:implementations.archivist ()
  in
  Fun.protect ~finally:(fun () -> A.close w) @@ fun () ->
  ended w (fun () -> run w)

let str s = `String s
let sha256 = A.sha256

(* Whether [actual] holds what [partial] states, as toMatchObject holds it:
   each member an object states held in turn, a list's items held one for
   one, as many as it has, and anything else equal. *)
let rec alike (partial : Yojson.Safe.t) (actual : Yojson.Safe.t) =
  match (partial, actual) with
  | `Assoc members, `Assoc _ ->
      List.for_all
        (fun (key, value) ->
          match Protocol.at actual [ key ] with
          | Some held -> alike value held
          | None -> false)
        members
  | `List items, `List held ->
      List.length items = List.length held && List.for_all2 alike items held
  | _ -> Check.canonical partial = Check.canonical actual

let like what partial actual =
  if not (alike partial actual) then
    failwith
      (Printf.sprintf "%s: %s does not hold %s" what (Check.shown actual)
         (Check.shown partial))

(* A text annotation's parts, as mark:create-request takes them. *)
let highlight resource_id exact start =
  `Assoc
    [
      ("motivation", str "highlighting");
      ( "target",
        `Assoc
          [
            ("source", str resource_id);
            ( "selector",
              `List
                [
                  `Assoc
                    [
                      ("type", str "TextPositionSelector");
                      ("start", `Int start);
                      ("end", `Int (start + List.length (A.utf16_units exact)));
                    ];
                  `Assoc
                    [ ("type", str "TextQuoteSelector"); ("exact", str exact) ];
                ] );
          ] );
    ]

(* How many code points [text] holds. *)
let code_points text =
  let rec count at n =
    if at >= String.length text then n
    else
      count
        (at + Uchar.utf_decode_length (String.get_utf_8_uchar text at))
        (n + 1)
  in
  count 0 0

(* An anchored-text entry's body for [text], one word per line of the page.
   A word's offsets count code points. *)
let anchored_entry text =
  let words = String.split_on_char ' ' text in
  let _, lines =
    List.fold_left
      (fun (offset, lines) (index, word) ->
        let points = code_points word in
        ( offset + points + 1,
          `Assoc
            [
              ("p", `Int 1);
              ("y", `Int (700 - (index * 14)));
              ("h", `Int 12);
              ( "words",
                `List
                  [
                    `List
                      [
                        `Int 72;
                        `Int (points * 6);
                        `Int offset;
                        `Int (offset + points);
                      ];
                  ] );
            ]
          :: lines ))
      (0, [])
      (List.mapi (fun i w -> (i, w)) words)
  in
  `Assoc
    [
      ("text", str text);
      ("lines", `List (List.rev lines));
      ("method", str "ocr");
    ]

(* [template], each [{hole}] filled from [fill]. *)
let filled template fill = Protocol.fill_template template fill
let messages () = (Protocol.tables ()).archivist_messages

module M = Xynolog_protocol.Tables.Archivist_messages

(* The refusal [name] of archivist/messages.json, filled: [channel] is the
   operation refused. *)
let refusal ?(channel = "") name fill =
  match
    List.find_opt
      (fun (r : M.refusals_item) -> r.name = name)
      (messages ()).refusals
  with
  | Some r -> filled r.message (("channel", channel) :: fill)
  | None -> failwith ("archivist/messages.json states no refusal " ^ name)

(* The error [name] a route answers with, filled, and its code. *)
let http_error name fill =
  match
    List.find_opt (fun (r : M.http_item) -> r.name = name) (messages ()).http
  with
  | Some r -> (r.status, filled r.error fill, r.code)
  | None -> failwith ("archivist/messages.json states no route error " ^ name)

(* The body an error [name] is answered with: its error, and its code. *)
let error_body name fill =
  let _, error, code = http_error name fill in
  `Assoc
    (("error", str error)
    :: (match code with Some c -> [ ("code", str c) ] | None -> []))

(* Fail unless [output] holds the Archivist's own boot refusal [name] in
   service-environment/boot-refusals.json's line, each hole [fill] gives
   filled and any other some text on its line. *)
let holds_boot_refusal name fill output =
  let template =
    match
      List.find_opt (fun (r : M.boot_item) -> r.name = name) (messages ()).boot
    with
    | Some r -> r.refusal
    | None -> failwith ("archivist/messages.json states no boot refusal " ^ name)
  in
  let pattern =
    Boot_refusals.pattern_of
      ~held:[ ("refusal", Boot_refusals.pattern_of template fill) ]
      (Boot_refusals.table ()).line []
  in
  if not (Re.execp (Re.compile pattern) output) then
    failwith
      (Printf.sprintf
         "its output holds no line %s of the refusal %S, given %s:\n%s"
         (Boot_refusals.table ()).line template
         (String.concat ", "
            (List.map (fun (k, v) -> Printf.sprintf "%s=%S" k v) fill))
         output)

(* The first [n] entity types of the vocabulary an Archivist seeds at boot
   (archivist/default-entity-types.json), in its order. *)
let entity_types n =
  let seeded =
    (Protocol.tables ()).archivist_default_entity_types.entity_types
  in
  if List.length seeded < n then
    failwith
      (Printf.sprintf
         "archivist/default-entity-types.json seeds %d entity types, and a \
          case needs %d"
         (List.length seeded) n);
  List.filteri (fun at _ -> at < n) seeded
