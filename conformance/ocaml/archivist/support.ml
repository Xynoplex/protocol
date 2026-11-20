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

let refusal_item name =
  match
    List.find_opt
      (fun (r : M.refusals_item) -> r.name = name)
      (messages ()).refusals
  with
  | Some r -> r
  | None -> failwith ("archivist/messages.json states no refusal " ^ name)

(* The refusal [name] of archivist/messages.json, filled: [channel] is the
   operation refused. *)
let refusal ?(channel = "") name fill =
  filled (refusal_item name).message (("channel", channel) :: fill)

(* The payload of the failure the refusal [name] is answered with: its
   message, filled, and its code where it carries one. *)
let refusal_body ?channel name fill =
  `Assoc
    (("message", str (refusal ?channel name fill))
    ::
    (match (refusal_item name).code with
    | Some c -> [ ("code", str (M.command_error_code_value c)) ]
    | None -> []))

(* The message [name] of archivist/messages.json's [unsent], unfilled. *)
let unsent name =
  match
    List.find_opt
      (fun (u : M.unsent_item) -> u.name = name)
      (messages ()).unsent
  with
  | Some u -> u.message
  | None -> failwith ("archivist/messages.json states no unsent " ^ name)

let http_row name =
  match
    List.find_opt (fun (r : M.http_item) -> r.name = name) (messages ()).http
  with
  | Some r -> r
  | None -> failwith ("archivist/messages.json states no route error " ^ name)

(* The error [name] a route answers with, filled, and its code. *)
let http_error name fill =
  let r = http_row name in
  (r.status, filled r.error fill, r.code)

(* The status a route answers the error [name] with. *)
let http_status name = (http_row name).status

(* The body an error [name] is answered with: its error, and its code. *)
let error_body name fill =
  let _, error, code = http_error name fill in
  `Assoc
    (("error", str error)
    :: (match code with Some c -> [ ("code", str c) ] | None -> []))

(* The message [name] archivist/messages.json says the Archivist logs. *)
let logged name =
  match
    List.find_opt
      (fun (r : M.logged_item) -> r.name = name)
      (messages ()).logged
  with
  | Some r -> r
  | None -> failwith ("archivist/messages.json states no logged message " ^ name)

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

(* Whether [actual] holds what [expected] states, as toMatchObject holds it:
   an object's members in part and in depth, an array's items one for one,
   anything else equal. *)
let rec holds (expected : Yojson.Safe.t) (actual : Yojson.Safe.t) =
  match (expected, actual) with
  | `Assoc members, `Assoc _ ->
      List.for_all
        (fun (key, value) ->
          match Protocol.at actual [ key ] with
          | Some held -> holds value held
          | None -> false)
        members
  | `List items, `List held ->
      List.length items = List.length held && List.for_all2 holds items held
  | _ -> Check.canonical expected = Check.canonical actual

let matching what expected actual =
  if not (holds expected actual) then
    failwith
      (Printf.sprintf "%s: %s does not hold %s" what (Check.shown actual)
         (Check.shown expected))

let lacks what json path =
  Check.that
    (Printf.sprintf "%s: %s holds %s" what (Check.shown json)
       (String.concat "." path))
    (Protocol.at json path = None)

(* A job id as the dispatcher mints one at admission (JOBS.md § The job
   record), held to JobId: the suite stands in for the admission. *)
let minted_job_id () =
  let id = "job-" ^ Fresh.hex 16 in
  (match Protocol.against "JobId" (`String id) with
  | [] -> ()
  | refusals ->
      failwith
        (Printf.sprintf "the suite's job id %s is no JobId: %s" id
           (String.concat "; " refusals)));
  id

(* The one value Annotation.json allows its [member]. *)
let annotation_constant member =
  match
    Option.bind (Protocol.json_of "components/schemas/Annotation.json")
      (fun schema -> Protocol.at schema [ "properties"; member; "enum" ])
  with
  | Some (`List [ value ]) -> value
  | _ -> failwith ("Annotation.json allows its " ^ member ^ " no one value")

(* Where [exact] starts in [text], in UTF-16 code units. *)
let offset_in text exact =
  match Re.exec_opt (Re.compile (Re.str exact)) text with
  | Some found ->
      List.length (A.utf16_units (String.sub text 0 (Re.Group.start found 0)))
  | None -> failwith (Printf.sprintf "%S holds no %S" text exact)

(* An annotation as a worker or an importer sends one whole, on [exact] in
   [text], with [extra]'s members over its own. *)
let whole ?(extra = []) text resource_id exact =
  let start = offset_in text exact in
  let own =
    [
      ("@context", annotation_constant "@context");
      ("type", annotation_constant "type");
      ("id", str (String.concat "" (String.split_on_char '-' (Fresh.uuid ()))));
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
      ("created", str "2026-01-01T00:00:00.000Z");
      ("modified", str "2026-01-01T00:00:00.000Z");
    ]
  in
  `Assoc
    (List.filter (fun (key, _) -> not (List.mem_assoc key extra)) own @ extra)

let id_of annotation = A.string_member "id" annotation
