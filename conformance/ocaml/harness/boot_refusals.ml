(* What a service writes when it will not start on its document or its
   environment, as specs/src/service-environment/boot-refusals.json states
   it: a refusal, word for word, each placeholder filled as its row says.
   A placeholder whose value varies (a path the harness did not choose, a
   system's or a validator's reason) is held to be some text on its line;
   every other is filled from what the case gives. *)

open Xynolog_protocol
module Table = Tables.Service_environment_boot_refusals

let table () = (Protocol.tables ()).service_environment_boot_refusals

let refusal name =
  match
    List.find_opt
      (fun (r : Table.refusals_item) -> r.name = name)
      (table ()).refusals
  with
  | Some r -> r.refusal
  | None ->
      failwith
        ("service-environment/boot-refusals.json states no refusal " ^ name)

(* The line of [members] named [name]. *)
let member_line name =
  match
    List.find_opt
      (fun (m : Table.members_item) -> m.name = name)
      (table ()).members
  with
  | Some m -> m.line
  | None ->
      failwith
        ("service-environment/boot-refusals.json states no member line " ^ name)

let exit_code () = (table ()).exit_code
let placeholder = Re.Perl.compile_pat {|\{([A-Za-z]+)\}|}

(* [template] as a pattern: its text as it is, each placeholder [fill] gives
   as its value or [held] as a pattern, and any other as some text on one
   line. *)
let pattern_of ?(held = []) template fill =
  let rec pieces at =
    match Re.exec_opt ~pos:at placeholder template with
    | None -> [ Re.str (String.sub template at (String.length template - at)) ]
    | Some found ->
        let start = Re.Group.start found 0 and stop = Re.Group.stop found 0 in
        let name = Re.Group.get found 1 in
        Re.str (String.sub template at (start - at))
        :: (match (List.assoc_opt name fill, List.assoc_opt name held) with
        | Some value, _ -> Re.str value
        | None, Some pattern -> pattern
        | None, None -> Re.rep1 (Re.compl [ Re.char '\n' ]))
        :: pieces stop
  in
  Re.seq (pieces 0)

(* What [service] fills in every refusal: its own name and its document's
   schema. *)
let fill_of ~service ~schema fill =
  [
    ("service", Protocol.Service_rows.services_item_value service);
    ("schema", schema);
  ]
  @ fill

(* The refusal [name] as a pattern; [members], each a line of [members] by
   name and what fills it, are its {members}, one to a line. *)
let refusal_pattern ~service ~schema ~members name fill =
  let held =
    match members with
    | [] -> []
    | first :: rest ->
        let line (name, fill) = pattern_of (member_line name) fill in
        [
          ( "members",
            Re.seq
              (line first
              :: List.concat_map (fun m -> [ Re.char '\n'; line m ]) rest) );
        ]
  in
  pattern_of ~held (refusal name) (fill_of ~service ~schema fill)

(* The refusal [name] as [service] writes it, in its line. *)
let line_pattern ~service ~schema ~members name fill =
  pattern_of
    ~held:[ ("refusal", refusal_pattern ~service ~schema ~members name fill) ]
    (table ()).line []

let shown_fill fill =
  String.concat ", " (List.map (fun (k, v) -> Printf.sprintf "%s=%S" k v) fill)

(* Fail unless [output] holds the refusal [name] of [service]: the refusal
   alone, or, with [line], the line it is written in. *)
let holds ?(line = false) ?(members = []) ~service ~schema name fill output =
  let pattern =
    (if line then line_pattern else refusal_pattern)
      ~service ~schema ~members name fill
  in
  if not (Re.execp (Re.compile pattern) output) then
    failwith
      (Printf.sprintf "its output holds no %s %S%s, given %s:\n%s"
         (if line then "line " ^ (table ()).line ^ " of the refusal"
          else "refusal")
         (refusal name)
         (String.concat ""
            (List.map
               (fun (m, fill) ->
                 Printf.sprintf ", its members %S (%s)" (member_line m)
                   (shown_fill fill))
               members))
         (shown_fill (fill_of ~service ~schema fill))
         output)
