(* What bus/registry.json holds that its schema cannot state (see
   contract/EVENT-BUS.md for the rules).

   Its lists: each channel is declared once and named only where declared;
   each crossing channel declares its kind and audience, and each emittable
   one its effect, with no default for any.

   Its prose, which every SDK's generator lays out for its own language, so the
   registry states it in no language's shape: a text is one line of words,
   with no line break, no tab, no space at either end or two together (no
   indent, no alignment), no comment or list marker and no [{@link}]; and a
   break neither leads a listing (the first channel of each of channelOrder's
   lists, the first operation) nor follows another. *)

module R = Tables.Bus_registry

let file = "bus/registry.json"
let indexed list = List.mapi (fun i x -> (i, x)) list

let name : R.channels_item -> string = function
  | Schema { channel; _ }
  | Envelope { channel; _ }
  | Stored_event { channel; _ }
  | Void { channel; _ }
  | Empty { channel; _ } ->
      channel

let lists (reg : R.t) =
  let problems = ref [] in
  let refuse pointer fmt =
    Printf.ksprintf
      (fun message ->
        problems := { Decode.file; pointer; message } :: !problems)
      fmt
  in
  let declared = Hashtbl.create 256 in
  List.iter
    (fun (i, (c : R.channels_item)) ->
      if Hashtbl.mem declared (name c) then
        refuse
          (Printf.sprintf "/channels/%d" i)
          "channel %S is declared more than once" (name c)
      else Hashtbl.replace declared (name c) c)
    (indexed reg.channels);
  let known pointer ch =
    if not (Hashtbl.mem declared ch) then
      refuse pointer "names %S, which is not declared in channels[]" ch
  in
  let known_list pointer list =
    List.iteri (fun i ch -> known (Printf.sprintf "%s/%d" pointer i) ch) list
  in
  known_list "/channelOrder/eventMap" reg.channel_order.event_map;
  known_list "/channelOrder/schemas" reg.channel_order.schemas;
  let requests =
    List.map (fun (o : R.operations_item) -> o.request) reg.operations
  in
  let replies =
    List.concat_map
      (fun (o : R.operations_item) -> [ o.result; o.failure ])
      reg.operations
  in
  let owners = Hashtbl.create 64 in
  List.iter
    (fun (i, (o : R.operations_item)) ->
      let at member = Printf.sprintf "/operations/%d/%s" i member in
      known (at "request") o.request;
      known (at "result") o.result;
      known (at "failure") o.failure;
      (match Hashtbl.find_opt declared o.request with
      | Some (R.Schema { validate = Some _; _ }) | None -> ()
      | Some _ ->
          refuse (at "request")
            "%S is an operation's request and names no validate schema: \
             /bus/emit could never accept it, so Bus.request could never send \
             it"
            o.request);
      if List.mem o.request replies then
        refuse (at "request") "%S is both a request and a reply" o.request;
      List.iter
        (fun (role, ch) ->
          match Hashtbl.find_opt owners ch with
          | Some owner ->
              refuse (at role)
                "%S is the %s of both %s and %s: a reply channel is one \
                 operation's"
                ch role owner o.request
          | None -> Hashtbl.replace owners ch o.request)
        [ ("result", o.result); ("failure", o.failure) ])
    (indexed reg.operations);
  let operation_channel ch = List.mem ch requests || List.mem ch replies in
  (* One owner per channel across an axis's values, which name only declared
     channels, each once. *)
  let axis name values =
    let owner = Hashtbl.create 64 in
    List.iter
      (fun (value, list) ->
        let pointer = Printf.sprintf "/%s/%s" name value in
        known_list pointer list;
        List.iteri
          (fun i ch ->
            let at = Printf.sprintf "%s/%d" pointer i in
            if
              List.exists (String.equal ch)
                (List.filteri (fun j _ -> j < i) list)
            then refuse at "%s.%s lists %S more than once" name value ch
            else
              match Hashtbl.find_opt owner ch with
              | Some prior ->
                  refuse at
                    "%S is declared both %s.%s and %s.%s: a channel crosses \
                     exactly one way"
                    ch name prior name value
              | None -> Hashtbl.replace owner ch value)
          list)
      values;
    owner
  in
  let kind_of =
    axis "kind" [ ("command", reg.kind.command); ("event", reg.kind.event) ]
  in
  let audience_of =
    axis "audience"
      [
        ("everyone", reg.audience.everyone);
        ("scoped", reg.audience.scoped);
        ("declared", reg.audience.declared);
      ]
  in
  known_list "/inProcess/channels" reg.in_process.channels;
  let in_process = reg.in_process.channels in
  List.iter
    (fun ch ->
      if operation_channel ch then (
        Option.iter
          (refuse "/kind"
             "%S belongs to an operation, so its kind is derived: remove it \
              from kind.%s"
             ch)
          (Hashtbl.find_opt kind_of ch);
        Option.iter
          (refuse "/audience"
             "%S belongs to an operation, so its audience is derived: remove \
              it from audience.%s"
             ch)
          (Hashtbl.find_opt audience_of ch));
      if List.mem ch in_process then (
        Option.iter
          (refuse "/inProcess/channels"
             "%S is declared inProcess and given audience.%s: inProcess means \
              it never crosses"
             ch)
          (Hashtbl.find_opt audience_of ch);
        Option.iter
          (refuse "/inProcess/channels"
             "%S is declared inProcess and given kind.%s: inProcess means it \
              never crosses"
             ch)
          (Hashtbl.find_opt kind_of ch)))
    (List.map name reg.channels);
  List.iter
    (fun (i, (c : R.channels_item)) ->
      let at = Printf.sprintf "/channels/%d" i in
      let channel = name c in
      if not (List.mem channel in_process || operation_channel channel) then (
        if not (Hashtbl.mem kind_of channel) then
          refuse at
            "channel %S declares no kind: add it to kind.command or \
             kind.event, or to inProcess if it never crosses. There is no \
             default."
            channel;
        if not (Hashtbl.mem audience_of channel) then
          refuse at
            "channel %S declares no audience: add it to audience.everyone, \
             audience.scoped or audience.declared. There is no default."
            channel);
      (match c with
      | Stored_event { event; _ } when event <> channel ->
          refuse at
            "%S is a stored event named %S: a persisted event is published on \
             the channel of its own type"
            channel event
      | Schema { schema; validate = Some validate; _ } when validate <> schema
        ->
          refuse at
            "%S carries %s and validates against %s: one channel, one schema"
            channel schema validate
      | Envelope { validate = Some validate; _ }
      | Stored_event { validate = Some validate; _ }
      | Void { validate = Some validate; _ }
      | Empty { validate = Some validate; _ } ->
          (* The schema's description says so, and Schema has no null
             literal to state it with. *)
          refuse at
            "%S validates against %s and carries no schema: only a channel \
             that carries one is validated against it"
            channel validate
      | _ -> ());
      match (Hashtbl.find_opt audience_of channel, c) with
      | Some "scoped", (Schema _ | Envelope _ | Void _ | Empty _) ->
          refuse at
            "%S has audience scoped and is no stored event: a resource's scope \
             carries the events of its record"
            channel
      | _ -> ())
    (indexed reg.channels);
  (* Who emits a channel: whether that emit changes the knowledge base is
     declared, since the gateway reads it to decide whether an emit was an
     act. *)
  let emittable = requests @ reg.kind.command in
  let effect_of = Hashtbl.create 64 in
  List.iter
    (fun (side, list) ->
      let pointer = "/effect/" ^ side in
      known_list pointer list;
      List.iteri
        (fun i ch ->
          let at = Printf.sprintf "%s/%d" pointer i in
          if
            List.exists (String.equal ch) (List.filteri (fun j _ -> j < i) list)
          then refuse at "effect.%s lists %S more than once" side ch
          else (
            (match Hashtbl.find_opt effect_of ch with
            | Some prior ->
                refuse at
                  "%S is declared both effect.%s and effect.%s: an emit either \
                   changes the knowledge base or it does not"
                  ch prior side
            | None -> Hashtbl.replace effect_of ch side);
            if not (List.mem ch emittable) then
              refuse at
                "effect.%s names %S, which nobody emits: the domain is an \
                 operation's request or a kind.command"
                side ch))
        list)
    [ ("writes", reg.effect_.writes); ("reads", reg.effect_.reads) ];
  List.iter
    (fun ch ->
      if not (Hashtbl.mem effect_of ch) then
        refuse "/effect"
          "emittable channel %S declares no effect: add it to effect.writes or \
           effect.reads. There is no default."
          ch)
    emittable;
  Hashtbl.iter
    (fun ch kind ->
      if kind = "command" && Hashtbl.find_opt audience_of ch = Some "everyone"
      then
        refuse "/kind/command"
          "%S is kind command with audience everyone: a directive is not a \
           broadcast"
          ch)
    kind_of;
  (* A directive, or a declaration, of a channel the TypeScript EventMap does
     not carry is drift. *)
  List.iter
    (fun (pointer, list) ->
      List.iter
        (fun ch ->
          if not (List.mem ch reg.channel_order.event_map) then
            refuse pointer "%S is classified but not in channelOrder.eventMap"
              ch)
        list)
    [
      ("/kind/command", reg.kind.command);
      ("/audience/declared", reg.audience.declared);
      ("/inProcess/channels", reg.in_process.channels);
    ];
  let field = reg.trace.field in
  let word_char c =
    match c with
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true
    | _ -> false
  in
  if
    not
      (String.length field >= 2
      && field.[0] = '_'
      && (match field.[1] with 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false)
      && String.for_all word_char field)
  then
    refuse "/trace/field"
      "is %S: it is a payload field the bus writes, so a name that begins with \
       \"_\" and a letter"
      field;
  List.rev !problems

let markup =
  Re.compile (Re.Perl.re {|^(//|/\*|\*|#|--|- |[0-9]+\. )|\*/|\{@link|})

let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token

let text at words =
  let refuse message = { Decode.file; pointer = at; message } in
  List.filter_map Fun.id
    [
      (if
         String.exists
           (function '\n' | '\r' | '\t' -> true | _ -> false)
           words
       then
         Some
           (refuse "breaks a line or holds a tab; a writer lays the text out")
       else None);
      (if
         String.trim words <> words || Re.execp (Re.compile (Re.str "  ")) words
       then
         Some
           (refuse
              "has a space at an end or two together: an indent or an alignment")
       else None);
      (if Re.execp markup words then
         Some (refuse "holds comment or list markup; a writer writes its own")
       else None);
    ]

let prose at (paragraphs : R.event_name_item list) =
  List.concat_map
    (fun (i, (paragraph : R.event_name_item)) ->
      match paragraph with
      | String words -> text (at / string_of_int i) words
      | Object { items } ->
          List.concat_map
            (fun (j, item) ->
              text (at / string_of_int i / "items" / string_of_int j) item)
            (indexed items))
    (indexed paragraphs)

let docs at ~first (docs : R.docs) =
  let trailing =
    if docs.trailing = "" then [] else text (at / "trailing") docs.trailing
  in
  let refuse pointer message = { Decode.file; pointer; message } in
  trailing
  @ List.concat_map
      (fun (i, (block : R.lead_item)) ->
        let where = at / "lead" / string_of_int i in
        match block with
        | String ->
            (if i = 0 && first then
               [ refuse where "breaks the listing before its first entry" ]
             else [])
            @
            if i > 0 && List.nth docs.lead (i - 1) = R.String then
              [ refuse where "breaks the listing where it is already broken" ]
            else []
        | Section { section } -> text (where / "section") section
        | Comment { comment } -> prose (where / "comment") comment
        | Doc { doc } -> prose (where / "doc") doc)
      (indexed docs.lead)

let channel_docs : R.channels_item -> R.docs * R.docs = function
  | Schema { docs; schema_docs; _ }
  | Envelope { docs; schema_docs; _ }
  | Stored_event { docs; schema_docs; _ }
  | Void { docs; schema_docs; _ }
  | Empty { docs; schema_docs; _ } ->
      (docs, schema_docs)

(* The registry's prose. Its [docs] member is read from [json], each of its
   members whatever its name. *)
let prose_refusals (reg : R.t) (json : Yojson.Safe.t) =
  let first_of list =
    match list with
    | [] -> None
    | first :: _ ->
        List.find_map
          (fun (i, c) -> if name c = first then Some i else None)
          (indexed reg.channels)
  in
  let first_in_event_map = first_of reg.channel_order.event_map in
  let first_in_schemas = first_of reg.channel_order.schemas in
  let channels =
    List.concat_map
      (fun (i, c) ->
        let at = "" / "channels" / string_of_int i in
        let own, schema = channel_docs c in
        docs (at / "docs") ~first:(first_in_event_map = Some i) own
        @ docs (at / "schemaDocs") ~first:(first_in_schemas = Some i) schema)
      (indexed reg.channels)
  in
  let operations =
    List.concat_map
      (fun (i, (o : R.operations_item)) ->
        docs
          ("" / "operations" / string_of_int i / "docs")
          ~first:(i = 0) o.docs)
      (indexed reg.operations)
  in
  let named =
    match Decode.at_pointer "/docs" json with
    | Some (`Assoc members) ->
        List.concat_map
          (fun (member, value) ->
            match Decode.run ~file value (Decode.list R.event_name_item) with
            | Ok (paragraphs, _) -> prose ("" / "docs" / member) paragraphs
            | Error error -> [ error ])
          members
    | _ -> []
  in
  let holders =
    prose "/audience/doc" reg.audience.doc
    @ prose "/effect/doc" reg.effect_.doc
    @ prose "/kind/doc" reg.kind.doc
    @ prose "/inProcess/doc" reg.in_process.doc
  in
  channels @ operations @ named @ holders

let refusals ~read_json (reg : R.t) =
  lists reg @ Option.fold ~none:[] ~some:(prose_refusals reg) (read_json file)
