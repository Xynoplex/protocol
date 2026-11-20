(* The registry's rules refuse a registry that breaks one, naming the place:
   each case is a small registry every rule holds, changed. *)

let docs = `Assoc [ ("lead", `List []); ("trailing", `String "") ]
let prose = `List [ `String "Words." ]
let strings list = `List (List.map (fun s -> `String s) list)

let channel name members : Yojson.Safe.t =
  `Assoc
    ((("channel", `String name) :: members)
    @ [ ("docs", docs); ("schemaDocs", docs) ])

let carrying name schema =
  channel name
    [
      ("shape", `String "schema");
      ("schema", `String schema);
      ("validate", `String schema);
    ]

(* One operation, one event, one in-process channel: a registry every rule
   holds. *)
let base : Yojson.Safe.t =
  `Assoc
    [
      ("$comment", `String "c");
      ( "channelOrder",
        `Assoc
          [
            ( "eventMap",
              strings
                [
                  "demo:limits-requested";
                  "demo:limits-result";
                  "demo:limits-failed";
                  "demo:happened";
                  "demo:internal";
                ] );
            ("schemas", `List []);
          ] );
      ( "channels",
        `List
          [
            carrying "demo:limits-requested" "DemoRequest";
            carrying "demo:limits-result" "DemoResult";
            carrying "demo:limits-failed" "CommandError";
            carrying "demo:happened" "DemoEvent";
            carrying "demo:internal" "DemoInternal";
          ] );
      ( "operations",
        `List
          [
            `Assoc
              [
                ("request", `String "demo:limits-requested");
                ("result", `String "demo:limits-result");
                ("failure", `String "demo:limits-failed");
                ("answeredBy", `String "librarian");
                ("docs", docs);
              ];
          ] );
      ( "docs",
        `Assoc
          [
            ("eventName", prose);
            ("enrichedEvents", prose);
            ("channelSchemas", prose);
            ("emittableChannel", prose);
          ] );
      ( "inProcess",
        `Assoc [ ("doc", prose); ("channels", strings [ "demo:internal" ]) ] );
      ( "kind",
        `Assoc
          [
            ("doc", prose);
            ("command", `List []);
            ("event", strings [ "demo:happened" ]);
          ] );
      ( "audience",
        `Assoc
          [
            ("doc", prose);
            ("everyone", strings [ "demo:happened" ]);
            ("scoped", `List []);
            ("declared", `List []);
          ] );
      ( "effect",
        `Assoc
          [
            ("doc", prose);
            ("writes", strings [ "demo:limits-requested" ]);
            ("reads", `List []);
          ] );
      ( "trace",
        `Assoc
          [
            ("meaning", `String "Where a relayed frame carries its trace.");
            ("field", `String "_carried");
          ] );
      ( "stamps",
        `Assoc
          [
            ("meaning", `String "Who emitted a payload.");
            ("emitter", `String "_emitter");
            ("roles", `String "_held");
          ] );
    ]

(* [json] with the value at [path] replaced, or added as an object's last
   member. *)
let rec set path value (json : Yojson.Safe.t) : Yojson.Safe.t =
  match (path, json) with
  | [], _ -> value
  | [ key ], `Assoc members when not (List.mem_assoc key members) ->
      `Assoc (members @ [ (key, value) ])
  | key :: rest, `Assoc members ->
      `Assoc
        (List.map
           (fun (k, v) -> if k = key then (k, set rest value v) else (k, v))
           members)
  | index :: rest, `List items ->
      `List
        (List.mapi
           (fun i v -> if string_of_int i = index then set rest value v else v)
           items)
  | _ -> invalid_arg ("no place " ^ String.concat "/" path)

let rec get path (json : Yojson.Safe.t) =
  match (path, json) with
  | [], _ -> json
  | key :: rest, `Assoc members -> get rest (List.assoc key members)
  | _ -> invalid_arg ("no place " ^ String.concat "/" path)

let append path item json =
  match get path json with
  | `List items -> set path (`List (items @ [ item ])) json
  | _ -> invalid_arg ("not an array: " ^ String.concat "/" path)

let refusals json =
  match
    Decode.run ~file:Registry_rules.file json Tables.Bus_registry.decode
  with
  | Error error -> [ Decode.error_to_string error ]
  | Ok (registry, _) ->
      List.map Decode.error_to_string
        (Registry_rules.refusals ~read_json:(fun _ -> Some json) registry)

let refuses json expected () =
  let found = refusals json in
  if not (List.mem ("bus/registry.json: " ^ expected) found) then
    Alcotest.failf "expected the refusal\n  %s\nfound\n  %s" expected
      (String.concat "\n  " found)

let accepts json () =
  Alcotest.(check (list string)) "refusals" [] (refusals json)

(* The base registry with one more channel, in-process and in the EventMap. *)
let with_channel members =
  base
  |> append [ "channels" ] (channel "demo:extra" members)
  |> append [ "channelOrder"; "eventMap" ] (`String "demo:extra")
  |> append [ "inProcess"; "channels" ] (`String "demo:extra")

let case name test = Alcotest.test_case name `Quick test

let axes =
  [
    case "the base registry holds every rule" (accepts base);
    case "a crossing channel with no audience"
      (refuses
         (set [ "audience"; "everyone" ] (`List []) base)
         "/channels/3: channel \"demo:happened\" declares no audience: add it \
          to audience.everyone, audience.scoped or audience.declared. There is \
          no default.");
    case "a crossing channel with no kind"
      (refuses
         (set [ "kind"; "event" ] (`List []) base)
         "/channels/3: channel \"demo:happened\" declares no kind: add it to \
          kind.command or kind.event, or to inProcess if it never crosses. \
          There is no default.");
    case "a channel in two audiences"
      (refuses
         (set [ "audience"; "scoped" ] (strings [ "demo:happened" ]) base)
         "/audience/scoped/0: \"demo:happened\" is declared both \
          audience.everyone and audience.scoped: a channel crosses exactly one \
          way");
    case "a channel listed twice in one audience"
      (refuses
         (set [ "audience"; "everyone" ]
            (strings [ "demo:happened"; "demo:happened" ])
            base)
         "/audience/everyone/1: audience.everyone lists \"demo:happened\" more \
          than once");
    case "an in-process channel given an audience"
      (refuses
         (set [ "audience"; "declared" ] (strings [ "demo:internal" ]) base)
         "/inProcess/channels: \"demo:internal\" is declared inProcess and \
          given audience.declared: inProcess means it never crosses");
    case "an in-process channel given a kind"
      (refuses
         (append [ "kind"; "event" ] (`String "demo:internal") base)
         "/inProcess/channels: \"demo:internal\" is declared inProcess and \
          given kind.event: inProcess means it never crosses");
    case "an operation's channel given a kind"
      (refuses
         (append [ "kind"; "event" ] (`String "demo:limits-result") base)
         "/kind: \"demo:limits-result\" belongs to an operation, so its kind \
          is derived: remove it from kind.event");
    case "an operation's channel given an audience"
      (refuses
         (append [ "audience"; "everyone" ] (`String "demo:limits-result") base)
         "/audience: \"demo:limits-result\" belongs to an operation, so its \
          audience is derived: remove it from audience.everyone");
    case "a command sent to everyone"
      (refuses
         (base
         |> append [ "channels" ] (carrying "demo:do-it" "DemoCommand")
         |> append [ "channelOrder"; "eventMap" ] (`String "demo:do-it")
         |> set [ "kind"; "command" ] (strings [ "demo:do-it" ])
         |> append [ "audience"; "everyone" ] (`String "demo:do-it")
         |> append [ "effect"; "writes" ] (`String "demo:do-it"))
         "/kind/command: \"demo:do-it\" is kind command with audience \
          everyone: a directive is not a broadcast");
    case "an axis naming an undeclared channel"
      (refuses
         (set [ "audience"; "declared" ] (strings [ "demo:typo" ]) base)
         "/audience/declared/0: names \"demo:typo\", which is not declared in \
          channels[]");
    case "the channel order naming an undeclared channel"
      (refuses
         (append [ "channelOrder"; "schemas" ] (`String "demo:typo") base)
         "/channelOrder/schemas/0: names \"demo:typo\", which is not declared \
          in channels[]");
    case "a declaration of a channel the EventMap lacks"
      (refuses
         (set
            [ "channelOrder"; "eventMap" ]
            (strings
               [
                 "demo:limits-requested";
                 "demo:limits-result";
                 "demo:limits-failed";
                 "demo:happened";
               ])
            base)
         "/inProcess/channels: \"demo:internal\" is classified but not in \
          channelOrder.eventMap");
    case "an emittable channel with no effect"
      (refuses
         (set [ "effect"; "writes" ] (`List []) base)
         "/effect: emittable channel \"demo:limits-requested\" declares no \
          effect: add it to effect.writes or effect.reads. There is no \
          default.");
    case "a channel both written and read"
      (refuses
         (set [ "effect"; "reads" ] (strings [ "demo:limits-requested" ]) base)
         "/effect/reads/0: \"demo:limits-requested\" is declared both \
          effect.writes and effect.reads: an emit either changes the knowledge \
          base or it does not");
    case "an effect listing a channel twice"
      (refuses
         (set [ "effect"; "writes" ]
            (strings [ "demo:limits-requested"; "demo:limits-requested" ])
            base)
         "/effect/writes/1: effect.writes lists \"demo:limits-requested\" more \
          than once");
    case "an effect of a channel nobody emits"
      (refuses
         (set [ "effect"; "reads" ] (strings [ "demo:happened" ]) base)
         "/effect/reads/0: effect.reads names \"demo:happened\", which nobody \
          emits: the domain is an operation's request or a kind.command");
    case "a request that names no validate schema"
      (refuses
         (set [ "channels"; "0"; "validate" ] `Null base)
         "/operations/0/request: \"demo:limits-requested\" is an operation's \
          request and names no validate schema: /bus/emit could never accept \
          it, so Bus.request could never send it");
    case "a channel declared twice"
      (refuses
         (append [ "channels" ] (carrying "demo:happened" "DemoEvent") base)
         "/channels/5: channel \"demo:happened\" is declared more than once");
    case "an operation naming an undeclared channel"
      (refuses
         (set [ "operations"; "0"; "failure" ] (`String "demo:typo") base)
         "/operations/0/failure: names \"demo:typo\", which is not declared in \
          channels[]");
    case "a reply two operations share"
      (refuses
         (base
         |> append [ "channels" ] (carrying "demo:again" "DemoRequest")
         |> append [ "operations" ]
              (`Assoc
                 [
                   ("request", `String "demo:again");
                   ("result", `String "demo:limits-result");
                   ("failure", `String "demo:limits-failed");
                   ("answeredBy", `String "librarian");
                   ("docs", docs);
                 ]))
         "/operations/1/result: \"demo:limits-result\" is the result of both \
          demo:limits-requested and demo:again: a reply channel is one \
          operation's");
    case "a request that is a reply"
      (refuses
         (append [ "operations" ]
            (`Assoc
               [
                 ("request", `String "demo:limits-result");
                 ("result", `String "demo:happened");
                 ("failure", `String "demo:internal");
                 ("answeredBy", `String "librarian");
                 ("docs", docs);
               ])
            base)
         "/operations/1/request: \"demo:limits-result\" is both a request and \
          a reply");
  ]

let payloads =
  [
    case "a channel of each shape" (fun () ->
        List.iter
          (fun members -> accepts (with_channel members) ())
          [
            [
              ("shape", `String "schema");
              ("schema", `String "DemoExtra");
              ("validate", `Null);
            ];
            [
              ("shape", `String "envelope");
              ("schema", `String "DemoExtra");
              ("validate", `Null);
            ];
            [ ("shape", `String "void"); ("validate", `Null) ];
            [ ("shape", `String "empty"); ("validate", `Null) ];
            [
              ("shape", `String "storedEvent");
              ("event", `String "demo:extra");
              ("payload", `String "DemoPayload");
              ("validate", `Null);
              ("system", `Bool true);
              ("enriched", `Bool true);
            ];
          ]);
    case "a channel that carries one schema and validates against another"
      (refuses
         (with_channel
            [
              ("shape", `String "schema");
              ("schema", `String "DemoExtra");
              ("validate", `String "DemoOther");
            ])
         "/channels/5: \"demo:extra\" carries DemoExtra and validates against \
          DemoOther: one channel, one schema");
    case "a validate schema on a channel that carries none"
      (refuses
         (with_channel
            [ ("shape", `String "empty"); ("validate", `String "DemoExtra") ])
         "/channels/5: \"demo:extra\" validates against DemoExtra and carries \
          no schema: only a channel that carries one is validated against it");
    case "a stored event on a channel not its own type"
      (refuses
         (with_channel
            [
              ("shape", `String "storedEvent");
              ("event", `String "demo:other");
              ("payload", `String "DemoPayload");
              ("validate", `Null);
            ])
         "/channels/5: \"demo:extra\" is a stored event named \"demo:other\": \
          a persisted event is published on the channel of its own type");
    case "a scoped channel that is no stored event"
      (let scoped members =
         base
         |> append [ "channels" ] (channel "demo:scoped" members)
         |> append [ "kind"; "event" ] (`String "demo:scoped")
         |> set [ "audience"; "scoped" ] (strings [ "demo:scoped" ])
       in
       fun () ->
         accepts
           (scoped
              [
                ("shape", `String "storedEvent");
                ("event", `String "demo:scoped");
                ("payload", `String "DemoScopedPayload");
                ("validate", `Null);
              ])
           ();
         refuses
           (scoped
              [
                ("shape", `String "schema");
                ("schema", `String "DemoScoped");
                ("validate", `Null);
              ])
           "/channels/5: \"demo:scoped\" has audience scoped and is no stored \
            event: a resource's scope carries the events of its record"
           ());
  ]

let trace =
  [
    case "a field whose name does not begin with _"
      (refuses
         (set [ "trace"; "field" ] (`String "carried") base)
         "/trace/field: is \"carried\": it is a payload field the bus writes, \
          so a name that begins with \"_\" and a letter");
    case "a stamp whose name does not begin with _"
      (refuses
         (set [ "stamps"; "roles" ] (`String "held") base)
         "/stamps/roles: is \"held\": it is a payload field the bus writes, so \
          a name that begins with \"_\" and a letter");
  ]

let lead blocks = set [ "operations"; "0"; "docs"; "lead" ] (`List blocks) base

let text =
  let layout =
    "has a space at an end or two together: an indent or an alignment"
  in
  let markup = "holds comment or list markup; a writer writes its own" in
  [
    case "prose of paragraphs and items, and a lead of every block"
      (accepts
         (base
         |> set [ "kind"; "doc" ]
              (`List
                 [
                   `String "Words."; `Assoc [ ("items", strings [ "a"; "b" ]) ];
                 ])
         |> set
              [ "operations"; "0"; "docs"; "lead" ]
              (`List
                 [
                   `Assoc [ ("section", `String "Part") ];
                   `String "";
                   `Assoc [ ("comment", prose) ];
                   `Assoc [ ("doc", prose) ];
                 ])));
    case "a lead line as one language writes it"
      (refuses
         (lead [ `String "  // a comment" ])
         "/operations/0/docs/lead/0: expected \"\", found \"  // a comment\"");
    case "a text that breaks a line"
      (refuses
         (set [ "kind"; "doc" ] (strings [ "One.\nTwo." ]) base)
         "/kind/doc/0: breaks a line or holds a tab; a writer lays the text out");
    case "a text that holds a tab"
      (refuses
         (set [ "effect"; "doc" ] (strings [ "One.\tTwo." ]) base)
         "/effect/doc/0: breaks a line or holds a tab; a writer lays the text \
          out");
    case "an indented item"
      (refuses
         (set [ "audience"; "doc" ]
            (`List [ `Assoc [ ("items", strings [ " indented" ]) ] ])
            base)
         ("/audience/doc/0/items/0: " ^ layout));
    case "an aligned text"
      (refuses
         (set [ "docs"; "eventName" ] (strings [ "a  b" ]) base)
         ("/docs/eventName/0: " ^ layout));
    case "a trailing note with a space at its end"
      (refuses
         (set [ "operations"; "0"; "docs"; "trailing" ] (`String "Note. ") base)
         ("/operations/0/docs/trailing: " ^ layout));
    case "a section in a comment's markup"
      (refuses
         (lead [ `Assoc [ ("section", `String "// Part") ] ])
         ("/operations/0/docs/lead/0/section: " ^ markup));
    case "a comment's text as a list item"
      (refuses
         (lead [ `Assoc [ ("comment", strings [ "- one" ]) ] ])
         ("/operations/0/docs/lead/0/comment/0: " ^ markup));
    case "a numbered item"
      (refuses
         (lead [ `Assoc [ ("doc", strings [ "1. first" ]) ] ])
         ("/operations/0/docs/lead/0/doc/0: " ^ markup));
    case "a text that closes a block comment"
      (refuses
         (set [ "inProcess"; "doc" ] (strings [ "Ends here */" ]) base)
         ("/inProcess/doc/0: " ^ markup));
    case "a text that links as one language does"
      (refuses
         (set
            [ "docs"; "channelSchemas" ]
            (strings [ "See {@link Bus}." ])
            base)
         ("/docs/channelSchemas/0: " ^ markup));
    case "a break that leads the operations"
      (refuses
         (lead [ `String "" ])
         "/operations/0/docs/lead/0: breaks the listing before its first entry");
    case "a break that leads the EventMap"
      (refuses
         (set [ "channels"; "0"; "docs"; "lead" ] (`List [ `String "" ]) base)
         "/channels/0/docs/lead/0: breaks the listing before its first entry");
    case "a break that follows a break"
      (refuses
         (set
            [ "channels"; "1"; "docs"; "lead" ]
            (`List [ `String ""; `String "" ])
            base)
         "/channels/1/docs/lead/1: breaks the listing where it is already \
          broken");
  ]

let () =
  Alcotest.run "registry_rules"
    [ ("axes", axes); ("payloads", payloads); ("trace", trace); ("text", text) ]
