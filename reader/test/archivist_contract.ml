(* archivist_contract.exe ROOT: contract/ARCHIVIST.md, under Protocol's ROOT,
   names what the Archivist keeps and answers, and a copy of it that leaves
   one of them out is refused. *)

let root = Sys.argv.(1)

let registry =
  match Spec.load (Filename.concat root "specs/src") with
  | Ok spec -> spec.tables.bus_registry
  | Error _ -> failwith "Protocol's specs/src does not load"

let prose =
  In_channel.with_open_bin
    (Filename.concat root Archivist_rules.protocol)
    In_channel.input_all

let messages errors = List.map Decode.error_to_string errors

(* A root whose ARCHIVIST.md is [text], beside Protocol's own Archivist
   API and tables. *)
let rooted text =
  let copied = Filename.temp_dir "archivist" "" in
  List.iter
    (fun dir -> Sys.mkdir (Filename.concat copied dir) 0o755)
    [ "contract"; "specs"; "specs/src"; "specs/src/archivist" ];
  let write path text =
    Out_channel.with_open_bin (Filename.concat copied path) (fun out ->
        Out_channel.output_string out text)
  in
  Option.iter (write Archivist_rules.protocol) text;
  List.iter
    (fun file ->
      write file
        (In_channel.with_open_bin
           (Filename.concat root file)
           In_channel.input_all))
    ("specs/src/archivist/openapi.json"
    :: List.filter_map
         (fun table ->
           if String.starts_with ~prefix:"archivist/" table then
             Some ("specs/src/" ^ table)
           else None)
         (Places.tables (Filename.concat root "specs/src")));
  copied

let without term = Re.replace_string (Re.compile (Re.str term)) ~by:"" prose

let refused ~why text registry expected =
  let found =
    messages (Archivist_rules.contract ~root:(rooted text) registry)
  in
  if not (List.mem ("contract/ARCHIVIST.md: /: " ^ expected) found) then (
    Printf.eprintf
      "An ARCHIVIST.md that %s was not refused with:\n  %s\nbut with:\n" why
      expected;
    List.iter (Printf.eprintf "  %s\n") found;
    exit 1)

let persisted =
  List.find_map
    (fun (c : Tables.Bus_registry.channels_item) ->
      match c with Stored_event { channel; _ } -> Some channel | _ -> None)
    registry.channels
  |> Option.get

let () =
  (match messages (Archivist_rules.contract ~root registry) with
  | [] -> ()
  | found ->
      prerr_endline "Protocol's ARCHIVIST.md is refused:";
      List.iter prerr_endline found;
      exit 1);
  refused ~why:"is missing" None registry "is missing";
  refused ~why:"leaves out a record schema"
    (Some (without "ResourceAnnotations"))
    registry "does not name the schema ResourceAnnotations";
  refused ~why:"leaves out the configuration"
    (Some (without "ArchivistConfig"))
    registry "does not name the schema ArchivistConfig";
  refused ~why:"leaves out the shard table"
    (Some (without "shard-cases.json"))
    registry "does not name the table shard-cases.json";
  refused ~why:"leaves out the layout table"
    (Some (without "layout.json"))
    registry "does not name the table layout.json";
  refused ~why:"leaves out a persisted event"
    (Some (without ("`" ^ persisted ^ "`")))
    registry
    ("does not name the persisted event `" ^ persisted ^ "`");
  refused ~why:"leaves out a route"
    (Some (without "`/health`"))
    registry "does not name the route `/health`";
  refused ~why:"is read against a registry with no persisted event" (Some prose)
    { registry with channels = [] }
    "is read against a bus registry that names no persisted event: the check \
     has lost what it reads";
  print_endline
    "ARCHIVIST.md names every record schema, the configuration, every table of \
     archivist/, every persisted event and every Archivist route, and a copy \
     that leaves one out is refused"
