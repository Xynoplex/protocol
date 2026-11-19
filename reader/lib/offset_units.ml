(* What an offset into a text counts. A text offset counts Unicode code points
   from the start of the text, and so does every length worked out from one:
   the W3C Web Annotation rule, and the same number in every language.
   text/offset-cases.json holds the counting itself, and each implementation
   runs it. This holds the wording: a member that carries an offset or a length
   of text and does not say its unit leaves a reader to assume their own
   language's, and a JavaScript string, a Rust [str] and a Python [str] each
   count differently.

   So each member below says "code points" in its description: they carry an
   offset or a length of text on the wire, in the record, in a store or in what
   a client hands its caller. Any other description of a component schema, a
   client type or a service's schema (one at the top of an area that is no
   table's) that speaks of an offset, of characters or of code units says it
   too, so a member added beside them is held whatever it is called. And each
   case table below, whose cases count offsets or lengths of text, has a
   character outside the Basic Multilingual Plane: only such a character tells
   a count of code points from one of UTF-16 code units or of bytes. *)

let members =
  [
    ("components/schemas/TextPositionSelector.json", "/properties/start");
    ("components/schemas/TextPositionSelector.json", "/properties/end");
    ("components/schemas/PdfTextItem.json", "/properties/start");
    ("components/schemas/PdfTextItem.json", "/properties/end");
    ( "archivist/AnchoredTextExtractedEntry.json",
      "/properties/lines/items/properties/words" );
    ("components/schemas/UnitCursor.json", "/properties/next");
    ( "components/schemas/GatherAnnotationOptions.json",
      "/properties/contextWindow" );
    ("client/types/TextSpan.json", "/$defs/Members/allOf/0/properties/start");
    ("client/types/TextSpan.json", "/$defs/Members/allOf/0/properties/end");
  ]

let counting_tables =
  [
    "text/offset-cases.json";
    "text/chunk-cases.json";
    "annotations/reconcile-cases.json";
    "annotations/pdf-locate-cases.json";
    "annotations/builder-cases.json";
    "annotations/id-cases.json";
    "worker/chunk-plan-cases.json";
    "worker/parser-cases.json";
    "worker/citation-cases.json";
    "librarian/window-cases.json";
  ]

let says_the_unit = Re.compile Re.(no_case (str "code point"))

let speaks_of_a_count_of_text =
  let whole w = Re.(seq [ bow; str w; opt (char 's'); eow ]) in
  Re.compile
    Re.(
      no_case
        (alt
           [
             whole "offset";
             whole "character";
             whole "char";
             str "utf-16";
             whole "code unit";
           ]))

(* The first [n] code points of [text]. *)
let opening n text =
  let rec cut i count =
    if i >= String.length text || count = n then i
    else
      let length = Uchar.utf_decode_length (String.get_utf_8_uchar text i) in
      cut (i + length) (count + 1)
  in
  String.sub text 0 (cut 0 0)

(* Whether [text] holds a character outside the Basic Multilingual Plane:
   UTF-8 writes one, and only one, in four bytes. *)
let outside_the_bmp text = String.exists (fun c -> Char.code c >= 0xF0) text

(* The schemas the sweep reads: each file of components/schemas, client/types
   and the top of each area, named in PascalCase, that is no table's schema. *)
let swept ~areas ~table_schemas files =
  let directories =
    Places.components :: "client/types/"
    :: List.map (fun area -> area ^ "/") areas
  in
  List.filter
    (fun file ->
      let name = Filename.basename file in
      List.mem (Filename.dirname file ^ "/") directories
      && Filename.check_suffix name ".json"
      && name.[0] >= 'A'
      && name.[0] <= 'Z'
      && not (List.mem file table_schemas))
    files

let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token

let rec sweep file pointer (json : Yojson.Safe.t) =
  match json with
  | `List items ->
      List.concat
        (List.mapi
           (fun index item -> sweep file (pointer / string_of_int index) item)
           items)
  | `Assoc fields ->
      let own =
        match List.assoc_opt "description" fields with
        | Some (`String description)
          when Re.execp speaks_of_a_count_of_text description
               && (not (Re.execp says_the_unit description))
               && not (List.mem (file, pointer) members) ->
            [
              {
                Decode.file;
                pointer;
                message =
                  Printf.sprintf
                    "speaks of an offset or of characters and does not say \
                     \"code points\": %S"
                    (opening 90 description);
              };
            ]
        | _ -> []
      in
      own
      @ List.concat_map
          (fun (name, member) -> sweep file (pointer / name) member)
          fields
  | _ -> []

let refusals ~root ~read_json files =
  let table_schemas = List.map Places.schema_of (Places.tables root) in
  let listed =
    List.concat_map
      (fun (file, pointer) ->
        let refusal message = [ { Decode.file; pointer; message } ] in
        match Option.bind (read_json file) (Decode.at_pointer pointer) with
        | None ->
            refusal
              "is listed as carrying an offset or a length of text, and is not \
               there"
        | Some member -> (
            match Decode.at_pointer "/description" member with
            | Some (`String description) when Re.execp says_the_unit description
              ->
                []
            | _ ->
                refusal
                  "carries an offset or a length of text, and its description \
                   does not say that it counts code points"))
      members
  in
  let swept =
    List.concat_map
      (fun file -> Option.fold ~none:[] ~some:(sweep file "") (read_json file))
      (swept ~areas:(Places.areas root) ~table_schemas files)
  in
  let counted =
    List.filter_map
      (fun table ->
        let refusal message =
          Some { Decode.file = table; pointer = ""; message }
        in
        let path = Filename.concat root table in
        if not (Sys.file_exists path) then
          refusal
            "is listed as counting offsets or lengths of text, and is not there"
        else if
          outside_the_bmp (In_channel.with_open_bin path In_channel.input_all)
        then None
        else
          refusal
            "counts offsets or lengths of text and has no character outside \
             the Basic Multilingual Plane to tell code points from UTF-16 code \
             units or bytes")
      counting_tables
  in
  listed @ swept @ counted
