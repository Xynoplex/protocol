(* What the checkout holds outside the model: how each JSON file of
   specs/src is spelled, and that every tracked file has an owner. The repository's check
   holds these; [Spec.load], which its consumers run on copies, does not. *)

let specs = "specs/src"
let inventory = "OWNERSHIP-INVENTORY.csv"

(* [json] as JavaScript's [JSON.stringify(json, null, 2)] spells what
   [JSON.parse] read: an object's keys like an array index first, in
   ascending order, then the others as written. Or why it cannot say: a
   number beyond what an integer or a plain decimal spells alike in both, and
   a key written twice. *)
let canonical (json : Yojson.Safe.t) =
  let out = Buffer.create 65536 in
  let string s =
    Buffer.add_char out '"';
    String.iter
      (fun c ->
        match c with
        | '"' -> Buffer.add_string out "\\\""
        | '\\' -> Buffer.add_string out "\\\\"
        | '\b' -> Buffer.add_string out "\\b"
        | '\012' -> Buffer.add_string out "\\f"
        | '\n' -> Buffer.add_string out "\\n"
        | '\r' -> Buffer.add_string out "\\r"
        | '\t' -> Buffer.add_string out "\\t"
        | c when Char.code c < 0x20 ->
            Buffer.add_string out (Printf.sprintf "\\u%04x" (Char.code c))
        | c -> Buffer.add_char out c)
      s;
    Buffer.add_char out '"'
  in
  (* An array index: a canonical number below 2{^32} - 1. *)
  let index key =
    if
      key = "0"
      || key <> ""
         && key.[0] >= '1'
         && key.[0] <= '9'
         && String.length key <= 10
         && String.for_all (fun c -> c >= '0' && c <= '9') key
    then
      let n = int_of_string key in
      if n < 4294967295 then Some n else None
    else None
  in
  let decimal f =
    let rec shortest p =
      let spelled = Printf.sprintf "%.*g" p f in
      if float_of_string spelled = f || p = 17 then spelled else shortest (p + 1)
    in
    let spelled = shortest 1 in
    if
      String.contains spelled 'e'
      || String.contains spelled 'n'
      || String.contains spelled 'i'
    then Error (Printf.sprintf "the number %s" spelled)
    else Ok spelled
  in
  let rec value depth (json : Yojson.Safe.t) =
    let indent d = Buffer.add_string out (String.make (2 * d) ' ') in
    match json with
    | `Null -> Ok (Buffer.add_string out "null")
    | `Bool b -> Ok (Buffer.add_string out (string_of_bool b))
    | `Int n -> Ok (Buffer.add_string out (string_of_int n))
    | `Intlit n -> Error ("the integer " ^ n)
    | `Float f -> Result.map (Buffer.add_string out) (decimal f)
    | `String s -> Ok (string s)
    | `List [] -> Ok (Buffer.add_string out "[]")
    | `Assoc [] -> Ok (Buffer.add_string out "{}")
    | `List items ->
        Buffer.add_string out "[\n";
        let rec each = function
          | [] -> Ok ()
          | item :: rest ->
              indent (depth + 1);
              Result.bind
                (value (depth + 1) item)
                (fun () ->
                  if rest <> [] then Buffer.add_string out ",\n";
                  each rest)
        in
        Result.map
          (fun () ->
            Buffer.add_char out '\n';
            indent depth;
            Buffer.add_char out ']')
          (each items)
    | `Assoc members -> (
        let keys = List.map fst members in
        match
          List.find_opt
            (fun k -> List.length (List.filter (( = ) k) keys) > 1)
            keys
        with
        | Some key -> Error (Printf.sprintf "the key %S, written twice" key)
        | None ->
            let indexed, named =
              List.partition
                (fun (key, _) -> Option.is_some (index key))
                members
            in
            let members =
              List.sort
                (fun (a, _) (b, _) -> compare (index a) (index b))
                indexed
              @ named
            in
            Buffer.add_string out "{\n";
            let rec each = function
              | [] -> Ok ()
              | (key, item) :: rest ->
                  indent (depth + 1);
                  string key;
                  Buffer.add_string out ": ";
                  Result.bind
                    (value (depth + 1) item)
                    (fun () ->
                      if rest <> [] then Buffer.add_string out ",\n";
                      each rest)
            in
            Result.map
              (fun () ->
                Buffer.add_char out '\n';
                indent depth;
                Buffer.add_char out '}')
              (each members))
  in
  Result.map (fun () -> Buffer.contents out ^ "\n") (value 0 json)

(* The JSON files under [dir], below [root]. *)
let rec json_files root dir =
  Sys.readdir (Filename.concat root dir)
  |> Array.to_list |> List.sort compare
  |> List.concat_map (fun name ->
      let path = dir ^ "/" ^ name in
      if Sys.is_directory (Filename.concat root path) then json_files root path
      else if Filename.check_suffix name ".json" then [ path ]
      else [])

(* Each JSON file of specs/src, under the repository's [root], is spelled as
   [JSON.stringify(value, null, 2)] and a newline write it: the tools that
   edit them spell some characters otherwise, and a one-line change made by
   another lands as a whole-file diff. *)
let spelling ~root =
  List.concat_map
    (fun file ->
      let refuse message = [ { Decode.file; pointer = ""; message } ] in
      let raw =
        In_channel.with_open_bin
          (Filename.concat root file)
          In_channel.input_all
      in
      match canonical (Yojson.Safe.from_string raw) with
      | Error what ->
          refuse
            (Printf.sprintf
               "holds %s, which this check cannot spell as JSON.stringify does"
               what)
      | Ok spelled when spelled = raw -> []
      | Ok _ ->
          refuse
            "is not spelled as JSON.stringify(value, null, 2) and a newline \
             write it: its content may be right, and its bytes (escapes, \
             indentation, the final newline) are not")
    (json_files root specs)

(* The rows of a CSV text, RFC 4180's quoting. *)
let rows text =
  let rows = ref [] and row = ref [] and field = Buffer.create 64 in
  let quoted = ref false in
  let n = String.length text in
  let end_field () =
    row := Buffer.contents field :: !row;
    Buffer.clear field
  in
  let end_row () =
    end_field ();
    let fields = List.rev !row in
    if List.exists (( <> ) "") fields then rows := fields :: !rows;
    row := []
  in
  let i = ref 0 in
  while !i < n do
    let c = text.[!i] in
    if !quoted then
      if c = '"' && !i + 1 < n && text.[!i + 1] = '"' then (
        Buffer.add_char field '"';
        incr i)
      else if c = '"' then quoted := false
      else Buffer.add_char field c
    else if c = '"' && Buffer.length field = 0 then quoted := true
    else if c = ',' then end_field ()
    else if c = '\n' then end_row ()
    else if c <> '\r' then Buffer.add_char field c;
    incr i
  done;
  if Buffer.length field > 0 || !row <> [] then end_row ();
  List.rev !rows

(* OWNERSHIP-INVENTORY.csv, under [root], classifies exactly the [tracked]
   files, each once, with a classification, an owner and notes. *)
let ownership ~root tracked =
  let refuse message = { Decode.file = inventory; pointer = ""; message } in
  match
    rows
      (In_channel.with_open_bin
         (Filename.concat root inventory)
         In_channel.input_all)
  with
  | [ "path"; "classification"; "owner"; "notes" ] :: entries ->
      let listed =
        List.filter_map (function path :: _ -> Some path | [] -> None) entries
      in
      let counted = Hashtbl.create 1024 in
      List.iter
        (fun path ->
          match Hashtbl.find_opt counted path with
          | Some times -> Hashtbl.replace counted path (times + 1)
          | None -> Hashtbl.replace counted path 1)
        listed;
      let tracked_set = Hashtbl.create 1024 in
      List.iter (fun path -> Hashtbl.replace tracked_set path ()) tracked;
      List.filter_map
        (fun path ->
          match Hashtbl.find_opt counted path with
          | Some times when times > 1 ->
              Hashtbl.replace counted path 1;
              Some (refuse (Printf.sprintf "lists %s %d times" path times))
          | _ -> None)
        listed
      @ List.filter_map
          (fun path ->
            if Hashtbl.mem counted path then None
            else Some (refuse ("does not classify the tracked file " ^ path)))
          tracked
      @ List.filter_map
          (fun path ->
            if Hashtbl.mem tracked_set path then None
            else
              Some
                (refuse ("classifies " ^ path ^ ", which Git does not track")))
          listed
      @ List.filter_map
          (fun entry ->
            match entry with
            | [ _; _; _; _ ] when List.for_all (( <> ) "") entry -> None
            | path :: _ -> Some (refuse ("has an incomplete row for " ^ path))
            | [] -> None)
          entries
  | _ ->
      [
        refuse "does not begin with the header path,classification,owner,notes";
      ]
