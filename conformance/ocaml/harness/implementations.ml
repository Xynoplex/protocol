(* The implementations a run is held to, read from the JSON file it names:
   [--implementations <manifest>] on the program's command line. There is
   no default. The file is the TypeScript harness's, of the shape
   harness/implementations.ts gives, and is decoded as strictly as the
   reader decodes Protocol: an unknown member, a missing one and a wrong type
   are each refused.

   Every list of strings in it is the argument list a suite starts a process
   with. An argument that starts with ./ or ../ is a path, resolved against
   the file's own directory. *)

type weaver = { command : string list; health_port : int }

type t = {
  gateway : string list;
  archivist : string list;
  weavers : (string * weaver) list;
  smelters : (string * string list) list;
  librarians : (string * string list) list;
  model_catalogue : string option;
}

let decode base =
  let resolved value =
    if
      String.starts_with ~prefix:"./" value
      || String.starts_with ~prefix:"../" value
    then Filename.concat base value
    else value
  in
  let command = Decode.list (fun cursor -> resolved (Decode.string cursor)) in
  Decode.obj (fun o ->
      let gateway = Decode.required o "gateway" command in
      let _dispatcher = Decode.required o "dispatcher" command in
      let archivist = Decode.required o "archivist" command in
      let _sdks = Decode.required o "sdks" Decode.json in
      let _workers = Decode.required o "workerServices" (Decode.map command) in
      let weavers =
        Decode.required o "weaverServices"
          (Decode.map
             (Decode.obj (fun w ->
                  {
                    command = Decode.required w "command" command;
                    health_port = Decode.required w "healthPort" Decode.int;
                  })))
      in
      let smelters = Decode.required o "smelterServices" (Decode.map command) in
      let librarians =
        Decode.required o "librarianServices" (Decode.map command)
      in
      let model_catalogue =
        Decode.optional o "modelCatalogue" (fun cursor ->
            resolved (Decode.string cursor))
      in
      { gateway; archivist; weavers; smelters; librarians; model_catalogue })

let read file =
  let absolute =
    if Filename.is_relative file then Filename.concat (Sys.getcwd ()) file
    else file
  in
  match
    Decode.parse_file
      ~root:(Filename.dirname absolute)
      (Filename.basename absolute)
  with
  | Error error -> failwith (Decode.error_to_string error)
  | Ok json -> (
      match Decode.run ~file json (decode (Filename.dirname absolute)) with
      | Ok (implementations, _) -> implementations
      | Error error ->
          failwith
            ("The implementations file is not of the shape \
              harness/implementations.ts gives: "
            ^ Decode.error_to_string error))
