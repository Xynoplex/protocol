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

(* How an SDK's live driver is started, and the tier of the live corpus the
   SDK is held to: fleet, what every live layer does, or parity, all of
   CACHE-SEMANTICS. *)
type live = {
  live_command : string list;
  tier : Xynolog_protocol.Tables.Conformance_case.tier;
}

(* An SDK's drivers, the programs the SDK and worker suites talk to in its
   place (conformance/sdk/README.md, "The driver protocol"): how its wire
   driver is started, its live driver, its worker driver when it has a
   worker's surface, and the wire cases its driver cannot be put through,
   each with why. The suite holds an exemption as it holds a case: the
   driver must answer unsupported to what the case asks. *)
type sdk = {
  wire : string list option;
  live : live option;
  worker : string list option;
  exempt : (string * string) list option;
}

type t = {
  gateway : string list;
  dispatcher : string list;
  archivist : string list;
  sdks : (string * sdk) list;
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
      let dispatcher = Decode.required o "dispatcher" command in
      let archivist = Decode.required o "archivist" command in
      let sdks =
        Decode.required o "sdks"
          (Decode.map
             (Decode.obj (fun d ->
                  {
                    wire = Decode.optional d "wire" command;
                    live =
                      Decode.optional d "live"
                        (Decode.obj (fun l ->
                             {
                               live_command =
                                 Decode.required l "command" command;
                               tier =
                                 Decode.required l "tier"
                                   Xynolog_protocol.Tables.Conformance_case.tier;
                             }));
                    worker = Decode.optional d "worker" command;
                    exempt =
                      Decode.optional d "exempt" (Decode.map Decode.string);
                  })))
      in
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
      {
        gateway;
        dispatcher;
        archivist;
        sdks;
        weavers;
        smelters;
        librarians;
        model_catalogue;
      })

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
