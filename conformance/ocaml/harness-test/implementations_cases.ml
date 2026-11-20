(* The implementations manifest: how a run names what it tests, without
   editing the kit. A file anywhere is read, its paths resolved against its
   own directory, and anything else refused before a suite starts. *)

open Xynolog_conformance
open Expect

let ( // ) = Filename.concat

(* A file of [content] in a directory of its own, and that directory. *)
let written content =
  let directory = Filename.temp_dir "implementations-" "" in
  let file = directory // "mine.json" in
  Out_channel.with_open_bin file (fun out ->
      Out_channel.output_string out content);
  (file, directory)

let command parts = `List (List.map (fun part -> `String part) parts)

(* A complete file's members, naming one build of each under [prefix]. *)
let complete prefix =
  [
    ("gateway", command [ prefix ^ "gateway" ]);
    ("dispatcher", command [ prefix ^ "dispatcher" ]);
    ("archivist", command [ prefix ^ "archivist" ]);
    ( "sdks",
      `Assoc
        [
          ( "mine",
            `Assoc
              [
                ("wire", command [ "node"; prefix ^ "wire.js" ]);
                ( "live",
                  `Assoc
                    [
                      ("command", command [ prefix ^ "live" ]);
                      ("tier", `String "fleet");
                    ] );
                ("exempt", `Assoc [ ("attach-gate", `String "no attach") ]);
              ] );
        ] );
    ("workerServices", `Assoc [ ("mine", command [ prefix ^ "worker"; "-P" ]) ]);
    ( "weaverServices",
      `Assoc
        [
          ( "mine",
            `Assoc
              [
                ("command", command [ prefix ^ "weaver" ]);
                ("healthPort", `Int 24999);
              ] );
        ] );
    ("smelterServices", `Assoc []);
    ("librarianServices", `Assoc []);
    ("modelCatalogue", `String (prefix ^ "models.json"));
  ]

let text members = Yojson.Safe.to_string (`Assoc members)

let resolves () =
  let file, directory = written (text (complete "./bin/")) in
  let read = Implementations.read file in
  let here path = directory // path in
  check "a ./ argument is resolved against the file's own directory"
    (read.gateway = [ here "./bin/gateway" ]);
  let mine = List.assoc "mine" read.sdks in
  check "and any other is passed as written"
    (mine.wire = Some [ "node"; here "./bin/wire.js" ]);
  check "a live driver's command is resolved too"
    (Option.map
       (fun (live : Implementations.live) -> live.live_command)
       mine.live
    = Some [ here "./bin/live" ]);
  check "an SDK's exemptions are read with why"
    (mine.exempt = Some [ ("attach-gate", "no attach") ]);
  check "a worker service's command, its flag as written"
    (read.workers = [ ("mine", [ here "./bin/worker"; "-P" ]) ]);
  check "a weaver's command and health port"
    (read.weavers
    = [ ("mine", { command = [ here "./bin/weaver" ]; health_port = 24999 }) ]);
  check "the model catalogue's path"
    (read.model_catalogue = Some (here "./bin/models.json"));
  let file, directory = written (text (complete "../elsewhere/")) in
  check "a ../ argument is resolved against the file's own directory"
    ((Implementations.read file).dispatcher
    = [ directory // "../elsewhere/dispatcher" ]);
  let file, _ = written (text (complete "/opt/")) in
  check "an absolute path is passed as written"
    ((Implementations.read file).archivist = [ "/opt/archivist" ])

let names_one () =
  let file, _ = written (text (complete "/opt/")) in
  refused "a run that names none" "names no suite"
    (Suite.suite_and_manifest [ "run" ]);
  refused "a run that names one twice" "more than once"
    (Suite.suite_and_manifest
       [ "gateway"; "--implementations"; file; "--implementations"; file ]);
  refused "a run that gives none after the flag" "names no file"
    (Suite.suite_and_manifest [ "gateway"; "--implementations" ]);
  refused "a run that gives a flag after the flag" "names no file"
    (Suite.suite_and_manifest [ "gateway"; "--implementations"; "--project" ]);
  check "a run that names one, with testo's arguments after it"
    (Suite.suite_and_manifest
       [ "gateway"; "--implementations"; file; "-t"; "census" ]
    = Ok ("gateway", file, [ "-t"; "census" ]))

let refuses () =
  let shape = "is not of the shape" in
  let file, _ = written {|{ "gateway": [|} in
  fails "a file that is not JSON" "mine\\.json" (fun () ->
      Implementations.read file);
  let lacking = List.remove_assoc "gateway" (complete "/opt/") in
  fails "a file that lacks a key" shape (fun () ->
      Implementations.read (fst (written (text lacking))));
  let unknown = complete "/opt/" @ [ ("gatway", command [ "/opt/gateway" ]) ] in
  fails "a file with a key it does not know" shape (fun () ->
      Implementations.read (fst (written (text unknown))));
  let tier =
    List.map
      (function
        | "sdks", _ ->
            ( "sdks",
              `Assoc
                [
                  ( "mine",
                    `Assoc
                      [
                        ( "live",
                          `Assoc
                            [
                              ("command", command [ "/opt/live" ]);
                              ("tier", `String "all");
                            ] );
                      ] );
                ] )
        | member -> member)
      (complete "/opt/")
  in
  fails "a file that gives a tier that is none" shape (fun () ->
      Implementations.read (fst (written (text tier))))

let tests =
  [
    Testo.create ~category:[ "implementations" ]
      "resolves an argument that starts with ./ or ../ against the file's own \
       directory, and passes any other as written"
      resolves;
    Testo.create ~category:[ "implementations" ]
      "refuses a run that names no file, names one twice, or gives none after \
       the flag"
      names_one;
    Testo.create ~category:[ "implementations" ]
      "refuses a file that is not JSON, lacks a key, has one it does not know, \
       or gives a tier that is none"
      refuses;
  ]
