(* output.test.ts: what a Worker service writes (WORKER-SERVICE.md
   § Output): every line a log line, on stdout, in the form its document's
   logFormat names; and nothing on stderr. *)

open Xynolog_conformance
open Support
open Case

(* A job of each kind whose answer a service reads element by element, each
   with a proposal that is in the text and one that is nowhere in it: what a
   service has to say of an answer, it says while it runs these. The worker
   is stopped once they are settled, so all it wrote is read. *)
let ran (w : W.t) log_format =
  let nowhere =
    "a difference engine of polished brass that was never described"
  in
  let job kind params =
    mark_job w.own (Printf.sprintf "output-%s-%s" log_format kind) params
  in
  let jobs =
    [
      job "highlighting" (params "highlighting");
      job "commenting" (params "commenting-plain");
      job "assessing" (params "assessing-plain");
      job "tagging" (tagging (argument_of_claims ()) [ "Claim" ]);
      job "linking" (params "linking-person");
    ]
  in
  let proposals members =
    answer
      (Yojson.Safe.to_string (`List (List.map (fun m -> `Assoc m) members)))
  in
  Ollama.script w.ollama
    [
      proposals
        [ [ ("exact", str "the first program") ]; [ ("exact", str nowhere) ] ];
      proposals
        [
          [
            ("exact", str "Ada Lovelace");
            ("comment", str "She wrote the notes.");
          ];
          [ ("exact", str nowhere); ("comment", str "Nowhere.") ];
        ];
      proposals
        [
          [
            ("exact", str "London was wrong.");
            ("assessment", str "No source is given.");
          ];
          [ ("exact", str nowhere); ("assessment", str "Nowhere.") ];
        ];
      proposals
        [ [ ("exact", str "London was wrong.") ]; [ ("exact", str nowhere) ] ];
      proposals
        [
          [ ("exact", str "Ada Lovelace"); ("entityType", str "Person") ];
          [ ("exact", str nowhere); ("entityType", str "Person") ];
        ];
      counting 2;
    ];
  let served =
    W.start w ~document:(fun document ->
        document
        |> Edit.set_at [ "logLevel" ] (str "debug")
        |> Edit.set_at [ "logFormat" ] (str log_format))
  in
  List.iter (fun job -> ignore (settled served job : Yojson.Safe.t)) jobs;
  Check.equal "its exit code" (`Int 0)
    (match Worker_service.stop served.worker with
    | Some code -> `Int code
    | None -> `Null);
  served

(* Hold that the worker wrote something on stdout. *)
let wrote_stdout stdout =
  Check.that
    (Printf.sprintf "it wrote %d lines on stdout, and must write more than 0"
       (List.length stdout))
    (List.length stdout > 0)

let json (w : W.t) =
  let served = ran w "json" in
  let stdout = Worker_service.stdout served.worker in
  wrote_stdout stdout;
  List.iter
    (fun line ->
      let entry =
        match Yojson.Safe.from_string line with
        | `Assoc _ as entry -> Some entry
        | _ -> None
        | exception Yojson.Json_error _ -> None
      in
      match entry with
      | None -> failwith ("not a JSON object: " ^ line)
      | Some entry ->
          Check.that
            ("no level or message: " ^ line)
            (match (member "level" entry, member "message" entry) with
            | `String _, `String _ -> true
            | _ -> false))
    stdout;
  Check.equal "its stderr" (`List [])
    (strs (Worker_service.stderr served.worker))

let simple (w : W.t) =
  let served = ran w "simple" in
  let stdout = Worker_service.stdout served.worker in
  wrote_stdout stdout;
  (* A log line begins with when and how grave. A line that is the rest of a
     message spread over several begins with neither. *)
  let unlike =
    List.filter
      (fun line ->
        not
          (Check.matches_re {|^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2} \[[A-Z]+\] |}
             line))
      stdout
  in
  Check.equal "its lines that are no plain log line" (`List []) (strs unlike);
  Check.equal "its stderr" (`List [])
    (strs (Worker_service.stderr served.worker))

let tests implementations =
  in_world implementations "output" ~category:[ "the worker's output" ]
    [
      case
        "writes every line as a JSON log line on stdout, and nothing on \
         stderr, when its document says json"
        json;
      case
        "writes every line as a plain log line on stdout, and nothing on \
         stderr, when its document says simple"
        simple;
    ]
