(* xynolog-protocol check ROOT: the Protocol checkout at ROOT. Its specs/src is
   read into the model, every file accounted for and every table what its
   schema says; what the checkout states outside specs/src is held to it;
   each JSON file of specs/src is spelled as JSON.stringify spells it; and
   every file Git tracks has an owner. Each refusal is printed, and any fails the run. *)

(* The files Git tracks under [root]. *)
let tracked root =
  let channel =
    Unix.open_process_args_in "git" [| "git"; "-C"; root; "ls-files"; "-z" |]
  in
  let listing = In_channel.input_all channel in
  match Unix.close_process_in channel with
  | Unix.WEXITED 0 ->
      Ok (List.filter (( <> ) "") (String.split_on_char '\000' listing))
  | _ ->
      Error
        [
          {
            Decode.file = Repository.inventory;
            pointer = "";
            message = "cannot be checked: git ls-files failed in " ^ root;
          };
        ]

let () =
  match Sys.argv with
  | [| _; "check"; root |] -> (
      let outside spec =
        Archivist_rules.contract ~root spec.Spec.tables.bus_registry
        @ Repository.spelling ~root
        @
        match tracked root with
        | Ok files -> Repository.ownership ~root files
        | Error e -> e
      in
      match
        Result.bind
          (Spec.load (Filename.concat root "specs/src"))
          (fun spec ->
            match outside spec with [] -> Ok spec | errors -> Error errors)
      with
      | Ok spec ->
          Printf.printf
            "Protocol read: 2 OpenAPI documents, %d path items, %d responses, \
             %d JSON Schemas (%d of them services'), and %d tables, each what \
             its schema says; each JSON file spelled as JSON.stringify spells \
             it, and every tracked file owned\n"
            (List.length spec.path_items)
            (List.length spec.responses)
            (List.length spec.schemas)
            (List.length spec.services)
            (List.length spec.table_files)
      | Error errors ->
          List.iter
            (fun error -> prerr_endline (Decode.error_to_string error))
            errors;
          Printf.eprintf "%d refusals\n" (List.length errors);
          exit 1)
  | _ ->
      prerr_endline "usage: xynolog-protocol check ROOT";
      exit 2
