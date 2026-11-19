(* xynolog-protocol check ROOT: the Protocol checkout at ROOT. Its specs/src is
   read into the model, every file accounted for and every table what its
   schema says, and what the checkout states outside specs/src is held to it.
   Each refusal is printed, and any fails the run. *)

let () =
  match Sys.argv with
  | [| _; "check"; root |] -> (
      match
        Result.bind
          (Spec.load (Filename.concat root "specs/src"))
          (fun spec ->
            match Archivist_rules.contract ~root spec.tables.bus_registry with
            | [] -> Ok spec
            | errors -> Error errors)
      with
      | Ok spec ->
          Printf.printf
            "Protocol read: 2 OpenAPI documents, %d path items, %d responses, \
             %d JSON Schemas (%d of them services'), and %d tables, each what \
             its schema says\n"
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
