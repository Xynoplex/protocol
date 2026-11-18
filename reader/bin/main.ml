(* xynolog-protocol check SPECS: Protocol's specs/src read into the model, every
   file accounted for and every table what its schema says. Each refusal is
   printed, and any fails the run. *)

let () =
  match Sys.argv with
  | [| _; "check"; specs |] -> (
      match Spec.load specs with
      | Ok spec ->
          Printf.printf
            "Protocol read: 2 OpenAPI documents, %d path items, %d responses, \
             %d JSON Schemas, and %d tables, each what its schema says\n"
            (List.length spec.path_items)
            (List.length spec.responses)
            (List.length spec.schemas)
            (List.length spec.table_files)
      | Error errors ->
          List.iter
            (fun error -> prerr_endline (Decode.error_to_string error))
            errors;
          Printf.eprintf "%d refusals\n" (List.length errors);
          exit 1)
  | _ ->
      prerr_endline "usage: xynolog-protocol check SPECS";
      exit 2
