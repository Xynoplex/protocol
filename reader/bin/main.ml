(* xynolog-protocol check ROOT: the Protocol checkout at ROOT. Its specs/src is
   read into the model, every file accounted for and every table what its
   schema says; what the checkout states outside specs/src is held to it;
   the conformance corpus is what its schema says and names what the spec
   has (Corpus); and each JSON file of specs/src is spelled as
   JSON.stringify spells it.

   xynolog-protocol bundle ROOT: specs/src read into the model, and each
   OpenAPI document of it bundled into specs/ (Bundle).

   Each refusal is printed, and any fails the run. *)

let refused errors =
  List.iter (fun error -> prerr_endline (Decode.error_to_string error)) errors;
  Printf.eprintf "%d refusals\n" (List.length errors);
  exit 1

let () =
  match Sys.argv with
  | [| _; "bundle"; root |] -> (
      match
        Result.bind
          (Spec.load (Filename.concat root Repository.specs))
          (fun (spec : Spec.t) ->
            match Bundle.write ~root ~ids:spec.ids with
            | [] -> Ok ()
            | e -> Error e)
      with
      | Ok () ->
          List.iter
            (fun bundled -> Printf.printf "Bundled %s\n" bundled)
            (List.map snd Bundle.documents)
      | Error errors -> refused errors)
  | [| _; "check"; root |] -> (
      let outside spec =
        Archivist_rules.contract ~root spec.Spec.tables.bus_registry
        @ Corpus.refusals ~root spec @ Repository.spelling ~root
      in
      match
        Result.bind
          (Spec.load (Filename.concat root Repository.specs))
          (fun spec ->
            match outside spec with [] -> Ok spec | errors -> Error errors)
      with
      | Ok spec ->
          Printf.printf
            "Protocol read: 2 OpenAPI documents, %d path items, %d responses, \
             %d JSON Schemas (%d of them services'), and %d tables, each what \
             its schema says; and each JSON file spelled as JSON.stringify \
             spells it\n"
            (List.length spec.path_items)
            (List.length spec.responses)
            (List.length spec.schemas)
            (List.length spec.services)
            (List.length spec.table_files)
      | Error errors -> refused errors)
  | _ ->
      prerr_endline "usage: xynolog-protocol check ROOT | bundle ROOT";
      exit 2
