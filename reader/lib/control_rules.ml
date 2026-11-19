(* control/methods.json names each schema of the control API by its [$id],
   as a [$ref] does: the version's, the error data's, each method's params and
   result, and each notification's params. Each names a schema some file
   states, and that schema is control/'s. A name not under [Decode.base] is
   its table's schema's to refuse, by its pattern. *)

let file = "control/methods.json"
let area = "control/"
let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token

let refusals ~ids (table : Tables.Control_methods.t) =
  let named pointer target =
    if not (String.starts_with ~prefix:Decode.base target) then []
    else
      let refuse fmt =
        Printf.ksprintf
          (fun message -> [ { Decode.file; pointer; message } ])
          fmt
      in
      match Decode.names ids { target; file; pointer } with
      | None -> refuse "names %s, an $id no JSON Schema states" target
      | Some (schema, _) when not (String.starts_with ~prefix:area schema) ->
          refuse "names %s, %s, which is not a schema of %s" target schema area
      | Some _ -> []
  in
  named "/apiVersion" table.api_version
  @ named "/errorData" table.error_data
  @ List.concat
      (List.mapi
         (fun m (method_ : Tables.Control_methods.methods_item) ->
           let at = "" / "methods" / string_of_int m in
           named (at / "params") method_.params
           @ named (at / "result") method_.result)
         table.methods)
  @ List.concat
      (List.mapi
         (fun n (notification : Tables.Control_methods.notifications_item) ->
           named
             ("" / "notifications" / string_of_int n / "params")
             notification.params)
         table.notifications)
