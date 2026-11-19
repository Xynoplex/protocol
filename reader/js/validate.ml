(* The reader's Validate, for JavaScript. [validator bundle] reads the text of
   a bundle the reader writes, and returns what holds a value, given as JSON
   text, to the component schema the bundle names: each refusal, as the JSON
   Pointer into the value and why. A schema the bundle does not name throws. *)

open Js_of_ocaml

let bundle = "bundle.json"

(* A bundle states no [$id]: each reference in it is to a place within it. *)
let ids = Result.get_ok (Decode.ids [])

let validator text =
  let json = Yojson.Safe.from_string (Js.to_string text) in
  let cache = Hashtbl.create 64 in
  let schema_at ({ file; pointer } : Validate.place) =
    match Hashtbl.find_opt cache pointer with
    | Some found -> found
    | None ->
        let found =
          if file <> bundle then None
          else
            Option.bind (Decode.at_pointer pointer json) (fun node ->
                Result.to_option
                  (Result.map fst (Decode.run ~file node Schema.decode)))
        in
        Hashtbl.replace cache pointer found;
        found
  in
  fun name value ->
    let name = Js.to_string name in
    match
      schema_at
        { file = bundle; pointer = "/components/schemas/" ^ Decode.escape name }
    with
    | None ->
        Js_error.raise_
          (Js_error.of_error
             (Js.Unsafe.new_obj Js.error_constr
                [|
                  Js.Unsafe.inject
                    (Js.string ("the bundle names no schema " ^ name));
                |]))
    | Some schema ->
        Js.array
          (Array.of_list
             (List.map
                (fun (pointer, why) ->
                  Js.string ((if pointer = "" then "/" else pointer) ^ " " ^ why))
                (Validate.errors ids schema_at schema
                   (Yojson.Safe.from_string (Js.to_string value))
                   "")))

let () =
  Js.export "validator"
    (Js.Unsafe.callback_with_arity 1 (fun text ->
         Js.Unsafe.callback_with_arity 2 (validator text)))
