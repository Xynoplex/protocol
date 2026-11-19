(* Validate reads an integer as an integer whatever its size: one past what
   the platform's int holds (natively past 2^62, under JavaScript past 2^31)
   arrives as Yojson's literal, and is held to its schema as any other.

   It reads [unevaluatedProperties] as JSON Schema 2020-12 does: a member is
   evaluated by [properties] and [additionalProperties] beside it, and by
   what a [$ref], an [allOf], the [anyOf] members a value is and the [oneOf]
   member it is evaluate in place; never by a [not], nor by the schema that
   refers to it. *)

let case name test = Alcotest.test_case name `Quick test

(* The ways [value] is not what x.json's root says, x.json being [document],
   each as the pointer into the value and why. *)
let refusals document value =
  let json = Yojson.Safe.from_string document in
  let schemas ({ file; pointer } : Validate.place) =
    if file <> "x.json" then None
    else
      Option.bind (Decode.at_pointer pointer json) (fun node ->
          Result.to_option
            (Result.map fst (Decode.run ~file node Schema.decode)))
  in
  match schemas { file = "x.json"; pointer = "" } with
  | None -> Alcotest.fail "x.json is no schema"
  | Some schema ->
      List.map
        (fun (pointer, message) ->
          if pointer = "" then message else pointer ^ ": " ^ message)
        (Validate.errors schemas schema (Yojson.Safe.from_string value) "")

let holds document value expected () =
  Alcotest.(check (list string)) value expected (refusals document value)

let huge = "100000000000000000000"

let extended =
  {|{"$defs":{"Members":{"type":"object","properties":{"a":{"type":"string"}}}},
     "allOf":[{"$ref":"#/$defs/Members"},{"properties":{"b":{"type":"string"}}}],
     "unevaluatedProperties":false}|}

let closed_base =
  {|{"$defs":{"Base":{"type":"object","properties":{"a":{"type":"string"}},
                     "unevaluatedProperties":false}},
     "allOf":[{"$ref":"#/$defs/Base"},{"properties":{"b":{"type":"string"}}}]}|}

let tagged =
  {|{"oneOf":[
      {"properties":{"k":{"enum":["x"]},"x":{"type":"string"}},"required":["k"]},
      {"properties":{"k":{"enum":["y"]},"y":{"type":"string"}},"required":["k"]}],
     "unevaluatedProperties":false}|}

let either =
  {|{"anyOf":[{"properties":{"a":{"type":"string"}}},
              {"properties":{"b":{"type":"string"}}}],
     "unevaluatedProperties":false}|}

let refusing =
  {|{"properties":{"a":{"type":"string"}},
     "not":{"properties":{"b":{"type":"integer"}},"required":["b"]},
     "unevaluatedProperties":false}|}

let () =
  Alcotest.run "validate"
    [
      ( "integers",
        [
          case "one past the platform's int is an integer"
            (holds {|{"type":"integer"}|} huge []);
          case "and a number" (holds {|{"type":"number","minimum":1}|} huge []);
          case "and is held to a bound"
            (holds {|{"type":"integer","maximum":65535}|} huge
               [ "is more than 65535" ]);
        ] );
      ( "unevaluatedProperties",
        [
          case "admits what the allOf's parts evaluate"
            (holds extended {|{"a":"1","b":"2"}|} []);
          case "refuses what no part evaluates"
            (holds extended {|{"a":"1","c":"3"}|}
               [ "/c: is a member the schema does not state" ]);
          case "a closed base refuses its extension's members"
            (holds closed_base {|{"a":"1","b":"2"}|}
               [ "/b: is a member the schema does not state" ]);
          case "a oneOf evaluates the member the value is"
            (holds tagged {|{"k":"x","x":"1"}|} []);
          case "a oneOf evaluates no member the value is not"
            (holds tagged {|{"k":"x","y":"1"}|}
               [ "/y: is a member the schema does not state" ]);
          case "an anyOf evaluates each member the value is"
            (holds either {|{"a":"1","b":"2"}|} []);
          case "a not evaluates nothing"
            (holds refusing {|{"a":"1","b":"2"}|}
               [ "/b: is a member the schema does not state" ]);
        ] );
    ]
