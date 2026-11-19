(* [unevaluatedProperties], as JSON Schema 2020-12 reads it: a member is
   evaluated by [properties] and [additionalProperties] beside it, and by
   what a [$ref], an [allOf], the [anyOf] members a value is and the [oneOf]
   member it is evaluate in place; never by a [not], nor by the schema that
   refers to it. *)

let case name test = Alcotest.test_case name `Quick test

(* The ways [value] is not what x.json's root says, x.json being
   [document]. *)
let held document value =
  let json = Yojson.Safe.from_string document in
  let schemas ({ file; pointer } : Validate.place) =
    if file <> "x.json" then None
    else
      Option.bind (Decode.at_pointer pointer json) (fun node ->
          Result.to_option (Result.map fst (Decode.run ~file node Schema.decode)))
  in
  match schemas { file = "x.json"; pointer = "" } with
  | None -> Alcotest.fail "x.json is no schema"
  | Some schema ->
      List.map
        (fun (pointer, message) -> pointer ^ ": " ^ message)
        (Validate.errors schemas schema (Yojson.Safe.from_string value) "")

let check document value expected () =
  Alcotest.(check (list string)) value expected (held document value)

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
      ( "unevaluatedProperties",
        [
          case "admits what the allOf's parts evaluate"
            (check extended {|{"a":"1","b":"2"}|} []);
          case "refuses what no part evaluates"
            (check extended {|{"a":"1","c":"3"}|}
               [ "/c: is a member the schema does not state" ]);
          case "a closed base refuses its extension's members"
            (check closed_base {|{"a":"1","b":"2"}|}
               [ "/b: is a member the schema does not state" ]);
          case "a oneOf evaluates the member the value is"
            (check tagged {|{"k":"x","x":"1"}|} []);
          case "a oneOf evaluates no member the value is not"
            (check tagged {|{"k":"x","y":"1"}|}
               [ "/y: is a member the schema does not state" ]);
          case "an anyOf evaluates each member the value is"
            (check either {|{"a":"1","b":"2"}|} []);
          case "a not evaluates nothing"
            (check refusing {|{"a":"1","b":"2"}|}
               [ "/b: is a member the schema does not state" ]);
        ] );
    ]
