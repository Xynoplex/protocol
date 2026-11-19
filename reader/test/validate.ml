(* Validate reads an integer as an integer whatever its size: one past what
   the platform's int holds (natively past 2^62, under JavaScript past 2^31)
   arrives as Yojson's literal, and is held to its schema as any other. *)

let refusals schema value =
  match
    Decode.run ~file:"x.json" (Yojson.Safe.from_string schema) Schema.decode
  with
  | Error error -> Alcotest.fail (Decode.error_to_string error)
  | Ok (schema, _) ->
      List.map snd
        (Validate.errors
           (Result.get_ok (Decode.ids []))
           (fun _ -> None)
           schema
           (Yojson.Safe.from_string value)
           "")

let holds schema value expected () =
  Alcotest.(check (list string)) value expected (refusals schema value)

let huge = "100000000000000000000"

let () =
  Alcotest.run "validate"
    [
      ( "integers",
        [
          Alcotest.test_case "one past the platform's int is an integer" `Quick
            (holds {|{"type":"integer"}|} huge []);
          Alcotest.test_case "and a number" `Quick
            (holds {|{"type":"number","minimum":1}|} huge []);
          Alcotest.test_case "and is held to a bound" `Quick
            (holds {|{"type":"integer","maximum":65535}|} huge
               [ "is more than 65535" ]);
        ] );
    ]
