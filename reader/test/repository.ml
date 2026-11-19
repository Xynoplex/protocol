(* [Repository.canonical] spells a value as [JSON.stringify(value, null, 2)]
   and a newline do, and refuses what it cannot spell alike;
   [Repository.ownership] classifies exactly the tracked files. *)

let case name test = Alcotest.test_case name `Quick test

let spelled json expected () =
  Alcotest.(check (result string string))
    "spelled" (Ok expected)
    (Repository.canonical json)

let unspelled json () =
  Alcotest.(check bool)
    "refused" true
    (Result.is_error (Repository.canonical json))

let root inventory =
  let dir = Filename.temp_dir "repository" "" in
  Out_channel.with_open_bin (Filename.concat dir Repository.inventory)
    (fun out -> Out_channel.output_string out inventory);
  dir

let owned inventory tracked expected () =
  Alcotest.(check (list string))
    "refusals" expected
    (List.map
       (fun (e : Decode.error) -> e.message)
       (Repository.ownership ~root:(root inventory) tracked))

let header = "path,classification,owner,notes\n"

let () =
  Alcotest.run "repository"
    [
      ( "canonical",
        [
          case "an empty object and an empty list"
            (spelled (`List [ `Assoc []; `List [] ]) "[\n  {},\n  []\n]\n");
          case "a nested object"
            (spelled
               (`Assoc [ ("a", `List [ `Int 1; `String "x" ]); ("b", `Null) ])
               "{\n  \"a\": [\n    1,\n    \"x\"\n  ],\n  \"b\": null\n}\n");
          case "an em dash as the character, a control character escaped"
            (spelled (`String "a\xe2\x80\x94b\x01\t\"\\")
               "\"a\xe2\x80\x94b\\u0001\\t\\\"\\\\\"\n");
          case "a line separator as the character"
            (spelled (`String "\xe2\x80\xa8") "\"\xe2\x80\xa8\"\n");
          case "a decimal at its shortest"
            (spelled
               (`List [ `Float 0.5; `Float 1.0; `Float 0.1 ])
               "[\n  0.5,\n  1,\n  0.1\n]\n");
          case "a key like an index"
            (unspelled (`Assoc [ ("b", `Null); ("1", `Null) ]));
          case "a key written twice"
            (unspelled (`Assoc [ ("a", `Null); ("a", `Null) ]));
          case "a number in an exponent" (unspelled (`Float 1e21));
        ] );
      ( "ownership",
        [
          case "every tracked file classified"
            (owned (header ^ "a,c,o,n\n") [ "a" ] []);
          case "a tracked file unclassified"
            (owned (header ^ "a,c,o,n\n") [ "a"; "b" ]
               [ "does not classify the tracked file b" ]);
          case "a row Git does not track"
            (owned
               (header ^ "a,c,o,n\nz,c,o,n\n")
               [ "a" ]
               [ "classifies z, which Git does not track" ]);
          case "a file listed twice"
            (owned
               (header ^ "a,c,o,n\na,c,o,n\n")
               [ "a" ] [ "lists a 2 times" ]);
          case "an incomplete row"
            (owned (header ^ "a,c,,n\n") [ "a" ]
               [ "has an incomplete row for a" ]);
          case "a quoted field"
            (owned (header ^ "a,c,o,\"n, \"\"quoted\"\"\"\n") [ "a" ] []);
          case "another header"
            (owned "path,owner\na,o\n" [ "a" ]
               [
                 "does not begin with the header \
                  path,classification,owner,notes";
               ]);
        ] );
    ]
