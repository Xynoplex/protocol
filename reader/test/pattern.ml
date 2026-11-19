(* [Pattern] reads a schema's pattern as ECMA-262 does with the [u] flag.
   Each row is a pattern, a text, and whether JavaScript's
   [new RegExp(pattern, "u").test(text)] holds, as Node 26 answered it. *)

let ecma_262 =
  [
    ("^[A-Za-z0-9_-]{1,128}$", "abc", true);
    ("^[A-Za-z0-9_-]{1,128}$", "abc\x0a", false);
    ("^[A-Za-z0-9_-]{1,128}$", "a b", false);
    ("^[A-Za-z0-9_-]{1,128}$", "\xc3\xa9", false);
    ("^did:\\S+$", "did:web:x", true);
    ("^did:\\S+$", "did:a\xc2\xa0b", false);
    ("^did:\\S+$", "did:a\xe2\x80\xa8b", false);
    ("^did:\\S+$", "did:a\x0db", false);
    ("^did:\\S+$", "did:a\xe3\x80\x80b", false);
    ("^did:\\S+$", "did:\xef\xbb\xbf", false);
    ("^did:\\S+$", "did:\xc3\xa9", true);
    ("^did:\\S+$", "did:\xf0\x9f\x98\x80", true);
    ("\\S", "\xc2\xa0\xe2\x80\x83", false);
    ("\\S", " \x09\x0d\x0a", false);
    ("\\S", "\xc2\xa0x", true);
    ("^p-.+-[0-9]+$", "p-a\x0db-1", false);
    ("^p-.+-[0-9]+$", "p-a\xe2\x80\xa9b-1", false);
    ("^p-.+-[0-9]+$", "p-\xc3\xa9\xc2\xa0-1", true);
    ("^e-[^:]+:[^:]+:.+$", "e-\xc3\xa9:\xc2\xa0:\xf0\x9f\x98\x80", true);
    ("^.{2}$", "\xf0\x9f\x98\x80\xf0\x9f\x98\x80", true);
    ("^.{2}$", "\xc3\xa9", false);
    ("^[^a]$", "\xf0\x9f\x98\x80", true);
    ("^\\w+$", "\xc3\xa9", false);
    ("^\\W$", "\xc3\xa9", true);
    ("^\\d$", "\xd9\xa3", false);
    ("^\\D$", "\xd9\xa3", true);
  ]

let refused =
  [
    ("\\bword", "a word boundary");
    ("^\\p{L}+$", "a Unicode property");
    ("\\x41", "a hexadecimal escape");
    ("\\1", "a back reference");
    ("(?=a)", "a lookahead");
    ("a\\", "a lone backslash");
    ("\xc3\xa9", "a character outside ASCII");
    ("a\tb", "a control character");
    ("[a", "an unclosed class");
  ]

let read = [ "^(?:a|b)\\.c$"; "[\\]\\-]"; "^\\S+$"; "^[^:]+:\\d{1,3}$" ]
let case name test = Alcotest.test_case name `Quick test

let () =
  Alcotest.run "pattern"
    [
      ( "matches",
        List.map
          (fun (pattern, text, expected) ->
            case (Printf.sprintf "%s on %S" pattern text) (fun () ->
                Alcotest.(check bool)
                  "as ECMA-262" expected
                  (Pattern.matches pattern text)))
          ecma_262 );
      ( "refusal",
        List.map
          (fun (pattern, what) ->
            case ("refuses " ^ what) (fun () ->
                Alcotest.(check bool)
                  "refused" true
                  (Option.is_some (Pattern.refusal pattern))))
          refused
        @ List.map
            (fun pattern ->
              case ("reads " ^ pattern) (fun () ->
                  Alcotest.(check (option string))
                    "read" None (Pattern.refusal pattern)))
            read );
    ]
