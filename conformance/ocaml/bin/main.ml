open Xynolog_conformance

let () = Suite.run [ Gateway_suite.suite; Librarian_suite.suite ]
