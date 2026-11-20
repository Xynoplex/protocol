open Xynolog_conformance

let () =
  Suite.run
    [
      Gateway_suite.suite;
      Dispatcher_suite.suite;
      Librarian_suite.suite;
      Archivist_suite.suite;
      Sdk_suite.suite;
      Worker_suite.suite;
    ]
