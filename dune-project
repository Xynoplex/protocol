(lang dune 3.24)
(name xynolog-protocol)
(formatting (enabled_for ocaml))
(generate_opam_files true)
(source (github xynolog/protocol))
(license Apache-2.0)
(authors "Xynolog")
(maintainers "Xynolog maintainers")
(package
 (name xynolog-protocol)
 (synopsis "Protocol's one reader of itself: its specs, read strictly, and the rules they are held to")
 (description "Reads specs/src into a model that refuses what it does not know, with each table decoded through the model its schema gives.")
 (depends
  (ocaml (= 5.5.2))
  (yojson (>= 3.0.0))
  (re (>= 1.14.0))
  (js_of_ocaml (>= 6.5.0))
  (alcotest :with-test)
  (ocamlformat (and :with-test (= 0.29.0)))))

(package
 (name xynolog-conformance)
 (synopsis "Protocol's conformance harness, on Protocol's reader")
 (description "Launches the implementations a manifest names, stands in for their peers, and holds what they do to what the reader reads of Protocol.")
 (depends
  xynolog-protocol
  (eio_main (>= 1.6))
  (cohttp-eio (>= 6.3.0))
  (neodriver_eio (>= 0.9.0))
  (nats-client (>= 0.0.10))
  (opentelemetry (>= 0.92))
  (jose (>= 0.11.0))
  (decompress (>= 1.5.0))
  (testo (>= 0.5.0))))
