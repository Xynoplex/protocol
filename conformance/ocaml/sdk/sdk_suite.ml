(* The SDK suite (conformance/sdk/README.md): every SDK, through its
   driver, put through every case of the wire corpus and, for an SDK with a
   live layer, those of the live corpus its tier holds it to and one built
   from each row of specs/src/client/refresh.json, against a real gateway
   on each signal plane; and the client proxy the suite leans on, held
   against a real gateway. *)

open Xynolog_conformance
open Xynolog_protocol

let tests (implementations : Implementations.t) =
  let wire = Sdk_case.corpus "conformance/sdk/wire" in
  let live = Sdk_case.corpus "conformance/sdk/live" @ Refresh_cases.cases () in
  let on_the_gateway =
    Sdk_case.tests
      ~make:(fun plane -> World.create ~command:implementations.gateway plane)
      ~world:Fun.id ~close:World.close
  in
  Restated.tests implementations wire
  @ List.concat_map
      (fun (sdk, (drivers : Implementations.sdk)) ->
        (match drivers.wire with
          | None -> []
          | Some command ->
              on_the_gateway ?exempt:drivers.exempt
                (Printf.sprintf "the %s SDK on the wire" sdk)
                command wire Wire)
        @
        match drivers.live with
        | None -> []
        | Some { live_command; tier } ->
            (* The fleet's cases hold every live layer; the rest, an SDK at
               full parity. *)
            on_the_gateway
              (Printf.sprintf "the %s SDK's live layer" sdk)
              live_command
              (List.filter
                 (fun (c : Corpus.case) ->
                   c.case.tier <> Some Parity || tier = Parity)
                 live)
              Live)
      implementations.sdks
  @ Proxy_cases.tests implementations

let suite : Suite.t = { name = "sdk"; workers = 4; tests }
