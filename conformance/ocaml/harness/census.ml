(* Every gate that holds what the harness works out, or looks up, to the
   table in specs/src that states it, each by that table. A ported case
   meets its gate on first use; harness-test runs each one alone, with no
   manifest and no store, so a gate fails where Protocol changes and not
   only in the suite whose case first meets it. *)

let gates =
  [
    ( "archivist/shard-cases.json",
      fun () -> Lazy.force Archivist_world.shards_checked );
    ("graph/database-cases.json", fun () -> Lazy.force Neo4j.databases_checked);
    ("bus/event-id-cases.json", fun () -> Lazy.force Bus_stream.ids_checked);
    ( "vectors/payload-cases.json's qdrantId",
      fun () -> Lazy.force Qdrant.ids_checked );
    ( "principals/cases.json",
      fun () -> ignore (Lazy.force Protocol.principals_checked) );
    ( "kb-identity/cases.json",
      fun () -> ignore (Protocol.kb_identity Protocol.suite_kb : Protocol.kb) );
    ( "the gateway document's bearerAuth roles and claims",
      fun () ->
        List.iter
          (fun declared -> ignore (declared () : string))
          [ Protocol.service_role; Protocol.kind_claim; Protocol.agent_kind ] );
  ]
