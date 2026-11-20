(* kb-index.test.ts: where a Smelter keeps its knowledge base's index
   (SMELTER.md § The index): in collections of that knowledge base's own,
   named from its DID as specs/src/vectors/payload-cases.json states.
   Another knowledge base's collections on the same Qdrant are none of its:
   its start neither reads them nor purges them, making its own index anew
   leaves them as they are, and what it writes lands in its own. A Smelter
   whose settings name another knowledge base than the one at its gateway
   refuses to start before it reaches the index, and makes no collection of
   the one they name (SMELTER.md § Boot). With it, that the suite names a
   knowledge base's collections as the table's collectionsNamed cases
   state, and mints the token of each service it starts with the claims
   specs/src/vectors/token-cases.json states, and no others. *)

open Xynolog_conformance
open Support
module Payload_cases = Qdrant.Payload_cases

let named_cases () =
  List.filter_map
    (function
      | Payload_cases.Collections_named { why; collections_named } ->
          Some (why, collections_named)
      | _ -> None)
    (Protocol.tables ()).vectors_payload_cases.cases

let names_as_stated (named : Payload_cases.collections_named_collections_named)
    () =
  Check.equal "the collections' names"
    (`Assoc
       [
         ("resources", str named.names.resources);
         ("annotations", str named.names.annotations);
         ("declined", str named.names.declined);
       ])
    (`Assoc
       (List.map
          (fun (word, name) -> (word, str name))
          (Qdrant.collection_names named.kb)))

(* The services the suite mints a Qdrant token for. *)
let holders = [ "librarian"; "smelter" ]
let three_parts = Re.Perl.compile_pat {|^[\w-]+\.[\w-]+\.[\w-]+$|}

(* What a token minted with [key] carries, its form and its signature held
   to a JWT's, signed HS256 with the key. *)
let carried key token =
  Check.that
    (Printf.sprintf "%s is three parts of base64url without padding" token)
    (Re.execp three_parts token);
  match Jose.Jwt.unsafe_of_string token with
  | Error _ -> failwith (token ^ " is not a JWT")
  | Ok jwt -> (
      Check.equal "its header"
        (`Assoc [ ("alg", str "HS256"); ("typ", str "JWT") ])
        (Yojson.Safe.from_string
           (Base64.decode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet
              jwt.raw_header));
      match Jose.Jwt.validate_signature ~jwk:(Jose.Jwk.make_oct key) jwt with
      | Ok jwt -> jwt.payload
      | Error _ -> failwith (token ^ " is not signed HS256 with the key"))

let minted_as_stated
    (case : Xynolog_protocol.Tables.Vectors_token_cases.cases_item) () =
  let { Qdrant.key; _ } = Smelter_world.database () in
  let raw =
    List.find
      (fun c -> Check.at c [ "kb" ] = str case.kb)
      (Yojson.Safe.Util.to_list
         (Check.at (Qdrant.raw "vectors/token-cases.json") [ "cases" ]))
  in
  (* A token for each service the case names, and none it does not, each
     carrying the case's claims and no others. *)
  Check.equal "what each token carries"
    (Check.at raw [ "claims" ])
    (`Assoc
       (List.map
          (fun holder ->
            ( holder,
              carried key (Qdrant.minted key (Qdrant.claims case.kb holder)) ))
          holders))

(* ── A Smelter beside another knowledge base ─────────────────────────── *)

(* Another knowledge base than the one the suite's Smelter serves. *)
let other_kb () =
  (Protocol.kb_identity "plain scalars, and a version, which names nothing").did

(* A knowledge base the gateway does not serve, which a Smelter's settings
   are made to name. *)
let elsewhere () = Protocol.kb_identity "a comment is not part of the value"

(* The first case of the table holding [what]. *)
let first what =
  match
    List.find_map
      (fun case -> Protocol.at case [ what ])
      (Yojson.Safe.Util.to_list
         (Check.at (Qdrant.raw Qdrant.payload_table) [ "cases" ]))
  with
  | Some found -> found
  | None ->
      failwith
        (Printf.sprintf "specs/src/%s states no %s" Qdrant.payload_table what)

let point id payload vector =
  `Assoc [ ("id", id); ("payload", payload); ("vector", vector) ]

(* What the other knowledge base's index holds: the points the table states
   of a resource, an annotation and a decline, each with its vector. None of
   their resources is of the record the Smelter reads, so a Smelter that
   read these collections as its own would purge every one of them. *)
let planted () =
  let stored = first "resourceStored" in
  let annotated = first "annotationStored" in
  let declined = first "declineStored" in
  let chunks = Yojson.Safe.Util.to_list (Check.at stored [ "chunks" ]) in
  [
    ( "resources",
      List.mapi
        (fun at p ->
          point
            (Check.at p [ "qdrantId" ])
            (Check.at p [ "payload" ])
            (Check.at (List.nth chunks at) [ "embedding" ]))
        (Yojson.Safe.Util.to_list (Check.at stored [ "points" ])) );
    ( "annotations",
      [
        point
          (Check.at annotated [ "point"; "qdrantId" ])
          (Check.at annotated [ "point"; "payload" ])
          (Check.at annotated [ "embedding" ]);
      ] );
    (* A point of the declined has the vector that says nothing. *)
    ( "declined",
      [
        point
          (Check.at declined [ "point"; "qdrantId" ])
          (Check.at declined [ "point"; "payload" ])
          (`List [ `Int 0 ]);
      ] );
  ]

let collection_names did = List.map snd (Qdrant.collection_names did)
let sorted names = strs (List.sort compare names)

let beside_another (w : W.t) =
  started w;
  let text =
    "This text is of the knowledge base the Smelter serves, and of no other."
  in
  let id = W.created w text plain ~name:"Its own" in
  Check.like "the decision"
    (`Assoc [ ("outcome", str "indexed") ])
    (W.settled w id);
  let other = Qdrant.index w.database (other_kb ()) in
  let planted = planted () in
  ignore (W.stop w : int option);
  W.clear_index w;
  (* The other knowledge base's index, made as a store makes one: as wide as
     the table's vectors, which are not the Smelter's model's. *)
  Qdrant.make other "resources" 3;
  Qdrant.make other "annotations" 3;
  Qdrant.make other "declined" 1;
  List.iter (fun (word, points) -> Qdrant.put other word points) planted;
  let held () =
    `List
      (List.map
         (fun (word, _) -> `List (Qdrant.points ~vectors:true other word))
         planted)
  in
  let before = held () in
  W.start w;
  (* Its start found its own index gone, made it anew, and filled it from
     the record: its one resource, and nothing purged. *)
  let reconcile = Check.at (W.health w) [ "reconcile" ] in
  Check.equal "its reconcile's phase" (str "done") (W.phase (W.health w));
  Check.like "its reconcile's summary"
    (`Assoc
       [
         ("resourcesEmbedded", count 1);
         ("resourceVectorsDeleted", count 0);
         ("annotationVectorsDeleted", count 0);
       ])
    (Check.at reconcile [ "summary" ]);
  Check.that "its own resource has points"
    (Qdrant.points_of w.index "resources" id <> []);
  List.iter
    (fun (word, points) ->
      List.iter
        (fun p ->
          let resource =
            Yojson.Safe.Util.to_string (Check.at p [ "payload"; "resourceId" ])
          in
          Check.equal
            (Printf.sprintf "its own %s of %s" word resource)
            (`List [])
            (`List (Qdrant.points_of w.index word resource)))
        points)
    planted;
  (* The other's points are as they were put, and none of the Smelter's is
     among them. *)
  Check.equal "the other knowledge base's points" before (held ());
  Check.equal "the other knowledge base's points of its resource" (`List [])
    (`List (Qdrant.points_of other "resources" id));
  Check.equal "the collections on the Qdrant"
    (sorted
       (collection_names w.kb.world.kb.did @ collection_names (other_kb ())))
    (sorted (Qdrant.collections_at w.database))

let names_another (w : W.t) =
  stopped w;
  let elsewhere = elsewhere () in
  let code, output =
    W.refused w
      (W.changed w [ "kb" ]
         (Some
            (`Assoc
               [
                 ("name", str elsewhere.name); ("domain", str elsewhere.domain);
               ])))
  in
  Check.that "it exits with other than 0" (code <> Some 0);
  List.iter
    (fun did ->
      Check.that
        (Printf.sprintf "its output names %s:\n%s" did output)
        (Re.execp (Re.compile (Re.str did)) output))
    [ w.kb.world.kb.did; elsewhere.did ];
  (* Its token manages the whole Qdrant: only the refusal kept it from making
     the index of the knowledge base its settings name. *)
  let theirs = collection_names elsewhere.did in
  Check.equal "the collections of the knowledge base its settings name"
    (`List [])
    (strs
       (List.filter
          (fun name -> List.mem name theirs)
          (Qdrant.collections_at w.database)));
  W.start w

let tests implementations =
  List.map
    (fun (why, named) ->
      let c = Case.case why (names_as_stated named) in
      Case.test
        ~category:[ "a knowledge base's collections, as the suite names them" ]
        c c.run)
    (named_cases ())
  @ List.map
      (fun (case : Xynolog_protocol.Tables.Vectors_token_cases.cases_item) ->
        let c = Case.case case.why (minted_as_stated case) in
        Case.test
          ~category:[ "the tokens the suite mints for the services it starts" ]
          c c.run)
      (Protocol.tables ()).vectors_token_cases.cases
  @
  let category = [ "a Smelter beside another knowledge base on its Qdrant" ] in
  let cases = in_world implementations "kb-index" in
  cases ~category
    [
      Case.case
        "neither reads nor purges nor writes the other knowledge base's \
         collections, and fills its own"
        beside_another;
    ]
  @ cases
      ~category:
        (category
        @ [
            "started with settings that name another knowledge base than the \
             one at its gateway";
          ])
      [
        Case.case
          "refuses to start, naming both, and makes no collection of the one \
           its settings name"
          names_another;
      ]
