(* The vector index, read and changed by the suite's own calls: Qdrant, over
   its REST interface, on cohttp-eio. The index's writer is what the suite
   judges, so nothing here is a Smelter's code.

   What the index is, specs/src/vectors/payload-cases.json states: for each
   knowledge base, collections named by the SHA-256 of its DID, a hyphen and
   the collection's word, with what a point's payload holds. The words and
   every payload member a point is found by are read from that table; the
   tokens each service is handed are the claims specs/src/vectors/token-cases.json
   states for the knowledge base, signed HS256 with the Qdrant's API key.

   A point's id is the MD5 of its key, laid out as a UUID, as the table's
   qdrantId rows state it: a chunk's key is its resource's id, a hyphen and
   its index; an annotation's, its id; a decline's, its resource's id. *)

let port = 6333
let token_variable = "QDRANT_TOKEN"

type database = { host : string; key : string }

let member = Yojson.Safe.Util.member
let payload_table = "vectors/payload-cases.json"

let raw file =
  match Protocol.json_of file with
  | Some json -> json
  | None -> failwith ("specs/src has no " ^ file)

(* The words of every collection the table states a knowledge base has. *)
let words () = Yojson.Safe.Util.keys (member "collections" (raw payload_table))

let collection_names did =
  let hashed = Digestif.SHA256.(to_hex (digest_string did)) in
  List.map (fun word -> (word, hashed ^ "-" ^ word)) (words ())

(* The payloads the table states for the points of a collection. *)
let stated_payloads word =
  List.concat_map
    (fun case ->
      match word with
      | "resources" -> (
          match member "resourceStored" case with
          | `Null -> []
          | stored ->
              List.map (member "payload")
                (Yojson.Safe.Util.to_list (member "points" stored)))
      | "annotations" -> (
          match member "annotationStored" case with
          | `Null -> []
          | c -> [ member "payload" (member "point" c) ])
      | "declined" -> (
          match member "declineStored" case with
          | `Null -> []
          | c -> [ member "payload" (member "point" c) ])
      | other -> failwith ("the harness reads no collection " ^ other))
    (Yojson.Safe.Util.to_list (member "cases" (raw payload_table)))

(* A payload member the table states for every point of a collection. *)
let payload_member word name =
  match stated_payloads word with
  | [] ->
      failwith
        (Printf.sprintf "specs/src/%s states no point of the %s" payload_table
           word)
  | payloads ->
      if List.for_all (fun p -> member name p <> `Null) payloads then name
      else
        failwith
          (Printf.sprintf
             "specs/src/%s does not state %s in every point of the %s"
             payload_table name word)

(* The claims token-cases.json states for the knowledge base [did], by the
   service: each case's collection names held to the naming arithmetic. *)
let claims did service =
  let cases =
    Yojson.Safe.Util.to_list (member "cases" (raw "vectors/token-cases.json"))
  in
  match List.find_opt (fun c -> member "kb" c = `String did) cases with
  | None -> failwith ("vectors/token-cases.json states no claims for " ^ did)
  | Some case ->
      let named = List.map snd (collection_names did) in
      List.iter
        (fun access ->
          match member "collection" access with
          | `String name when not (List.mem name named) ->
              failwith
                ("vectors/token-cases.json names " ^ name
               ^ ", which the naming arithmetic does not give " ^ did)
          | _ -> ())
        (match member "access" (member "librarian" (member "claims" case)) with
        | `List l -> l
        | _ -> []);
      member service (member "claims" case)

let base64url text =
  Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet text

(* A token for the Qdrant whose API key is [key]: a JWT of [claims] and no
   others, signed HS256 with the key's bytes. *)
let minted key claims =
  let signed =
    base64url
      (Yojson.Safe.to_string
         (`Assoc [ ("alg", `String "HS256"); ("typ", `String "JWT") ]))
    ^ "."
    ^ base64url (Yojson.Safe.to_string claims)
  in
  signed ^ "."
  ^ base64url Digestif.SHA256.(to_raw_string (hmac_string ~key signed))

let origin database = Printf.sprintf "http://%s:%d" database.host port

let ask database meth path body =
  Call.call ?json:body
    ~headers:[ ("api-key", database.key) ]
    (origin database) meth path

let answered what (reply : Call.reply) =
  if reply.status <> 200 then
    failwith
      (Printf.sprintf "Qdrant answered %d when asked to %s: %s" reply.status
         what (Call.shown reply.text));
  member "result" (Option.get reply.json)

let collections_at database =
  List.map
    (fun c -> Yojson.Safe.Util.(member "name" c |> to_string))
    (Yojson.Safe.Util.to_list
       (member "collections"
          (answered "list its collections"
             (ask database "GET" "/collections" None))))

type index = { database : database; names : (string * string) list }

let index database did = { database; names = collection_names did }

let named index word =
  match List.assoc_opt word index.names with
  | Some name -> name
  | None ->
      failwith
        (Printf.sprintf "specs/src/%s states no collection of %s" payload_table
           word)

(* Every point of a collection that a filter takes, a page at a time, with
   its vector when [vectors]; none of a collection that is not there. *)
let scroll ?(vectors = false) index word filter =
  let name = named index word in
  let rec page offset acc =
    let reply =
      ask index.database "POST"
        ("/collections/" ^ name ^ "/points/scroll")
        (Some
           (`Assoc
              ([
                 ("limit", `Int 256);
                 ("with_payload", `Bool true);
                 ("with_vector", `Bool vectors);
               ]
              @ (match filter with Some f -> [ ("filter", f) ] | None -> [])
              @ match offset with Some o -> [ ("offset", o) ] | None -> [])))
    in
    if reply.status = 404 then []
    else
      let result = answered ("list the points of " ^ name) reply in
      let points = acc @ Yojson.Safe.Util.to_list (member "points" result) in
      match member "next_page_offset" result with
      | `Null -> points
      | next -> page (Some next) points
  in
  page None []

let where key value =
  `Assoc
    [
      ( "must",
        `List
          [
            `Assoc
              [
                ("key", `String key);
                ("match", `Assoc [ ("value", `String value) ]);
              ];
          ] );
    ]

let payload point = member "payload" point
let points ?vectors index word = scroll ?vectors index word None

(* The points of one resource: of the resources, its chunks in order; of
   the annotations, the annotations on it; of the declined, its decline. *)
let points_of ?vectors index word resource_id =
  let found =
    scroll ?vectors index word
      (Some (where (payload_member word "resourceId") resource_id))
  in
  if word = "resources" then
    let chunk = payload_member "resources" "chunkIndex" in
    List.sort
      (fun a b -> compare (member chunk (payload a)) (member chunk (payload b)))
      found
  else found

(* The point of one annotation: None while the index holds none. *)
let annotation_point ?vectors index annotation_id =
  match
    scroll ?vectors index "annotations"
      (Some (where (payload_member "annotations" "annotationId") annotation_id))
  with
  | [] -> None
  | [ point ] -> Some point
  | points ->
      failwith
        (Printf.sprintf "the index holds %d points of the annotation %s"
           (List.length points) annotation_id)

(* How many points a collection holds; none while it is not there. *)
let count index word =
  let name = named index word in
  let reply =
    ask index.database "POST"
      ("/collections/" ^ name ^ "/points/count")
      (Some (`Assoc [ ("exact", `Bool true) ]))
  in
  if reply.status = 404 then 0
  else
    Yojson.Safe.Util.to_int
      (member "count" (answered ("count the points of " ^ name) reply))

(* Take a resource's points out of a collection. *)
let delete_points_of index word resource_id =
  let name = named index word in
  ignore
    (answered
       (Printf.sprintf "take the points of %s out of %s" resource_id name)
       (ask index.database "POST"
          ("/collections/" ^ name ^ "/points/delete?wait=true")
          (Some
             (`Assoc
                [
                  ( "filter",
                    where (payload_member word "resourceId") resource_id );
                ]))))

(* What a collection's vectors are compared by, in Qdrant's word, as the
   table states it. *)
let distance word =
  let c = (Protocol.tables ()).vectors_payload_cases.collections in
  match word with
  | "resources" -> c.resources.distance
  | "annotations" -> c.annotations.distance
  | "declined" -> c.declined.distance
  | other -> failwith ("the harness reads no collection " ^ other)

(* Make a collection that is not there, [size] numbers wide and compared as
   the table states, as a store makes one. *)
let make index word size =
  let name = named index word in
  match
    answered
      ("make the collection " ^ name)
      (ask index.database "PUT" ("/collections/" ^ name)
         (Some
            (`Assoc
               [
                 ( "vectors",
                   `Assoc
                     [
                       ("size", `Int size); ("distance", `String (distance word));
                     ] );
               ])))
  with
  | `Bool true -> ()
  | other ->
      failwith
        (Printf.sprintf "Qdrant did not make the collection %s: it answered %s"
           name
           (Yojson.Safe.to_string other))

(* Put points into a collection, each an id, a payload and a vector. *)
let put index word points =
  let name = named index word in
  ignore
    (answered
       (Printf.sprintf "put %d point(s) into %s" (List.length points) name)
       (ask index.database "PUT"
          ("/collections/" ^ name ^ "/points?wait=true")
          (Some (`Assoc [ ("points", `List points) ]))))

(* The id Qdrant keys a point by: the MD5 of its key's UTF-8 bytes, its 32
   hexadecimal digits in a UUID's groups, with no bit changed. *)
let id_arithmetic key =
  let hex = Digestif.MD5.(to_hex (digest_string key)) in
  String.concat "-"
    [
      String.sub hex 0 8;
      String.sub hex 8 4;
      String.sub hex 12 4;
      String.sub hex 16 4;
      String.sub hex 20 12;
    ]

module Payload_cases = Xynolog_protocol.Tables.Vectors_payload_cases

(* Each point the table states, by its key: a chunk's, an annotation's and
   a decline's. *)
let stated_ids () =
  List.concat_map
    (function
      | Payload_cases.Resource_stored { why; resource_stored = r } ->
          List.map
            (fun (point : Payload_cases.points_item) ->
              ( why,
                Printf.sprintf "%s-%d" point.payload.resource_id
                  point.payload.chunk_index,
                point.qdrant_id ))
            r.points
      | Annotation_stored { why; annotation_stored = a } ->
          [ (why, a.annotation_id, a.point.qdrant_id) ]
      | Decline_stored { why; decline_stored = d } ->
          [ (why, d.resource_id, d.point.qdrant_id) ]
      | _ -> [])
    (Protocol.tables ()).vectors_payload_cases.cases

let ids_checked =
  lazy
    (List.iter
       (fun (why, key, stated) ->
         if id_arithmetic key <> stated then
           failwith
             (Printf.sprintf
                "vectors/payload-cases.json: \"%s\": the arithmetic gives %s \
                 for %s, the case says %s"
                why (id_arithmetic key) key stated))
       (stated_ids ()))

let qdrant_id key =
  Lazy.force ids_checked;
  id_arithmetic key

(* Clear the database: every collection of every knowledge base gone, as a
   wiped volume leaves it. *)
let clear index =
  List.iter
    (fun name ->
      ignore
        (answered
           ("delete the collection " ^ name)
           (ask index.database "DELETE" ("/collections/" ^ name) None)))
    (collections_at index.database)
